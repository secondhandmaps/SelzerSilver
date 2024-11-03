library(rjson)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(mvtnorm)
library(MASS)

current_538_outcomes = read_csv("https://static.dwcdn.net/data/EFZGh.csv",
                                col_types = cols(
                                  .default = col_double(),
                                  state = col_character(),
                                )) |>
  mutate(margin_med = coalesce(margin_d, margin_r, margin_n), .keep = "unused")

current_538_rshare = current_538_outcomes |>
  mutate(r_share_hi = (margin_hi+100)/2,
         r_share_lo = (margin_lo+100)/2,
         r_share_med = (margin_med+100)/2,
         .keep = "unused")

# Now I'm going to pull some of the data for specific states to figure out degrees of freedom.
states = c("az" = "https://static.dwcdn.net/data/tkiXZ.csv",
           "fl" = "https://static.dwcdn.net/data/cZ8v2.csv",
           "ga" = "https://static.dwcdn.net/data/Fl0Dr.csv",
           "ia" = "https://static.dwcdn.net/data/Q24mb.csv",
           "mi" = "https://static.dwcdn.net/data/rqRDK.csv",
           "mn" = "https://static.dwcdn.net/data/GhMxp.csv",
           "nv" = "https://static.dwcdn.net/data/B41jG.csv",
           "nh" = "https://static.dwcdn.net/data/obXtM.csv",
           "nc" = "https://static.dwcdn.net/data/oPN5W.csv",
           "pa" = "https://static.dwcdn.net/data/5JXY1.csv",
           "tx" = "https://static.dwcdn.net/data/1izHO.csv",
           "va" = "https://static.dwcdn.net/data/7xrN1.csv",
           "wi" = "https://static.dwcdn.net/data/tUf6Y.csv")
f = function(url, state) {
  outcomes = read_csv(url,
                      col_types = cols(.default = col_double()))
  draws = sample(outcomes$voteshare, size = 40000, replace = TRUE, prob = outcomes$voteshareprob_chal)
  draws = draws - mean(draws)
  res = fitdistr(draws, "t")$estimate
  data.frame(state = state, df = res["df"], var = var(draws))
}
library(purrr)
fits = map2_dfr(states, names(states), f)

# I mean, honestly, the tails don't look that big - like we're dealing with 10+ df
# But maybe I should use it anyway? I'm not sure.
# Ok, that helps.
mean(fits$df)
sd(fits$df)

# Now we need to calculate variances... no, ok, we have that already.
# So we can go straight to calculating covariances:
corr_matrix_raw = read_csv("data/2024_correlation_matrix.csv")
corr_matrix = corr_matrix_raw |>
  dplyr::select(-State) |>
  rename_with(tolower) |>
  as.matrix()
rownames(corr_matrix) = colnames(corr_matrix)
sds_raw = fits
rownames(sds_raw) = sds_raw$state
sds = sds_raw[colnames(corr_matrix), ]
sds$sd = sqrt(sds$var)
cov_matrix = corr_matrix * NA
for (ii in seq_len(nrow(corr_matrix))) {
  for (jj in seq_len(ncol(corr_matrix))) {
    if (ii == jj) {
      cov_matrix[ii, jj] = 1 * sds$sd[ii] * sds$sd[jj]
    } else {
      cov_matrix[ii, jj] = corr_matrix[ii, jj] * sds$sd[ii] * sds$sd[jj]
    }
  }
}
df_avg = mean(sds$df)
df_avg
Sigma = cov_matrix * (df_avg-2)/df_avg
state_abbr_lookup = c("Iowa" = "IA",
                      "Michigan" = "MI",
                      "Pennsylvania" = "PA",
                      "Wisconsin" = "WI",
                      "Georgia" = "GA",
                      "North Carolina" = "NC",
                      "Arizona" = "AZ",
                      "Nevada" = "NV")
current_538_rshare$state_abbr = tolower(state_abbr_lookup[current_538_rshare$state])
limited_outcomes = current_538_rshare |>
  filter(!is.na(state_abbr)) |>
  as.data.frame()
rownames(limited_outcomes) = limited_outcomes$state_abbr
delta = limited_outcomes[rownames(Sigma), ]$r_share_med
mockup_538_sims = rmvt(40000, sigma = Sigma, df = df_avg, delta = delta,
                       type = "shifted")

colnames(mockup_538_sims) = colnames(Sigma)
q = as.data.frame(mockup_538_sims)
pa_outcomes = read_csv(states["pa"],
                    col_types = cols(.default = col_double()))
pa_draws = sample(pa_outcomes$voteshare, size = 40000, replace = TRUE, prob = pa_outcomes$voteshareprob_chal)
library(ggplot2)
ggplot(q) +
  geom_freqpoly(aes(x = pa)) +
  geom_freqpoly(data = data.frame(pa = pa_draws), aes(x = pa), color = "red")
# It's not perfect, but I think it's good enough for now.


# Now read in the polls and pull the latest Selzer poll
# This was Harris 47, Trump 44, 808 LV
latest_selzer_poll = data.frame(REP = 44, DEM = 47, sample_size = 808)
# polls_df = read_csv("https://projects.fivethirtyeight.com/polls/data/senate_polls.csv")
# latest_selzer_poll = polls_df |>
#   filter(pollster_id == 437,# Selzer & Co
#          race_id == 8930) |> # Grassley/Franken senate race
#   mutate(end_date = mdy(end_date),
#          start_date = mdy(start_date)) |>
#   slice_max(end_date, with_ties = TRUE)

# The number we need is the D v. R split
latest_d_r_split = latest_selzer_poll |>
  # group_by(party, start_date, end_date, sample_size, population, poll_id) |>
  # summarize(pct = sum(pct, na.rm = TRUE), .groups = "drop") |>
  # mutate(pct_2party = pct/sum(pct)*100) |>
  # pivot_wider(names_from = "party", values_from = "pct_2party",
  #             id_cols = -pct) |>
  mutate(total = REP + DEM,
         REP = REP/total*100,
         DEM = DEM/total*100) |>
  mutate(r_lead = REP - DEM,
         r_decimal = REP/100,
         moe = sqrt(r_decimal*(1-r_decimal)/sample_size)*200)

# Now we're ready to start calculating weights
r_share_df = as_tibble(as.data.frame(mockup_538_sims))
outcome_df_w = r_share_df |>
  mutate(weights = dnorm(ia,
                         mean = latest_d_r_split$REP,
                         sd = latest_d_r_split$moe/2))

winner_7_states = outcome_df_w |>
  mutate(across(mi:nv, \(x) ifelse(x > 50, "R", "D"))) |>
  group_by(pick(mi:nv)) |>
  summarize(prob = sum(weights),
            .groups = "drop") |>
  mutate(prob = prob/sum(prob)) |>
  arrange(desc(prob))
paths_to_victory = read_csv("https://static.dwcdn.net/data/SPUuh.csv")
colnames(paths_to_victory ) = tolower(colnames(paths_to_victory))
df2 = paths_to_victory |>
  dplyr::select(az:wi, p_harris_wins_given_outcome = winner_inc)
prob_harris_wins = left_join(winner_7_states, df2) |>
  summarize(p_harris_wins = sum(prob * p_harris_wins_given_outcome/100))
new_col = outcome_df_w |>
  mutate(across(mi:nv, \(x) ifelse(x > 50, "R", "D"))) |>
  left_join(df2)
outcome_df_w$prob_harris_wins_given_outcome = new_col$p_harris_wins_given_outcome/100

# Now save to disk
# We need to save the results of our forecast and the latest poll (if it's new)
# I think the easiest/safest thing to do is just to save the outcome data
# itself, since we can recreate everything from that
outcome_df_save = outcome_df_w |>
  # filter(weights >= 0.01) |>
  mutate(timestamp = now(tzone = "UTC"))
outcome_fn = "data/2024_all_outcomes.RDS"
if (file.exists(outcome_fn)) {
  prev_outcomes = readRDS(outcome_fn)
  last_day_ran = floor_date(max(prev_outcomes$timestamp), "day")
  curr_day = floor_date(outcome_df_save$timestamp[[1]], "day")
  if (last_day_ran != curr_day) {
    curr_outcomes = bind_rows(prev_outcomes, outcome_df_save)
    saveRDS(curr_outcomes, outcome_fn)
  }
} else {
  saveRDS(outcome_df_save, outcome_fn)
}
#
# # We'll also save the Selzer polls, if we haven't already.
# poll_fn = "data/selzer_polls.RDS"
# if (file.exists(poll_fn)) {
#   prev_polls = readRDS(poll_fn)
#   if (!latest_d_r_split$poll_id %in% prev_polls$poll_id) {
#     all_polls = bind_rows(prev_polls, latest_d_r_split)
#     saveRDS(all_polls, poll_fn)
#   }
# } else {
#   saveRDS(latest_d_r_split, poll_fn)
# }

