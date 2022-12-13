library(rjson)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

current_538_sims =
  fromJSON(file="https://projects.fivethirtyeight.com/2022-flip-senate-house/simmed-maps.json")

states <- current_538_sims$states
n_sims <- length(current_538_sims$maps)
sims <- array(NA, c(n_sims, length(states) + 6), dimnames=list(NULL, c("R Win Senate", "R Senate Seats", "D Senate Seats", "R Win House", "R House Seats", "D House Seats", states)))
for (i in 1:n_sims){
  sims[i,] <- current_538_sims$maps[[i]]
}
sims_df = sims |>
  as_tibble()
r_share_df = sims_df |>
  mutate(across(`AK-S3`:`WI-S3`, \(x) (x + 100)/2))
# That's our data.frame of the current 538 simulations

# Now read in the polls and pull the latest Selzer poll
polls_df = read_csv("https://projects.fivethirtyeight.com/polls/data/senate_polls.csv")
latest_selzer_poll = polls_df |>
  filter(pollster_id == 437,# Selzer & Co
         race_id == 8930) |> # Grassley/Franken senate race
  mutate(end_date = mdy(end_date),
         start_date = mdy(start_date)) |>
  slice_max(end_date, with_ties = TRUE)

# The number we need is the D v. R split
latest_d_r_split = latest_selzer_poll |>
  group_by(party, start_date, end_date, sample_size, population, poll_id) |>
  summarize(pct = sum(pct, na.rm = TRUE), .groups = "drop") |>
  mutate(pct_2party = pct/sum(pct)*100) |>
  pivot_wider(names_from = "party", values_from = "pct_2party",
              id_cols = -pct) |>
  mutate(r_lead = REP - DEM,
         r_decimal = REP/100,
         moe = sqrt(r_decimal*(1-r_decimal)/sample_size)*200)

# Now we're ready to start calculating weights
outcome_df_w = r_share_df |>
  mutate(weights = dnorm(`IA-S3`,
                         mean = latest_d_r_split$REP,
                         sd = latest_d_r_split$moe/2)) |>
  select(`R Win Senate`:`D House Seats`, weights, `IA-S3`)



# Now save to disk
# We need to save the results of our forecast and the latest poll (if it's new)
# I think the easiest/safest thing to do is just to save the outcome data
# itself, since we can recreate everything from that
outcome_df_save = outcome_df_w |>
  filter(weights >= 0.01) |>
  mutate(timestamp = now(tzone = "UTC"))
outcome_fn = "data/all_outcomes.RDS"
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

# We'll also save the Selzer polls, if we haven't already.
poll_fn = "data/selzer_polls.RDS"
if (file.exists(poll_fn)) {
  prev_polls = readRDS(poll_fn)
  if (!latest_d_r_split$poll_id %in% prev_polls$poll_id) {
    all_polls = bind_rows(prev_polls, latest_d_r_split)
    saveRDS(all_polls, poll_fn)
  }
} else {
  saveRDS(latest_d_r_split, poll_fn)
}

