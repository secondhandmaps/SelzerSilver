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
polls_df = read_csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv")
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
                         sd = latest_d_r_split$moe/2))

outcome_df_save_full = outcome_df_w |>
  mutate(timestamp = now(tzone = "UTC"))
saveRDS(outcome_df_save_full, "data/all_outcomes_full.RDS")