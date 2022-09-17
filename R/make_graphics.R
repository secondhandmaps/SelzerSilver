results_df_w = outcome_df_w |>
  summarize(p_r_win_senate = sum(`R Win Senate` * weights) / sum(weights),
            p_r_win_house = sum(`R Win House` * weights) / sum(weights))

senate_histogram_df = outcome_df_w |>
  group_by(`D Senate Seats`) |>
  summarize(n_outcome = sum(weights)) |>
  mutate(p_outcome = n_outcome/sum(n_outcome),
         majority_party = ifelse(`D Senate Seats` >= 50, "DEM", "REP"))

ggplot(senate_histogram_df, aes(x = `D Senate Seats`)) +
  geom_col(aes(y = p_outcome, fill = majority_party),
           width = 1, color = "gray80") +
  scale_fill_manual(values = c("blue", "red"), name = "") +
  ggthemes::theme_economist(base_size = 13) +
  labs(x = "DEM Seats", y = "Probability", title = "Senate Outcomes")

house_histogram_df = outcome_df_w |>
  filter(weights >= 0.01) |>
  group_by(`D House Seats`) |>
  summarize(n_outcome = sum(weights)) |>
  mutate(p_outcome = n_outcome/sum(n_outcome),
         majority_party = ifelse(`D House Seats` >= 218, "DEM", "REP"))

ggplot(house_histogram_df, aes(x = `D House Seats`)) +
  geom_col(aes(y = p_outcome, fill = majority_party),
           width = 1, color = "gray80", size = 0.3) +
  scale_fill_manual(values = c("blue", "red"), name = "") +
  ggthemes::theme_economist(base_size = 13) +
  labs(x = "DEM Seats", y = "Probability", title = "House Outcomes")

pie_df_w = results_df_w |>
  mutate(p_d_win_senate = 1 - p_r_win_senate,
         p_d_win_house = 1 - p_r_win_house) |>
  pivot_longer(names_to = c("party", "house"), names_pattern = "p_(.)_win_(.+)",
               values_to = "prob",
               cols = everything())

ggplot(pie_df_w, aes(x="", y=prob, fill=party)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_manual(values = c("blue", "red"), name = "") +
  geom_text(aes(label = paste0(round(prob*100), "%"), color = party),
            position = position_stack(vjust=0.5), show.legend = FALSE) +
  scale_color_manual(values = c(d = "white", r = "black"), ) +
  facet_wrap("house")
