library(dplyr)
library(ggplot2)
library(tidyr)

# Data
outcome_df_w_all = readRDS("data/all_outcomes.RDS")
outcome_df_w = outcome_df_w_all %>%
  slice_max(timestamp, with_ties = TRUE)
biden_blue = "#003c8e" #Biden Blue
trump_red = "#c90627"
selzer_polls = readRDS("data/selzer_polls.RDS")

# Functions
transparent_background = function() {
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )
}

wquantile.generic <- function(x, probs, cdf.gen, weights = NA) {
  n <- length(x)
  if (any(is.na(weights)))
    weights <- rep(1 / n, n)
  nw <- sum(weights)^2 / sum(weights^2) # Kish's effective sample size

  indexes <- order(x)
  x <- x[indexes]
  weights <- weights[indexes]

  weights <- weights / sum(weights)
  cdf.probs <- cumsum(c(0, weights))

  sapply(probs, function(p) {
    cdf <- cdf.gen(nw, p)
    q <- cdf(cdf.probs)
    w <- tail(q, -1) - head(q, -1)
    sum(w * x)
  })
}

wquantile <- function(x, probs, weights = NA) {
  cdf.gen <- function(n, p) return(function(cdf.probs) {
    h <- p * (n - 1) + 1
    u <- pmax((h - 1) / n, pmin(h / n, cdf.probs))
    u * n - h + 1
  })
  wquantile.generic(x, probs, cdf.gen, weights)
} # From https://aakinshin.net/posts/weighted-quantiles/


# Processed Data
results_df_w = outcome_df_w |>
  summarize(p_r_win_senate = sum(`R Win Senate` * weights) / sum(weights),
            p_r_win_house = sum(`R Win House` * weights) / sum(weights))

results_df_w_dem = outcome_df_w |>
  summarize(p_d_win_Senate = 1 - sum(`R Win Senate` * weights) / sum(weights),
            p_d_win_House = 1 - sum(`R Win House` * weights) / sum(weights))

donut_df1 <- results_df_w_dem |>
  pivot_longer(names_prefix = "p_d_win_", names_to = "house", cols = everything()) |>
  mutate(names = "positive")
donut_df2 = donut_df1 |>
  mutate(value = 1 - value,
         names = "negative")
donut_df = bind_rows(donut_df1, donut_df2) |>
  mutate(x = 1,
         house = factor(house, levels = c("Senate", "House")),
         label = ifelse(names == "positive",
                        scales::percent(value, accuracy = 1),
                        ""))
big_number_text_label <- scales::percent(donut_df1$value, accuracy = 1)

dem_bounds_df_w = outcome_df_w |>
  summarize(d_lower_bound_senate = wquantile(`D Senate Seats`, 0.1, weights),
            d_upper_bound_senate = wquantile(`D Senate Seats`, 0.9, weights),
            d_lower_bound_house = wquantile(`D House Seats`, 0.1, weights),
            d_upper_bound_house = wquantile(`D House Seats`, 0.9, weights))

senate_histogram_df = outcome_df_w |>
  group_by(`D Senate Seats`) |>
  summarize(n_outcome = sum(weights)) |>
  mutate(p_outcome = n_outcome/sum(n_outcome),
         majority_party = ifelse(`D Senate Seats` >= 50, "DEM", "REP"))

house_histogram_df = outcome_df_w |>
  # filter(weights >= 0.01) |>
  group_by(`D House Seats`) |>
  summarize(n_outcome = sum(weights)) |>
  mutate(p_outcome = n_outcome/sum(n_outcome),
         majority_party = ifelse(`D House Seats` >= 218, "DEM", "REP"))



# Donut Plot

caption1 = paste0("Date of FiveThirtyEight simulations: ",
format(max(outcome_df_w_all$timestamp), "%d %b %Y"))
caption2 = paste0("Date of most recent Selzer poll: ",
                  format(max(selzer_polls$end_date), "%d %b %Y"))
g_donut = ggplot(donut_df,
                 aes(x = x,
                     y = value,
                     fill = names)) +
  geom_col(show.legend = FALSE) +
  coord_polar(theta = "y",
              direction = 1) +
  xlim(c(-2, 2)) +

  scale_fill_manual(values = c(positive = biden_blue, negative = "grey90")) +

  theme_void() +
  facet_wrap("house") +
  geom_text(aes(x = -2, y = 0, label = label), fontface = "bold",  color = biden_blue,
            size = 14) +
  labs(caption = paste(caption1, caption2, sep = "\n")) +
  theme(strip.text.x = element_text(size = 30),
        plot.caption = element_text(size = 13, color = "gray60", hjust = 0.95))


g_donut_wordy =
g_donut +
  labs(caption = paste(caption1, caption2, sep = "\n"),
       title = " Probability of the Democrats\n controlling each house of Congress") +
  theme(strip.text.x = element_text(size = 20),
        plot.caption = element_text(size = 13, color = "gray60", hjust = 1,
                                    margin = margin(b = 10)),
        plot.title.position = "panel",
        plot.title = element_text(size = 25, margin = margin(b = 20, t = 15), hjust = 0))


# Senate plot
g_senate = ggplot(senate_histogram_df, aes(x = `D Senate Seats`)) +
  geom_rect(aes(xmin = dem_bounds_df_w$d_lower_bound_senate,
                xmax = dem_bounds_df_w$d_upper_bound_senate,
                ymin = -Inf, ymax = Inf), fill = "gray70", alpha = 0.1) +
  geom_col(aes(y = p_outcome, fill = majority_party),
           width = 1, color = "gray80", show.legend = FALSE) +
  scale_fill_manual(values = c(biden_blue, trump_red), name = "") +
  ggthemes::theme_economist(base_size = 18) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20, hjust = 0),
        panel.grid.major.y = element_line(color = "gray70")) +
  labs(x = "Democractic Seats", y = "Probability", title = "Senate Outcomes") +
  scale_y_continuous(limits = c(0, max(senate_histogram_df$p_outcome)*1.05), expand = c(0, 0)) +
  transparent_background()

g_house = ggplot(house_histogram_df, aes(x = `D House Seats`)) +
  geom_rect(aes(xmin = dem_bounds_df_w$d_lower_bound_house,
                xmax = dem_bounds_df_w$d_upper_bound_house,
                ymin = -Inf, ymax = Inf), fill = "gray70", alpha = 0.1) +
  geom_col(aes(y = p_outcome, fill = majority_party),
           width = 1, color = "gray80", size = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c(biden_blue, trump_red), name = "") +
  ggthemes::theme_economist(base_size = 18) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20, hjust = 0),
        panel.grid.major.y = element_line(color = "gray70")) +
  labs(x = "Democractic Seats", y = "Probability", title = "House Outcomes") +
  scale_y_continuous(limits = c(0, max(house_histogram_df$p_outcome)*1.05), expand = c(0, 0)) +
  transparent_background()

# Save files
w = 1344
h = 960
s = 2.7

ragg::agg_png("figures/g_senate.png", width = w, height = h, units = "px",
              background = "transparent", scaling = s)
print(g_senate)
dev.off()

ragg::agg_png("figures/g_house.png", width = w, height = h, units = "px",
              background = "transparent", scaling = s)
print(g_house)
dev.off()

ragg::agg_png("figures/g_donut.png", width = w, height = h, units = "px",
              background = "transparent", scaling = s)
print(g_donut)
dev.off()

ragg::agg_png("figures/g_donut_wordy.png", width = w, height = h, units = "px",
              background = "transparent", scaling = s)
print(g_donut_wordy)
dev.off()

page_fn = "../shc/content/SelzerSilver_md.md"
if (file.exists(page_fn)) {
  lines = readLines(page_fn)
  house_line = grepl("seats in the House", lines)
  senate_line = grepl("seats in the Senate", lines)
  overall_line = grepl("winning the Senate", lines)

  new_overall_text = paste0("The Democrats currently have a **",
                            scales::percent(results_df_w_dem$p_d_win_Senate),
                            "** chance of winning the Senate and a **",
                            scales::percent(results_df_w_dem$p_d_win_House),
                            "** chance of winning the House.")
  new_senate_text = paste0("The Democrats have an 80% chance of controlling between **",
                           round(dem_bounds_df_w$d_lower_bound_senate), "** and **",
                           round(dem_bounds_df_w$d_upper_bound_senate),
                           "** seats in the Senate.")
  new_house_text = paste0("The Democrats have an 80% chance of controlling between **",
                          round(dem_bounds_df_w$d_lower_bound_house), "** and **",
                          round(dem_bounds_df_w$d_upper_bound_house),
                          "** seats in the House")
  lines[overall_line] = new_overall_text
  lines[house_line] = new_house_text
  lines[senate_line] = new_senate_text
  writeLines(lines, page_fn)
}
