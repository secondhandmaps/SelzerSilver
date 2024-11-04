library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(forcats)

# Data
outcome_df_w_all = readRDS("data/2024_all_outcomes.RDS")
outcome_df_w = outcome_df_w_all %>%
  slice_max(timestamp, with_ties = TRUE)
biden_blue = "#003c8e" #Biden Blue
trump_red = "#c90627"
# selzer_polls = readRDS("data/selzer_polls.RDS")

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
  summarize(across(ia:nv, \(x) sum((x > 50) * weights) / sum(weights)),
            overall = sum(weights * (1-prob_harris_wins_given_outcome))/sum(weights))

results_df_w_dem = outcome_df_w |>
  summarize(across(ia:nv, \(x) sum((x < 50) * weights) / sum(weights)),
            overall = sum(weights * (prob_harris_wins_given_outcome))/sum(weights))


donut_df1 <- results_df_w_dem |>
  pivot_longer(names_to = "location", cols = everything()) |>
  mutate(names = "positive")
donut_df2 = donut_df1 |>
  mutate(value = 1 - value,
         names = "negative")
donut_df = bind_rows(donut_df1, donut_df2) |>
  filter(location == "overall") |>
  mutate(x = 1,
         # house = factor(house, levels = c("Senate", "House")),
         label = ifelse(names == "positive",
                        scales::percent(value, accuracy = 1),
                        ""))
big_number_text_label <- scales::percent(donut_df1$value, accuracy = 1)


r_bounds_df_w =
  outcome_df_w |>
  pivot_longer(ia:nv, names_to = "state") |>
    group_by(state) |>
    summarize(r_lower_bound = wquantile(value, 0.1, weights),
              r_upper_bound = wquantile(value, 0.9, weights),
              r_med = wquantile(value, 0.5, weights))

# senate_histogram_df = outcome_df_w |>
#   group_by(`D Senate Seats`) |>
#   summarize(n_outcome = sum(weights)) |>
#   mutate(p_outcome = n_outcome/sum(n_outcome),
#          majority_party = ifelse(`D Senate Seats` >= 50, "DEM", "REP"))
#
# house_histogram_df = outcome_df_w |>
#   # filter(weights >= 0.01) |>
#   group_by(`D House Seats`) |>
#   summarize(n_outcome = sum(weights)) |>
#   mutate(p_outcome = n_outcome/sum(n_outcome),
#          majority_party = ifelse(`D House Seats` >= 218, "DEM", "REP"))



# Donut Plot

caption1 = paste0("Date of Silver Bulletin simulations: ",
format(max(outcome_df_w_all$timestamp), "%d %b %Y"))
caption2 = paste0("Date of most recent Selzer poll: ",
                  format(dmy("31 October 2024"), "%d %b %Y"))
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
  # facet_wrap("location") +
  geom_text(aes(x = -2, y = 0, label = label), fontface = "bold",  color = biden_blue,
            size = 14) +
  labs(caption = paste(caption1, caption2, sep = "\n")) +
  theme(strip.text.x = element_text(size = 30),
        plot.caption = element_text(size = 13, color = "gray60", hjust = 0.95))


g_donut_wordy =
g_donut +
  labs(caption = paste(caption1, caption2, sep = "\n"),
       title = "Probability of Harris winning\nthe presidency") +
  theme(strip.text.x = element_text(size = 20),
        plot.caption = element_text(size = 13, color = "gray60", hjust = 1,
                                    margin = margin(b = 10)),
        plot.title.position = "panel",
        plot.title = element_text(size = 25, margin = margin(b = 20, t = 15), hjust = 0))



# State plot
q = c("Iowa" = "IA",
"Michigan" = "MI",
"Pennsylvania" = "PA",
"Wisconsin" = "WI",
"Georgia" = "GA",
"North Carolina" = "NC",
"Arizona" = "AZ",
"Nevada" = "NV")
state_name_lookup = setNames(names(q), tolower(q))

state_plot_df = left_join(r_bounds_df_w, donut_df1, by = c(state = "location")) |>
  mutate(p_harris_wins = scales::percent(value, accuracy = 1)) |>
  select(-names, -value) |>
  mutate(state_name = state_name_lookup[state]) |>
  mutate(state_name = fct_reorder(state_name, 1-r_med))


g_states = ggplot(state_plot_df, aes(x = state_name)) +
  geom_hline(yintercept = 50, color = "gray60", linetype = "solid") +
  geom_linerange(aes(ymin = r_lower_bound, ymax = r_upper_bound), color = "gray40", linewidth = 1) +
  geom_point(aes(y = r_med, color = r_med > 50), size = 3, show.legend = FALSE) +
  geom_label(aes(y = r_upper_bound, label = p_harris_wins, color = r_med>50), hjust = 0,
             show.legend = FALSE) +
  scale_color_manual(values = c(biden_blue, trump_red)) +
  ylab("Predicted Republican 2-party vote share") +
  xlab("") +
  ggthemes::theme_economist(base_size = 18) +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          plot.title = element_text(size = 20, hjust = 0),
          panel.grid.major.y = element_line(color = "gray90", linewidth = 0.4),
          panel.grid.major.x = element_line(color = "gray80", linetype = "solid")) +
    labs(title = "State Outcomes", subtitle = "Dots show the median outcome with 80% uncertainty interval\nBoxes show the probability of Harris winning each state") +
  scale_y_continuous(breaks = c(46, 48, 50, 52, 54), limits = c(45, 54.5)) +
    transparent_background() +
  coord_flip()



# Save files
w = 1344
h = 960
s = 2.7

ragg::agg_png("figures/g_states_2024.png", width = w, height = h, units = "px",
              background = "transparent", scaling = s)
print(g_states)
dev.off()


ragg::agg_png("figures/g_donut_2024.png", width = w, height = h, units = "px",
              background = "transparent", scaling = s)
print(g_donut)
dev.off()

ragg::agg_png("figures/g_donut_wordy_2024.png", width = w, height = h, units = "px",
              background = "transparent", scaling = s)
print(g_donut_wordy)
dev.off()

page_fn = "../shc/content/SelzerSilver.md"
if (file.exists(page_fn)) {
  lines = readLines(page_fn)
  overall_line = grepl("winning the Presidency", lines)

  new_overall_text = paste0("The Democrats currently have a **",
                            scales::percent(results_df_w_dem$overall),
                            "** chance of winning the Presidency")
  lines[overall_line] = new_overall_text
  writeLines(lines, page_fn)
}
