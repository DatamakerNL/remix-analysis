# Sheet to analyse remix throwing data

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(gghighlight)
library(gt)

# Load data ---------------------------------------------------------------

throwing.df <- read.csv("data/REMIX THROWING.csv")

# Data cleaning -----------------------------------------------------------

throwing.df <- 
throwing.df %>%
  pivot_longer(cols = 3:last_col(),
               names_to = c("side", "distance"),
               names_sep = "_",
               values_to = "accuracy"
               )

# Analysis ----------------------------------------------------------------

# make a table of the average accuracy by distance
# Add icons for the accuracy a range from 1-5 stars, with 5 stars being the best
throwing.df %>%
  group_by(side, distance) %>%
  summarise(mean_accuracy = mean(accuracy, na.rm = TRUE)/2 %>% round(1),
            n = sum(accuracy > 0, na.rm = TRUE)
            ) %>%
  ungroup() %>%
  dplyr::mutate(rating = dplyr::case_when(
    mean_accuracy %% 2 == 0 ~ strrep("star,", mean_accuracy),
    mean_accuracy %% 2 != 0 ~ paste0(strrep("star,", floor(mean_accuracy)), "star-half")
  )) %>% 
  group_by(side) %>%
  gt() |>
  fmt_icon(
    columns = rating,
    fill_color = "orange"
  ) %>%
  cols_hide(c(mean_accuracy,n))
  
# make a ridge plot of accuracy by distance
throwing.df %>%
  ggplot(aes(x = accuracy, y = distance, fill = side)) +
  geom_density_ridges(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 10),
                     breaks = 1:10
                     ) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  labs(title = "Accuracy by distance",
       x = "Accuracy",
       y = "Distance (m)")

# make a scatter plot
# each thrower is a point
# the 10 and 15 meters on average are short throws (make a mean)
# the 25 and 30 meters on average are long throws (make a mean)
# the x axis is the BH accuracy
# the y axis is the FH accuracy
# facet by short and long throws
throwing.short_long <-
throwing.df %>%
  group_by(Name, side) %>%
  summarise(short = mean(accuracy[distance %in% c("10", "15")], na.rm = TRUE),
            long = mean(accuracy[distance %in% c("25", "30")], na.rm = TRUE)
            ) %>%
  pivot_longer(cols = c(short, long), names_to = "distance") %>%
  pivot_wider(names_from = side, values_from = value) %>%
  mutate(
    distance = factor(distance, levels = c("short", "long"),
                      labels = c("Short (10-15m)", "Long (25-30m)")
                      )
  ) 

throwing.short_long %>%
  ggplot(aes(x = BH, y = FH)) +
  ggrepel::geom_text_repel(aes(label = Name),
                           nudge_x = -.5,
                           nudge_y = +.5
                           ) +
  geom_point() +
  scale_x_continuous(limits = c(0, 10),
                     breaks = 1:10
                     ) +
  scale_y_continuous(limits = c(0, 10),
                     breaks = 1:10
                     ) +
  # add a diagonal line at 5
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~distance) +
  theme_bw() +
  labs(x = "Backhand",
       y = "Forehand")

# make one table with the overal accuracy split by backhand and forehand
throwing_accuracy <-
  throwing.df %>%
  group_by(Name, side) %>%
  summarise(accuracy = mean(accuracy, na.rm = TRUE)) %>%
  pivot_wider(names_from = side, values_from = accuracy)

throwing_accuracy %>%
  ggplot(aes(x = BH, y = FH)) +
  ggrepel::geom_text_repel(aes(label = Name),
                           nudge_x = -.5,
                           nudge_y = +.5
  ) +
  geom_point() +
  scale_x_continuous(limits = c(0, 10),
                     breaks = 1:10
  ) +
  scale_y_continuous(limits = c(0, 10),
                     breaks = 1:10
  ) +
  # add a diagonal line at 5
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_bw() +
  labs(x = "Backhand",
       y = "Forehand")
