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
  ungroup() #%>%
  dplyr::mutate(rating = dplyr::case_when(
    mean_accuracy %% 2 == 0 ~ strrep("star,", mean_accuracy),
    mean_accuracy %% 2 != 0 ~ paste0(strrep("star,", floor(mean_accuracy)), "star-half")
  ))
  gt() |>
  fmt_icon(
    columns = rating,
    fill_color = "red",
    fill_alpha = from_column("stars", fn = function(x) x / 4)
  )
  gt() %>%
  fmt_icon("mean_accuracy",
           icon = "star",
           icon_color = "orange",
           icon_label = "mean_accuracy"
  )


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



