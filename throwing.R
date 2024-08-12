# Sheet to analyse remix throwing data

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(gghighlight)
library(gt)
library(magrittr)

# Load data ---------------------------------------------------------------

throwing.df <- read.csv2("data/REMIX THROWING.csv")
players.df <- read.csv2("data/PLAYERS.csv")

# Data cleaning -----------------------------------------------------------

throwing.df <- 
throwing.df %>%
  pivot_longer(cols = 4:last_col(),
               names_to = c("side", "distance"),
               names_sep = "_",
               values_to = "accuracy"
               ) %>%
  janitor::clean_names() %>%
  mutate(
    session = as.Date(session, format = "%d/%m/%Y"),
    side = factor(side, levels = c("BH", "FH"), labels = c("Backhand", "Forehand")),
    distance = as.numeric(distance),
    accuracy_pct = accuracy/10
  )

players.df %<>%
  janitor::clean_names() %>%
  mutate(
    gender = factor(gender, levels = c("M", "F"), labels = c("Male", "Female"))
  )

throwing.df %<>%
  left_join(players.df, by = "player_name")

# Analysis ----------------------------------------------------------------

## Accuracy by distance ----------------------------------------------------

throwing.df %>% 
  select(player_name, gender) %>% 
  distinct() %>% 
  group_by(gender) %>% tally()

# Make a ridgeplot for accuracy by distance
throwing.df %>%
  mutate(distance = as.character(distance)) %>%
  ggplot(aes(x = accuracy_pct, y = distance)) +
  geom_density_ridges(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 10)/10,
                     breaks = 1:10/10,
                     labels = scales::percent_format(accuracy = 1)
                     ) +
  theme_minimal() +
  labs(title = "Accuracy by distance",
       x = "Accuracy in %",
       y = "Distance (m)")
  
# make a ridge plot of accuracy by distance
# split it out by side
throwing.df %>%
  mutate(distance = as.character(distance)) %>%
  ggplot(aes(x = accuracy_pct, y = distance, fill = side)) +
  geom_density_ridges(alpha = 0.5, col = "white", linewidth = 2,
                      scale = 1.5
                      ) +
  scale_x_continuous(limits = c(0, 10)/10,
                     breaks = c(1),
                     labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  labs(title = "Accuracy by distance",
       x = "Accuracy in %",
       y = "Distance (m)",
       fill = "") +
  scale_fill_manual(values = c("Backhand" = "steelblue", "Forehand" = "orange")) +
  scale_y_discrete(limits = c("10", "15", "25", "30"),
                   labels = c("10m", "15m", "25m", "30m"),
                   expand = c(0, 0)) +
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20, color = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 20, color = "white"),
    panel.background = element_blank(), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_blank(), #transparent legend bg
    legend.box.background = element_blank() #transparent legend panel
  )

ggsave("figures/accuracy_by_distance.png", width = 10, height = 6)

# make a ridge plot of accuracy by distance
# split it out by side and gender
throwing.df %>%
  mutate(distance = as.character(distance)) %>%
  ggplot(aes(x = accuracy_pct, y = distance, fill = gender)) +
  geom_density_ridges(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 10)/10,
                     breaks = seq(1,10,2)/10,
                     labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  facet_wrap(~side) +
  labs(title = "Accuracy by distance",
       x = "Accuracy in %",
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
  group_by(player_name, side, gender) %>%
  summarise(short = mean(accuracy_pct[distance %in% c("10", "15")], na.rm = TRUE),
            long = mean(accuracy_pct[distance %in% c("25", "30")], na.rm = TRUE)
            ) %>%
  pivot_longer(cols = c(short, long), names_to = "distance") %>%
  pivot_wider(names_from = side, values_from = value) %>%
  mutate(
    distance = factor(distance, levels = c("short", "long"),
                      labels = c("Short (10-15m)", "Long (25-30m)")
                      )
  ) 

throwing.short_long %>%
  ggplot(aes(x = Backhand, y = Forehand, fill = gender, col = gender)) +
  ggrepel::geom_text_repel(aes(label = player_name),
                           nudge_x = -.05,
                           nudge_y = +.05
                           ) +
  geom_point() +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks = seq(0, 1, 0.2)
                     ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks = seq(0, 1, 0.2)
                     ) +
  # add a diagonal line at 5
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~distance) +
  theme_bw() +
  labs(x = "Backhand",
       y = "Forehand")

# make one plot with the overal accuracy split by backhand and forehand
throwing_accuracy <-
  throwing.df %>%
  group_by(player_name, side) %>%
  summarise(accuracy = mean(accuracy, na.rm = TRUE)) %>%
  pivot_wider(names_from = side, values_from = accuracy)

throwing_accuracy %>%
  ggplot(aes(x = Backhand, y = Forehand)) +
  ggrepel::geom_text_repel(aes(label = player_name),
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

# Clustering --------------------------------------------------------------

# What roles do certain players have on the team based on their throwing prowess?

# based on the characteristics of the throwers over certain distances
# can we group the players into different clusters
# based on the accuracy of the throws
# we will use the kmeans algorithm

cluster.df <-
throwing.df %>%
  select(player_name, gender, distance,side, accuracy_pct) %>%
  pivot_wider(
    names_from = c(side,distance), 
    values_from = accuracy_pct)
  
# dont take gender into account in this analysis (should be gender independent)
set.seed(42)
clusters <- 
  cluster.df %>% 
  #mutate(ismale = gender == "Male") %>%
  na.omit() %>%
  select(-player_name, -gender) %>%
  kmeans(x = ., centers = 4, trace = TRUE,
         iter.max = 1000)

clusters
# This particular setup: no gender, 4 clusters, 1000 iterations
# yields 4 clusters.
# cluster 1: (cutters) Loads high on 10 meter throws (> 60%), on both sides
# Cluster 2: (handlers) Loads high on all throws (> 60%)
# Cluster 3: (handlers FH) Forehand dominant, high on 10 meter throws (> 60%)
# Cluster 4: (power cutters) Short accurate, medium on middle-distance throws

# use the clusters to assign a cluster to each player
throwing.groups <- cluster.df %>% 
  na.omit() %>%
  mutate(cluster = clusters$cluster,
         # name the clusters
         cluster = case_when(
           cluster == 1 ~ "Cutters (Short throws)",
           cluster == 2 ~ "Handlers",
           cluster == 3 ~ "Cutters (Forehand)",
           cluster == 4 ~ "Power Cutters"
         )
         ) %>%
  select(player_name, cluster)
  
# visualize the results of the clustering using throwing.df
throwing.short_long %>%
  left_join(throwing.groups, by = "player_name") %>%
  ggplot(aes(x = Backhand, y = Forehand, col = factor(cluster))) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = player_name),
                           nudge_x = -.05,
                           nudge_y = +.05,
                           show.legend = FALSE
  ) +
  scale_x_continuous(breaks = (0:10)/10,
                     labels = scales::percent_format(accuracy = 1)
                     ) +
  scale_y_continuous(breaks = (0:10)/10,
                     labels = scales::percent_format(accuracy = 1)
  ) +
  # add a diagonal line at 5
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_bw() +
  labs(x = "Backhand",
       y = "Forehand") + 
  facet_grid(cols = vars(distance), rows = vars(gender))

# Linear model ------------------------------------------------------------

# What does the throwing accuracy look like if we model it as a function over distance?

# A regression model for accuracy 
# using distance in meters and throwing side as parameters
# the outcome should be accuracy between 0 and 1
# the model should be stored in a variable called throws.lm
library(betareg)
throws.lm <- 
  throwing.df %>%
  betareg(accuracy_pct ~ distance + side + gender, 
          data = .)
  
summary(throws.lm)

# predict male and female throwing accuracy over distance
# predict all distances from 0 to 100 meters
# for each gender and each side
predicted.accuracy = 
  expand.grid(distance = seq(0, 100, 1),
              side = c("Backhand", "Forehand"),
              gender = c("Male", "Female")
  ) %>%
  mutate(accuracy = predict(throws.lm, newdata = .))
              
# plot the results of the model
# use ggplot
predicted.accuracy %>%
  ggplot(aes(x = distance, 
             y = accuracy, 
             color = gender)) +
  geom_line(size = 1) +
  ggrepel::geom_text_repel(data = predicted.accuracy %>% filter(distance %in% c(10, 30, 50)),
            aes(label = scales::percent(accuracy,
                                        accuracy = 1)),
            nudge_x = 15,
            show.legend = FALSE) +
  geom_point(data = predicted.accuracy %>% filter(distance %in% c(10, 30, 50)),
             size = 3
  ) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.1),
                     labels = scales::percent_format(accuracy = 1)
                     ) +
  theme_minimal() +
  facet_wrap(~side) +
  labs(title = "Accuracy by distance",
       x = "Distance (m)",
       y = "Accuracy")

# Monte Carlo simulation --------------------------------------------------

# What is mathematically the best group of 7 players
# Run a monte carlo simulation to find the best group of 7 players
# Assume each group to have between 2 to 4 handlers
# Assume each group to have between 2 to 4 cutters
# the gender distribution should be 3 male, 4 female or 4 male, 3 female
# each simulation should simulate anywhere between 5 and 20 throws
# sample for each player a player to throw to (not including themselves) and use the accuracy to predict the outcome
# if the outcome is 0, the throw is unsuccessful
# if the outcome is 1, the throw is successful and the string continues
# measure success by the number of consecutive successful throws

# impute Nike's long throws with the average
throwing.df.mc <-
throwing.df %>%
  group_by(distance, side) %>%
  mutate(
    accuracy_pct = ifelse(player_name == "Nike" & is.na(accuracy_pct),
                      mean(accuracy_pct, na.rm = TRUE),
                      accuracy_pct
                      )
                      ) %>%
  filter(
    # Dave won't be playing
    player_name != "Dave"
  )

# set the seed
set.seed(42)

# simulate a throw
simulate_throw <- function(player, distance){
  # sample forehand or backhand
  side <- sample(c("Forehand", "Backhand"), 1)
  
  accuracy <-
    throwing.df.mc$accuracy_pct[
      throwing.df.mc$player_name == player & 
        throwing.df.mc$distance == distance &
        throwing.df.mc$side == side
  ]
  
  # predict the outcome using the measured accuracy
  outcome <- rbinom(1, 1, accuracy)
  
  # return the outcome of the throw and the next player
  return(list(outcome = outcome, side = side))
}

# test
simulate_throw("Nike", 30)

# select a line up
men <- players.df %>% 
  filter(gender == "Male" &
           player_name %in% throwing.df.mc$player_name) %>%
  pull(player_name)
female <- players.df %>% 
  filter(gender == "Female" &
           player_name %in% throwing.df.mc$player_name) %>%
  pull(player_name)

# using the above mentioned constraints
select_lineup <- function(){
  
  # decide gender distribution
  n_men <- sample(c(3,4), 1)
  n_women = ifelse(n_men == 3, 4, 3)
  gender_ratio = sprintf("M%sF%s", n_men, n_women)
  
  # randomly select players based on the distribution
  line = c(sample(men,n_men), sample(female, n_women))
  
  # always return the lineup alphabetically
  # so the differences in the simulation are not due to the order of the players
  line <- sort(line)
  
  return(list(line = line, ratio = gender_ratio))
}

# simulate a point
# take one random starting player
# simulate a throw
# if the throw is successful, continue with the next player
# stop if the throw is unsuccessful
# return the number of successful throws
play_point <- function(lineup){
  
  # randomly select a starting player
  player <- sample(lineup$line, 1)
  
  # simulate a throw
  outcome <- 1
  # vector of throws
  throws_vector <- c()
  
  while(outcome == 1){
    # sample distances between 10 and 30 meters.
    # the amount of 10 meter throws is 50%, 15 meter throws is 30%, 
    # 25 meter throws is 15% and 30 meter throws is 5%
    distance <- sample(c(10, 15, 25, 30), 1, 
                       prob = c(.35, .28, .22, 0.15))
    
    # simulate a throw
    outcome <- simulate_throw(player, distance)$outcome
    throws_vector <- c(throws_vector, distance)
    # randomly sample the next player
    player <- sample(lineup$line[lineup$line != player], 1)
  }
  
  return(throws_vector)
}

# for each line up, simulate 1000 points
simulate_line <- function(lineup){
  
  # simulate 1000 points
  points <- replicate(1000, play_point(lineup))
  
  # to each element in points
  # calculate the number of successful throws
  # calculate the average distance of the throws
  throws <- sapply(points, function(x) length(x))
  avg_distance <- sapply(points, function(x) mean(x))
  total_distance <- sapply(points, function(x) sum(x))
  
  # return a dataframe with all the simulation results and the lineup
  
  out <- data.frame(throws = throws, 
                    avg_distance = avg_distance,
                    total_distance = total_distance) %>%
    mutate(
      ratio = lineup$ratio,
      lineup = list(lineup$line)
    )
  
  message(
    sprintf("Lineup: %s, Success rate: %s, Average distance: %s",
                  lineup$line %>% paste0(collapse = "|"),
                  mean(throws),
                  mean(avg_distance)
                  ))
          
  return(out)
}

# test
simulate_line(select_lineup())

# simulate 1000 different line ups
# safe the results in a dataframe
simulated_results.150 <- 
  replicate(150, simulate_line(select_lineup()), 
            simplify = FALSE) %>%
  bind_rows()

simulated_results.1000 <- 
  replicate(1000, simulate_line(select_lineup()), 
            simplify = FALSE) %>%
  bind_rows()

# save the results
saveRDS(simulated_results.1000, "data/simulated_results_1000lines.rds")
simulated_results <- readRDS("data/simulated_results_1000lines.rds")

# 100 different lineups are run for 1000 points each

library(gghighlight)

# for each line:
# calculate the average number of successful throws
# calculate the average distance of the throws
# calculate the amount of times more than 5 successful throws
derived_results <- 
  simulated_results %>%
  group_by(lineup, ratio) %>%
  summarise(
    sample_size = n(),
    avg_throws = mean(throws),
    avg_distance = mean(avg_distance),
    total = mean(total_distance),
    success_rate = sum(throws > 5)
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    # shorten each name to 4 characters
    lineup = lineup %>% substr(1,4) %>% paste0(collapse = "|"),
    success_rate = success_rate / sample_size
  ) %>%
  ungroup()

derived_results %>%
  ggplot(aes(x = avg_throws, y = avg_distance, col = ratio)) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ratio) +
  gghighlight::gghighlight(
    avg_throws > 5,
    calculate_per_facet = TRUE
  ) +
  labs(title = "Monte Carlo simulation",
       x = "Average number of successful throws",
       y = "Average distance of throws")


derived_results %>%
  ggplot(aes(x = total, y = avg_distance, col = ratio)) +
  geom_point() +
  theme_minimal()

# create a boxplot of the number of successful throws for the
# top 10 lines
top_lines <- 
  simulated_results %>%
  group_by(lineup, ratio) %>%
  summarise(
    avg_throws = mean(throws)
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    # shorten each name to 4 characters
    line = lineup %>% substr(1,4) %>% paste0(collapse = "|")
  ) %>%
  ungroup() %>%
  arrange(desc(avg_throws)) %>%
  # label the top 10 and bottom 10 lines
  mutate(
    top = row_number() <= 10,
    bottom = row_number() > n() - 10
  )

top_lines %>% filter(top)
top_lines %>% filter(bottom)

# for the top 10 lines and bottom 10 lines
simulated_results %>%
  inner_join(top_lines %>%
               filter(top | bottom), by = c("lineup" = "lineup", "ratio" = "ratio")) %>%
  ggplot(aes(x = throws, y = line, fill = top)) +
  geom_density_ridges(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 10),
                     breaks = 1:10
  ) +
  theme_minimal() +
  facet_wrap(~top)

# top 10% of the lines by consecutive throws
simulated_results$throws %>% quantile(0.9)

# Individual player contributions
# Assuming `simulated_results.100` is the dataframe with the results
# Unnest the lineup column
unnested_results <- simulated_results %>%
  unnest(lineup) %>%
  rename(player = lineup)

# Calculate player contribution metrics
player_contributions <- unnested_results %>%
  group_by(player, ratio) %>%
  summarise(
    avg_throws = mean(throws, na.rm = TRUE),
    success = sum(throws > 7, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    success_ratio = success / count
  ) %>%
  arrange(desc(success_ratio)) %>%
  left_join(players.df, by = c("player" = "player_name"))

# Identify top and bottom players based on average throws
# Visualize player contributions
ggplot(player_contributions, 
       aes(x = reorder(player, success_ratio), 
           y = success_ratio)) +
  geom_col() +
  geom_text(aes(label = scales::percent(success_ratio, accuracy = .1)),
            nudge_y = 0.05) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(~ratio, scales = "free_y") +
  gghighlight::gghighlight(
    success_ratio,
    max_highlight = 10L, 
    calculate_per_facet = TRUE) +
  labs(title = "Contribution to succes ratio of Players",
       subtitle = "% of points played that strings together 7 or more successful throws (top 10%)",
       x = "Player",
       y = "Succes ratio")

# ....

# Input values
p <- .9       # sample proportion
n <- 1       # sample size
confidence_level <- 0.95

# Calculate the z-value for the desired confidence level
z <- qnorm((1 + confidence_level) / 2)

# Calculate the standard error
standard_error <- sqrt(p * (1 - p) / n)

# Calculate the margin of error
margin_of_error <- z * standard_error

# Calculate the confidence interval
lower_bound <- p - margin_of_error
upper_bound <- p + margin_of_error

# Print the confidence interval
cat("The", confidence_level*100, "% confidence interval is: [", lower_bound, ", ", upper_bound, "]\n")


