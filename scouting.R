# The goal of this script is to create a scouting summary for the teams we will play against

# We will measure:
# - Amount of throws until score or turnover
# - Connections (how many and how succesful)
# - Which force is most effective
# - Which players are entered most important
# - Any things that come back often in the remarks

# Initialisatie ----------------------------------------------------------

library(tidyverse)
library(readxl)

# Import data -------------------------------------------------------------

scouting_names <- readxl::read_excel("data/REMIX Scouting.xlsx", 
                                     sheet = "Data",
                            n_max = 1)

scouting <- readxl::read_excel("data/REMIX Scouting.xlsx", 
                               sheet = "Data",
                               skip = 2, 
                               col_names = names(scouting_names)) %>% 
  janitor::clean_names()

rm(scouting_names)

# Data analysis -----------------------------------------------------------

# get all PUC data
puc_data <- scouting %>% 
  filter(str_detect(team, "Puc")) %>% 
  filter(offense_or_defense == "Offense")

# adjust the data (if result is not "score", add the last_intended_receiver to the throws-string)
puc_analysis <- puc_data %>% 
  mutate(throws = if_else(result == "Score", 
                          throws, 
                          paste0(throws, last_intended_reciever)),
         amount_of_throws = nchar(throws),
         succesful_throws = if_else(result == "Score", 
                                    amount_of_throws, 
                                    amount_of_throws - 1),
         MM_connections = str_count(throws, "MM"),
         MF_connections = str_count(throws, "MF"),
         FM_connections = str_count(throws, "FM"),
         FF_connections = str_count(throws, "FF"),
         connection_with_turnover = if_else(result != "Score",
                                            # last two characters of the throws-string
                                            str_sub(throws, -2, -1),
                                            NA_character_))

# average point length
  puc_analysis %>%
    mutate(turn_or_score = if_else(result == "Score", "score", "turnover")) %>%
  ggplot(aes(x = amount_of_throws, y = turn_or_score)) +
  ggridges::geom_density_ridges(
    height = 0.5
  )
  
  puc_analysis %>%
    mutate(turn_or_score = if_else(result == "Score", "score", "turnover")) %>%
    group_by(turn_or_score) %>%
    summarise(
      mean_throws = mean(amount_of_throws)
    )

# summarize the completion rate of the connections
puc_connection_summary <- 
  puc_analysis %>%
  group_by(gender_ratio) %>%
  summarise(
    possessions = n(),
    scores = sum(result == "Score"),
    total_throws = sum(amount_of_throws),
    succesful_throws = sum(succesful_throws),
  ) %>%
  mutate(
    completion_rate = scales::percent(succesful_throws / total_throws)
  )

puc_gender_connections <-
  puc_analysis %>%
  pivot_longer(cols = 
                 c(MM_connections, 
                   MF_connections, FM_connections, FF_connections),
               names_to = "connection",
               values_to = "amount",
               names_pattern = "(.*)_connections"
               ) %>%
  group_by(connection) %>%
  summarise(
    total = sum(amount),
    unsuccesful = sum(connection_with_turnover == connection, na.rm = TRUE)
  ) %>%
  mutate(
    succesful = total - unsuccesful,
    completion_rate = succesful / total
  )



