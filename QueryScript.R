library(tidyverse)
library(geosphere)
library(lubridate)
library(caret)
library(dplyr)
library(ggplot2)
library(e1071)
library(magrittr)


set.seed(42)

df_tournament <- read.csv("Data/MNCAATourneyDetailedResults.csv")

df_team_data <- read.csv("brosius data/Tournament Team Data (Including 2023).csv", colClasses = c("CHAMPION" = "factor"))

df_coach_tenure <- read.csv("Data/CoachTenure.csv")

df_teams_id_name <- read.csv("Data/MTeams.csv")

df_current_team_data <- df_team_data %>%
  filter(YEAR > 2022)

kenpom_before_2023 <- df_team_data %>%
  filter(YEAR < 2023)

east_region <- df_current_team_data[c(4,6,11,15,17,23,25,32,34,39,46,49,54,57,62,63,68),] %>%
  arrange(SEED)

south_region <- df_current_team_data[c(1,5,9,16,20,21,26,31,36,40,44,47,51,58,60,67,66),] %>%
  arrange(SEED)

midwest_region <- df_current_team_data[c(2,7,12,14,18,22,28,30,33,38,45,48,53,56,59,65,42),] %>%
  arrange(SEED)

west_region <- df_current_team_data[c(3,8,10,13,19,24,27,29,35,37,41,50,52,55,61,64,43),] %>%
  arrange(SEED)

#Create valid date column
df_tournament <- df_tournament %>%
  mutate(Day = lubridate::day(as.Date(DayNum, origin = paste0(Season, "-01-01")))) %>%
  mutate(Month = lubridate::month(as.Date(DayNum, origin = paste0(Season, "-01-01")))) %>%
  mutate(Date =  as.Date(paste(Season, Month, Day, sep = "/"))) %>%
  select(Date, Season, Month, Day, everything(), -DayNum)

#Key in team name with team id
df_historical_data <- df_tournament %>%
  left_join(df_teams_id_name, by = c("WTeamID" = "TeamID")) %>%
  select(-FirstD1Season, -LastD1Season) %>%
  rename(WTeam = TeamName) %>%
  left_join(df_teams_id_name, by = c("LTeamID" = "TeamID")) %>%
  select(-FirstD1Season, -LastD1Season, -WLoc) %>%
  rename(LTeam = TeamName)

# Append 1's and 0's to observation to indicate winners
df_unflipped <- slice_sample(df_historical_data, prop = 0.5, replace = F)

# Sets winning team metrics to posses a "R" (right) prefix instead
# Will be made the right column when joined back in
df_flipped <- df_historical_data %>%
  anti_join(df_unflipped, by = colnames(df_historical_data)) %>%
  rename_with(~ str_replace(.x, pattern = "^W", replacement = "R")) %>%
  mutate(Winner = 0) # 0 denotes that the right side team won

df_unflipped <- df_unflipped %>%
  rename_with(~ str_replace(.x, pattern = "^L", replacement = "R")) %>%
  rename_with(~ str_replace(.x, pattern = "^W", replacement = "L")) %>%
  mutate(Winner = 1) # 1 denotes that the left side team won

df_flagged_tournament <- df_unflipped %>%
  full_join(df_flipped, by = colnames(df_flipped)) %>%
  slice_sample(prop = 1) 

# Split the dataset into training and testing sets

df_training <- df_flagged_tournament %>%
  filter(Season != 2022)

df_testing <- df_flagged_tournament %>%
  filter(Season == 2022)

# Define the range of parameter values to search
tune_params <- list(
  kernel = c("linear", "polynomial", "radial", "sigmoid"),
  gamma = c(0.1, 1, 5, 10),
  cost = c(0.1, 1, 10, 100)
)

# Perform a grid search with 5-fold cross-validation
tune_result <- tune(
  svm, 
  Winner ~ . -Season -Day -LTeamID -RTeamID -LTeam -RTeam, 
  data = df_training,
  ranges = tune_params,
  tunecontrol = tune.control(cross = 5)
)

# Print the best parameters
print(tune_result$best.parameters)

# Train the SVM model with the best parameters
tournament_model <- svm(Winner ~ . -Season -DayNum -LTeamID -RTeamID -LTeam -RTeam, 
                        data = df_training, 
                        kernel = tune_result$best.parameters$kernel, 
                        gamma = tune_result$best.parameters$gamma, 
                        cost = tune_result$best.parameters$cost)

# Make predictions on the test set
predictions <- predict(tournament_model, df_testing %>% select(-Winner))

df_predictions <- data.frame(predictions)

df_comparison <- df_testing %>%
  select(Season, Day, LTeam, RTeam, Winner) %>%
  cbind(df_predictions)

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(as.numeric(predictions) - as.numeric(df_testing$Winner)))

# Print the MAE
print(mae)

# Arena Coordinates for Power Rankings
arena_coords <- read.csv("Data/arena coordinates.csv")

# Team Coordinates for Power Rankings
team_coords <- read.csv("Data/team coordinates.csv")

#combined team and arena coords to calculate distance
east_coords <- read.csv("Data/east region.csv")
east_metrics <- east_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  left_join(east_region, by = c("Team" = "TEAM")) %>%
  select(-YEAR,-ROUND, -TEAM.1, -Tlat, -Tlon, -Alat, -Alon) %>%
  left_join(df_coach_tenure, by = c("Team" = "TEAM")) %>%
  arrange(SEED)

west_coords <- read.csv("Data/west region.csv")
west_metrics <- west_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  left_join(west_region, by = c("Team" = "TEAM")) %>%
  select(-YEAR,-ROUND, -TEAM.1,-Tlat, -Tlon, -Alat, -Alon) %>%
  left_join(df_coach_tenure, by = c("Team" = "TEAM")) %>%
  arrange(SEED)

midwest_coords <- read.csv("Data/midwest region.csv")
midwest_metrics <- midwest_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  left_join(midwest_region, by = c("Team" = "TEAM")) %>%
  select(-YEAR,-ROUND, -TEAM.1,-Tlat, -Tlon, -Alat, -Alon) %>%
  left_join(df_coach_tenure, by = c("Team" = "TEAM")) %>%
  arrange(SEED)

south_coords <- read.csv("Data/south region.csv")
south_metrics <- south_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  arrange(distance) %>%
  left_join(south_region, by = c("Team" = "TEAM")) %>%
  select(-YEAR,-ROUND, -TEAM.1,-Tlat, -Tlon, -Alat, -Alon) %>%
  left_join(df_coach_tenure, by = c("Team" = "TEAM")) %>%
  arrange(SEED)


