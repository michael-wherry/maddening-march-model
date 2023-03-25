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

df_team_data <- read.csv("brosius data/Tournament Team Data (Including 2023).csv") %>%
  select(-TEAM.1)

df_coach_tenure <- read.csv("Data/CoachTenure.csv")

df_teams_id_name <- read.csv("Data/MTeams.csv")

df_team_predictions <- read.csv("brosius data/2023 Game Data.csv")

df_matchup_predictions <- read.csv("brosius data/round predictions.csv")

df_current_team_data <- df_team_data %>%
  filter(YEAR > 2022) %>%
  select(-CHAMPION)

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

df_team_data_historical <- filter(df_team_data, YEAR != 2023)

# # Define the range of parameter values to search
 tune_params <- list(
   kernel = c("linear", "polynomial", "radial", "sigmoid"),
   gamma = c(0.1, 1, 5, 10),
   cost = c(0.1, 1, 10, 100)
 )
 
 # Perform a grid search with 5-fold cross-validation
 tune_result <- tune(
   svm, 
   CHAMPION ~ . -YEAR -SEED -ROUND -TEAM, 
   data = df_team_data_historical,
   ranges = tune_params,
   tunecontrol = tune.control(cross = 5)
 )

tune_result$best.parameters$kernel <- 'linear'
tune_result$best.parameters$gamma <- .1
tune_result$best.parameters$cost <- .1

# Print the best parameters
print(tune_result$best.parameters)

# Train the SVM model with the best parameters
tournament_model <- svm(CHAMPION ~ . -YEAR -SEED -ROUND -TEAM, 
                        data = df_team_data_historical,
                        kernel = tune_result$best.parameters$kernel, 
                        gamma = tune_result$best.parameters$gamma, 
                        cost = tune_result$best.parameters$cost)

# Make predictions on the test set
predictions <- predict(tournament_model, df_current_team_data)

df_predictions <- data.frame(predictions)

df_champions <- df_current_team_data %>%
  select(TEAM) %>%
  cbind(df_predictions) %>%
  arrange(desc(predictions)) %>%
  rename("PREDICTIONS" = "predictions")

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(as.numeric(predictions) - as.numeric(df_testing$Winner)))

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
  select(-YEAR,-ROUND, -Tlat, -Tlon, -Alat, -Alon) %>%
  left_join(df_coach_tenure, by = c("Team" = "TEAM")) 

west_coords <- read.csv("Data/west region.csv")
west_metrics <- west_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  left_join(west_region, by = c("Team" = "TEAM")) %>%
  select(-YEAR,-ROUND,-Tlat, -Tlon, -Alat, -Alon) %>%
  left_join(df_coach_tenure, by = c("Team" = "TEAM"))

midwest_coords <- read.csv("Data/midwest region.csv")
midwest_metrics <- midwest_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  left_join(midwest_region, by = c("Team" = "TEAM")) %>%
  select(-YEAR,-ROUND,-Tlat, -Tlon, -Alat, -Alon) %>%
  left_join(df_coach_tenure, by = c("Team" = "TEAM"))

south_coords <- read.csv("Data/south region.csv")
south_metrics <- south_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  arrange(distance) %>%
  left_join(south_region, by = c("Team" = "TEAM")) %>%
  select(-YEAR,-ROUND,-Tlat, -Tlon, -Alat, -Alon) %>%
  left_join(df_coach_tenure, by = c("Team" = "TEAM"))

#Determine each individual matchup by the predictions from our model
df_matchup_predictions <- df_matchup_predictions %>%
  left_join(df_champions, by = c("TEAM" = "TEAM")) 
  
df_matchup_predictions_first_round <- df_matchup_predictions %>%
  select(REGION, MATCHUP.KEY, GAME.KEY, TEAM, SEED, PREDICTIONS) %>%
  arrange( REGION, MATCHUP.KEY, GAME.KEY) %>%
  mutate(WINNER = PREDICTIONS > lead(PREDICTIONS))

df_matchup_predictions_first_round[13,7] <- TRUE
df_matchup_predictions_first_round[21,7] <- TRUE
df_matchup_predictions_first_round[34,7] <- TRUE
df_matchup_predictions_first_round[51,7] <- TRUE
df_matchup_predictions_first_round[63,7] <- FALSE

df_matchup_predictions_second_round <- df_matchup_predictions_first_round %>%
  filter(WINNER == TRUE) %>%
  arrange( REGION, MATCHUP.KEY) %>%
  mutate(WINNER = PREDICTIONS > lead(PREDICTIONS))

df_matchup_predictions_second_round[2,7] <- TRUE
df_matchup_predictions_second_round[4,7] <- TRUE
df_matchup_predictions_second_round[5,7] <- TRUE
df_matchup_predictions_second_round[7,7] <- TRUE
df_matchup_predictions_second_round[9,7] <- TRUE
df_matchup_predictions_second_round[12,7] <- TRUE
df_matchup_predictions_second_round[13,7] <- TRUE
df_matchup_predictions_second_round[15,7] <- TRUE
df_matchup_predictions_second_round[17,7] <- TRUE
df_matchup_predictions_second_round[20,7] <- TRUE
df_matchup_predictions_second_round[22,7] <- TRUE
df_matchup_predictions_second_round[23,7] <- TRUE
df_matchup_predictions_second_round[25,7] <- TRUE
df_matchup_predictions_second_round[27,7] <- TRUE
df_matchup_predictions_second_round[29,7] <- TRUE
df_matchup_predictions_second_round[31,7] <- TRUE

df_matchup_predictions_third_round <- df_matchup_predictions_second_round %>%
  filter(WINNER == TRUE) %>%
  arrange(REGION) %>%
  mutate(WINNER = PREDICTIONS > lead(PREDICTIONS))

df_matchup_predictions_third_round[2,7] <- TRUE
df_matchup_predictions_third_round[4,7] <- TRUE
df_matchup_predictions_third_round[5,7] <- TRUE
df_matchup_predictions_third_round[8,7] <- TRUE
df_matchup_predictions_third_round[9,7] <- TRUE
df_matchup_predictions_third_round[11,7] <- TRUE
df_matchup_predictions_third_round[14,7] <- TRUE
df_matchup_predictions_third_round[16,7] <- TRUE

df_matchup_predictions_fourth_round <- df_matchup_predictions_third_round %>%
  filter(WINNER == TRUE) %>%
  arrange(REGION) %>%
  mutate(WINNER = PREDICTIONS > lead(PREDICTIONS))

df_matchup_predictions_fourth_round[2,7] <- TRUE
df_matchup_predictions_fourth_round[4,7] <- TRUE
df_matchup_predictions_fourth_round[5,7] <- TRUE
df_matchup_predictions_fourth_round[7,7] <- TRUE

df_matchup_predictions_fifth_round <- df_matchup_predictions_fourth_round %>%
  filter(WINNER == TRUE) %>%
  arrange(GAME.KEY) %>%
  mutate(WINNER = PREDICTIONS > lead(PREDICTIONS)) 

df_matchup_predictions_fifth_round[1,7] <- TRUE
df_matchup_predictions_fifth_round[4,7] <- TRUE

df_matchup_predictions_sixth_round <- df_matchup_predictions_fifth_round %>%
  filter(WINNER == TRUE) %>%
  mutate(WINNER = PREDICTIONS > lead(PREDICTIONS))

df_matchup_predictions_sixth_round[1,7] <- TRUE

df_national_champion <- df_matchup_predictions_sixth_round %>%
  filter(WINNER == TRUE)

write_csv(df_matchup_predictions_first_round,"Data/firstRound.csv")
write_csv(df_matchup_predictions_second_round, "Data/secondRound.csv")
write_csv(df_matchup_predictions_third_round, "Data/thirdRound.csv")
write_csv(df_matchup_predictions_fourth_round, "Data/fourthRound.csv")
write_csv(df_matchup_predictions_fifth_round, "Data/fifthRound.csv")
write_csv(df_matchup_predictions_sixth_round, "Data/sixthRound.csv")
write_csv(df_national_champion, "Data/nationalChampion.csv")
write_csv(df_champions, "Data/predictionScores.csv")
