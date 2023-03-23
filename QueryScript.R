library(tidyverse)
library(geosphere)
library(lubridate)
library(caret)
library(dplyr)
library(ggplot2)
library(e1071)
library(magrittr)

df_tournament <- read.csv("Data/MNCAATourneyDetailedResults.csv")

df_team_data <- read.csv("brosius data/Tournament Team Data (Including 2023).csv", colClasses = c("CHAMPION" = "factor"))

df_coach_tenure <- read.csv("Data/CoachTenure.csv")

df_teams_id_name <- read.csv("Data/MTeams.csv")

df_current_team_data <- df_team_data %>%
  filter(YEAR > 2022)

kenpom_before_2023 <- df_team_data %>%
  filter(YEAR < 2023)
view(iris)
set.seed(123) # Set seed for reproducibility
train_index <- sample(nrow(kenpom_before_2023), 0.8 * nrow(kenpom_before_2023)) # 80% for training
train_data <- kenpom_before_2023[train_index, ] # Training data
test_data <- kenpom_before_2023[-train_index, ] # Testing data
x <- select(train_data, -KENPOM.ADJUSTED.EFFICIENCY) 
y <- select(train_data, KENPOM.ADJUSTED.EFFICIENCY)

df_team_data <- select(df_team_data, -YEAR, -ROUND, -TEAM, )
svm_model <- svm(CHAMPION ~ ., data = df_team_data, kernel = "linear", cost = 10)

predictions <- predict(svm_model)

confusionMatrix(predictions, df_team_data$CHAMPION)

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
  select(-FirstD1Season, -LastD1Season) %>%
  rename(LTeam = TeamName)

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


