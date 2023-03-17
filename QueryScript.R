library(tidyverse)
library(geosphere)
library(lubridate)
library(dplyr)
library(ggplot2)
library(e1071)
install.packages("tidyverse")
rm(list = ls())

# Make sure you set your working directory
# An easy and temporary way to look through each csv
fnames <- list.files(paste0(getwd(), "/Data"))
csv <- lapply(paste0("Data/", fnames), read.csv)
result <- do.call(rbind, csv)

df_tournament <- read.csv("Data/MNCAATourneyDetailedResults.csv")

Team_data <- read.csv("brosius data/Tournament Team Data (Including 2023).csv")

kenpom_before_2023 <- Team_data %>%
  filter(YEAR < 2023)

set.seed(123) # Set seed for reproducibility
train_index <- sample(nrow(kenpom_before_2023), 0.8 * nrow(kenpom_before_2023)) # 80% for training
train_data <- kenpom_before_2023[train_index, ] # Training data
test_data <- kenpom_before_2023[-train_index, ] # Testing data
x <- select(train_data, -KENPOM.ADJUSTED.EFFICIENCY) 
y <- select(train_data, KENPOM.ADJUSTED.EFFICIENCY)

svm_model <- svm(KENPOM.ADJUSTED.EFFICIENCY ~ ., data = train_data, kernel = "linear", cost = 10)

predictions <- fitted(svm_model) 

east_region <- Team_data %>%
  arrange(SEED)
  
#Create valid date column
df_tournament <- df_tournament %>%
  mutate(Day = lubridate::day(as.Date(DayNum, origin = paste0(Season, "-01-01")))) %>%
  mutate(Month = lubridate::month(as.Date(DayNum, origin = paste0(Season, "-01-01")))) %>%
  mutate(Date =  as.Date(paste(Season, Month, Day, sep = "/"))) %>%
  select(Date, Season, Month, Day, everything(), -DayNum)

#Key in team name with team id
df_historical_data <- df_tournament %>%
  left_join(teams_id_name, by = c("WTeamID" = "TeamID")) %>%
  select(-FirstD1Season, -LastD1Season) %>%
  rename(WTeam = TeamName) %>%
  left_join(teams_id_name, by = c("LTeamID" = "TeamID")) %>%
  select(-FirstD1Season, -LastD1Season) %>%
  rename(LTeam = TeamName)

# Arena Coordinates for Power Rankings
arena_coords <- read.csv("Data/arena coordinates.csv")

# Team Coordinates for Power Rankings
team_coords <- read.csv("Data/team coordinates.csv")

#combined team and arena coords to calculate distance
east_coords <- read.csv("Data/east region.csv")
east_coords <- east_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  left_join(kenpom_2023, by = c("Team" = "Team"))

west_coords <- read.csv("Data/west region.csv")
west_coords <- west_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137)

midwest_coords <- read.csv("Data/midwest region.csv")
midwest_coords <- midwest_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137)

south_coords <- read.csv("Data/south region.csv")
south_coords <- south_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  arrange(distance)

# Combine all regions into one data frame
combined_team_dist <- rbind(east_coords, west_coords, midwest_coords, south_coords)

# place in ascending order for power rankings
power_rankings <- combined_team_dist[order(combined_team_dist$distance), ]

