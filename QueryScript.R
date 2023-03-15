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

#Create valid date column
df_tournament <- df_tournament %>%
  mutate(Day = lubridate::day(as.Date(DayNum, origin = paste0(Season, "-01-01")))) %>%
  mutate(Month = lubridate::month(as.Date(DayNum, origin = paste0(Season, "-01-01")))) %>%
  mutate(Date =  as.Date(paste(Season, Month, Day, sep = "/"))) %>%
  select(Date, Season, Month, Day, everything(), -DayNum)

# Arena Coordinates for Power Rankings
arena_coords <- read.csv("Data/arena coordinates.csv")

# Team Coordinates for Power Rankings
team_coords <- read.csv("Data/team coordinates.csv")

#combined team and arena coords to calculate distance
east_coords <- read.csv("Data/east region.csv")
east_coords <- east_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137)

# Power ranking based on region
average_east_dist <- east_coords %>%
  group_by(Team) %>%
  summarize(avg_dist = mean(distance))

west_coords <- read.csv("Data/west region.csv")
west_coords <- west_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  arrange(distance)

# Power rankings based on region
average_west_dist <- west_coords %>%
  group_by(Team) %>%
  summarize(avg_dist = mean(distance))

midwest_coords <- read.csv("Data/midwest region.csv")
midwest_coords <- midwest_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  arrange(distance)

# Power region based on region
average_midwest_dist <- midwest_coords %>%
  group_by(Team) %>%
  summarize(avg_dist = mean(distance))

south_coords <- read.csv("Data/south region.csv")
south_coords <- south_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137) %>%
  arrange(distance)

# Power rankings based on region
average_south_dist <- south_coords %>%
  group_by(Team) %>%
  summarize(avg_dist = mean(distance))

# Combine all regions into one data frame
combined_team_dist <- rbind(east_coords, west_coords, midwest_coords, south_coords)

# place in ascending order for power rankings
power_rankings <- combined_team_dist[order(combined_team_dist$distance), ]