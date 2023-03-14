library(tidyverse)
library(geosphere)
library(dplyr)
library(ggplot2)

rm(list = ls())

# Make sure you set your working directory
# An easy and temporary way to look through each csv
fnames <- list.files(paste0(getwd(), "/Data"))
csv <- lapply(paste0("Data/", fnames), read.csv)
result <- do.call(rbind, csv)
csv

csv[1]

# Arena Coordinates for Power Rankings
arena_coords <- read.csv("Data/arena coordinates.csv")

# Team Coordinates for Power Rankings
team_coords <- read.csv("Data/team coordinates.csv")

#combined team and arena coords to calculate distance
east_coords <- read.csv("Data/east region.csv")
east_coords =
  east_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137)

west_coords <- read.csv("Data/west region.csv")
west_coords =
  west_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137)

midwest_coords <- read.csv("Data/midwest region.csv")
midwest_coords =
  midwest_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137)

south_coords <- read.csv("Data/south region.csv")
south_coords =
  south_coords %>%
  dplyr::rowwise() %>%
  mutate(distance = distHaversine(c(Tlon, Tlat), c(Alon, Alat))) %>%
  mutate(distance = distance * 0.00062137)

# Combine all regions into one data frame
combined_team_dist <- rbind(east_coords, west_coords, midwest_coords, south_coords)

# place in ascending order for power rankings
power_rankings <- combined_team_dist[order(combined_team_dist$distance), ]

