library(tidyverse)
library(geosphere)

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
