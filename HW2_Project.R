
getwd()
library(tidyverse)
library(dplyr)
library(readxl)

# Problem 1 Data
nyc_subway <- read.csv("./NYC_Transit_Subway_Entrance_And_Exit_Data.csv", 
              header = TRUE, 
              sep = "," ,
              na = c("NA", "")) |>
              janitor::clean_names() |>
              select(line:station_longitude, starts_with("route"), 
              entry, vending, entrance_type, ada) |>
              mutate(entry = ifelse(entry == "YES", TRUE, FALSE))

# Problem 2 Data
trash_wheel <- read_excel('./Trash_Wheel_Data.xlsx', 
                          sheet = 1 , 
                          na = c("NA", ".", "")) |> 
  janitor::clean_names() |> 
  select(dumpster:homes_powered) |> 
  drop_na(dumpster) |>
  mutate(sports_balls = as.integer(round(sports_balls)),
         trash_sheet = "Mr. Trash Wheel", 
         year = as.integer(year))

professor_trash <- read_excel('./Trash_Wheel_Data.xlsx', 
                              sheet = 2,
                              na = c("NA", ".", "")) |> 
  janitor::clean_names() |> 
  drop_na(dumpster) |>
  select(dumpster:homes_powered) |>
  mutate(
    trash_sheet = "Professor",
    year = as.integer(year))


gwynnda <- read_excel('./Trash_Wheel_Data.xlsx', 
            sheet = 3,
            skip = 1,
            na = c("NA", ".", "")) |> 
          janitor::clean_names() |>
          select(dumpster:homes_powered) |>
          drop_na(dumpster) |>
  mutate(
    trash_sheet = "Gwynnda",
    year = as.integer(year))

trash <-bind_rows(trash_wheel, professor_trash, gwynnda)   

# Problem 3 Data

bakers <- read.csv("./gbb_datasets/bakers.csv", 
                   na = c("NA", ".", "")) |>
                   janitor::clean_names() |>
                   separate(baker_name, into = c("baker", "last_name"), sep = ' ')
                   
bakes <- read.csv("./gbb_datasets/bakes.csv",
                  na = c("NA", ".", "")) |>
                  janitor::clean_names()
  
results <- read.csv("./gbb_datasets/results.csv",
                    na = c("NA", ".", ""),
                    skip = 2) |>
                    janitor::clean_names() |>
                    drop_na(, result)

bakers <- bakers %>%
    mutate( baker = ifelse(baker == "Jo","Joanne", baker))
    bakes$baker <- gsub("\"|'", "", bakes$baker)
bakes <- bakes %>%
      mutate( baker = ifelse(baker == "Jo","Joanne", baker))

                          
anti_join(bakers, bakes, by = "baker")
anti_join(bakers, results, by = "baker")
anti_join(bakes, results, by = c("series", "episode"))