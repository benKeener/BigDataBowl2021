library(tidyverse)
library(glue)
#Load the data from nflscrapR

seasons <- 2000:2020
full_pbp <- purrr::map_df(seasons, function(x) {
  pbp <- readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

#Change the char data for the old game id to numeric to be joined later

pbp <- full_pbp %>%
  mutate(old_game_id = as.numeric(old_game_id))

#Filter to only field goals

FG <- pbp %>%
  filter(play_type == "field_goal") %>%
  filter(field_goal_result != "blocked") 

first <- weather[match(unique(weather$game_id), weather$game_id),]

fg_test <- FG %>%
  inner_join(first, by = c("old_game_id" = "game_id"))
#Filter out unused data
fg_test <- fg_test %>%
  select(old_game_id, play_id, kick_distance, roof, surface, wind, success,  Humidity, 
          Precipitation, WindDirection, Pressure)

#Change all descriptive variables to binary
fg_test <- fg_test %>%
  filter(!is.na(success)) %>%
  mutate(Humidity = ifelse(roof == "dome",0, Humidity)) %>%
  mutate(Precipitation = ifelse(roof == "dome",0, Precipitation)) %>%
  mutate(wind = ifelse(roof == "dome",0, wind)) %>%
  mutate(roof = ifelse(roof == "dome",1, 0)) %>%
  mutate(surface = ifelse(surface == "grass", 1, 0))

