seasons <- 2018:2020
full_pbp <- purrr::map_df(seasons, function(x) {
  pbp <- readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})


pbp <- pbp %>%
  mutate(passer_player_name = ifelse(rusher_player_name == "G.Minshew", "G.Minshew II", rusher_player_name))

f20 <- f20 %>%
  mutate(passer_player_name = ifelse(passer_player_name == "M.Ingram", "M.Ingram II", passer_player_name))

f20 <- readRDS(
  url(
    glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")
  )
)



