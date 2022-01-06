seasons <- 2018:2020
full_pbp <- purrr::map_df(seasons, function(x) {
  pbp <- readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})


