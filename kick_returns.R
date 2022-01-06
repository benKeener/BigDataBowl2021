returns <- plays %>%
  filter(specialTeamsPlayType == "Kickoff") %>%
  filter(specialTeamsResult == "Return") %>% 
  filter(!is.na(returnerId)) %>%
  select(playId, gameId)

tracking_full <- rbind(tracking2018, tracking2019, tracking2020)

return_data <- data.frame()

for (i in 1:2794)
{
  example <- tracking_full %>%
    filter(playId == returns$playId[i]) %>%
    filter(gameId == returns$gameId[i]) %>%
    filter(event == "kick_received" | event == "kick_recovered")
  
  end  <- tracking_full %>%
    filter(playId == returns$playId[i]) %>%
    filter(gameId == returns$gameId[i]) %>%
    filter(event == "tackle" | event == "touchdown"| event == "out_of_bounds")
  
  if(dim(end)[1] == 0 )
  {
    f <- tracking_full %>%
      filter(playId == returns$playId[i]) %>%
      filter(gameId == returns$gameId[i]) %>%
      filter(frameId >= example$frameId[1])
  }else{
    f <- tracking_full %>%
      filter(playId == returns$playId[i]) %>%
      filter(gameId == returns$gameId[i]) %>%
      filter(frameId >= example$frameId[1] & frameId <= end$frameId[1])
  }
  
  return_data <- rbind(return_data, f)
}


ball_data <- return_data %>%
  filter(displayName == "football") %>%
  select(time, playId, frameId, gameId, x, y, s, a)

colnames(ball_data) <- c("time", "playId", "frameId", "gameId", 'ball_x', 'ball_y', 'ball_s', 'ball_a')

tmp <- return_data %>%
  right_join(ball_data, by = c("time", "playId", "frameId", "gameId"))

tmp <- tmp %>%
  mutate(diff_x = ball_x - x, diff_y = ball_y - y)

join_data <- plays %>%
  select(playId, gameId, possessionTeam, true_return)


tmp <- tmp %>%
  left_join(join_data, by = c("playId", "gameId"))

games_join_data <- games %>%
  select(gameId, homeTeamAbbr, visitorTeamAbbr)

tmp <- tmp %>%
  left_join(games_join_data, by = c("gameId"))

tmp <- tmp %>%
  mutate(OfforDef = ifelse(team == "home" & possessionTeam == homeTeamAbbr, 1, 0)) %>%
  mutate(OfforDef = ifelse(team == "away" & possessionTeam == visitorTeamAbbr, 1, OfforDef)) %>%
  mutate(OfforDef = ifelse(team == "football", 2, OfforDef))


