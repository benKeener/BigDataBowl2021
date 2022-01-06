kickers <- full_pbp %>%
  filter(play_type == "kickoff") %>%
  select(old_game_id, play_id, kicker_player_name, kicker_player_id, season)

colnames(kickers) <- c("gameId", "playId", "kicker_name", "kicker_id", "season")

plays <- plays %>%
  mutate(gameId = as.character(gameId)) %>%
  left_join(kickers, by = c("gameId", "playId"))

kicker_data <- plays %>%
  group_by(kicker_name, kicker_id, season.x) %>%
  select(kicker_name, kicker_id, RYOE, true_return, kickLength, yardlineNumber, season.x) %>%
  mutate(xRY = true_return - RYOE) %>%
  summarise(xRetYards = mean(xRY), n = n(), bad_kicks = sum(xRY > 25)) %>%
  filter(n > 16)

colnames(kicker_data) <- c("kicker_name", "kicker_id", "season", "xRetYards", "n", "bad_kicks")



kickerstb <- plays_tb %>%
  mutate(gameId = as.character(gameId)) %>%
  left_join(kickers, by = c("gameId", "playId")) %>%
  group_by(kicker_name, kicker_id, season) %>%
  summarise(touchbacks = n())

kicker_data <- kicker_data %>%
  left_join(kickerstb, by = c("kicker_name", "kicker_id", "season")) 

kicker_data <- kicker_data %>%
  mutate(total_XRY = (xRetYards*n + touchbacks*25)/(n + touchbacks), touchback_percentage = touchbacks/(n + touchbacks), 
         bad_kick_percentage = bad_kicks/(n + touchbacks))
