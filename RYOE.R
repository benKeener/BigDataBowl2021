dt = sort(sample(nrow(flattened_df), nrow(flattened_df)*.75))

train<-flattened_df[dt,]
test<-flattened_df[-dt,]

newtrain <- xgb.DMatrix(data = as.matrix(train %>% dplyr::select(-gameId, -playId, -frameId, -true_return)), label = train$true_return)
newtest <- xgb.DMatrix(data = as.matrix(test %>% dplyr::select(-gameId, -playId, -frameId, -true_return)), label = test$true_return)
newfull <- xgb.DMatrix(data = as.matrix(flattened_df %>% dplyr::select(-gameId, -playId, -frameId, -true_return)), label = flattened_df$true_return)

params <-
  list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = 0.15,
    gamma = 0.2,
    max_depth = 3,
    min_child_weight = 8,
    subsample = .55,
    colsample_bytree = 0.3,
    max_delta_step = 0.5,
    base_score = mean(test$true_return)
  )

XGboost_CV <- xgb.cv(params = params, data = newtrain, nrounds = 100000, 
                     nfold = 10, showsd = TRUE, stratified = TRUE, print_every_n = 100, early_stopping_rounds = 25)

nrounds <- XGboost_CV$best_iteration

train_model <- xgboost(params = params, data = newtrain, nrounds = nrounds, verbose = 2, metrics = list("auc", "rmse"))

importance <- xgb.importance(feature_names = colnames(train_model), model = train_model)
print(importance)

importance_plot <- xgb.ggplot.importance(importance_matrix = importance)

print(importance_plot)


xgb_predict <- predict(train_model, newfull)

flattened_df$prediction <- xgb_predict

temp_predict <- ryoe %>%
  select(gameId, playId, frameId, prediction)

test_join <- tracking_full %>%
  left_join(temp_predict, by = c("gameId", "playId", "frameId"))

ryoe <- flattened_df %>%
  distinct(gameId, playId, .keep_all = TRUE)


ryoe <- ryoe %>%
  mutate(RYOE = true_return - prediction)

ryoe <- ryoe %>%
  select(gameId, playId, RYOE)

plays <- plays %>%
  left_join(ryoe, by = c("gameId", "playId"))

kickoff_returners <- full_pbp %>%
  filter(play_type == "kickoff") %>%
  select(old_game_id, play_id, kickoff_returner_player_name, kickoff_returner_player_id, season)

colnames(kickoff_returners) <- c("gameId", "playId", "kickoff_returner_name", "kickoff_returner_id", "season")

plays <- plays %>%
  mutate(gameId = as.character(gameId)) %>%
  left_join(kickoff_returners, by = c("gameId", "playId"))

plays <- plays %>%
  filter(!is.na(RYOE))

returner_rankings <- plays %>%
  group_by(kickoff_returner_name, kickoff_returner_id, season) %>%
  summarise(ryoe = mean(RYOE), ryoe_sd = sd(RYOE), n = n(), return_avg = mean(true_return), return_med = median(true_return)) %>%
  filter(n >= 8)

ggplot(returner_rankings, aes(x = ryoe, y = ryoe_sd)) +
  geom_point()

returner_rankings <- returner_rankings %>%
  mutate(returner_rating = .68*ryoe*exp((6 - ryoe_sd)) + .16*ryoe_sd) %>%
  mutate(norm_rr = (returner_rating + 80)/1.1)

touchback_rankings <- plays %>%
  filter(100 - yardlineNumber - kickLength <= 0) %>%
  group_by(kickoff_returner_name, kickoff_returner_id, season) %>%
  summarise(ryoe = mean(RYOE), ryoe_sd = sd(RYOE), n = n(), return_avg = mean(true_return), return_median = median(true_return)) %>%
  filter(n >= 8)
  
  
