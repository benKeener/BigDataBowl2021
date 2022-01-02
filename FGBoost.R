library(xgboost)

fg_test <- fg_test %>%
  filter(!is.na(success))
#Divide the data randomly into train and test data 

set.seed(2002)

dt = sort(sample(nrow(fg_test), nrow(fg_test)*.7))

train<-fg_test[dt,]
test<-fg_test[-dt,]

#Convert train and test data to xgb.DMatrix

FGOE_train <- xgb.DMatrix(data = as.matrix(train %>% select(-old_game_id, -play_id, -success)), label = train$success)
FGOE_test <- xgb.DMatrix(data = as.matrix(test %>% select(-old_game_id, -play_id, -success )), label = test$success)
FGOE_full <- xgb.DMatrix(data = as.matrix(fg_test %>% select(-old_game_id, -play_id, -success )), label = fg_test$success)

#State the parameters for the gradient boost

params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eta = 0.2,
    gamma = .8,
    max_depth = 4,
    min_child_weight = 1,
    subsample = .5,
    colsample_bytree = 0.7,
    base_score = mean(fg_test$success)
  )


#Fine tuning using cv built into the xgboost package

XGboost_CV <- xgb.cv(params = params, data = FGOE_train, nrounds = 200, 
                         nfold = 5, showsd = TRUE, stratified = TRUE, print_every_n = 1, early_stopping_rounds = 25)

nrounds <- XGboost_CV$best_iteration


train_model <- xgboost(params = params, data = FGOE_train, nrounds = nrounds, verbose = 2)


#The feature importance of each data column factored in to the final model

importance <- xgb.importance(feature_names = colnames(train_model), model = train_model)

print(importance)

importance_plot <- xgb.ggplot.importance(importance_matrix = importance)

print(importance_plot)

#using the model to predict probable outcome of field goal

xgb_predict <- predict(train_model, FGOE_test)


test$prediction <- xgb_predict

test <- test %>% 
  mutate(error = success - prediction)

#Predict fg put come on full set of data

full_model <- xgboost(params = params, data = FGOE_full, nrounds = nrounds, verbose = 2)


xgb_full_predict <- predict(full_model, FGOE_full)


full_prediction_data <- data.frame("success" = fg_test$success, "prediction" = xgb_full_predict) %>% 
  mutate(error = success - prediction) %>%
  summarise(mean_abs_error = mean(abs(error)),
            mean_sq_error = mean(error^2),
            root_mean_sq_error = sqrt(mean(error^2)))

full_prediction_data$mean_abs_error 

full_prediction_data$mean_sq_error 

full_prediction_data$root_mean_sq_error

#Adding fg predictions to data

fg_test$efg <- xgb_full_predict

fg_join <- fg_test %>%
  select(old_game_id, play_id, efg)

pbp_kicker <- pbp %>%
  left_join(fg_join, by = c("old_game_id", "play_id")) %>%
  mutate(FGOE = success - efg) %>%
  filter(play_type == "field_goal" | play_type == "extra_point")


Kicker_ranking <- pbp_kicker %>%
  filter(play_type == "field_goal") %>%
  filter(season > 2017) %>%
  filter(!is.na(FGOE)) %>%
  group_by(kicker_player_name, season) %>%
  summarise(FGOE = mean(FGOE), kicks = n()) %>%
  filter(kicks > 20)
