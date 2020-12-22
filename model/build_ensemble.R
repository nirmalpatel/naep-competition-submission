options(readr.default_locale=readr::locale(tz="America/New_York"))

library(tidyverse)
library(caret)
library(caretEnsemble)
library(parallel)
library(doMC)
library(caTools)

# parallel processing stuff
registerDoMC(cores = (detectCores() - 1))

source("helpers.R")

# loading data
load("../features/features_combined.Rdata")

load("selected_features.Rdata")

train_features_10_fe <- train_features_10 %>%
  select(c("STUDENTID", train_feature_names[[1]], "outcome"))

train_features_20_fe <- train_features_20 %>%
  select(c("STUDENTID", train_feature_names[[2]], "outcome"))

train_features_30_fe <- train_features_30 %>%
  select(c("STUDENTID", train_feature_names[[3]], "outcome"))


# resampling spec ---------------------------------------------------------

set.seed(99)
train_control_10 <- trainControl(method="repeatedcv", number=10, repeats = 3,
                                 classProbs = TRUE, savePredictions = "final",
                                 index = createResample(train_features_10_fe$outcome, 30))
set.seed(99)
train_control_20 <- trainControl(method="repeatedcv", number=10, repeats = 3,
                                 classProbs = TRUE, savePredictions = "final",
                                 index = createResample(train_features_20_fe$outcome, 30))
set.seed(99)
train_control_30 <- trainControl(method="repeatedcv", number=10, repeats = 3,
                                 classProbs = TRUE, savePredictions = "final",
                                 index = createResample(train_features_30_fe$outcome, 30))

# ensembling --------------------------------------------------------------

mod_lists <- pmap(list(
  list(train_features_10_fe[, -c(1)], train_features_20_fe[, -c(1)],train_features_30_fe[, -c(1)]),
  list(test_features_10, test_features_20, test_features_30),
  list(train_control_10, train_control_20, train_control_30)),
  function(x, y, z) {
    
    parameter_grid_gbm <- expand.grid(interaction.depth = c(1, 2), n.trees = seq(200, 1000, by = 50),
                                      shrinkage=c(0.03, 0.02, 0.01, 0.0075, 0.005),
                                      n.minobsinnode = c(30, 35, 40, 45, 50))
    parameter_grid_RRF <- expand.grid(mtry = seq(2, 15, by = 2), coefImp = 0.0, coefReg = c(0.7, 0.85, 1.0))
    parameter_grid_rf <- expand.grid(mtry = seq(2, 15, by = 2))
    parameter_grid_regLogistic <- NULL
    parameter_grid_dwdPoly <- expand.grid(lambda = c(0.005, 0.01), qval = seq(0.00, 0.5, by = 0.05),
                                          degree = 2, scale = 0.01)
    parameter_grid_knn <- expand.grid(k = seq(10, 70, by = 5))
    parameter_grid_nb <- expand.grid(usekernel = c(TRUE, FALSE), fL = seq(0, 2), adjust = seq(0, 2, by = .5))
    parameter_grid_pls <- expand.grid(ncomp = seq(1, 10))
    parameter_grid_svmRadial <- expand.grid(C = seq(0.1, 1.0, by = 0.1), sigma = 2 ^ seq(-10, -5, length.out = 10))
    parameter_grid_dwdLinear <- expand.grid(qval = seq(0.01, 1, by = 0.05), lambda = 10 ^ seq(-6, -2, by = 1))
    parameter_grid_nnet <- expand.grid(size = seq(5, 9, by = 1), decay = seq(1, 5, length.out = 15))
    
    set.seed(99)
    
    model_list <- caretList(
      outcome ~ .,
      data = x,
      trControl = z,
      metric = "Kappa",
      methodList = c("bayesglm", "qda"),
      tuneList=list(
        gbm = caretModelSpec(method = "gbm", tuneGrid = parameter_grid_gbm),
        RRF = caretModelSpec(method = "RRF", tuneGrid = parameter_grid_RRF),
        rf = caretModelSpec(method = "rf", tuneGrid = parameter_grid_rf, ntree = 200),
        regLogistic = caretModelSpec(method = "regLogistic", tuneGrid = parameter_grid_regLogistic),
        dwdPoly = caretModelSpec(method = "dwdPoly", tuneGrid = parameter_grid_dwdPoly),
        knn = caretModelSpec(method = "knn", tuneGrid = parameter_grid_knn),
        nb = caretModelSpec(method = "nb", tuneGrid = parameter_grid_nb),
        pls = caretModelSpec(method = "pls", tuneGrid = parameter_grid_pls),
        svmRadial = caretModelSpec(method = "svmRadial", tuneGrid = parameter_grid_svmRadial),
        dwdLinear = caretModelSpec(method = "dwdLinear", tuneGrid = parameter_grid_dwdLinear),
        nnet = caretModelSpec(method = "nnet", tuneGrid = parameter_grid_nnet)
      )
    )
    
    model_list
    
  })

# ensemble model ----------------------------------------------------------

model_stacked_ensemble <- function(model_list, train_features, test_features) {
  
  set.seed(99)
  
  glm_ensemble <- caretStack(
    model_list,
    method="bayesglm",
    tuneGrid = NULL,
    metric="Kappa",
    trControl=trainControl(
      method="boot",
      number=10,
      savePredictions="final",
      classProbs=TRUE,
      summaryFunction=defaultSummary
    )
  )
  
  model_preds <- lapply(model_list, predict, type="prob")
  model_preds <- lapply(model_preds, function(x) x[,"SIG"])
  model_preds <- data.frame(model_preds)
  ens_preds <- predict(glm_ensemble, type="prob")
  model_preds$ensemble <- ens_preds
  
  print(colAUC(model_preds, train_features$outcome))
  
  predprobs_df <- tibble(
    STUDENTID = test_features$STUDENTID,
    prob = predict(glm_ensemble, newdata = test_features, type="prob")
  )
  
  list(
    mod_list = model_list,
    mod = glm_ensemble,
    test_pred_df = predprobs_df,
    train_pred_df = model_preds
  )
  
}

res_ensemble <- pmap(list(
  list(train_features_10_fe[, -c(1)], train_features_20_fe[, -c(1)],train_features_30_fe[, -c(1)]),
  list(test_features_10, test_features_20, test_features_30),
  mod_lists),
  function(x, y, z) {
  
  ens_resamples <- resamples(z)
  
  res_ensemble <- model_stacked_ensemble(z, x, y)
  
  list(
    ens = res_ensemble,
    resamples = ens_resamples
  )
  
})

modelCor(res_ensemble[[1]]$resamples)
modelCor(res_ensemble[[2]]$resamples)
modelCor(res_ensemble[[3]]$resamples)

res_ensemble[[1]]$ens$mod
res_ensemble[[2]]$ens$mod
res_ensemble[[3]]$ens$mod

summary(res_ensemble[[1]]$ens$mod$ens_model$finalModel)
summary(res_ensemble[[2]]$ens$mod$ens_model$finalModel)
summary(res_ensemble[[3]]$ens$mod$ens_model$finalModel)

# data export -------------------------------------------------------------

train_pred_df <- bind_rows(
  mutate(res_ensemble[[1]]$ens$train_pred_df, STUDENTID = train_features_10$STUDENTID,
         outcome = train_features_10$outcome, dataset = "10min"),
  mutate(res_ensemble[[2]]$ens$train_pred_df, STUDENTID = train_features_20$STUDENTID,
         outcome = train_features_20$outcome, dataset = "20min"),
  mutate(res_ensemble[[3]]$ens$train_pred_df, STUDENTID = train_features_30$STUDENTID,
         outcome = train_features_30$outcome, dataset = "30min")
) %>%
  mutate(method = "test") 

test_pred_df <- bind_rows(
  res_ensemble[[1]]$ens$test_pred_df,
  res_ensemble[[2]]$ens$test_pred_df,
  res_ensemble[[3]]$ens$test_pred_df
) %>%
  mutate(method = "test")

hist(test_pred_df$prob)

prop.table(table(test_pred_df$prob < .5))

df_hidden_labels <- read_csv("../data/hidden_label.csv", col_types = "c")

df_hidden_labels %>%
  left_join(test_pred_df) %>%
  mutate(prob = if_else(is.na(prob), mean(prob, na.rm = TRUE), prob)) %>%
  write_csv("../output/output.csv")

train_pred_df %>%
  group_by(dataset) %>%
  summarise(n_errors = sum(outcome == "SIG" & ensemble < .5) + sum(outcome == "NOSIG" & ensemble >= .5)) %>%
  ungroup()
