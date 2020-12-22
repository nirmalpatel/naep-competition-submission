options(readr.default_locale=readr::locale(tz="America/New_York"))

library(tidyverse)
library(caret)
library(gbm)
library(parallel)
library(doMC)

# parallel processing stuff
registerDoMC(cores = (detectCores() - 1))

source("helpers_v1.R")

# loading data
source("../load_data.R")

# preprocessing

generate_features <- function(eventdata){
  blacklist_problems_1 <- eventdata %>%
    filter(Observable %in% c("Enter Item", "Exit Item")) %>%
    arrange(STUDENTID, EventTime) %>%
    count(STUDENTID, AccessionNumber) %>%
    filter(n %% 2 != 0)
  
  blacklist_problems_2 <- eventdata %>%
    anti_join(blacklist_problems_1) %>%
    filter(Observable %in% c("Enter Item", "Exit Item")) %>%
    arrange(STUDENTID, AccessionNumber, EventTime) %>%
    group_by(STUDENTID, AccessionNumber) %>%
    mutate(Observable_Next = lead(Observable)) %>%
    ungroup() %>%
    filter((Observable == "Enter Item" & Observable_Next == "Enter Item") |
             (Observable == "Exit Item" & Observable_Next == "Exit Item")) %>%
    select(STUDENTID, AccessionNumber) %>% 
    unique()
  
  blacklist_problems_3 <- eventdata %>% 
    filter(AccessionNumber %in% c('EOSTimeLft', 'SecTimeOut')) %>% 
    select(STUDENTID, AccessionNumber) %>% 
    unique()
  
  filtered_eventdata <- eventdata %>% 
    filter(!is.na(EventTime)) %>% 
    anti_join(blacklist_problems_1) %>% 
    anti_join(blacklist_problems_2) %>% 
    anti_join(blacklist_problems_3) %>% 
    group_by(STUDENTID) %>% 
    arrange(EventTime) %>% 
    mutate(event_end_time = lead(EventTime)) %>% 
    ungroup() %>% 
    mutate(event_end_time = if_else(is.na(event_end_time), EventTime, event_end_time),
           event_time_taken = as.numeric(difftime(event_end_time, EventTime, units = 'secs')))
  
  perc_time_df_1 <- filtered_eventdata %>% 
    filter(str_detect(AccessionNumber, '^V')) %>% 
    group_by(AccessionNumber, STUDENTID) %>%
    summarise(time_taken = sum(event_time_taken)) %>% 
    ungroup() %>% 
    group_by(AccessionNumber) %>% 
    arrange(time_taken) %>% 
    mutate(percentile = cume_dist(time_taken)*100) %>% 
    ungroup() %>% 
    mutate(less_than_25 = percentile < 25) %>% 
    select(-time_taken)
  
  perc_time_df_2 <- perc_time_df_1 %>% 
    group_by(STUDENTID) %>% 
    arrange(percentile) %>% 
    summarise(percentile_min = min(percentile),
              percentile_max = max(percentile),
              # percentile_mean = mean(percentile),
              # percentile_median = median(percentile),
              percentile_1st_quantile = quantile(percentile)[[2]],
              percentile_3rd_quantile = quantile(percentile)[[4]]) %>% 
    ungroup()
  
  perc_time_df <- perc_time_df_1 %>% 
    group_by(STUDENTID) %>% 
    summarise(percentile_less_than_25 = sum(as.integer(less_than_25))/n()*100) %>% 
    ungroup() %>% 
    left_join(perc_time_df_2)
  
  problem_vs_other_df <- filtered_eventdata %>% 
    mutate(category = if_else(str_detect(AccessionNumber, '^V'), 'Problem', 'Other')) %>% 
    group_by(STUDENTID, category) %>% 
    summarise(n_content = n_distinct(AccessionNumber),
              time_spent = sum(event_time_taken)) %>% 
    ungroup() %>% 
    gather(feature, value, -STUDENTID, -category) %>% 
    mutate(feature = paste0(category, "_", feature)) %>% 
    select(-category) %>% 
    spread(feature, value, 0) %>% 
    mutate(total_time = Problem_time_spent + Other_time_spent,
           Problem_solving_time_perc = Problem_time_spent/total_time*100) %>% 
    select(-Other_time_spent, -Problem_time_spent, -Problem_n_content, -total_time)
  
  n_observable_df <- filtered_eventdata %>% 
    group_by(STUDENTID) %>% 
    summarise(n_observables = n_distinct(Observable)) %>% 
    ungroup()
  
  observable_filter_df <- data.frame(ItemType = c('MCSS', 'MCSS', 'MultipleFillInBlank', 'MCSS', 'FillInBlank'),
                                     Observable = c('Calculator Buffer', 'Eliminate Choice', 'Lose Focus', 'Vertical Item Scroll', 'Vertical Item Scroll'),
                                     keep = rep(1, 5))

  observable_time_taken_df <- filtered_eventdata %>%
    filter(ItemType %in% c('BlockReview', 'CompositeCR', 'FillInBlank', 'MatchMS', 'MCSS', 'MultipleFillInBlank'),
           !Observable %in% c('Enter Item', 'Next', 'Exit Item')) %>%
    filter(Observable %in% c('Calculator Buffer', 'Eliminate Choice', 'Lose Focus', 'Vertical Item Scroll', 'Click Choice', 'DropChoice',
                             'Change Theme', 'Draw', 'Equation Editor Button', 'Erase', 'Increase Zoom', 'Decrease Zoom', 'Highlight',
                             'Move Calculator', 'Receive Focus', 'Scratchwork Mode On', 'Scratchwork Mode Off', 'Scratchwork Draw Mode On',
                             'Scratchwork Erase Mode On', 'Scratchwork Highlight Mode On', 'Hide Timer', 'Show Timer')) %>%
    left_join(observable_filter_df) %>%
    mutate(keep = if_else(is.na(keep) & !Observable %in% c('Calculator Buffer', 'Eliminate Choice', 'Lose Focus', 'Vertical Item Scroll'),
                          1, keep)) %>%
    filter(keep == 1) %>%
    select(-keep) %>%
    mutate(Observable = if_else(str_detect(Observable, 'Zoom'), 'Zoom Event', Observable),
           Observable = if_else(str_detect(Observable, 'Scratchwork'), 'Scratchwork Event', Observable),
           Observable = if_else(str_detect(Observable, 'Timer'), 'Timer Event', Observable),
           ItemType = str_replace_all(ItemType, ' ', '_'),
           Observable = str_replace_all(Observable, ' ', '_'),
           Observable = paste0(ItemType, '_', Observable, '_time_taken')) %>%
    group_by(STUDENTID, Observable) %>%
    summarise(time_taken = as.numeric(sum(event_time_taken))) %>%
    ungroup() %>%
    spread(Observable, time_taken, 0)
  
  mcss_problem_solved_df <- filtered_eventdata %>% 
    filter(ItemType == 'MCSS') %>% 
    group_by(STUDENTID) %>% 
    mutate(MCSS_items_attempted = n_distinct(AccessionNumber)) %>% 
    ungroup() %>% 
    group_by(STUDENTID, AccessionNumber) %>% 
    mutate(MCSS_items_solved = as.integer(any(Observable == 'Click Choice'))) %>% 
    ungroup() %>% 
    select(STUDENTID, AccessionNumber, MCSS_items_attempted, MCSS_items_solved) %>% 
    unique() %>% 
    group_by(STUDENTID, MCSS_items_attempted) %>% 
    summarise(MCSS_items_solved = sum(MCSS_items_solved)) %>% 
    ungroup() %>% 
    mutate(MCSS_items_solved_perc = MCSS_items_solved/MCSS_items_attempted*100) %>% 
    select(-MCSS_items_solved)
  
  
  attempts_per_item_df <- filtered_eventdata %>% 
    filter(str_detect(AccessionNumber, '^V'), Observable == 'Enter Item') %>% 
    group_by(STUDENTID, AccessionNumber) %>% 
    summarise(n_attempts = n()) %>% 
    ungroup() %>% 
    mutate(one_attempt_flag = if_else(n_attempts == 1, 1, 0)) %>% 
    group_by(STUDENTID) %>% 
    summarise(avg_attempts_per_item = sum(n_attempts)/n(),
              perc_items_with_1_attempt = sum(one_attempt_flag)/n()*100) %>% 
    ungroup() 
  
  avg_time_event_per_item_df <- filtered_eventdata %>% 
    filter(str_detect(AccessionNumber, '^V'), !Observable %in% c('Enter Item', 'Next', 'Exit Item')) %>% 
    group_by(STUDENTID, AccessionNumber) %>% 
    summarise(n_events = n(),
              time = sum(event_time_taken)) %>% 
    ungroup() %>% 
    group_by(STUDENTID) %>% 
    summarise(avg_events_per_item = sum(n_events)/n_distinct(AccessionNumber),
              avg_time_per_item = sum(time)/n_distinct(AccessionNumber)) %>% 
    ungroup()
  
  perc_empty_extended_info_df <- filtered_eventdata %>% 
    filter(!Observable %in% c('Enter Item', 'Next', 'Exit Item')) %>% 
    mutate(extended_empty = if_else(is.na(ExtendedInfo), 1, 0)) %>% 
    group_by(STUDENTID) %>% 
    summarise(perc_events_empty_extended_info = sum(extended_empty)/n()*100,
              perc_time_empty_extended_info = sum(if_else(extended_empty == 1, event_time_taken, 0))/sum(event_time_taken)*100) %>% 
    ungroup()
  
  stu_item_time_deviation_df <- filtered_eventdata %>% 
    filter(str_detect(AccessionNumber, '^V')) %>% 
    group_by(STUDENTID, AccessionNumber) %>% 
    summarise(total_time = sum(event_time_taken),
              start_time = min(EventTime)) %>% 
    ungroup() %>% 
    group_by(AccessionNumber) %>% 
    mutate(avg_time = sum(total_time)/n_distinct(STUDENTID)) %>% 
    ungroup() %>% 
    mutate(time_deviation_from_avg = total_time/avg_time) %>% 
    group_by(STUDENTID) %>% 
    arrange(start_time) %>% 
    mutate(seq = row_number()) %>% 
    do(mod_lin = lm(time_deviation_from_avg~seq, data = .)) %>% 
    mutate(time_deviation_intercept = mod_lin$coefficients[1],
           time_deviation_slope = mod_lin$coefficients[2]) %>% 
    ungroup() %>% 
    select(-mod_lin)
  
  mcss_answer_df <- filtered_eventdata %>% 
    filter(ItemType == 'MCSS', Observable == 'Click Choice') %>% 
    mutate(choice_selected = str_extract(ExtendedInfo, '_\\d+')) %>% 
    group_by(AccessionNumber, STUDENTID) %>% 
    filter(EventTime == max(EventTime)) %>% 
    ungroup() %>% 
    group_by(AccessionNumber, choice_selected) %>% 
    summarise(n_students = n_distinct(STUDENTID)) %>% 
    ungroup() %>% 
    group_by(AccessionNumber) %>% 
    mutate(total_students = sum(n_students)) %>% 
    filter(n_students == max(n_students)) %>% 
    ungroup() %>% 
    mutate(perc_students = n_students/total_students*100) %>% 
    rename(probable_answer = choice_selected) %>% 
    select(AccessionNumber, probable_answer)
  
  mcss_correct_answer_df <- filtered_eventdata %>% 
    filter(ItemType == 'MCSS', Observable == 'Click Choice') %>% 
    mutate(choice_selected = str_extract(ExtendedInfo, '_\\d+')) %>% 
    group_by(AccessionNumber, STUDENTID) %>% 
    filter(EventTime == max(EventTime)) %>% 
    ungroup() %>% 
    left_join(mcss_answer_df) %>% 
    mutate(correct_answer = as.integer(choice_selected == probable_answer)) %>% 
    group_by(STUDENTID) %>% 
    summarise(MCSS_perc_probable_correct_answer = sum(correct_answer)/n()*100) %>% 
    ungroup()
  
  
  perc_time_df %>% 
    inner_join(problem_vs_other_df) %>% 
    inner_join(n_observable_df) %>% 
    inner_join(observable_time_taken_df) %>%
    # inner_join(mcss_problem_solved_df) %>% 
    # inner_join(mcss_correct_answer_df) %>% 
    inner_join(stu_item_time_deviation_df) %>% 
    inner_join(perc_empty_extended_info_df) %>% 
    inner_join(avg_time_event_per_item_df) %>% 
    inner_join(attempts_per_item_df) %>% 
    select(-time_deviation_intercept, -perc_time_empty_extended_info, -perc_items_with_1_attempt, -percentile_less_than_25,
           -avg_time_per_item, -perc_events_empty_extended_info)
}

add_labels <- function(features, labels) {
  
  features %>%
    inner_join(labels, by = "STUDENTID") %>%
    mutate(EfficientlyCompletedBlockB = if_else(EfficientlyCompletedBlockB == "True", 1, 0)) %>%
    rename(outcome = EfficientlyCompletedBlockB) %>%
    mutate(outcome = factor(outcome, c(1, 0))) %>%
    ungroup()
  
}

keep_cols_train <- function(df1, df2) {
  if(all(colnames(df2) %in% colnames(df1 %>% select(-outcome)))){
    df1 %>%
      select(c(colnames(df2), "outcome"))
  }
  else{
    df1 %>% 
      select(c(colnames(df2[colnames(df2) %in% colnames(df1)])), "outcome")
  }
}

keep_cols_test <- function(df1, df2) {
  
  if(all(colnames(df1) %in% colnames(df2 %>% select(-outcome)))){
    df1
  }
  else{
    df1 %>% 
      select(c(colnames(df2 %>% select(-outcome))))
  }
}

test_features_10_aditya <- generate_features(df_hidden_10) 
test_features_20_aditya <- generate_features(df_hidden_20) 
test_features_30_aditya <- generate_features(df_hidden_30) 

train_features_10_aditya <- generate_features(df_train_10) %>%
  add_labels(df_train_labels) %>%
  keep_cols_train(test_features_10_aditya) %>%
  mutate(outcome = factor(if_else(outcome == "1", "SIG", "NOSIG"), c("SIG", "NOSIG")))

train_features_20_aditya <- generate_features(df_train_20) %>%
  add_labels(df_train_labels) %>%
  keep_cols_train(test_features_20_aditya) %>%
  mutate(outcome = factor(if_else(outcome == "1", "SIG", "NOSIG"), c("SIG", "NOSIG")))

train_features_30_aditya <- generate_features(df_train_30) %>%
  add_labels(df_train_labels) %>%
  keep_cols_train(test_features_30_aditya) %>%
  mutate(outcome = factor(if_else(outcome == "1", "SIG", "NOSIG"), c("SIG", "NOSIG")))

test_features_10_aditya <- keep_cols_test(test_features_10_aditya, train_features_10_aditya)
test_features_20_aditya <- keep_cols_test(test_features_20_aditya, train_features_20_aditya)
test_features_30_aditya <- keep_cols_test(test_features_30_aditya, train_features_30_aditya)

any(is.na(train_features_10_aditya))
any(is.na(train_features_20_aditya))
any(is.na(train_features_30_aditya))
any(is.na(test_features_10_aditya))
any(is.na(test_features_20_aditya))
any(is.na(test_features_30_aditya))

set.seed(99)
gbm_varimp_mods <- lapply(list(train_features_10_aditya, train_features_20_aditya, train_features_30_aditya), function(x) {
  
  mod <- train(outcome ~ ., data = x[, -1],
               method = "gbm", trControl = trainControl(method = "repeatedcv", repeats = 3),
               tuneGrid = expand.grid(interaction.depth=c(1, 2), n.trees = c(200, 250, 300, 400, 500),
                                      shrinkage=c(0.04, 0.03, 0.02, 0.01),
                                      n.minobsinnode=c(5, 10, 20)),
               metric = "Kappa")
  
  mod
  
})

confusionMatrix(gbm_varimp_mods[[1]])
confusionMatrix(gbm_varimp_mods[[2]])
confusionMatrix(gbm_varimp_mods[[3]])

plot(varImp(gbm_varimp_mods[[1]]))
plot(varImp(gbm_varimp_mods[[2]]))
plot(varImp(gbm_varimp_mods[[3]]))

train_feature_names <- lapply(gbm_varimp_mods, function(x) {
  
  varImp(x)$importance %>%
    mutate(varnm = rownames(.)) %>%
    select(varnm, Overall) %>%
    arrange(desc(Overall)) %>%
    # filter(Overall > 4) %>%
    slice(1:10) %>%
    .$varnm
  
})

features_as_top_20 <- lapply(gbm_varimp_mods, function(x) {
  
  varImp(x)$importance %>%
    mutate(varnm = rownames(.)) %>%
    select(varnm, Overall) %>%
    arrange(desc(Overall)) %>%
    slice(1:20) %>%
    .$varnm
  
})

train_features_10_as <- train_features_10_aditya %>%
  select(c("STUDENTID", train_feature_names[[1]], "outcome"))

train_features_20_as <- train_features_20_aditya %>%
  select(c("STUDENTID", train_feature_names[[2]], "outcome"))

train_features_30_as <- train_features_30_aditya %>%
  select(c("STUDENTID", features_as_top_20[[3]], "outcome"))

# train_features_10_as <- train_features_10_aditya %>%
#   select(c("STUDENTID", starts_with("percentile"), "outcome"))
# 
# train_features_20_as <- train_features_20_aditya %>%
#   select(c("STUDENTID", starts_with("percentile"), "outcome"))
# 
# train_features_30_as <- train_features_30_aditya %>%
#   select(c("STUDENTID", starts_with("percentile"), "outcome"))

test_features_10_as <- keep_cols_test(test_features_10_aditya, train_features_10_as)
test_features_20_as <- keep_cols_test(test_features_20_aditya, train_features_20_as)
test_features_30_as <- keep_cols_test(test_features_30_aditya, train_features_30_as)

save(train_features_10_as, train_features_20_as, train_features_30_as,
     test_features_10_as, test_features_20_as, test_features_30_as,
     file = "features_as.Rdata")

# save(features_as_top_20, file = "features_as_top_20.Rdata")
