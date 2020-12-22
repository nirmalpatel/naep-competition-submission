library(tidyverse)

source("helpers_v2.R")

# loading data
source("../load_data.R")

generate_features <- function(eventdata) {
  
  lapply(unique(eventdata$STUDENTID), function(sid) {
    
    stu_df <- eventdata %>%
      filter(STUDENTID == sid) %>%
      filter(Observable == "DropChoice")
    
    tstamps <-  stu_df$EventTime
    choice_texts <- stu_df$ExtendedInfo
    
    pmap(list(
      str_match_all(choice_texts, '\\"source\\"\\:\\"([1-4])\\",\\"target\\"\\:([1-4])'),
      tstamps), function(x, y) {
        mat <- x[, c(2, 3), drop = FALSE]
        colnames(mat) <- c("source", "target")
        as.data.frame(mat, stringsAsFactors = FALSE) %>%
          mutate(tstamp = y)
      }) %>%
      bind_rows() %>%
      mutate(STUDENTID = sid)
    
  }) %>%
    bind_rows() %>%
    as_tibble() %>%
    distinct() -> tmpdf
  
  tmpdf %>%
    arrange(STUDENTID, target, tstamp) %>%
    group_by(STUDENTID, target, source) %>%
    summarise(min_ts = min(tstamp)) %>%
    ungroup() %>%
    arrange(STUDENTID, target, min_ts) %>%
    group_by(STUDENTID, target) %>%
    mutate(next_ts = lead(min_ts)) %>%
    ungroup() %>%
    mutate(hiatus = as.numeric(next_ts - min_ts)) %>%
    replace_na(list(hiatus = 0)) %>%
    group_by(STUDENTID) %>%
    summarise(choicechange_tot_time = sum(hiatus)) %>%
    ungroup() -> df
  
  n_change_df <- tmpdf %>%
    select(STUDENTID, source, target) %>%
    distinct() %>%
    group_by(STUDENTID, source) %>%
    summarise(n_changes = n_distinct(target) - 1) %>%
    ungroup() %>%
    group_by(STUDENTID) %>%
    summarise(choicechange_tot_changes = sum(n_changes)) %>%
    ungroup()
  
  expand(eventdata, nesting(STUDENTID)) %>%
    left_join(df) %>%
    left_join(n_change_df) %>%
    replace_na(list(choicechange_tot_changes = 0,
                    choicechange_tot_time = 0))
  
}

test_features_choicechange_10 <- generate_features(df_hidden_10)
test_features_choicechange_20 <- generate_features(df_hidden_20)
test_features_choicechange_30 <- generate_features(df_hidden_30)

train_features_choicechange_10 <- generate_features(df_train_10) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_choicechange_10)

train_features_choicechange_20 <- generate_features(df_train_20) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_choicechange_20)

train_features_choicechange_30 <- generate_features(df_train_30) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_choicechange_30)

save(train_features_choicechange_10, train_features_choicechange_20, train_features_choicechange_30,
     test_features_choicechange_10, test_features_choicechange_20, test_features_choicechange_30,
     file = "features_choicechange.Rdata")