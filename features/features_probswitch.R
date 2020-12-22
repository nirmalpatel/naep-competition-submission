library(tidyverse)

source("helpers_v2.R")

# loading data
source("../load_data.R")

df_train_30 %>%
  arrange(STUDENTID, EventTime) %>%
  filter(str_detect(AccessionNumber, "VH"),
         Observable == "Enter Item") %>%
  group_by(STUDENTID) %>%
  mutate(rnum = row_number()) %>%
  ungroup() %>%
  group_by(AccessionNumber) %>%
  summarise(avg_rnum = mean(rnum)) %>%
  ungroup() %>%
  arrange(avg_rnum) %>%
  .$AccessionNumber -> AccessionNumberOrd

AccessionNumberPos <- setNames(1:length(AccessionNumberOrd), AccessionNumberOrd)

generate_features <- function(eventdata, prob_pos) {
  
  df_entries <- eventdata %>%
    filter(Observable == "Enter Item") %>%
    count(STUDENTID) %>%
    rename(probswitch_n_entries = n)
  
  df_fwdbwd <- eventdata %>%
    select(STUDENTID, EventTime, AccessionNumber) %>%
    group_by(STUDENTID) %>%
    arrange(EventTime) %>%
    mutate(Prev_AccessionNumber = lag(AccessionNumber)) %>%
    ungroup() %>%
    filter(AccessionNumber != Prev_AccessionNumber | is.na(Prev_AccessionNumber)) %>%
    mutate(cur_prob_pos = prob_pos[AccessionNumber],
           prev_prob_pos = prob_pos[Prev_AccessionNumber]) %>%
    filter(!is.na(cur_prob_pos), !is.na(prev_prob_pos)) %>%
    mutate(direction = if_else(prev_prob_pos < cur_prob_pos, "fwd", "bwd")) %>%
    group_by(STUDENTID) %>%
    summarise(probswitch_tot_fwd = sum(direction == "fwd"),
              probswitch_tot_bwd = sum(direction == "bwd"),
              probswitch_tot_switch = probswitch_tot_fwd + probswitch_tot_bwd,
              probswitch_pct_fwd = probswitch_tot_fwd / probswitch_tot_switch) %>%
    select(STUDENTID, probswitch_pct_fwd) %>%
    mutate(probswitch_pct_fwd = if_else(!is.finite(probswitch_pct_fwd), 0, probswitch_pct_fwd))
  
  df_entries %>%
    left_join(df_fwdbwd)
}

test_features_probswitch_10 <- generate_features(df_hidden_10, AccessionNumberPos)
test_features_probswitch_20 <- generate_features(df_hidden_20, AccessionNumberPos)
test_features_probswitch_30 <- generate_features(df_hidden_30, AccessionNumberPos)

train_features_probswitch_10 <- generate_features(df_train_10, AccessionNumberPos) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_probswitch_10)

train_features_probswitch_20 <- generate_features(df_train_20, AccessionNumberPos) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_probswitch_20)

train_features_probswitch_30 <- generate_features(df_train_30, AccessionNumberPos) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_probswitch_30)

save(train_features_probswitch_10, train_features_probswitch_20, train_features_probswitch_30,
     test_features_probswitch_10, test_features_probswitch_20, test_features_probswitch_30,
     file = "features_probswitch.Rdata")
