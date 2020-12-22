library(tidyverse)

source("helpers_v2.R")

# loading data
source("../load_data.R")

generate_features <- function(eventdata) {
  
  qtypes <- c("CompositeCR", "FillInBlank", "MatchMS", "MCSS", "MultipleFillInBlank")
  
  all_probs <- eventdata %>%
    filter(ItemType %in% qtypes) %>%
    count(AccessionNumber) %>%
    .$AccessionNumber
  
  lapply(qtypes, function(x) {
    
    df <- eventdata %>%
      filter(ItemType == x)
    
    if (x == "CompositeCR") {
      
      df %>%
        mutate(qpart = str_match(ExtendedInfo, "Part ([ABC])")[, 2]) %>%
        group_by(STUDENTID, AccessionNumber) %>%
        summarise(n_unq_part = n_distinct(qpart, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(pct_complete = n_unq_part / 3 * 100) %>%
        select(-n_unq_part)
      
    } else if (x == "FillInBlank") {
      
      df %>%
        group_by(STUDENTID, AccessionNumber) %>%
        summarise(pct_complete = any("Math Keypress" %in% Observable) * 100) %>%
        ungroup()
      
    } else if (x == "MatchMS") {
      
      df %>%
        filter(Observable == "DropChoice") %>%
        arrange(STUDENTID, AccessionNumber, desc(EventTime)) %>%
        group_by(STUDENTID, AccessionNumber) %>%
        mutate(rnum = row_number()) %>%
        filter(rnum == 1) %>%
        ungroup() %>%
        mutate(n_target = str_count(ExtendedInfo, "target")) %>%
        mutate(pct_complete = n_target / 4 * 100) %>%
        select(STUDENTID, AccessionNumber, pct_complete)
      
    } else if (x == "MCSS") {
      
      df %>%
        group_by(STUDENTID, AccessionNumber) %>%
        summarise(pct_complete = any(Observable == "Click Choice") * 100) %>%
        ungroup()
      
    } else if (x == "MultipleFillInBlank") {
      
      df %>%
        group_by(STUDENTID, AccessionNumber) %>%
        summarise(pct_complete = any(Observable == "Math Keypress") * 100) %>%
        ungroup()
      
    }
    
  }) %>%
    bind_rows() -> edata_pcmplt
  
  feature_df <- expand(eventdata, nesting(STUDENTID), AccessionNumber = all_probs) %>%
    left_join(edata_pcmplt) %>%
    replace_na(list(pct_complete = 0)) %>%
    mutate(AccessionNumber = paste0("pcmplt_", AccessionNumber)) %>%
    spread(AccessionNumber, pct_complete, fill = 0)
  
  tot_df <- expand(eventdata, nesting(STUDENTID), AccessionNumber = all_probs) %>%
    left_join(edata_pcmplt) %>%
    replace_na(list(pct_complete = 0)) %>%
    group_by(STUDENTID) %>%
    summarise(pcmplt_tot_pct_complete = mean(pct_complete)) %>%
    ungroup()
  
  feature_df %>%
    left_join(tot_df)
  
}

test_features_pcmplt_10 <- generate_features(df_hidden_10) %>%
  select(STUDENTID, pcmplt_tot_pct_complete)
test_features_pcmplt_20 <- generate_features(df_hidden_20) %>%
  select(STUDENTID, pcmplt_tot_pct_complete)
test_features_pcmplt_30 <- generate_features(df_hidden_30) %>%
  select(STUDENTID, pcmplt_tot_pct_complete)

train_features_pcmplt_10 <- generate_features(df_train_10) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_pcmplt_10) %>%
  select(STUDENTID, pcmplt_tot_pct_complete, outcome)

train_features_pcmplt_20 <- generate_features(df_train_20) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_pcmplt_20) %>%
  select(STUDENTID, pcmplt_tot_pct_complete, outcome)

train_features_pcmplt_30 <- generate_features(df_train_30) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_pcmplt_30) %>%
  select(STUDENTID, pcmplt_tot_pct_complete, outcome)

save(train_features_pcmplt_10, train_features_pcmplt_20, train_features_pcmplt_30,
     test_features_pcmplt_10, test_features_pcmplt_20, test_features_pcmplt_30,
     file = "features_pcmplt.Rdata")
