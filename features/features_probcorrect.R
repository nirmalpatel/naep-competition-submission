library(tidyverse)

source("helpers_v2.R")

# loading data
source("../load_data.R")

mcss_correctans <- df_train_30 %>%
  filter(ItemType == "MCSS", Observable == "Click Choice") %>%
  count(AccessionNumber, ExtendedInfo) %>%
  arrange(AccessionNumber, desc(n)) %>%
  group_by(AccessionNumber) %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  select(-n) %>%
  rename(CorrectAns = ExtendedInfo)

generate_procorrect_features <- function(rdf, mcss_corrects) {
  
  df <- rdf %>%
    arrange(STUDENTID, AccessionNumber, EventTime)
  
  all_stuid <- tibble(STUDENTID = as.character(unique(df$STUDENTID)))
  
  ### Match MS (all done)
  
  # VH139047
  df_VH139047 <- df %>%
    filter(AccessionNumber == "VH139047") %>%
    mutate(n_opt = str_count(ExtendedInfo, "source")) %>%
    filter(n_opt == 4) %>%
    group_by(STUDENTID) %>%
    filter(EventTime == max(EventTime)) %>%
    ungroup() %>%
    mutate(probcorrect_VH139047_score = str_detect(ExtendedInfo, '\\"source\\"\\:\\"1\\",\\"target\\"\\:2') +
           str_detect(ExtendedInfo, '\\"source\\"\\:\\"2\\",\\"target\\"\\:3') +
           str_detect(ExtendedInfo, '\\"source\\"\\:\\"3\\",\\"target\\"\\:1') +
           str_detect(ExtendedInfo, '\\"source\\"\\:\\"4\\",\\"target\\"\\:4')) %>%
    select(STUDENTID, probcorrect_VH139047_score)
   
  
  ### MultipleFillInBlank (all done)
   
  # VH134366
  df_VH134366_subprob <- df %>%
    filter(AccessionNumber == "VH134366") %>%
    group_by(STUDENTID) %>%
    mutate(min_ts = min(na.omit(EventTime[Observable == "Receive Focus"])),
           max_ts = max(na.omit(EventTime[Observable == "Lose Focus"]))) %>%
    ungroup() %>%
    filter(is.finite(min_ts), is.finite(max_ts)) %>%
    filter(EventTime >= min_ts, EventTime <= max_ts) %>%
    filter(Observable %in% c("Math Keypress", "Receive Focus", "Lose Focus")) %>%
    mutate(subprobnum = if_else(str_detect(Observable, "Focus"), as.integer(ExtendedInfo), NA_integer_)) %>%
    mutate(subprobnum = DataCombine::FillDown(Var = subprobnum)) %>%
    filter(Observable == "Math Keypress") %>%
    group_by(STUDENTID, subprobnum) %>%
    filter(EventTime == max(EventTime)) %>%
    ungroup() %>%
    mutate(typed_num = str_match(ExtendedInfo, '"contentLaTeX":"\\$(.*)\\$","code":"(.*)"')[, 2],
           next_code = str_match(ExtendedInfo, '"contentLaTeX":"\\$(.*)\\$","code":"(.*)"')[, 3]) %>%
    mutate(subprob_score = case_when(
      subprobnum == 1 ~ case_when(
        typed_num == "3.7" & next_code == "Digit5" ~ 1,
        typed_num == "3.75" & next_code != "Backspace" ~ 1,
        TRUE ~ 0
      ),
      subprobnum == 2 ~ case_when(
        typed_num == "5" & !(str_detect(next_code, ("Backspace|Digit"))) ~ 1,
        typed_num == "5.0" & next_code == "Digit0" ~ 1,
        typed_num == "5.00" & !(str_detect(next_code, "Digit")) ~ 1,
        TRUE ~ 0
      ),
      subprobnum == 3 ~ case_when(
        typed_num == "6.2" & next_code == "Digit5" ~ 1,
        typed_num == "6.25" & next_code != "Backspace" ~ 1,
        TRUE ~ 0
      ),
      subprobnum == 4 ~ case_when(
        typed_num == "7.5" & next_code == "Digit0" ~ 1,
        typed_num == "7." & next_code == "Digit5" ~ 1,
        typed_num == "7.50" & next_code != "Backspace" ~ 1,
        TRUE ~ 0
      ),
      subprobnum == 5 ~ case_when(
        typed_num == "8.7" & next_code == "Digit5" ~ 1,
        typed_num == "8.75" & next_code != "Backspace" ~ 1,
        TRUE ~ 0
      ),
      TRUE ~ 0
    ))
  
  df_VH134366 <- df_VH134366_subprob %>%
    group_by(STUDENTID) %>%
    summarise(probcorrect_VH134366_score = sum(subprob_score)) %>%
    ungroup()
  
  ### MCSS
  df_mcss <- rdf %>%
    filter(ItemType == "MCSS", Observable == "Click Choice") %>%
    group_by(STUDENTID, AccessionNumber) %>%
    filter(EventTime == max(EventTime)) %>%
    ungroup() %>%
    inner_join(mcss_corrects) %>%
    mutate(correct = as.integer(ExtendedInfo == CorrectAns)) %>%
    select(STUDENTID, AccessionNumber, correct) %>%
    mutate(AccessionNumber = paste0("probcorrect_", AccessionNumber, "_score")) %>%
    spread(AccessionNumber, correct, fill = 0)
  
  ### CompositeCR (1 remaining)
  
  
  ### FillInBlank (2 remaining)
  
  pctval <- function(x) {
    x / max(x, na.rm = TRUE) * 100
  }
  
  # browser()
  
  # combining results
  res <- all_stuid %>%
    left_join(df_VH139047) %>%
    left_join(df_VH134366) %>%
    left_join(df_mcss) %>% # v2 feature
    mutate_if(is.numeric, pctval) %>%
    gather(key = "varnm", value = "varval", -STUDENTID) %>%
    mutate(varval = if_else(is.na(varval), 0, varval)) %>%
    group_by(STUDENTID) %>%
    summarise(probcorrect_avg_score = mean(varval)) %>%
    ungroup()
  
  res[is.na(res)] <- 0
  
  res
  
}

test_features_probcorrect_10 <- generate_procorrect_features(df_hidden_10, mcss_correctans)

test_features_probcorrect_20 <- generate_procorrect_features(df_hidden_20, mcss_correctans)

test_features_probcorrect_30 <- generate_procorrect_features(df_hidden_30, mcss_correctans)

train_features_probcorrect_10 <- generate_procorrect_features(df_train_10, mcss_correctans) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_probcorrect_10)

train_features_probcorrect_20 <- generate_procorrect_features(df_train_20, mcss_correctans) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_probcorrect_20)

train_features_probcorrect_30 <- generate_procorrect_features(df_train_30, mcss_correctans) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_probcorrect_30)


save(train_features_probcorrect_10, train_features_probcorrect_20, train_features_probcorrect_30,
     test_features_probcorrect_10, test_features_probcorrect_20, test_features_probcorrect_30,
     file = "features_probcorrect.Rdata")
