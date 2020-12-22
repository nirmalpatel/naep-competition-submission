library(tidyverse)

source("helpers_v2.R")

# loading data
source("../load_data.R")

generate_features <- function(eventdata) {
  
  # Total times calculator opened
  # Total times calculator opened per problem in which calc was used
  calc_df <- eventdata %>%
    filter(Observable == "Open Calculator") %>%
    group_by(STUDENTID) %>%
    summarise(obsct_n_prob_calc_used = n_distinct(AccessionNumber),
              obsct_n_calc_open = n(),
              obsct_n_calc_per_prob = obsct_n_calc_open / obsct_n_prob_calc_used) %>%
    ungroup()
  
  # Click Progress Navigator
  pnav_df <- eventdata %>%
    filter(Observable == "Click Progress Navigator") %>%
    group_by(STUDENTID) %>%
    summarise(obsct_n_prob_pnav_open = n_distinct(AccessionNumber),
              obsct_n_pnav_open = n()) %>%
    ungroup()
  
  # Total choices made
  nchoice_df <- eventdata %>%
    filter(str_detect(Observable, "Choice")) %>%
    group_by(STUDENTID) %>%
    summarise(obsct_n_choice = n()) %>%
    ungroup()
  
  # Total down scrolls
  # Total up scrolls
  nscroll_df <- eventdata %>%
    filter(str_detect(Observable, "Scroll")) %>%
    mutate(Observable = if_else(str_detect(ExtendedInfo, "down"),
                                "obsct_n_down_scrolls",
                                "obsct_n_up_scrolls")) %>%
    group_by(STUDENTID, Observable) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    spread(Observable, n)
  
  # Times received focus
  nfocus_df <- eventdata %>%
    filter(str_detect(Observable, "Receive Focus")) %>%
    group_by(STUDENTID) %>%
    summarise(obsct_n_recv_focus = n()) %>%
    ungroup()
  
  # # of times Texttospeech mode ON
  ttsmode_df <- eventdata %>%
    filter(ExtendedInfo == "TextToSpeech Read: TextToSpeechMode On") %>%
    group_by(STUDENTID) %>%
    summarise(obsct_n_tts_on = n()) %>%
    ungroup()
  
  # Times eliminated choice
  elimchoice_df <- eventdata %>%
    filter(Observable == "Eliminate Choice") %>%
    group_by(STUDENTID) %>%
    summarise(obsct_n_elim_choice = n()) %>%
    ungroup()
  
  # # of draws
  # of draws per unique problem
  draw_df <- eventdata %>%
    filter(Observable == "Draw") %>%
    group_by(STUDENTID) %>%
    summarise(obsct_n_draws = n(),
              obsct_n_draw_prob = n_distinct(AccessionNumber),
              obsct_n_draw_per_prob = obsct_n_draws / obsct_n_draw_prob) %>%
    ungroup()
  
  
  feature_df <- expand(eventdata, nesting(STUDENTID)) %>%
    left_join(calc_df) %>%
    left_join(pnav_df) %>%
    left_join(nchoice_df) %>%
    left_join(nscroll_df) %>%
    left_join(nfocus_df) %>%
    left_join(ttsmode_df) %>%
    left_join(elimchoice_df) %>%
    left_join(draw_df) %>%
    replace_na(list(obsct_n_prob_calc_used = 0,
                    obsct_n_calc_open = 0,
                    obsct_n_calc_per_prob = 0,
                    obsct_n_prob_pnav_open = 0,
                    obsct_n_pnav_open = 0,
                    obsct_n_choice = 0,
                    obsct_n_down_scrolls = 0,
                    obsct_n_up_scrolls = 0,
                    obsct_n_recv_focus = 0,
                    obsct_n_tts_on = 0,
                    obsct_n_elim_choice = 0,
                    obsct_n_draws = 0,
                    obsct_n_draw_prob = 0,
                    obsct_n_draw_per_prob = 0))
    
  

  feature_df
}

test_features_obsct_10 <- generate_features(df_hidden_10)
test_features_obsct_20 <- generate_features(df_hidden_20)
test_features_obsct_30 <- generate_features(df_hidden_30)

train_features_obsct_10 <- generate_features(df_train_10) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_obsct_10)

train_features_obsct_20 <- generate_features(df_train_20) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_obsct_20)

train_features_obsct_30 <- generate_features(df_train_30) %>%
  add_labels(df_train_labels) %>%
  keep_cols(test_features_obsct_30)

save(train_features_obsct_10, train_features_obsct_20, train_features_obsct_30,
     test_features_obsct_10, test_features_obsct_20, test_features_obsct_30,
     file = "features_obsct.Rdata")