library(tidyverse)
library(rstudioapi)
library(tcR)
library(markovchain)
library(doMC)
library(plot.matrix)
library(caret)

source("helpers_v1.R")

# loading data
source("../load_data.R")

# loading separate features
load("features_choicechange.Rdata")
load("features_obsct.Rdata")
load("features_pcmplt.Rdata")
load("features_probcorrect.Rdata")
load("features_probswitch.Rdata")
load("features_as.Rdata")

# preprocessing

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

accession_pos <- setNames(1:length(AccessionNumberOrd), AccessionNumberOrd)

generate_problem_eventlog <- function(eventdata) {
  
  eventdata_clean <- eventdata %>%
    select(STUDENTID, EventTime, AccessionNumber) %>%
    group_by(STUDENTID) %>%
    arrange(EventTime) %>%
    mutate(Prev_AccessionNumber = lag(AccessionNumber)) %>%
    ungroup() %>%
    filter(AccessionNumber != Prev_AccessionNumber | is.na(Prev_AccessionNumber)) %>%
    select(-Prev_AccessionNumber)
  
  list(df = eventdata_clean,
       possible_states = unique(eventdata$AccessionNumber))
}

# this function generates the 'behavior prototypes' of efficient and inefficient students
# transition matrices and their unrolled vectors are calculated for both positive and negative examples
# this function returns 2 vectors for global comparison and nothing student level is returned here
generate_transition_vecs <- function(problem_eventlog, labels, possible_states) {
  
  true_ids <- labels %>%
    filter(EfficientlyCompletedBlockB == "True")
  
  seqdat1_df <- filter(problem_eventlog, STUDENTID %in% true_ids$STUDENTID)
  
  seqdat1 <- lapply(unique(seqdat1_df$STUDENTID), function(x) {
    seqdat1_df %>%
      filter(STUDENTID == x) %>%
      .$AccessionNumber
  })
  
  seqdat2_df <- filter(problem_eventlog, !(STUDENTID %in% true_ids$STUDENTID))
  
  seqdat2 <- lapply(unique(seqdat2_df$STUDENTID), function(x) {
    seqdat2_df %>%
      filter(STUDENTID == x) %>%
      .$AccessionNumber
  })
  
  pos_states <- possible_states
  
  mcfit_true <- markovchainFit(seqdat1, "map", possibleStates = pos_states)
  mcfit_false <- markovchainFit(seqdat2, "map", possibleStates = pos_states)
  
  tmat_vec_true <- as.vector(mcfit_true$estimate@transitionMatrix)
  tmat_vec_false <- as.vector(mcfit_false$estimate@transitionMatrix)
  
  list(
    eventlog_df = problem_eventlog,
    true_vec = tmat_vec_true,
    true_mat = mcfit_true$estimate@transitionMatrix,
    false_vec = tmat_vec_false,
    false_mat = mcfit_false$estimate@transitionMatrix
  )
  
}

# this function computes several features
# jump to the later part of this function to see the cosine similarity features
# read the inline comments to know about other features
generate_features <- function(eventdata, problem_eventlog, true_tvec, false_tvec,
                              possible_states) {
  
  eventdata <- eventdata %>%
    filter(AccessionNumber %in% possible_states)
  
  problem_eventlog <- problem_eventlog %>%
    filter(AccessionNumber %in% possible_states)
  
  # time spent on each problem features
  
  # if you enter an item, you should exit it
  # if this doesn't hold, we throw out such instances
  blacklist_problems_1 <- eventdata %>%
    filter(Observable %in% c("Enter Item", "Exit Item")) %>%
    arrange(STUDENTID, EventTime) %>%
    count(STUDENTID, AccessionNumber) %>%
    filter(n %% 2 != 0)
  
  # you cannot enter an item twice and exit an item twice
  # these data points are also thrown out
  blacklist_problems_2 <- eventdata %>%
    anti_join(blacklist_problems_1) %>%
    filter(Observable %in% c("Enter Item", "Exit Item")) %>%
    arrange(STUDENTID, AccessionNumber, EventTime) %>%
    group_by(STUDENTID, AccessionNumber) %>%
    mutate(Observable_Next = lead(Observable)) %>%
    ungroup() %>%
    filter((Observable == "Enter Item" & Observable_Next == "Enter Item") |
             (Observable == "Exit Item" & Observable_Next == "Exit Item")) %>%
    select(STUDENTID, AccessionNumber)
  
  # calculating the time spent on each question
  df_calc_time_1 <- eventdata %>%
    anti_join(blacklist_problems_1) %>%
    anti_join(blacklist_problems_2) %>%
    filter(Observable %in% c("Enter Item", "Exit Item")) %>%
    arrange(STUDENTID, AccessionNumber, EventTime) %>%
    group_by(STUDENTID, AccessionNumber) %>%
    mutate(Observable_Next = lead(Observable),
           EventTime_Next = lead(EventTime)) %>%
    ungroup() %>%
    filter(!(Observable == "Exit Item" & is.na(Observable_Next)))
  
  df_calc_time_within <- df_calc_time_1 %>%
    filter(Observable == "Enter Item" & Observable_Next == "Exit Item") %>%
    mutate(Duration = (EventTime_Next - EventTime))
  
  df_calc_time_within$Duration = as.numeric(df_calc_time_within$Duration)
  
  agg_time <- df_calc_time_within %>%
    group_by(STUDENTID,AccessionNumber) %>%
    summarise(total_time = sum(Duration)) %>%
    ungroup()
  
  # featurizing the time spent between each action/question
  df_hiatus <- df_calc_time_1 %>%
    filter(Observable == "Exit Item" & Observable_Next == "Enter Item") %>%
    mutate(Hiatus = (EventTime_Next - EventTime)) %>%
    group_by(STUDENTID,AccessionNumber) %>%
    summarise(total_hiatus = sum(Hiatus)) %>%
    ungroup() %>%
    group_by(STUDENTID) %>%
    summarise(min_hiatus = as.numeric(min(total_hiatus)),
              mean_hiatus = as.numeric(mean(total_hiatus)),
              max_hiatus = as.numeric(max(total_hiatus))) %>%
    ungroup()
  
  # featurizing the hiatus distribution of a student
  df_hiatus_extra <- eventdata %>%
    group_by(STUDENTID) %>%
    mutate(EventTime_Next = lead(EventTime)) %>%
    ungroup() %>%
    filter(!is.na(EventTime_Next)) %>%
    mutate(tdiff = as.numeric(EventTime_Next - EventTime)) %>%
    group_by(STUDENTID) %>%
    summarise(n_hiatus_0_5 = sum(tdiff < 0.5),
              n_hiatus_1 = sum(tdiff >= 0.5 & tdiff < 1),
              n_hiatus_5 = sum(tdiff >= 1 & tdiff < 5),
              n_hiatus_10 = sum(tdiff >= 5 & tdiff < 10),
              n_hiatus_20 = sum(tdiff >= 10 & tdiff < 20),
              n_hiatus_50 = sum(tdiff >= 20 & tdiff < 50),
              n_hiatus_100 = sum(tdiff >= 50 & tdiff < 100),
              n_hiatus_100_plus = sum(tdiff >= 100)) %>%
    ungroup()
  
  # ttc means time to choice
  # how fast do students choose the MCQ option
  df_ttc <- eventdata %>%
    filter(ItemType == "MCSS", Observable %in% c("Enter Item", "Click Choice")) %>%
    arrange(STUDENTID, AccessionNumber, EventTime) %>%
    group_by(STUDENTID) %>%
    mutate(EventTime_Next = lead(EventTime),
           Observable_Next = lead(Observable)) %>%
    ungroup() %>%
    filter(Observable == "Enter Item", Observable_Next == "Click Choice") %>%
    group_by(STUDENTID, AccessionNumber) %>%
    filter(EventTime == min(EventTime)) %>%
    ungroup() %>%
    mutate(time_to_choice = as.numeric(EventTime_Next - EventTime)) %>%
    group_by(AccessionNumber) %>%
    mutate(time_to_choice = as.numeric(scale(time_to_choice)),
           time_to_choice = if_else(time_to_choice > 3.5, 3.5, time_to_choice),
           time_to_choice = if_else(time_to_choice < -3.5, -3.5, time_to_choice)) %>%
    group_by(STUDENTID) %>%
    summarise(min_ttc = min(time_to_choice),
              mean_ttc = mean(time_to_choice),
              max_ttc = max(time_to_choice),
              sd_ttc = sd(time_to_choice)) %>%
    ungroup()
  
  features <- spread(agg_time, key = AccessionNumber, value = total_time, fill = 0) %>%
    left_join(df_hiatus) %>%
    left_join(df_hiatus_extra) %>%
    left_join(df_ttc)
  
  # cosine similarity to transition dynamics related to true and false labels
  pos_states <- possible_states
  
  # for every student, apply the given function that calculates 3 features
  # true_sim - cosine similarity to the positive behavior prototype
  # false_sim - cosine similarity to the negative behavior prototype
  lapply(unique(problem_eventlog$STUDENTID), function(sel_studentid) {
    
    evts <- problem_eventlog %>%
      filter(STUDENTID == sel_studentid) %>%
      .$AccessionNumber
    
    if (length(evts) < 2) {
      return(data.frame())
    }
    
    mcfit <- markovchainFit(evts, "map", possibleStates = pos_states)
    
    true_sim <- cosine.similarity(as.vector(mcfit$estimate@transitionMatrix), true_tvec)
    false_sim <- cosine.similarity(as.vector(mcfit$estimate@transitionMatrix), false_tvec)
    
    data.frame(
      STUDENTID = sel_studentid,
      true_sim = true_sim,
      false_sim = false_sim,
      stringsAsFactors = FALSE
    )
    
  }) %>%
    bind_rows() -> tmat_sim_rdf
  
  tmat_sim_rdf %>%
    as_tibble() %>%
    mutate(true_minus_false_sim = true_sim - false_sim) -> tmat_sim_df
  
  inner_join(features, tmat_sim_df, by = "STUDENTID")
}

generate_tslotct_features <- function(eventdata, tslot_len, accession_pos_vec) {
  
  # eventdata <- df_train_30
  # tslot_len <- 30
  
  lapply(split(eventdata, eventdata$STUDENTID), function(x) {
    

    etimes <- x$EventTime
    etimes_slot <- seq(min(etimes, na.rm = TRUE), min(etimes, na.rm = TRUE) + minutes(tslot_len),
                       by = "1 min")
    
    x %>%
      mutate(problem_pos = accession_pos_vec[AccessionNumber]) %>%
      mutate(q1 = quantile(etimes_slot, .2),
             q2 = quantile(etimes_slot, .4),
             q3 = quantile(etimes_slot, .6),
             q4 = quantile(etimes_slot, .8),
             tslot = case_when(
               EventTime <= q1 ~ "S1",
               EventTime <= q2 ~ "S2",
               EventTime <= q3 ~ "S3",
               EventTime <= q4 ~ "S4",
               TRUE ~ "S5"
             )) %>%
      ungroup() %>%
      group_by(STUDENTID, tslot) %>%
      summarise(n_prob = n_distinct(AccessionNumber),
                n_enters = sum(Observable == "Enter Item"),
                n_calc_opens = sum(Observable == "Open Calculator"),
                n_rows = log(n()),
                avg_problem_pos = mean(problem_pos, na.rm = TRUE),
                n_problems = n_distinct(AccessionNumber),
                min_problem_pos = min(problem_pos, na.rm = TRUE),
                sd_problem_pos = sd(problem_pos, na.rm = TRUE),
                max_problem_pos = max(problem_pos, na.rm = TRUE)) %>%
      ungroup()
    
  }) %>%
    bind_rows() -> rdf
  
  nrow_zscoreavg_rdf <- rdf %>%
    gather(key = "varnm", value = "varval", -STUDENTID, -tslot) %>%
    group_by(tslot, varnm) %>%
    mutate(varval = as.numeric(scale(varval))) %>%
    ungroup() %>%
    group_by(STUDENTID, varnm) %>%
    summarise(varval_zscoreavg = mean(varval, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(varnm = paste0("tslotct_", varnm, "_avgzval")) %>%
    spread(varnm, varval_zscoreavg)
  
  max_problem_pos_rdf <- rdf %>%
    select(STUDENTID, tslot, max_problem_pos) %>%
    mutate(tslot = paste0("tslotct_max_problem_pos_", tslot)) %>%
    spread(tslot, max_problem_pos)
  
  nrow_zscoreavg_rdf %>%
    full_join(max_problem_pos_rdf) %>%
    mutate_if(is.numeric, meanimpute)
  
}

add_labels <- function(features, labels) {
  
  features %>%
    inner_join(labels, by = "STUDENTID") %>%
    mutate(EfficientlyCompletedBlockB = if_else(EfficientlyCompletedBlockB == "True", 1, 0)) %>%
    rename(outcome = EfficientlyCompletedBlockB) %>%
    mutate(outcome = factor(outcome, c(1, 0))) %>%
    ungroup()
  
}

keep_cols <- function(df1, df2) {
  df1 %>%
    select(c(colnames(df2), "outcome"))
}

# problem eventlogs
df_train_10_problem_elog <- generate_problem_eventlog(df_train_10)
df_train_20_problem_elog <- generate_problem_eventlog(df_train_20)
df_train_30_problem_elog <- generate_problem_eventlog(df_train_30)

df_hidden_10_problem_elog <- generate_problem_eventlog(df_hidden_10)
df_hidden_20_problem_elog <- generate_problem_eventlog(df_hidden_20)
df_hidden_30_problem_elog <- generate_problem_eventlog(df_hidden_30)

# transition vectors
df_10_tvecs <- generate_transition_vecs(df_train_10_problem_elog$df, df_train_labels,
                                        df_train_10_problem_elog$possible_states)
df_20_tvecs <- generate_transition_vecs(df_train_20_problem_elog$df, df_train_labels,
                                        df_train_20_problem_elog$possible_states)
df_30_tvecs <- generate_transition_vecs(df_train_30_problem_elog$df, df_train_labels,
                                        df_train_30_problem_elog$possible_states)

test_features_10_raw <- generate_features(df_hidden_10, df_hidden_10_problem_elog$df,
                                      df_10_tvecs$true_vec, df_10_tvecs$false_vec,
                                      df_train_10_problem_elog$possible_states) %>%
  left_join(generate_tslotct_features(df_hidden_10, 10, accession_pos)) %>%
  left_join(test_features_choicechange_10) %>%
  left_join(test_features_obsct_10) %>%
  left_join(test_features_pcmplt_10) %>%
  left_join(test_features_probcorrect_10) %>%
  left_join(test_features_probswitch_10) %>%
  left_join(test_features_10_as) %>%
  mutate_if(is.numeric, meanimpute)
  

test_features_20_raw <- generate_features(df_hidden_20, df_hidden_20_problem_elog$df,
                                      df_20_tvecs$true_vec, df_20_tvecs$false_vec,
                                      df_train_20_problem_elog$possible_states) %>%
  left_join(generate_tslotct_features(df_hidden_20, 20, accession_pos)) %>%
  left_join(test_features_choicechange_20) %>%
  left_join(test_features_obsct_20) %>%
  left_join(test_features_pcmplt_20) %>%
  left_join(test_features_probcorrect_20) %>%
  left_join(test_features_probswitch_20) %>%
  left_join(test_features_20_as) %>%
  mutate_if(is.numeric, meanimpute)

test_features_30_raw <- generate_features(df_hidden_30, df_hidden_30_problem_elog$df,
                                      df_30_tvecs$true_vec, df_30_tvecs$false_vec,
                                      df_train_30_problem_elog$possible_states) %>%
  left_join(generate_tslotct_features(df_hidden_30, 30, accession_pos)) %>%
  left_join(test_features_choicechange_30) %>%
  left_join(test_features_obsct_30) %>%
  left_join(test_features_pcmplt_30) %>%
  left_join(test_features_probcorrect_30) %>%
  left_join(test_features_probswitch_30) %>%
  left_join(test_features_30_as) %>%
  mutate_if(is.numeric, meanimpute)

train_features_10_raw <- generate_features(df_train_10, df_train_10_problem_elog$df,
                                       df_10_tvecs$true_vec, df_10_tvecs$false_vec,
                                       df_train_10_problem_elog$possible_states) %>%
  left_join(generate_tslotct_features(df_train_10, 10, accession_pos)) %>%
  left_join(select(train_features_choicechange_10, -outcome)) %>%
  left_join(select(train_features_obsct_10, -outcome)) %>%
  left_join(select(train_features_pcmplt_10, -outcome)) %>%
  left_join(select(train_features_probcorrect_10, -outcome)) %>%
  left_join(select(train_features_probswitch_10, -outcome)) %>%
  left_join(select(train_features_10_as, -outcome)) %>%
  mutate_if(is.numeric, meanimpute) %>%
  add_labels(df_train_labels) %>%
  mutate(outcome = factor(if_else(outcome == "1", "SIG", "NOSIG"), c("SIG", "NOSIG"))) %>%
  keep_cols(test_features_10_raw)

train_features_20_raw <- generate_features(df_train_20, df_train_20_problem_elog$df,
                                       df_20_tvecs$true_vec, df_20_tvecs$false_vec,
                                       df_train_20_problem_elog$possible_states) %>%
  left_join(generate_tslotct_features(df_train_20, 20, accession_pos)) %>%
  left_join(select(train_features_choicechange_20, -outcome)) %>%
  left_join(select(train_features_obsct_20, -outcome)) %>%
  left_join(select(train_features_pcmplt_20, -outcome)) %>%
  left_join(select(train_features_probcorrect_20, -outcome)) %>%
  left_join(select(train_features_probswitch_20, -outcome)) %>%
  left_join(select(train_features_20_as, -outcome)) %>%
  mutate_if(is.numeric, meanimpute) %>%
  add_labels(df_train_labels) %>%
  mutate(outcome = factor(if_else(outcome == "1", "SIG", "NOSIG"), c("SIG", "NOSIG"))) %>%
  keep_cols(test_features_20_raw)

train_features_30_raw <- generate_features(df_train_30, df_train_30_problem_elog$df,
                                       df_30_tvecs$true_vec, df_30_tvecs$false_vec,
                                       df_train_30_problem_elog$possible_states) %>%
  left_join(generate_tslotct_features(df_train_30, 30, accession_pos)) %>%
  left_join(select(train_features_choicechange_30, -outcome)) %>%
  left_join(select(train_features_obsct_30, -outcome)) %>%
  left_join(select(train_features_pcmplt_30, -outcome)) %>%
  left_join(select(train_features_probcorrect_30, -outcome)) %>%
  left_join(select(train_features_probswitch_30, -outcome)) %>%
  left_join(select(train_features_30_as, -outcome)) %>%
  mutate_if(is.numeric, meanimpute) %>%
  add_labels(df_train_labels) %>%
  mutate(outcome = factor(if_else(outcome == "1", "SIG", "NOSIG"), c("SIG", "NOSIG"))) %>%
  keep_cols(test_features_30_raw)


preproc_features <- function(x) {
  
  require(DescTools)
  
  x_win <- x %>%
    mutate_if(is.numeric, Winsorize, na.rm = TRUE)
  
  preproc_mod <- preProcess(as.data.frame(select(x_win, -STUDENTID, -outcome)),
                            method = c("zv", "nzv", "center", "scale", "corr", "YeoJohnson"))
  
  preproc_x <- predict(preproc_mod, newdata = x_win)
  
  list(
    train_df = filter(preproc_x, !is.na(outcome)) %>%
      as_tibble(),
    test_df = filter(preproc_x, is.na(outcome)) %>%
      select(-outcome) %>%
      as_tibble()
  )
  
}

preproc_features_10 <- preproc_features(bind_rows(train_features_10_raw, test_features_10_raw))
preproc_features_20 <- preproc_features(bind_rows(train_features_20_raw, test_features_20_raw))
preproc_features_30 <- preproc_features(bind_rows(train_features_30_raw, test_features_30_raw))

train_features_10 <- preproc_features_10$train_df
test_features_10 <- preproc_features_10$test_df

train_features_20 <- preproc_features_20$train_df
test_features_20 <- preproc_features_20$test_df

train_features_30 <- preproc_features_30$train_df
test_features_30 <- preproc_features_30$test_df

train_features_10 %>%
  ggplot(aes(outcome, n_hiatus_0_5)) +
  geom_boxplot()

# preproc_features_20 <- preproc_features(bind_rows(
#   select(train_features_20_raw, colnames(preproc_features_10$train_df)),
#   select(test_features_20_raw, colnames(preproc_features_10$test_df))))
# 
# preproc_features_30 <- preproc_features(bind_rows(
#   select(train_features_30_raw, colnames(preproc_features_10$train_df)),
#   select(test_features_30_raw, colnames(preproc_features_10$test_df))))
# 
# train_features_20 <- preproc_features_20$train_df %>%
#   keep_cols(train_features_10)
# test_features_20 <- preproc_features_20$test_df
# 
# train_features_30 <- preproc_features_30$train_df %>%
#   keep_cols(train_features_10)
# test_features_30 <- preproc_features_30$test_df

any(is.na(train_features_10))
any(is.na(train_features_20))
any(is.na(train_features_30))
any(is.na(test_features_10))
any(is.na(test_features_20))
any(is.na(test_features_30))

save(train_features_10, train_features_20, train_features_30,
     test_features_10, test_features_20, test_features_30,
     file = "features_combined.Rdata")
