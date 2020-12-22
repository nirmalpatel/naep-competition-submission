library(tidyverse)
library(rstudioapi)
library(lubridate)


df_hidden_10 <- read_csv(file.path(rstudioapi::getActiveProject(), "data/data_a_hidden_10.csv"),
                         col_types = cols(STUDENTID = col_character()))

df_hidden_20 <- read_csv(file.path(rstudioapi::getActiveProject(), "data/data_a_hidden_20.csv"),
                         col_types = cols(STUDENTID = col_character()))

df_hidden_30 <- read_csv(file.path(rstudioapi::getActiveProject(), "data/data_a_hidden_30.csv"),
                         col_types = cols(STUDENTID = col_character()))

df_hidden_labels <- read_csv(file.path(rstudioapi::getActiveProject(), "data/hidden_label.csv"),
                             col_types = cols(STUDENTID = col_character()))

df_train <- read_csv(file.path(rstudioapi::getActiveProject(), "data/data_a_train.csv"),
                     col_types = cols(STUDENTID = col_character()))

df_train_labels <- read_csv(file.path(rstudioapi::getActiveProject(), "data/data_train_label.csv"),
                            col_types = "cc")

df_train_ts <- df_train %>%
  group_by(STUDENTID) %>%
  mutate(start_time = min(EventTime)) %>%
  ungroup() %>%
  mutate(ts_10min = start_time + lubridate::minutes(10),
         ts_20min = start_time + lubridate::minutes(20))

df_train_10 <- df_train_ts %>%
  filter(EventTime <= ts_10min) %>%
  select(-start_time, -ts_10min, -ts_20min)

df_train_20 <- df_train_ts %>%
  filter(EventTime <= ts_20min) %>%
  select(-start_time, -ts_10min, -ts_20min)

df_train_30 <- df_train

rm(df_train)
rm(df_train_ts)

