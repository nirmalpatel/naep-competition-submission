options(readr.default_locale=readr::locale(tz="America/New_York"))

library(tidyverse)
library(caret)
library(caretEnsemble)
library(parallel)
library(doMC)

# parallel processing stuff
registerDoMC(cores = (detectCores() - 1))

source("helpers.R")

# loading data
load("../features/features_combined.Rdata")

ga_control <- gafsControl(method = "repeatedcv", number = 10, repeats = 3,
                          functions = treebagGA, allowParallel = FALSE, genParallel = TRUE,
                          metric = c(internal = "Kappa", external = "Kappa"),
                          maximize = c(internal = TRUE, external = TRUE),
                          verbose = TRUE)

# DO NOT RUN
# TAKES MANY HOURS

# set.seed(99)
# ga_mod_10_mix_v6 <- gafs(x = as.data.frame(select(train_features_10, -STUDENTID, -outcome)),
#                y = train_features_10$outcome,
#                iters = 60,
#                popSize = 1500,
#                gafsControl = ga_control)
# save(ga_mod_10_mix_v6, file = "ga/ga_mod_10_mix_v6.Rdata")

# set.seed(99)
# ga_mod_20_mix_v3 <- gafs(x = as.data.frame(select(train_features_20, -STUDENTID, -outcome)),
#                y = train_features_20$outcome,
#                iters = 60,
#                popSize = 1500,
#                gafsControl = ga_control)
# save(ga_mod_20_mix_v3, file = "ga/ga_mod_20_mix_v3.Rdata")

# set.seed(99)
# ga_mod_30_mix_v2 <- gafs(x = as.data.frame(select(train_features_30, -STUDENTID, -outcome)),
#                y = train_features_30$outcome,
#                iters = 60,
#                popSize = 600,
#                gafsControl = ga_control)
# save(ga_mod_30_mix_v2, file = "ga/ga_mod_30_mix_v2.Rdata")
