library(rstudioapi)
library(tidyverse)

setwd(file.path(rstudioapi::getActiveProject(), "features"))
rm(list = ls(all.names = TRUE))
source("features_as.R")
rm(list = ls(all.names = TRUE))
source("features_choicechange.R")
rm(list = ls(all.names = TRUE))
source("features_obsct.R")
rm(list = ls(all.names = TRUE))
source("features_pcmplt.R")
rm(list = ls(all.names = TRUE))
source("features_probcorrect.R")
rm(list = ls(all.names = TRUE))
source("features_probswitch.R")
rm(list = ls(all.names = TRUE))
source("features_combined.R")

setwd(file.path(rstudioapi::getActiveProject(), "model"))
rm(list = ls(all.names = TRUE))
source("build_ensemble.R")

setwd(rstudioapi::getActiveProject())
predictions <- read_csv("output/output.csv")$prob

write_lines(paste0(predictions, collapse = ","), "preds.txt")
