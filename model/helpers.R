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
    select(c(intersect(colnames(df2), colnames(df1)), "outcome"))
}



summarise_result <- function(res) {
  
  cat("### Confusion Matrix ###\n")
  
  print(confusionMatrix(res$mod))
  
  cat("\n")
  cat("\n")
  
  cat("### Top Model Metrics ###\n")
  
  print(res$mod$results %>%
          arrange(desc(Kappa)) %>%
          slice(1:10))
  
  cat("\n")
  cat("\n")
  
  cat("### Metric Summary ###\n")
  
  print(res$mod$results %>%
          arrange(desc(Kappa)) %>%
          slice(1:10) %>%
          summary())
  
  return(NULL)
  
}

