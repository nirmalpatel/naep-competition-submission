keep_cols <- function(df1, df2) {
  
  if ("outcome" %in% colnames(df1)) {
    outc_cnm <- "outcome"
  } else {
    outc_cnm <- NULL
  }
  
  df1 %>%
    select(c(intersect(colnames(df2), colnames(df1)), outc_cnm))
}

add_labels <- function(features, labels) {
  
  features %>%
    inner_join(labels, by = "STUDENTID") %>%
    rename(outcome = EfficientlyCompletedBlockB) %>%
    mutate(outcome = if_else(outcome == "True", "SIG", "NOSIG"),
           outcome = factor(outcome, c("SIG", "NOSIG"))) %>%
    ungroup()
  
}

scale_numeric <- function(x) { as.numeric(scale(x)) }

impute_mean <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}
