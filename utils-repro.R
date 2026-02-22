############################################
#  Called automatically from main-repro.R  #
############################################
compare_csvs <- function(path1, path2) {
  df1 <- read.csv(path1, stringsAsFactors = FALSE)
  df2 <- read.csv(path2, stringsAsFactors = FALSE)
  df1 <- df1[do.call(order, df1),]
  df2 <- df2[do.call(order, df2),]
  discrepancies <- list()
  
  # Schema differences
  cols_only_in_1 <- setdiff(names(df1), names(df2))
  cols_only_in_2 <- setdiff(names(df2), names(df1))
  
  if (length(cols_only_in_1) > 0) {
    discrepancies <- append(discrepancies, lapply(cols_only_in_1, function(col) {
      data.frame(type = "missing_column", row = NA, column = col,
                 value_file1 = "present", value_file2 = "absent", stringsAsFactors = FALSE)
    }))
  }
  
  if (length(cols_only_in_2) > 0) {
    discrepancies <- append(discrepancies, lapply(cols_only_in_2, function(col) {
      data.frame(type = "missing_column", row = NA, column = col,
                 value_file1 = "absent", value_file2 = "present", stringsAsFactors = FALSE)
    }))
  }
  
  # Row count difference
  if (nrow(df1) != nrow(df2)) {
    discrepancies <- append(discrepancies, list(
      data.frame(type = "row_count", row = NA, column = NA,
                 value_file1 = as.character(nrow(df1)),
                 value_file2 = as.character(nrow(df2)), stringsAsFactors = FALSE)
    ))
  }
  
  # Cell-level differences on common columns and rows
  common_cols <- intersect(names(df1), names(df2))
  common_rows <- seq_len(min(nrow(df1), nrow(df2)))
  
  for (col in common_cols) {
    v1 <- as.character(df1[[col]][common_rows])
    v2 <- as.character(df2[[col]][common_rows])
    diff_rows <- which(v1 != v2 | (is.na(v1) != is.na(v2)))
    
    if (length(diff_rows) > 0) {
      discrepancies <- append(discrepancies, list(
        data.frame(type = "value_mismatch", row = diff_rows, column = col,
                   value_file1 = v1[diff_rows],
                   value_file2 = v2[diff_rows], stringsAsFactors = FALSE)
      ))
    }
  }
  
  if (length(discrepancies) == 0) {
    message("No discrepancies found.")
    return(invisible(data.frame()))
  }
  
  do.call(rbind, discrepancies)
}