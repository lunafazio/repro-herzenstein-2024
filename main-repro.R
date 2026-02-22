library(dplyr)
library(tools)
# Manual setup required:
# - copy author-shared results into og-results folder,
#   will be used to compare against local outputs
# - set working directory
setwd("YOUR-PATH-HERE/Code and Data/code")
# disable lines 29, 32 in interpretation_main
# modify line 17 (ncores), disable 38 in prediction_supp_<elasticnet/xgboost>
# disable lines 39, 53 in prediction_marketprice
# disable line 36, 50 in prediction_oldnew_split
# modify line 17 (ncores), disable 41, 49, 62 in prediction_main

# helper function
source("utils-repro.R")
# script parameters
sections <- c("abstract", "fulltext", "study")
embedding_params_list <- c(
  "sg0_window3_dim100_epochs50_stopwords1",
  "sg1_window3_dim100_epochs50_stopwords1",
  "sg0_window5_dim100_epochs50_stopwords1",
  "sg0_window3_dim50_epochs50_stopwords1",
  "sg0_window3_dim150_epochs50_stopwords1",
  "sg0_window3_dim100_epochs100_stopwords1",
  "sg0_window3_dim100_epochs50_stopwords0"
)

# COMPUTATIONAL REPRODUCTION -------------------------------------------------
# 1. Clear results folder and metadata_imputed
## (these are outputs of the pipeline)
file.remove(list.files("../results", full.names = TRUE))
file.remove("../processed_data/metadata_imputed.csv")

# 2. Generate imputed metadata
source("impute_missing_values.R")
compare_csvs(
  "../processed_data/metadata_imputed.csv",
  "../og-results/og-metadata_imputed.csv"
) # different but reasonably close, procedure is stochastic
  # using original to avoid spurious downstream discrepancies
file.copy(
  from = "../og-results/og-metadata_imputed.csv", 
  to = "../processed_data/metadata_imputed.csv", 
  overwrite = TRUE
)

# 3. Interpretation scripts
source("interpretation_vignette.R")
compare_csvs(
  "../results/study_vignette_comparisons.csv",
  "../og-results/study_vignette_comparisons.csv"
) # identical!

source("interpretation_narrative.R")
compare_csvs(
  "../results/arc_of_narrative_coefficients_neat.csv",
  "../og-results/arc_of_narrative_coefficients_neat.csv"
) # identical!

for (section in sections) {
  source("interpretation_main.R")
}
for (section in sections) {
  compare_csvs(
    sprintf("../results/%s_liwc_and_control_coefs.csv", section),
    sprintf("../og-results/%s_liwc_and_control_coefs.csv", section)
  ) %>% mutate(diff = as.numeric(value_file1) - as.numeric(value_file2)) %>%
    pull(diff) %>% quantile %>%
    print # trivial differences
}

# 4. Prediction scripts
## tasks below append into performance_summary_supp.csv
for (section in sections) {
  source("prediction_supp_elasticnet.R")
  source("prediction_supp_xgboost.R")
}

compare_csvs(
  "../results/performance_summary_supp.csv",
  "../og-results/performance_summary_supp.csv"
) %>% mutate(diff = as.numeric(value_file1) - as.numeric(value_file2)) %>%
  pull(diff) %>% quantile # trivial differences

## tasks below append into performance_summary_main
for (section in sections) {
  for (embedding_params in embedding_params_list) {
    source("prediction_marketprice.R")
  }
}

for (section in sections) {
  for (embedding_params in embedding_params_list) {
    source("prediction_oldnew_split.R")
  }
}

for (exclude_econ in c(FALSE, TRUE)) {
  for (section in sections) {
    for (embedding_params in embedding_params_list) {
      # we only use 100 reps for speed
      source("prediction_main.R")
    }
  }
}

compare_csvs(
  "../results/performance_summary_main.csv",
  "../og-results/performance_summary_main.csv"
) %>% mutate(diff = as.numeric(value_file1) - as.numeric(value_file2)) %>%
  pull(diff) %>% quantile # reasonably close given lower reps

# 5. Postprocessing files
source("combine_tables.R")
compare_csvs(
  "../results/one_at_a_time_liwc_coefficients_neat.csv",
  "../og-results/one_at_a_time_liwc_coefficients_neat.csv"
) # identical!
source("supplemental_material.R")
for (i in c("a","b","c")) {
  compare_csvs(
    sprintf("../results/SM_tableS5%s.csv", i),
    sprintf("../og-results/SM_tableS5%s.csv", i)
  )
} # identical!
