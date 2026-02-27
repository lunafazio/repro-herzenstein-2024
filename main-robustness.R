# Manual setup  --------------------------------------------------------------
setwd("YOUR-PATH-HERE/Code and Data/code") # set working directory
ncores = 8 # cores available for parallelization, adjust to your hardware

# Packages -------------------------------------------------------------------
library(future)
library(furrr)
library(patchwork)
library(progressr)
library(purrr)
library(readr)
library(writexl)
handlers(global = TRUE)
source("utils-robustness.R")

# Robustness specification ---------------------------------------------------
robustness_savepath = "results/ROBUSTNESS_oldnew_results.csv"
years = tibble(year = 2007:2014)
sections = tibble(section = c("abstract", "fulltext", "study"))
emb_params = tibble(emb_param = c("sg0_window3_dim100_epochs50_stopwords1",
                                  "sg1_window3_dim100_epochs50_stopwords1",
                                  "sg0_window5_dim100_epochs50_stopwords1",
                                  "sg0_window3_dim50_epochs50_stopwords1",
                                  "sg0_window3_dim150_epochs50_stopwords1",
                                  "sg0_window3_dim100_epochs100_stopwords1",
                                  "sg0_window3_dim100_epochs50_stopwords0"))
metric = tibble(metric = c("auc", "brier"))
seeds = tibble(seed = c(25,2025,42,1991,777,331,909,58,531,9720))

all_conditions = years %>%
  cross_join(sections) %>%
  cross_join(emb_params) %>%
  cross_join(metric) %>%
  cross_join(seeds)  

# Run all conditions ---------------------------------------------------------
# (loads saved results if already available)
if(file.exists(robustness_savepath)) {
  message("Saved results will be loaded\n",
    "To re-calculate, delete ", robustness_savepath)
  results = read_csv(robustness_savepath)
} else {
  plan(multisession, workers = ncores)
  with_progress({
    pbar = progressor(steps = nrow(all_conditions))
    results = all_conditions %>%
      mutate(result = future_pmap(
        list(year, section, emb_param, metric, seed),
        func_oldnew,
        .options = furrr_options(
          seed = NULL,
          globals = c(globals_furrr, pbar = pbar)
        )
      )) %>%
      mutate(result = map(result, as_tibble)) %>%
      unnest(result) %>%
      select(-section, -emb_param, -`Number of Splits`)
  })
  plan(sequential)
  write.csv(results, robustness_savepath, row.names = FALSE)
}

# Robustness analysis --------------------------------------------------------
## Only keep years with reasonable sample size
results = results %>% filter(2008 <= year & year <= 2013)
## Result plots
## > Shows that general behavior is stable across for w2v choice
plot_auc_mean = func_robplot_mean(results, "auc")
plot_brier_mean = func_robplot_mean(results, "brier")
## > Shows that the estimates are fairly stable despite low nrep
plot_auc_range = func_robplot_range(results, "auc")
plot_brier_range = func_robplot_range(results, "brier")
plot_joint_range = (plot_auc_range | plot_brier_range) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
## > Make table showing sample size per year
table_sampxtime = func_nstudy(intersect(years, results[,"year"]))
## > Álso present plot information in table form
table_joint_range = func_rangetb(results)

# Export results -------------------------------------------------------------
write_xlsx(table_sampxtime, "results/ROBUSTNESS_sample_over_time.xlsx")
write_xlsx(table_joint_range, "results/ROBUSTNESS_results_table.xlsx")
ggsave("results/ROBUSTNESS_plot_auc_mean.png", plot_auc_mean, width = 11,
  height = 4, dpi = 300)
ggsave("results/ROBUSTNESS_plot_brier_mean.png", plot_brier_mean, width = 11,
  height = 4, dpi = 300)
ggsave("results/ROBUSTNESS_plot_joint_range.png", plot_joint_range, width = 6,
  height = 4, dpi = 300)
