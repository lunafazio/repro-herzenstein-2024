###############################################
#  Called automatically from main-robustness  #
###############################################
# Additional functions for robustness analysis -------------------------------
## Based on prediction_oldnew_split.R
func_oldnew <- function(
  year_threshold,  # originally 2011
  section, # "abstract", "fulltext", "study"
  embedding_params, # "sg0_window3_dim100_epochs50_stopwords1"
                    # "sg1_window3_dim100_epochs50_stopwords1"
                    # "sg0_window5_dim100_epochs50_stopwords1"
                    # "sg0_window3_dim50_epochs50_stopwords1"
                    # "sg0_window3_dim150_epochs50_stopwords1"
                    # "sg0_window3_dim100_epochs100_stopwords1"
                    # "sg0_window3_dim100_epochs50_stopwords0"
  metric, # "auc", "brier"
  seed = 25 # default to seed used in original analysis
) {
  set.seed(seed)
  pbar() # update parallel progress

  if(identical(metric, "auc")) {
    metric_ridge = "auc"
    metric_perf = "calc_auc"
    metric_label = "Test AUC"
  } else if(identical(metric, "brier")) {
    metric_ridge = "mse"
    metric_perf = "calc_mse"
    metric_label = "Test Brier score"
  } else {
    stop("Metric must be one of 'auc', 'mse'")
  }

  liwc_filename = "data/liwc2015_scores.csv"
  embed_filename = paste0("data/", section, "_embeddings_", embedding_params, ".csv")
  metadata_filename = "data/metadata_imputed.csv"
  readability_filename = "data/readability_scores.csv"
  narrative_filename = "data/narrative_arc.xlsx"

  full_data = get_meta(metadata_filename)
  liwc = get_liwc(full_data, liwc_filename, section)
  embed = get_embed(embed_filename)
  full_data = full_data[embed, on = "study_id"]

  if(section == "study"){
    #when at study level, there is 1 row in the LIWC data per study
    full_data = full_data[liwc, on = "study_id"]
  }else{
    #when at abstract or fulltext level, many-to-one mapping
    #because a single paper can have multiple studies
    full_data = full_data[liwc, on = "paper_id"]
  }

  setkey(full_data, study_id)
  readability_table = fread(readability_filename)
  full_data[readability_table, readability := get(paste0(section, "_readability")), on = "study_id"]
  get_narrative(full_data, narrative_filename, section)
  liwc[, `:=`(c("readability", "staging", "plot_progression", "cognitive_tension"), NA)]
  text_mat = prepare_text(full_data)
  liwc_mat = prepare_liwc(liwc, full_data)
  
  controls_mat = prepare_controls(full_data)
  keywords_mat = prepare_keywords(full_data)
  controls_mat = cbind(controls_mat, keywords_mat)

  if(section == "abstract"){
    #remove observations with abstracts <100 words (narrative arc missing for these obs)
    short_abstracts_inds = is.na(liwc_mat[, "staging"])
    controls_mat = controls_mat[!short_abstracts_inds,]
    text_mat = text_mat[!short_abstracts_inds,]
    liwc_mat = liwc_mat[!short_abstracts_inds,]
    full_data = full_data[!short_abstracts_inds]
  }

  train_inds = full_data[, pub_year <= year_threshold]
  folds = createFolds(full_data$replicated[train_inds], k = 10, list = FALSE)
  model1 = train_ridge_model(x=controls_mat[train_inds,],
                            y=full_data$replicated[train_inds],
                            folds=folds,metric=metric_ridge)
  model2 = train_ridge_model(x=text_mat[train_inds,],
                            y=full_data$replicated[train_inds],
                            folds=folds,metric=metric_ridge)
  model3 = train_ridge_model(x=cbind(controls_mat,text_mat)[train_inds,],
                            y=full_data$replicated[train_inds],
                            folds=folds,metric=metric_ridge)
  model4 = train_ridge_model(x=liwc_mat[train_inds,],
                            y=full_data$replicated[train_inds],
                            folds=folds,metric=metric_ridge)
  model5 = train_ridge_model(x=cbind(controls_mat,liwc_mat)[train_inds,],
                            y=full_data$replicated[train_inds],
                            folds=folds,metric=metric_ridge)
  model6 = train_ridge_model(x=cbind(controls_mat,liwc_mat,text_mat)[train_inds,],
                            y=full_data$replicated[train_inds],
                            folds=folds,metric=metric_ridge)
  model_list = list(model1, model2, model3, model4, model5, model6)
  test_auc = calc_all_performance(text_mat, liwc_mat, controls_mat,
                                  cbind(controls_mat, liwc_mat, text_mat),
                                  inds = !train_inds,
                                  y = full_data$replicated,
                                  fun = metric_perf,
                                  model_list = model_list)
  colnames(test_auc) = c("Controls", 
                                "Text", 
                                "Controls + Text", 
                                "LIWC", 
                                "Controls + LIWC",
                                "Controls + LIWC + Text")
  performance_table = data.table(test_auc)
  performance_table[, Stat := metric_label]
  performance_table[, Data := paste0(section, ", predicting new from old")]
  performance_table[, `Word2Vec Settings` := embedding_params]
  performance_table[, `Number of Splits` := NA]
  setcolorder(performance_table, c("Data", "Number of Splits", "Stat", "Word2Vec Settings"))
  return(performance_table)
}

## Get samplesize for each year
func_nstudy = function(years) {
  full_data = get_meta("data/metadata_imputed.csv")
  tb = full_data %>%
    cross_join(years) %>%
    summarise(
      train = sum(pub_year <= year),
      test = sum(pub_year > year),
      pct_train = round(100*train/(train + test)),
      pct_test = round(100*test/(train + test)),
      .by = year
    )

  tb = as.data.frame(t(tb))
  colnames(tb) = tb["year",]
  tb = tibble::rownames_to_column(tb[rownames(tb) != "year", ], var = " ")
  return(tb)
}

## Plot all results of robustness check (point estimates)
func_robplot_mean = function(results, whichmetric) {
  if(identical(whichmetric, "auc")) {
    metric_label = "Test AUC"
    metric_sign = 1
    metric_worst = 0.5
  } else if(identical(whichmetric, "brier")) {
    metric_label = "Test -(Brier score)"
    metric_sign = -1
    metric_worst = -0.25
  } else {
    stop("Metric must be one of 'auc', 'mse'")
  }
  results %>%
    mutate(year = year + 1) %>% # to match text
    filter(metric == whichmetric) %>%
    rename(w2v = `Word2Vec Settings`) %>%
    mutate(Data = sub(",.*$", "", Data)) %>%
    mutate(w2v = sub("window", "wi", w2v)) %>%
    mutate(w2v = sub("epochs", "ep", w2v)) %>%
    mutate(w2v = sub("stopwords", "sw", w2v)) %>%
    mutate(w2v = gsub("_", "", w2v)) %>%
    pivot_longer(cols = -c(year, Data, Stat, w2v, metric, seed)) %>%
    mutate(value = metric_sign*value) %>%
    summarise(value = mean(value), .by = c(year, Data, name, w2v, metric)) %>%
    mutate(name = factor(name, levels = c(
        "LIWC", "Controls", "Controls + LIWC",
        "Text", "Controls + Text", "Controls + LIWC + Text"
      ), ordered = TRUE
    )) %>%
    ggplot(aes(x = year, y = value, color = w2v)) +
    geom_hline(yintercept = metric_worst, linetype = "dashed",
      color = "darkred", alpha = 0.5) +
    geom_vline(xintercept = 2012, color = "lightblue",
      alpha = 0.25, linewidth = 2) +
    geom_line(alpha = 0.7, linewidth = 0.7) +
    geom_point(alpha = 0.7) +
    facet_grid(Data~name) + theme_bw() +
    labs(x = "Cutoff year", y = metric_label,
      color = "Word2Vec parameters") + 
    scale_color_viridis_d(option = "H") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

## Plot mean + range for Text, main specification only
func_robplot_range = function(results, whichmetric) {
  if(identical(whichmetric, "auc")) {
    metric_label = "Test AUC"
    metric_sign = 1
    metric_worst = 0.5
  } else if(identical(whichmetric, "brier")) {
    metric_label = "Test -(Brier score)"
    metric_sign = -1
    metric_worst = -0.25
  } else {
    stop("Metric must be one of 'auc', 'mse'")
  }
  results %>%
    mutate(year = year + 1) %>% # to match text
    filter(metric == whichmetric) %>%
    rename(w2v = `Word2Vec Settings`) %>%
    filter(w2v == "sg0_window3_dim100_epochs50_stopwords1") %>%
    mutate(Data = sub(",.*$", "", Data)) %>%
    pivot_longer(cols = -c(year, Data, Stat, w2v, metric, seed)) %>%
    filter(grepl("Text", name) | name == "Controls") %>%
    mutate(value = metric_sign*value) %>%
    summarise(
      meanv  = mean(value),
      se    = sd(value) / sqrt(n()),
      lower = meanv - qt(0.975, df = n() - 1) * se,
      upper = meanv + qt(0.975, df = n() - 1) * se,
      .by = c(year, Data, name, metric)
    ) %>%
    rename(value = meanv) %>%
    mutate(name = factor(name, levels = c(
        "LIWC", "Controls", "Controls + LIWC",
        "Text", "Controls + Text", "Controls + LIWC + Text"
      ), ordered = TRUE
    )) %>%
    ggplot(aes(x = year, y = value, color = name,
      ymin = lower, ymax = upper)) +
    geom_hline(yintercept = metric_worst, linetype = "dashed",
      color = "darkred", alpha = 0.5) +
    geom_vline(xintercept = 2012, color = "lightblue",
      alpha = 0.25, linewidth = 2) +
    geom_line(linewidth = 0.7, position = position_dodge(0.3)) +
    geom_pointrange(position = position_dodge(0.3)) +
    facet_grid(Data~.) + theme_bw() +
    labs(x = "Cutoff year", y = metric_label,
      color = "Data") + 
    scale_color_viridis_d(option = "H") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# Author provided utils.R, lightly modified ----------------------------------
#Various utility/helper functions used in the other code files
#will be source()'d in where needed, no need to run directly

#Loads required packages (installs first if not already installed)
#for data reading, writing, and processing
if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyr)){install.packages("tidyr"); library(tidyr)}
#for statistical power calculations
if(!require(pwr)){install.packages("pwr"); library(pwr)}
#for making train-test and cross-validation splits of data
if(!require(caret)){install.packages("caret"); library(caret)}
#for fitting ridge regression models
if(!require(glmnet)){install.packages("glmnet"); library(glmnet)}
#for calculating AUC
if(!require(MLmetrics)){install.packages("MLmetrics"); library(MLmetrics)}
#for running XGBoost model
if(!require(xgboost)){install.packages("xgboost"); library(xgboost)}
if(!require(NMOF)){install.packages("NMOF"); library(NMOF)}
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(readxl)){install.packages("readxl"); library(readxl)}

get_meta<-function(metadata_filename){
  #loading and cleaning the metadata (non-textual data) about each paper
  #calculates post-hoc power and performs mean imputation on missing values
  #read file
  metadata <- fread(metadata_filename, colClasses = c(paper_id = "character"))

  #calculate post-hoc power based on effect size and sample size
  metadata[!(is.na(n.o) | is.na(effect_size.o)), 
           power.o := pwr.r.test(n = n.o, r = effect_size.o, sig.level = 0.05)$power]
  
  #replication projects, encoded into categorical variable
  #ml (includes ml1,2,3,4) - 49 studies
  #rpp - 96 studies
  #ssrp - 20 studies
  #ee - 18 studies
  #other (includes pn, rrr, spsi and odr) - 116 studies
  
  #collapse all the many labs projects (ml1, ml2, ml3, ml4) together into just "ml"
  metadata[, project_collapsed := sub("[1-4]", "", project)]
  #collapse pn, rrr, spsi, and odr into "other" category
  metadata[project_collapsed %chin% c("pn", "rrr", "spsi", "odr"), project_collapsed := "other"]
  #code into factor with "other" as first so that it's dropped as the reference level in regression
  metadata[, project_collapsed := factor(project_collapsed, levels = c("other", "ml", "ee", "ssrp", "rpp"))]

  #concatenate the paper ID and suffix to get the filename/study ID
  metadata[, study_id := paste0(paper_id, suffix)]
  
  #fill in missing keywords as discipline-specific "other"
  metadata[is.na(keyword), keyword := paste0(discipline, "_other")]
  metadata[keyword == "", keyword := paste0(discipline, "_other")]
  
  #move the ID columns to the front
  setcolorder(metadata, c("paper_id", "study_id"))
  return(metadata)
}

impute_missing_vals = function(dat, maxiter = 50, ntree = 100){
  #use random forest to iteratively impute handful of missing metadata variables
  dt.mf = missForest(dat, variablewise = TRUE, verbose = TRUE, 
                     maxiter = maxiter, ntree = ntree)$ximp
  
  #replace NAs with the imputed values
  dat[is.na(dat[,'log(n.o)']), 'log(n.o)'] = dt.mf[is.na(dat[,'log(n.o)']), 'log(n.o)']
  dat[is.na(dat[,'effect_size.o']), 'effect_size.o'] = dt.mf[is.na(dat[,'effect_size.o']), 'effect_size.o']
  dat[is.na(dat[,'power.o']), 'power.o'] = dt.mf[is.na(dat[,'power.o']), 'power.o']
  dat[is.na(dat[,'p_value.o']), 'p_value.o'] = dt.mf[is.na(dat[,'p_value.o']), 'p_value.o']
  dat[is.na(dat[,'`Full Prof Ratio`']), '`Full Prof Ratio`'] = dt.mf[is.na(dat[,'`Full Prof Ratio`']), '`Full Prof Ratio`']
  
  #return filled in dataset
  return(dat)
}

get_embed = function(embed_filename){
  #reads in the embedding data and cleans up the column names
  embed = fread(embed_filename, header = TRUE)
  colnames(embed)[-1] = paste('dim',colnames(embed)[-1],sep='')
  colnames(embed)[1] = "study_id"
  
  return(embed)
}

get_liwc<-function(full_data, liwc_filename, section){
  #read in LIWC scores and subset to relevant section
  liwc_all = fread(liwc_filename)
  
  liwc_section = liwc_all[grep(section, Filename)]
  #paper ID: just the numbers from the filename
  liwc_section[, paper_id := gsub("[^0-9]*", "", Filename)]
  #study ID: paper ID plus an optional suffix if multiple studies from same paper
    #i.e., removing section name and ".txt" extension
  liwc_section[, study_id := substring(Filename, nchar(section)+1, nchar(Filename)-4)]
  
  #remove papers that aren't actually included in text analysis
  if(section == "study"){
    liwc_section = liwc_section[study_id %chin% full_data$study_id]
  }else{
    liwc_section = liwc_section[paper_id %chin% full_data$paper_id]
  }
  
  #check which columns have non-zero values for at least 50% of documents
  keep_cols = liwc_section[, sapply(.SD, function(x){mean(x == 0) < 0.5})]
  #remove nonsense dictionaries
  keep_cols[c("netspeak", "informal", "filler", "nonflu", "swear")] = FALSE
  
  #log-transform the word count
  liwc_section[,log_WC:=log(WC)]
  
  #don't need these anymore either
  keep_cols[c("Filename", "Segment", "WC")] = FALSE
  
  #keep only the columns that aren't too sparse
  liwc_section[, `:=`(which(!keep_cols), NULL)]
  
  #move the derived columns to the front
  setcolorder(liwc_section, c("paper_id", "study_id", "log_WC"))
  
  return(liwc_section)
}

get_narrative = function(full_data, narrative_filename, section){
  #assumes "full_data" exists in the environment
  #and will just modify in place to add narrative arc scores directly
  if(section == "study"){
    narrative = data.table(read_excel(narrative_filename, sheet = "study"))
    narrative[, study_id := ifelse(is.na(suffix), ID, paste0(ID, suffix))]
    full_data[narrative, `:=`(staging = `staging`,
                                    plot_progression = `plot progression`,
                                    cognitive_tension = `cognitive tension`),
              on = "study_id"]
  }else if(section == "fulltext"){
    narrative = data.table(read_excel(narrative_filename, sheet = "fulltext"))
    narrative[, ID := as.character(ID)]
    full_data[narrative, `:=`(staging = `staging`,
                              plot_progression = `plot progression`,
                              cognitive_tension = `cognitive tension`),
              on = c("paper_id" = "ID")]
  }else if(section == "abstract"){
    narrative = data.table(read_excel(narrative_filename, sheet = "abstract"))
    narrative[, ID := as.character(ID)]
    full_data[narrative, `:=`(staging = `staging`,
                                       plot_progression = `plot progression`,
                                       cognitive_tension = `cognitive tension`),
              on = c("paper_id" = "ID")]
  }
  
}

prepare_controls<-function(dat){
  #extract, transform, and dummy code (where applicable) paper metadata to control for, convert to matrix
  #make sure NAs don't get dropped (so they can be imputed later)
  options(na.action='na.pass')
  model.matrix(replicated ~ 
                 power.o +
                 p_value.o +
                 effect_size.o +
                 log(1+citations) +
                 log(n.o) +
                 n_authors.o +
                 discipline +
                 effect_type +
                 authors_male.o +
                 us_lab.o +
                 subjects.o +
                 num_refs +
                 `Full Prof Ratio` +
                 num_tables +
                 num_figures +
                 supp_material +
                 num_studies +
                 I(pub_year-2000)+
                 project_collapsed+
                 author_involvement, 
               data = dat)[,-1]
}

prepare_keywords=function(dat){
  #expand into dummy matrix
  keywords_dat<-model.matrix(~ 0 + keyword, dat)
  #drop the biggest "other" category for intercept identification
  #drop the biggest econ category to avoid collinearity with the econ discipline indicator
    #since econ-related keywords are nested with the econ discipline indicator
  return(keywords_dat[,-which(colnames(keywords_dat) %in% c("keywordCognitive_other", "keywordnon-cooperative games"))])
}

prepare_liwc = function(liwc, dat){
  #extracts the LIWC dictionary values and converts to matrix
  #assumes data table named "liwc" exists in environment with relevant column names
  liwc_colnames = setdiff(names(liwc),c("paper_id", "study_id"))
  as.matrix(dat[, ..liwc_colnames])
}

prepare_text<-function(dat){
  #extracts the average embeddings for each doc and converts to matrix
  as.matrix(dat[,.SD,.SDcols=patterns('dim')])
}

return_train_inds<-function(dat, p = 0.8){
  #randomly select fraction p of total papers as training set
  #note: since sometimes there are multiple studies from the same paper,
  #we avoid data leakage by splitting by *paper* ID, not *study* ID
 
  #slight complication: we want to make train/test splits balanced by replication outcome,
  #but are splitting by paper rather than study.
  #occasionally, a paper has studies with differing replication outcomes
  #in these cases, use "majority" outcome 
  #(e.g., if 2 out of 3 studies in a paper replicated, put it in the replicated stratum)
  #in the rare case of a tie, tie-break randomly
  #do this *inside* the loop over train/test splits so any randomness
  #arising from the tie-breaking also gets captured by the variability across splits
  
  #calculate number of replicated/non-replicated studies by each paper
  rep_summary = dat[, .N, by = c("paper_id", "replicated")]
  #generate random numbers for tie-breaking
  rep_summary[, rand := runif(.N)]
  #sort by paper_id, 
  #*then* by number of replicated/non-replicated studies (descending order)
  #*then* by random number to tie-break
  setorder(rep_summary, paper_id, -N, rand)
  
  #unique() keeps *first* observation of each paper_id
  #which by the above sorting means the majority outcome is retained
  paper_outcomes = unique(rep_summary[, .(paper_id, replicated)], by = "paper_id")
  
  #get a proportion p subsample balanced by replication outcomes
  trainIDs  <- createDataPartition(paper_outcomes$replicated, p = p, list = FALSE) #indices - balanced samples
  
  #map paper IDs back to study-level true/false
  traininds <- dat[, paper_id %in% paper_outcomes[trainIDs, paper_id]] #indices
  return(traininds)
}

train_ridge_model<-function(x, y, folds, metric)
{
  #estimate a ridge regression model with penalty selected by cross-validation
  #range of possible ridge penalties
  lambdas = 10^seq(3, -2, by = -.1)
  
  #fit a ridge logistic regression using cross-validation
  #maximizing the validation AUC
  model = cv.glmnet(x,
                    alpha = 0,#ridge
                    lambda = lambdas,
                    y,
                    family = "binomial",
                    type.measure = metric,
                    foldid = folds)
  return(model)
}

train_elasticnet_model<-function(x, y, folds)
{
  #I do not specify a lambda range for lasso regularization because we expect the range to be different than that specified for ridge.
  #I just allow the solver to pick a range
  
  #fit an elastic net logistic regression using cross-validation
  #maximizing the validation AUC
  model = cv.glmnet(x,
                    alpha = 0.5,#elastic net
                    y,
                    family = "binomial",
                    type.measure = "mse",
                    foldid = folds)
  return(model)
}


CV_xgboost_model<-function(x, y, folds)
{ # we only pass training data to this for the cross validation - training, validation split
  # cross validation + grid search across different parameters to find the xgboost task parameters with the best validation auc
  
  eval <- "auc"
  nrounds<-20
  
  # Parameter list to search
  params <- list(eta = c(0.7,0.1,0.01), #learning_rate
                 max_depth = c(2,3,4,6)) #maximum depth of the tree (more depth is more complex and more likely to over fit)
  
  xgbCV <- function (parameters) {
    
  #performs k-fold cross validation on a model that is fit with a given set of parameters passed to it by the grid search function defined later
    fit <- xgb.cv(
      metrics = eval,
      objective = 'binary:logistic',
      type="classification",
      data = x, #do we want to cross validate over only the training set? parameters would change every split with change in training data
      label = y, 
      # param = parameters,
      missing = NA, 
      folds=folds, 
      prediction = FALSE,
      scale_pos_weight = 1,
      maximize = TRUE, #minimize log loss, maximize auc
      nrounds = nrounds,
      verbose = FALSE,
      early_stopping_rounds = 3,
      eta = parameters$eta,
      max_depth = parameters$max_depth
    )
    
    idx <- fit$best_iteration
    val <- fit$evaluation_log[idx]$test_auc_mean
    return(val)
    }

  sol <- gridSearch(fun = xgbCV,
                    levels = params,
                    keepNames = TRUE,
                    asList = TRUE)
  
  aucs<-sol$values
  
  #return the hyperparameters associated with the highest auc values
  best_params <- list(eta=sol$levels[which.max(aucs)][[1]]$eta,max_depth=sol$levels[which.max(aucs)][[1]]$max_depth,objective = 'binary:logistic', eval_metric = eval,nrounds=8)
  return(best_params)
}


train_xgboost_model<-function(x,y,folds){
  #cross validate to find best parameters for the model
  solved_params<-CV_xgboost_model(x,y,folds)
  
  #training model with entire training dataset
  dtrain0 <- xgb.DMatrix(x, label = y)
  model   <- xgb.train(data = dtrain0,
                       param = solved_params,
                       nrounds=solved_params$nrounds)
  return(model)
}


#functions for evaluating test performance of ridge regression model
# Brier score (= MSE for binary outcomes)
calc_mse = function(x, y, model){
  MLmetrics::MSE(predict(model, newx = x, s = "lambda.min", type = "response"), y)
}
#AUC, correlation between predictions and outcomes, classification accuracy
calc_auc = function(x, y, model){
  MLmetrics::AUC(predict(model, newx = x, s = "lambda.min", type = "response"), y)
}

calc_auc_xgb = function(x, y, model){
  MLmetrics::AUC(predict(model, newdata = x), y)
}

calc_all_performance = function(text_mat, liwc_mat, controls_mat, x, inds, y,
  fun, model_list, paper_study_map){
  #function for calculating test set accuracy of the 5 focal models
  
  #getting relevant x/y variables for bootstrap sample
  controls_b = x[inds,colnames(controls_mat)]
  text_b = x[inds,colnames(text_mat)]
  liwc_b = x[inds,colnames(liwc_mat)]
  y_b = y[inds]
  
  
  #calculating performance on the bootstrap test sample
  model1_r = do.call(fun,list(x=controls_b,y=y_b,model=model_list[[1]]))
  model2_r = do.call(fun,list(x=text_b,y=y_b,model=model_list[[2]]))
  model3_r = do.call(fun,list(x=cbind(controls_b,text_b),y=y_b,model=model_list[[3]]))
  model4_r = do.call(fun,list(x=liwc_b,y=y_b,model=model_list[[4]]))
  model5_r = do.call(fun,list(x=cbind(controls_b,liwc_b),y=y_b,model=model_list[[5]]))
  model6_r = do.call(fun,list(x=cbind(controls_b,liwc_b,text_b),y=y_b,model=model_list[[6]]))

  #outputting row vector of performance metrics
  res = cbind(model1_r,model2_r,model3_r,model4_r,model5_r,model6_r)
  return(res)
}

# List of global definitions for furrr ---------------------------------------
globals_furrr = list(
  get_meta = get_meta,
  get_liwc = get_liwc,
  calc_auc = calc_auc,
  calc_mse = calc_mse,
  fread = fread,
  pwr.r.test = pwr.r.test,
  get_embed = get_embed,
  get_narrative = get_narrative,
  read_excel = read_excel,
  prepare_text = prepare_text,
  prepare_liwc = prepare_liwc,
  prepare_controls = prepare_controls,
  prepare_keywords = prepare_keywords,
  createFolds = createFolds,
  train_ridge_model = train_ridge_model,
  cv.glmnet = cv.glmnet,
  calc_all_performance = calc_all_performance
)