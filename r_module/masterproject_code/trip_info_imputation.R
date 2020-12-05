
# Code to do trip info (mode and purpose) imputation ----------------------

suppressPackageStartupMessages(library('data.table'))
suppressPackageStartupMessages(library('caret'))
suppressPackageStartupMessages(library('dplyr'))
library('parallel')
suppressPackageStartupMessages(library('kernlab'))
library('keras')

source('2_code/r_module/adasyn.R', chdir = TRUE) 
source('2_code/r_module/stepwise_classification.R', chdir = TRUE) 
source('2_code/r_module/namelist.R', chdir = TRUE) 
source('2_code/r_module/get_poi.R', chdir = TRUE) 
source('2_code/r_module/poi_categorize.R', chdir = TRUE) 


# Trip purpose imputation -------------------------------------------------

################################ labeled data analysis ----
# In the following, we use set of abbreviations:
# tp: trip purpose; tm: trip mode; rf: random forest; sf: selected features
# cv: cross validation; sw: stepwise; prn: person

load('3_output/28_Apr/purpose_data.RData')

purpose_labeled <- droplevels(purpose_data[
    which(purpose_data$labels != 'Unknown'), ]
)
rm(purpose_data)


################ (ok) use whole labeled data, all features and svm ----
# overall accuracy: 

set.seed(123)
activities_2fold <- caret::groupKFold(purpose_labeled$ID[1:20000], k = 2)
# user_2fold <- caret::groupKFold(purpose_labeled$user_id, k = 2)

system.time(
    tp_svm_train <- kernlab::ksvm(
        labels ~ ., 
        data = purpose_labeled[
            activities_2fold[[1]], .SD, .SDcol = c("labels", tp_sf_all[[1]])], 
        )
)

tp_svm_test <- predict(tp_svm_train, purpose_labeled[
    activities_2fold[[2]], .SD, .SDcol = tp_sf_all[[1]]])
caret::confusionMatrix(table(purpose_labeled$labels[activities_2fold[[2]]], 
                             tp_svm_test))

save(tp_svm_train, file = '3_output/28_Apr/tp_svm_train.RData')


# tp_svm_cv_2fold <- list(
#     split_activities = rep(list(0), length(activities_2fold)), 
#     split_user =  rep(list(0), length(activities_2fold))
# )
# for(i in 1:length(activities_2fold)){
#     tp_svm_cv_2fold$split_activities[[i]] <- stepwise_model(
#         traindata = purpose_labeled[activities_2fold[[i]], ], 
#         testdata = purpose_labeled[-activities_2fold[[i]], ], 
#         categories = activity_categories, 
#         selected_features = tp_sf_all, 
#         factor_features = c(participants_factorfeatures, 
#                             activities_factorfeatures), 
#         par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
#                       do.trace = 5, keep.forest = TRUE)
#     )
#     tp_svm_cv_2fold$split_user[[i]] <- stepwise_model(
#         traindata = purpose_labeled[user_2fold[[i]], ], 
#         testdata = purpose_labeled[-user_2fold[[i]], ], 
#         categories = activity_categories, 
#         selected_features = tp_sf_all, 
#         factor_features = c(participants_factorfeatures, 
#                             activities_factorfeatures), 
#         par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
#                       do.trace = 5, keep.forest = TRUE)
#     )
# }


################ use whole labeled data, all features and ANN ----
# overall accuracy: %

set.seed(123)
activities_2fold <- caret::groupKFold(purpose_labeled$ID, k = 2)
# user_2fold <- caret::groupKFold(purpose_labeled$user_id, k = 2)


traindata = purpose_labeled[activities_2fold[[1]],
                            .SD, .SDcol = c("labels", tp_sf_all[[1]])]
testdata = purpose_labeled[-activities_2fold[[1]],
                           .SD, .SDcol = c("labels", tp_sf_all[[1]])]


################ (ok) use whole labeled data, all features and random forest ----
# overall accuracy: 85.35% for 500 trees, 84.79% for 100 trees

tp_rf_train_whole <- stepwise_model(
    traindata = purpose_labeled, 
    categories = activity_categories, 
    selected_features = tp_sf_all,
    stepwise = FALSE, 
    par_rf = list(ntree = 500, mtry = 6, 
                  importance = TRUE, do.trace = 5, 
                  keep.forest = FALSE
    )
)
save(tp_rf_train_whole, file = '3_output/28_Apr/tp_rf_train_whole.RData')

tp_rf_train_whole1 <- stepwise_model(
    traindata = purpose_labeled, 
    categories = activity_categories, 
    selected_features = tp_sf_all,
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 6, 
                  importance = FALSE, do.trace = 5, 
                  keep.forest = TRUE
    )
)
save(tp_rf_train_whole1, file = '3_output/28_Apr/tp_rf_train_whole1.RData')


# use whole labeled data set and the most significant 20 features from above
# analysis, overall accuracy: 84.37%

tp_rf_train_20sf <- stepwise_model(
    traindata = purpose_labeled, 
    categories = activity_categories, 
    selected_features = list(names(sort(tp_rf_train_whole$train$importance[
        , 10], decreasing = TRUE) )[1:20]),
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 6, 
                  importance = TRUE, do.trace = 5, 
                  keep.forest = FALSE
    )
)
save(tp_rf_train_20sf, file = '3_output/28_Apr/tp_rf_train_20sf.RData')

# use whole labeled data set and the most significant 6 features from above
# analysis, overall accuracy: 83.25%

tp_rf_train_6sf <- stepwise_model(
    traindata = purpose_labeled, 
    categories = activity_categories, 
    selected_features = list(names(sort(tp_rf_train_whole$train$importance[
        , 10], decreasing = TRUE) )[1:6]),
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 6, 
                  importance = TRUE, do.trace = 5, 
                  keep.forest = FALSE
    )
)
save(tp_rf_train_6sf, file = '3_output/28_Apr/tp_rf_train_6sf.RData')

# use PCA for numeric features





################ (ok) use whole labeled data and with PCA ----
# overall accuracy: 83.3%

load('3_output/28_Apr/purpose_data_pca.RData')

purpose_pca_labeled <- droplevels(purpose_data_pca[
    which(purpose_data_pca$labels != 'Unknown'), ]
)
rm(purpose_data_pca)


tp_rf_train_pca <- stepwise_model(
    traindata = purpose_pca_labeled, 
    categories = activity_categories, 
    selected_features = tp_sf_pca11,
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 6, 
                  importance = TRUE, do.trace = 5, 
                  keep.forest = FALSE
    )
)
save(tp_rf_train_pca, file = '3_output/28_Apr/tp_rf_train_pca.RData')


################ (ok) use whole labeled data and without personal features ----
# overall accuracy: 83.3%

tp_rf_train_noprn <- stepwise_model(
    traindata = purpose_labeled, 
    categories = activity_categories, 
    selected_features = tp_sf_noprn,
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 6, 
                  importance = TRUE, do.trace = 5, 
                  keep.forest = FALSE
    )
)
save(tp_rf_train_noprn, file = '3_output/28_Apr/tp_rf_train_noprn.RData')


################ (ok) use whole labeled data and all features ----
# trying different m
# accuracy
# 

tp_rf_train_whole_mtry <- list(0)
for(i in 1 : length(tp_selectedfeatures_all[[1]])){
    tp_rf_train_whole_mtry[[i]] <- stepwise_model(
        traindata = purpose_labeled, 
        categories = activity_categories, 
        selected_features = tp_sf_all,
        stepwise = FALSE, 
        par_rf = list(ntree = 100, mtry = i, 
                      importance = TRUE, do.trace = 5, 
                      keep.forest = FALSE
        )
    )
    print(paste(i, 'ok'))
    save(tp_rf_train_whole_mtry, 
         file = '3_output/28_Apr/tp_rf_train_whole_mtry.RData')
    mem_used()
    object_size(tp_rf_train_whole_mtry)
}



################ (ok) use train and test data for cross validation ----
# split activities: train 82.12%, test 82.41%
# split user: train 85.46%, test 67.92%

set.seed(123)
activities_2fold <- caret::groupKFold(purpose_labeled$ID, k = 2)
user_2fold <- caret::groupKFold(purpose_labeled$user_id, k = 2)

tp_rf_cv_2fold <- list(split_activities = 0, split_user = 0)
for(i in 1:length(activities_2fold)){
    tp_rf_cv_2fold$split_activities[[i]] <- stepwise_model(
        traindata = purpose_labeled[activities_2fold[[i]], ], 
        testdata = purpose_labeled[-activities_2fold[[i]], ], 
        categories = activity_categories, 
        selected_features = tp_sf_all, 
        stepwise = FALSE, 
        par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
                      do.trace = 5, keep.forest = TRUE)
    )
    tp_rf_cv_2fold$split_user[[i]] <- stepwise_model(
        traindata = purpose_labeled[user_2fold[[i]], ], 
        testdata = purpose_labeled[-user_2fold[[i]], ], 
        categories = activity_categories, 
        selected_features = tp_sf_all, 
        stepwise = FALSE, 
        par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
                      do.trace = 5, keep.forest = TRUE)
    )
}

save(tp_rf_cv_2fold, file = '3_output/28_Apr/tp_rf_cv_2fold.RData')

################ (ok) use train and test data for cross validation ----
# no cluster info
# split activities: train 82.12%, test 82.41%
# split user: train 85.46, test 67.92%

set.seed(123)
activities_2fold <- caret::groupKFold(purpose_labeled$ID, k = 2)
user_2fold <- caret::groupKFold(purpose_labeled$user_id, k = 2)

tp_rf_cv_2fold_nocluster <- list(split_activities = 0, split_user = 0)
for(i in 1:length(activities_2fold)){
    tp_rf_cv_2fold_nocluster$split_activities[[i]] <- stepwise_model(
        traindata = purpose_labeled[activities_2fold[[i]], ], 
        testdata = purpose_labeled[-activities_2fold[[i]], ], 
        categories = activity_categories, 
        selected_features = tp_sf_nocluster, 
        stepwise = FALSE, 
        par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
                      do.trace = 5, keep.forest = TRUE)
    )
    tp_rf_cv_2fold_nocluster$split_user[[i]] <- stepwise_model(
        traindata = purpose_labeled[user_2fold[[i]], ], 
        testdata = purpose_labeled[-user_2fold[[i]], ], 
        categories = activity_categories, 
        selected_features = tp_sf_nocluster, 
        stepwise = FALSE, 
        par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
                      do.trace = 5, keep.forest = TRUE)
    )
}

save(tp_rf_cv_2fold_nocluster,
     file = '3_output/28_Apr/tp_rf_cv_2fold_nocluster.RData')


################ (ok) use whole labeled data, all features, and stepwise clf ----
# split activities: test 80.85%
# split user: test 65%

set.seed(123)
activities_2fold <- caret::groupKFold(purpose_labeled$ID, k = 2)
user_2fold <- caret::groupKFold(purpose_labeled$user_id, k = 2)

tp_swrf_cv_2fold <- list(
    split_activities = rep(list(0), length(activities_2fold)), 
    split_user =  rep(list(0), length(activities_2fold))
)
for(i in 1:length(activities_2fold)){
    tp_swrf_cv_2fold$split_activities[[i]] <- stepwise_model(
        traindata = purpose_labeled[activities_2fold[[i]], ], 
        testdata = purpose_labeled[-activities_2fold[[i]], ], 
        categories = activity_categories, 
        selected_features = tp_sf_all, 
        factor_features = c(participants_factorfeatures, 
                            activities_factorfeatures), 
        stepwise = TRUE, 
        balance_methods = NULL, 
        par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
                      do.trace = 5, keep.forest = TRUE)
    )
    tp_swrf_cv_2fold$split_user[[i]] <- stepwise_model(
        traindata = purpose_labeled[user_2fold[[i]], ], 
        testdata = purpose_labeled[-user_2fold[[i]], ], 
        categories = activity_categories, 
        selected_features = tp_sf_all, 
        factor_features = c(participants_factorfeatures, 
                            activities_factorfeatures), 
        stepwise = TRUE, 
        balance_methods = NULL, 
        par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
                      do.trace = 5, keep.forest = TRUE)
    )
}

save(tp_swrf_cv_2fold,
     file = '3_output/28_Apr/tp_swrf_cv_2fold.RData')




################ (ok) use whole labeled data, all features, stepwise, adasyn ----
# split activities: test 81.0%
# split user: test 65.85%

set.seed(123)
activities_2fold <- caret::groupKFold(purpose_labeled$ID, k = 2)
user_2fold <- caret::groupKFold(purpose_labeled$user_id, k = 2)

tp_swrf_adasyn_cv_2fold <- list(
    split_activities = rep(list(0), length(activities_2fold)), 
    split_user =  rep(list(0), length(activities_2fold))
)
for(i in 1:length(activities_2fold)){
    tp_swrf_adasyn_cv_2fold$split_activities[[i]] <- stepwise_model(
        traindata = purpose_labeled[activities_2fold[[i]], ], 
        testdata = purpose_labeled[-activities_2fold[[i]], ], 
        categories = activity_categories, 
        selected_features = tp_sf_all, 
        factor_features = c(participants_factorfeatures, 
                            activities_factorfeatures), 
        par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
                      do.trace = 5, keep.forest = TRUE)
    )
    tp_swrf_adasyn_cv_2fold$split_user[[i]] <- stepwise_model(
        traindata = purpose_labeled[user_2fold[[i]], ], 
        testdata = purpose_labeled[-user_2fold[[i]], ], 
        categories = activity_categories, 
        selected_features = tp_sf_all, 
        factor_features = c(participants_factorfeatures, 
                            activities_factorfeatures), 
        par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
                      do.trace = 5, keep.forest = TRUE)
    )
}

save(tp_swrf_adasyn_cv_2fold,
     file = '3_output/28_Apr/tp_swrf_adasyn_cv_2fold.RData')



################################ use ten persons data, each generates around ----
# 1000 activities in Zuerich, investigate benefits of POI info from google 
# places API
# SVM: 82.7%
# original: 89.4%
# 89.5%

load('3_output/28_Apr/purpose_zuerich_subpoi.RData')

tp_rf_10prn <- list(
    original = 0, 
    add_poi = 0, 
    svm = list(train = list(0), test = list(0), 
               confusion = list(0), error_rate = NULL))

# Code POI occurence
poi_categories <- as.data.frame(t(sapply(
    purpose_zuerich_subpoi$poi, poi_categorize) ))
for(i in 1 : (length(poi_types) - 1 )){
    purpose_zuerich_subpoi[, poi_types[i]] <-
        t(as.data.frame(poi_categories[, poi_types[i]]))
}
for(i in 1 : dim(purpose_zuerich_subpoi)[1]){
    purpose_zuerich_subpoi[i, poi_types[-10]] <- 
        purpose_zuerich_subpoi[i, poi_types[-10]] / 
        sum(purpose_zuerich_subpoi[i, poi_types[-10]])
}
purpose_zuerich_subpoi[is.na(purpose_zuerich_subpoi)] <- 0


tp_rf_10prn$original <- stepwise_model(
    traindata = purpose_zuerich_subpoi,
    categories = activity_categories,
    selected_features = tp_sf_all,
    stepwise = FALSE,
    par_rf = list(ntree = 100, mtry = 6,
                  importance = TRUE, do.trace = 20,
                  keep.forest = TRUE
    )
)

tp_rf_10prn$add_poi <- stepwise_model(
    traindata = purpose_zuerich_subpoi,
    categories = activity_categories,
    selected_features = tp_sf_all_poi9,
    stepwise = FALSE,
    par_rf = list(ntree = 100, mtry = 6,
                  importance = TRUE, do.trace = 20,
                  keep.forest = TRUE
    )
)

# Use support vector machine and 10 fold cross validation

set.seed(111)
activities_10fold <- caret::groupKFold(purpose_zuerich_subpoi$ID, k = 10)
purpose_zuerich_subpoi$labels <- factor(
    purpose_zuerich_subpoi$labels, levels = activity_categories)

for(i in 1 : length(activities_10fold)){
    tp_rf_10prn$svm$train[[i]] <- kernlab::ksvm(
        labels ~ .,
        data = purpose_zuerich_subpoi[
            activities_10fold[[i]], c("labels", tp_sf_all[[1]])],
        kernel = 'rbfdot'
    )
    tp_rf_10prn$svm$test[[i]] <- predict(
        tp_rf_10prn$svm$train[[i]],
        purpose_zuerich_subpoi[
            -activities_10fold[[i]], tp_sf_all[[1]] ]
    )
    tp_rf_10prn$svm$confusion[[i]] <- caret::confusionMatrix(
        table(
            purpose_zuerich_subpoi$labels[-activities_10fold[[i]]],
            tp_rf_10prn$svm$test[[i]]
        )
    )
    tp_rf_10prn$svm$error_rate <- as.data.frame(rbind(
        tp_rf_10prn$svm$error_rate, c(
        tp_rf_10prn$svm$confusion[[i]]$overall[1], 
        tp_rf_10prn$svm$confusion[[i]]$byClass[, 6])) 
    )
    print(i)
}
tp_rf_10prn$svm$error_rate[is.na(tp_rf_10prn$svm$error_rate)] <- 0
tp_rf_10prn$svm$error_rate[11, ] <- apply(tp_rf_10prn$svm$error_rate, 2, mean)

save(tp_rf_10prn, file = '3_output/28_Apr/tp_rf_10prn.RData')

################################ Dependence on duration and Number of participants----

load('3_output/28_Apr/user_alllabelled_days.RData')
load('3_output/28_Apr/alllabelled_purpose.RData')

# length(which(table(user_alllabelled_days$user_id) >= 180) )

alllabelled_purpose$labels <- 
    factor(alllabelled_purpose$labels, levels = activity_categories)
survey_length <- c(180, 150, 120, 90, 60, 30)
singleres <- list(data = 0, rf_res = list(0), accuracy = 0)
dependence_res <- list(
    d180 = singleres, d150 = singleres, d120 = singleres, 
    d90 = singleres, d60 = singleres, d30 = singleres)
rm(singleres)

for(i in 1:length(survey_length)){
    dependence_res[[i]]$data <- droplevels(alllabelled_purpose[
        alllabelled_purpose$user_id %in% (
            names(which(table(user_alllabelled_days$user_id) >= 
                            survey_length[i]))
            )])
    for(j in 1 : survey_length[i]){
        dependence_res[[i]]$rf_res[[j]] <- stepwise_model(
            traindata = droplevels(data.table::merge.data.table(
                dependence_res[[i]]$data, 
                dependence_res[[i]]$data[
                    , .N, by = .(user_id, yday)][
                        , .(yday = head(yday, j) ), by = .(user_id)], 
                by = c('user_id', 'yday')
            )), 
            categories = NULL, 
            selected_features = tp_sf_all,
            stepwise = FALSE, 
            par_rf = list(ntree = 100, mtry = 6, 
                          importance = FALSE, do.trace = 100, 
                          keep.forest = FALSE
            )
        )
        dependence_res[[i]]$accuracy[j] <- 
            dependence_res[[i]]$rf_res[[j]]$train$err.rate[100, 1]
    print(paste('i = ', i, '; j = ', j, sep = ''))
    }
}

save(dependence_res, file = '3_output/28_Apr/dependence_res.RData')





# Trip mode imputation -------------------------------------------------

################################ labeled data set analysis
# In the following, we use set of abbreviations:

load('3_output/28_Apr/mode_data.RData')

mode_labeled <- droplevels(mode_data[
    which(mode_data$labels != 'Unknown'), ]
)
rm(mode_data)


################ use whole labeled data set and all features, 
# overall accuracy: 86.30% for 100 trees, 
# 86.15% for 50 trees

tm_rf_train_whole <- stepwise_model(
    traindata = mode_labeled, 
    categories = mode_categories, 
    selected_features = tm_sf_all,
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 7, 
                  importance = TRUE, do.trace = 1, 
                  keep.forest = TRUE
    )
)
save(tm_rf_train_whole, file = '3_output/28_Apr/tm_rf_train_whole.RData')



# use whole labeled data set and the most significant 20 features from above
# analysis, overall accuracy: %

tm_rf_train_20sf <- stepwise_model(
    traindata = mode_labeled, 
    categories = mode_categories, 
    selected_features = list(names(sort(tm_rf_train_whole$train$importance[
        , 9], decreasing = TRUE) )[1:20]),
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 7, 
                  importance = TRUE, do.trace = 5, 
                  keep.forest = FALSE
    )
)
save(tm_rf_train_20sf, file = '3_output/28_Apr/tm_rf_train_20sf.RData')

# use whole labeled data set and the most significant 5 features from above
# analysis, overall accuracy: %

tm_rf_train_5sf <- stepwise_model(
    traindata = mode_labeled, 
    categories = mode_categories, 
    selected_features = list(names(sort(tm_rf_train_whole$train$importance[
        , 9], decreasing = TRUE) )[1:5]),
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 7, 
                  importance = TRUE, do.trace = 5, 
                  keep.forest = FALSE
    )
)
save(tm_rf_train_5sf, file = '3_output/28_Apr/tm_rf_train_5sf.RData')


################ use whole labeled data set and without personal features
# overall accuracy: %

tm_rf_train_noprn <- stepwise_model(
    traindata = mode_labeled, 
    categories = mode_categories, 
    selected_features = tm_sf_noprn,
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 7, 
                  importance = TRUE, do.trace = 5, 
                  keep.forest = FALSE
    )
)
save(tm_rf_train_noprn, file = '3_output/28_Apr/tm_rf_train_noprn.RData')



################ use whole labeled data set and all features, trying different m
# accuracy
# 

tm_rf_train_whole_mtry <- list(0)
for(i in 1 : 30){
    tm_rf_train_whole_mtry[[i]] <- stepwise_model(
        traindata = mode_labeled, 
        categories = mode_categories, 
        selected_features = tm_sf_all,
        stepwise = FALSE, 
        par_rf = list(ntree = 50, mtry = i, 
                      importance = FALSE, do.trace = 10, 
                      keep.forest = FALSE
        )
    )
    print(paste(i, 'ok'))
    save(tm_rf_train_whole_mtry, 
         file = '3_output/28_Apr/tm_rf_train_whole_mtry.RData')
}



# Interaction between trip purpose and mode imputation --------------------


load('3_output/28_Apr/purpose_data.RData')
load('3_output/28_Apr/mode_data.RData')
load('3_output/28_Apr/tp_rf_train_whole1.RData')
load('3_output/28_Apr/tm_rf_train_whole.RData')


purpose_data$predicted_purpose <- predict(
    tp_rf_train_whole1$train, 
    purpose_data[, .SD, .SDcols = tp_sf_all[[1]]]
)

purpose_data$predicted_purpose[which(purpose_data$labels != 'Unknown')] <-
    droplevels(purpose_data$labels[which(purpose_data$labels != 'Unknown')])

# caret::confusionMatrix(
#     purpose_data$predicted_purpose[which(purpose_data$labels != 'Unknown')],
#     droplevels(purpose_data$labels[which(purpose_data$labels != 'Unknown')]) )

mode_data$predicted_mode <- predict(
    tm_rf_train_whole$train, 
    mode_data[, .SD, .SDcols = tm_sf_all[[1]]]
)

mode_data$predicted_mode[which(mode_data$labels != 'Unknown')] <- 
    droplevels(mode_data$labels[which(mode_data$labels != 'Unknown')])

# caret::confusionMatrix(
#     mode_data$predicted_mode[which(mode_data$labels != 'Unknown')],
#     droplevels(mode_data$labels[which(mode_data$labels != 'Unknown')] ))

interaction_tp_tm <- list(
    purpose_data = purpose_data, mode_data = mode_data
)


################ Impact of activities on mode imputation ----

interaction_tp_tm$mode_data$next_activity_id <- 
    as.character(interaction_tp_tm$mode_data$next_activity_id)

interaction_tp_tm$mode_data <- data.table::merge.data.table(
    interaction_tp_tm$mode_data, 
    interaction_tp_tm$purpose_data[
        , .SD, .SDcols = c('ID', "predicted_purpose")],
    by.x = "next_activity_id", by.y = 'ID', all.x = TRUE
)

interaction_tp_tm$mode_data$predicted_purpose <- as.character(
    interaction_tp_tm$mode_data$predicted_purpose
)
interaction_tp_tm$mode_data$predicted_purpose[
    is.na(interaction_tp_tm$mode_data$predicted_purpose)] <- 'Unknown'
interaction_tp_tm$mode_data$predicted_purpose <- as.factor(
    interaction_tp_tm$mode_data$predicted_purpose
)

interaction_tp_tm$tm_rf <- stepwise_model(
    traindata = droplevels(
        interaction_tp_tm$mode_data[
            which(interaction_tp_tm$mode_data$labels != 'Unknown'), ]
    ), 
    categories = mode_categories, 
    selected_features = list(c(tm_sf_all[[1]],"predicted_purpose")),
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 7, 
                  importance = TRUE, do.trace = 1, 
                  keep.forest = FALSE
    )
)

save(interaction_tp_tm, file = '3_output/28_Apr/interaction_tp_tm.RData')


################ Impact of activities on mode imputation ----


interaction_tp_tm$purpose_data <- data.table::merge.data.table(
    interaction_tp_tm$purpose_data, 
    interaction_tp_tm$mode_data[
        , tail(.SD, 1), .SDcols = c("predicted_mode"), 
        by = "next_activity_id"], 
    by.x = 'ID', by.y = 'next_activity_id', all.x = TRUE
)


interaction_tp_tm$purpose_data$predicted_mode <- as.character(
    interaction_tp_tm$purpose_data$predicted_mode
)
interaction_tp_tm$purpose_data$predicted_mode[
    is.na(interaction_tp_tm$purpose_data$predicted_mode)] <- 'Unknown'
interaction_tp_tm$purpose_data$predicted_mode <- as.factor(
    interaction_tp_tm$purpose_data$predicted_mode
)

interaction_tp_tm$tp_rf <- stepwise_model(
    traindata = droplevels(
        interaction_tp_tm$purpose_data[
            which(interaction_tp_tm$purpose_data$labels != 'Unknown'), ]
    ), 
    categories = activity_categories, 
    selected_features = list(c(tp_sf_all[[1]],"predicted_mode")),
    stepwise = FALSE, 
    par_rf = list(ntree = 100, mtry = 7, 
                  importance = TRUE, do.trace = 1, 
                  keep.forest = FALSE
    )
)

save(interaction_tp_tm, file = '3_output/28_Apr/interaction_tp_tm.RData')




















