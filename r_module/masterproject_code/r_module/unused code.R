
# map <- ggmap::get_map(location = c(lon = 8.225, lat = 46.815), 
#                       zoom = 7, maptype = "roadmap")

# swiss_map <- ggmap::get_map(
#     make_bbox(lon = c(5.9, 10.55), lat = c(45.78, 47.85), f = 0.2), 
#     maptype = "roadmap")



# unclassified_swiss_dst <- MASS::kde2d(unclassified$lon, 
#                                       unclassified$lat, n = 2000)

system.time(
    unclassified_swiss_dst <- pointdensityP::pointdensity(
        unclassified_swiss, lat_col = lat, lon_col = lon, grid_size = 0.001, radius = 0
    )
)



ddd = unclassified_swiss_dst$z


# tp_svm_train <- caret::train(labels ~ ., data = purpose_labeled[
#     1:20000, .SD, .SDcol = c("labels", tp_sf_all[[1]])], method="svmRadial",
#     trControl=trainControl(method='cv', number=2))

# system.time(
#     tp_svm_train <- e1071::svm(
#         labels ~ ., 
#         data = purpose_labeled[1:10000, .SD, 
#                                .SDcol = c("labels", tp_sf_all[[1]])], 
#         kernel = 'radial', cross = 2)
# )



# Function to handling each group of a tbl and append results -------------


suppressPackageStartupMessages(library('dplyr'))

group_function <- function(singlegroup, col_names, methods = mean){
    
    # Input ----
    # 
    # singlegroup: tbl or data.frame
    # 
    # col_names: colnames used for output
    # 
    # methods: methods applied to each column in singlegroup, e.g., mean, 
    # sd, length, or self defined
    # 
    # 
    # Output ----
    # 
    # group_res: a data frame returned results from 'methods'
    # 
    # 
    # Function ----
    
    group_res <- as.data.frame(t(apply(singlegroup, 2, methods)))
    
    colnames(group_res) <- col_names
    
    return(group_res)
}


purpose_leisure <- purpose_labeled[which(purpose_labeled$labels == 'Leisure'), ]

purpose_leisure_cluster<- klaR::kmodes(purpose_leisure[
    1:42224, .SD, .SDcols = tp_sf_all[[1]]], modes = 2)



# system.time(
#     purpose_leisure_dist <- cluster::daisy(purpose_leisure[
#         1:42224, .SD, .SDcols = tp_sf_all[[1]]], metric = "gower")
# )


if(cls_methods == 'randomForest'){
    classification_res$train <- randomForest::randomForest(
        labels ~ ., 
        data = traindata[, c('labels', selected_features[[1]])],
        ntree = par_rf$ntree, mtry = par_rf$mtry, 
        importance = par_rf$importance, do.trace = par_rf$do.trace, 
        keep.forest = par_rf$keep.forest
    )
    if(! is.null(testdata)){
        classification_res$test <- predict(
            classification_res$train, 
            testdata[, c('labels', selected_features[[1]])])
    }
}




legs <- dplyr::bind_cols(
    legs, (legs %>% group_by(user_id)  %>% group_modify(
        ~ h_clustering(.x[, c('start_lon', 'start_lat')],
                       suffix = 'start')) 
    )[, -1]
)

legs <- dplyr::bind_cols(
    legs, (legs %>% group_by(user_id)  %>% group_modify(
        ~ h_clustering(.x[, c('end_lon', 'end_lat')],
                       suffix = 'end')) 
    )[, -1]
)

# infer how many legs for each participants
legs <- legs %>% dplyr::left_join(
    legs %>% group_by(user_id) %>% group_modify(
        ~group_function(.x[, 'trip_id'], 
                        col_names = 'legs_per_user', 
                        methods = length)
    ), by = 'user_id'
)

# Occurrences per surveyed day

legs <- legs %>% dplyr::left_join(
    legs %>% group_by(user_id) %>% group_modify(
        ~group_function(.x[, 'yday'],
                        col_names = 'surveyed_day',
                        methods = function(x) length(unique(x)))
    ), by = 'user_id'
)

# Count and frequency of trip starts/end in each cluster
legs <- legs %>% dplyr::left_join(
    legs %>% group_by(user_id, start_hc_cat) %>% group_modify(
        ~ group_function(.x[, 'trip_id'], 
                         col_names = 'legs_per_startcluster',
                         methods = length
        )
    ), by = c('user_id', 'start_hc_cat')
)

legs <- legs %>% dplyr::left_join(
    legs %>% group_by(user_id, end_hc_cat) %>% group_modify(
        ~ group_function(.x[, 'trip_id'], 
                         col_names = 'legs_per_endcluster',
                         methods = length
        )
    ), by = c('user_id', 'end_hc_cat')
)

legs$startcluster_freq <- legs$legs_per_startcluster / legs$legs_per_user
legs$endcluster_freq <- legs$legs_per_endcluster / legs$legs_per_user

legs$daily_freq_start <- legs$legs_per_startcluster / legs$surveyed_day
legs$daily_freq_end <- legs$legs_per_endcluster / legs$surveyed_day

# mean and standard 'duration_min', 'starttime', 'endtime' of each start and
# end cluster
legs <- legs %>% dplyr::left_join(
    legs %>% group_by(user_id, start_hc_cat) %>% group_modify(
        ~ group_function(.x[, c('duration_min', 'starttime', 
                                'endtime', 'speed')],
                         col_names = c('mean_duration_startcluster', 
                                       'mean_starttime_startcluster', 
                                       'mean_endtime_startcluster', 
                                       'mean_speed_startcluster'), 
                         methods = mean
        )
    ), by = c('user_id', 'start_hc_cat')
)

legs <- legs %>% dplyr::left_join(
    legs %>% group_by(user_id, start_hc_cat) %>% group_modify(
        ~ group_function(.x[, c('duration_min', 'starttime', 
                                'endtime', 'speed')],
                         col_names = c('sd_duration_startcluster', 
                                       'sd_starttime_startcluster', 
                                       'sd_endtime_startcluster', 
                                       'sd_speed_startcluster'), 
                         methods = sd
        )
    ), by = c('user_id', 'start_hc_cat')
)

legs <- legs %>% dplyr::left_join(
    legs %>% group_by(user_id, end_hc_cat) %>% group_modify(
        ~ group_function(.x[, c('duration_min', 'starttime', 
                                'endtime', 'speed')],
                         col_names = c('mean_duration_endcluster', 
                                       'mean_starttime_endcluster', 
                                       'mean_endtime_endcluster', 
                                       'mean_speed_endcluster'), 
                         methods = mean
        )
    ), by = c('user_id', 'end_hc_cat')
)

legs <- legs %>% dplyr::left_join(
    legs %>% group_by(user_id, end_hc_cat) %>% group_modify(
        ~ group_function(.x[, c('duration_min', 'starttime', 
                                'endtime', 'speed')],
                         col_names = c('sd_duration_endcluster', 
                                       'sd_starttime_endcluster', 
                                       'sd_endtime_endcluster', 
                                       'sd_speed_endcluster'), 
                         methods = sd
        )
    ), by = c('user_id', 'end_hc_cat')
)

# Percentage of weekdays
legs <- legs %>% dplyr::left_join(
    legs %>% group_by(user_id, start_hc_cat) %>% group_modify(
        ~ group_function(.x[, 'weekday'], 
                         col_names = 'weekday_freq',
                         method = function(x) 
                             length(which(x < 6)) / length(x)
                         
        )
    ), by = c('user_id', 'start_hc_cat')
)


# for(i in 1: dim(legs)[2]){
#     if(length(which(is.na(legs[, i]))) != 0 ){
#         print(colnames(legs)[i])
#     }
# }


folds3 <- caret::groupKFold(train_rf$ID, k = 2)
res_9srf_2ftrain3 <- stepwise_clf(train_rf[folds3[[1]], ], categories, 
                                  selected_features)
res_9srf_2ftest3 <- stepwise_test(res_9srf_2ftrain3, train_rf[folds3[[2]], ], 
                                  categories, selected_features) # 78.7%


folds5 <- caret::groupKFold(train_rf$ID, k = 2)
res_9srf_2ftrain5 <- randomForest::randomForest(
    labels ~ ., data = train_rf[folds5[[1]], 
                                c('labels', selected_features[[1]])], 
    ntree = 100, mtry = 4, importance = TRUE, 
    do.trace = 5, keep.forest = TRUE)  # 80.29%
res_9srf_2ftest5 <- predict(res_9srf_2ftrain5, 
                            train_rf[folds5[[2]], 
                                     c('labels', selected_features[[1]])]) # 80.8%
caret::confusionMatrix(table(res_9srf_2ftest5, train_rf[folds5[[2]], 'labels']))


#### (ok) comparison with/without personal attributes (83.1%, FS = 84.1%) ----
res_all <- randomForest(x = train_rf[, c(participants_attr,
                                         activities_attr, 
                                         cluster_attr,
                                         landuse)], 
                        y = train_rf[, 'purpose_new'],
                        ntree = 100, mtry = 8, importance = TRUE, 
                        do.trace = 5, keep.forest = TRUE)

save(res_all,file = '3_output/rf_res/res_all.RData')
load('3_output/rf_res/res_all.RData')

res_noprn <- randomForest(x = train_rf[, c(activities_attr, 
                                           cluster_attr,
                                           landuse)], 
                          y = train_rf[, 'purpose_new'],
                          ntree = 100, mtry = 4, importance = TRUE, 
                          do.trace = 5, keep.forest = TRUE)
save(res_noprn,file = '3_output/rf_res/res_noprn.RData')
load('3_output/rf_res/res_noprn.RData') # overall acuracy 83.1%

res_noprn_fs <- randomForest(x = train_rf[, c(
    "dist_center", 'sd_duration', 'mean_duration', 'daily_occr', 
    'act_prt', 'mean_starttime', 'daily_activities', 'wday_prt', 'duration_min', 
    'mean_endtime')], 
    y = train_rf[, 'purpose_new'],
    ntree = 100, mtry = 4, importance = TRUE, 
    do.trace = 5, keep.forest = TRUE)
save(res_noprn_fs,file = '3_output/rf_res/res_noprn_fs.RData')
load('3_output/rf_res/res_noprn_fs.RData') # overall acuracy 83.1%


#### nine step random forest, use adasyn to balance dateset(86%) ----


data <- train_rf
selected_features <- rep(list(c(activities_attr, cluster_attr, landuse)), 
                         (length(categories) + 1))


res_9srf <- stepwise_clf(data, categories, selected_features, 
                         balance_methods = NULL)
res_9srf_balanced <- stepwise_clf(data, categories, selected_features)


save(res_9srf,file = '3_output/stepwise_rf_res/res_9srf.RData')
save(res_9srf_balanced,file = '3_output/stepwise_rf_res/res_9srf_balanced.RData')
load('3_output/stepwise_rf_res/res_9srf.RData')
load('3_output/stepwise_rf_res/res_9srf_balanced.RData')

# a = 0
# for(i in 1:8){
#     a = a + res_9srf[[i]]$res$balanced_res$confusion[2, 2]
#     if (i < dim(res_9srf[[9]]$res$balanced_res$confusion)[1]){
#         a = a + res_9srf[[9]]$res$balanced_res$confusion[i, i]
#     }
# }
# a/dim(res_9srf$s1$data$original)[1] # 86.0% accuracy



# Fold experiment ----
# folds1 <- caret::groupKFold(train_rf$ID, k = 2)
# traindata <- train_rf[folds1[[1]], ]
# testdata <- train_rf[folds1[[2]], ]
# res_9srf_2ftrain <- stepwise_clf(traindata, categories, 
#                               selected_features, balance_methods = NULL)
# res_9srf_2ftest <- stepwise_test(res_9srf_2ftrain, testdata, 
#                                  categories, selected_features) # 78.5%
# 
# folds2 <- caret::groupKFold(train_rf$user_id, k = 2)
# res_9srf_2ftrain2 <- stepwise_clf(train_rf[folds2[[1]], ], categories, 
#                                  selected_features, balance_methods = NULL)
# res_9srf_2ftest2 <- stepwise_test(res_9srf_2ftrain2, train_rf[folds2[[2]], ], 
#                                  categories, selected_features) # 63.5%

# folds4 <- caret::groupKFold(train_rf$user_id, k = 2)
# res_9srf_2ftrain4 <- stepwise_clf(train_rf[folds4[[1]], ], categories, 
#                                   selected_features)
# res_9srf_2ftest4 <- stepwise_test(res_9srf_2ftrain4, train_rf[folds4[[2]], ], 
#                                   categories, selected_features) # 64.4%

# names(sort(res_9srf_2ftrain4$s1$balanced_res$importance[
#     , 'MeanDecreaseGini'], decreasing = TRUE))[1:10]

# folds6 <- caret::groupKFold(train_rf$user_id, k = 2)
# res_9srf_2ftrain6 <- randomForest::randomForest(
#     labels ~ ., data = train_rf[folds6[[1]], 
#                                 c('labels', selected_features[[1]])], 
#     ntree = 100, mtry = 4, importance = TRUE, 
#     do.trace = 5, keep.forest = TRUE)  # 83.6%
# res_9srf_2ftest6 <- predict(res_9srf_2ftrain6, 
#                             train_rf[folds6[[2]], 
#                                      c('labels', selected_features[[1]])]) # 66.9%
# caret::confusionMatrix(table(res_9srf_2ftest6, train_rf[folds6[[2]], 'labels']))








# a = 0
# for(i in 1:8){
#     a = a + res_9srf_2ftrain[[i]]$balanced_res$confusion[2, 2]
#     if (i < dim(res_9srf_2ftrain[[9]]$balanced_res$confusion)[1]){
#         a = a + res_9srf_2ftrain[[9]]$balanced_res$confusion[i, i]
#     }
# }
# a/dim(res_9srf_2ftrain[[1]]$original)[1] # 86.0% accuracy


stepwise_clf <- function(data, categories, selected_features, 
                         balance_methods = 'adasyn', 
                         cls_methods = 'randomForest',
                         par_adasyn = list(N_per_run = 5000, baseClass = NULL, 
                                           beta = 1, k = 5, dist = "HEOM"),
                         par_rf = list(ntree = 100, mtry = 4, importance = TRUE, 
                                       do.trace = 5, keep.forest = TRUE)){
    # Input ----
    # 
    # data: data used to do classification. 
    # The labeled column should have colname 'labels'
    # The instance id column must have colname 'ID'
    # 
    # categories: categories in labels, its order will be in confusion matrix
    # 
    # selected_features: selected features used to be predictors, each list
    # elements accounts for one step
    # 
    # balance_methods: methods used to balance imbalanced data set. Can be 
    # 'adasyn' or NULL
    # 
    # cls_methods: methods used to do classification
    # 
    # par_adasyn: parameters used for adasyn
    # par_rf: parameters used for random forest
    # 
    # 
    # Output ----
    # 
    # sw_clf_res: a list containing length(categories) + 1 lists for each 
    # classification step. Each list containing 
    # 
    # 
    # Function ----
    
    
    levels(data$labels) <- categories
    
    # Add one column to store classification results in each step
    data[, 'step_res'] <- NA
    
    # store classified instances for each step
    clf_res <- NULL
    
    # Add one column for each category as binary classification
    for(i in 1:length(categories)){
        data[, categories[i]] <- 
            factor(ifelse(data$labels == categories[i], 1, 0))
    }
    
    # create a list to store classification results 
    sw_clf_res <- NULL
    for(i in 1 : (length(categories) + 1)){
        sw_clf_res[[paste('s', i, sep = '')]] <- list(
            data = list(original = 0, reduced = 0, balanced = 0, unclf = 0), 
            res = list(balanced_res = 0, reduced_clf = 0))
    }
    
    for(i in 1 : (length(categories) + 1)){
        
        # specify original data as the input data (i=1) or unclassified data 
        # from last step (i>1)
        if(i==1){
            sw_clf_res[[i]]$data$original = data
        } else{
            sw_clf_res[[i]]$data$original = sw_clf_res[[i-1]]$data$unclf
        }
        
        # Extract data used for prediction
        sw_clf_res[[i]]$data$reduced <- 
            sw_clf_res[[i]]$data$original[
                , c(ifelse(i < (length(categories) + 1), categories[i], 'labels'), 
                    selected_features[[i]])]
        colnames(sw_clf_res[[i]]$data$reduced)[1] <- 'objective'
        
        # imbalanced data pre processing
        if(is.null(balance_methods)){
            sw_clf_res[[i]]$data$balanced <- sw_clf_res[[i]]$data$reduced
        } else if(balance_methods == 'adasyn'){
            sw_clf_res[[i]]$data$balanced <- 
                adasyn(objective ~ .,  sw_clf_res[[i]]$data$reduced, 
                       N_per_run = par_adasyn$N_per_run, 
                       baseClass = par_adasyn$baseClass, beta = par_adasyn$beta, 
                       k = par_adasyn$k, dist = par_adasyn$dist)
        }
        
        # classification based on selected algorithm
        if(cls_methods == 'randomForest'){
            sw_clf_res[[i]]$res$balanced_res <- 
                randomForest::randomForest(
                    objective ~ ., data = sw_clf_res[[i]]$data$balanced, 
                    ntree = par_rf$ntree, mtry = par_rf$mtry, 
                    importance = par_rf$importance, do.trace = par_rf$do.trace, 
                    keep.forest = par_rf$keep.forest)
        }
        
        # predict the results using trained model
        sw_clf_res[[i]]$res$reduced_clf <- 
            predict(sw_clf_res[[i]]$res$balanced_res, 
                    sw_clf_res[[i]]$data$reduced)
        
        if(i < (length(categories) + 1)){
            # store the unclassified data
            sw_clf_res[[i]]$data$unclf <- 
                droplevels(sw_clf_res[[i]]$data$original[
                    which(sw_clf_res[[i]]$res$reduced_clf == 0), ])
            
            # Add predicted labels to initial data in each step
            sw_clf_res[[i]]$data$original[
                which(sw_clf_res[[i]]$res$reduced_clf == 1), 
                'step_res'] <- categories[i]
            clf_res <- rbind(
                clf_res, sw_clf_res[[i]]$data$original[
                    which(sw_clf_res[[i]]$res$reduced_clf == 1), 
                    c('ID', 'step_res')])
        } else{
            sw_clf_res[[i]]$data$original[, 'step_res'] <- 
                sw_clf_res[[i]]$res$reduced_clf
            clf_res <- rbind(
                clf_res, sw_clf_res[[i]]$data$original[, c('ID', 'step_res')])
        }
    }
    colnames(clf_res)[2] <- 'clf_res'
    
    sw_clf_res[[1]]$data$original <- merge(sw_clf_res[[1]]$data$original, 
                                           clf_res, by.x = 'ID', by.y = 'ID',
                                           sort = FALSE)
    
    sw_clf_res[[1]]$data$original$clf_res <- factor(
        sw_clf_res[[1]]$data$original$clf_res, levels = categories)
    
    sw_clf_res[[1]]$res$confusion <- table(sw_clf_res[[1]]$data$original[
        , c('labels', 'clf_res')])
    sw_clf_res[[1]]$res$categories <- categories
    sw_clf_res[[1]]$res$selected_features <- selected_features
    
    return(sw_clf_res)
}



stepwise_test <- function(
    object, testdata, 
    factor_features = c("weekday", "inside_day", "construction_zone")){
    # Input ----
    # 
    # object: results returned by function stepwise_clf
    # 
    # testdata: same structure as in the input 'data' of stepwise_clf.
    # 
    # factor_features: used when the levels of factors in test set is larger 
    # than the train set, assign these factors to the most frequent factors in 
    # train set
    # 
    # 
    # Output ----
    # 
    # sw_test_res: a list containing length(categories) + 1 lists for each 
    # classification step.
    # 
    # 
    # Function ----
    
    # create a list to store classification results 
    sw_test_res <- NULL
    for(i in 1 : (length(object[[1]]$res$categories) + 1)){
        sw_test_res[[paste('s', i, sep = '')]] <- list(
            original = 0, reduced = 0, unclf = 0)
    }
    
    levels(testdata$labels) <- object[[1]]$res$categories
    
    # Add two column to store classification results in each step
    # reduced_clf: classification results returned by randomForest model
    # step_res: predicted labels
    testdata[, c('reduced_clf', 'step_res')] <- NA
    
    # store classified instances for each step
    clf_res <- NULL
    
    for(i in 1 : (length(categories) + 1)){
        
        # specify original data as the input data (i=1) or unclassified data 
        # from last step (i>1)
        if(i==1){
            sw_test_res[[i]]$original = testdata
        } else{
            sw_test_res[[i]]$original = sw_test_res[[i-1]]$unclf
        }
        
        # Extract data used for prediction
        sw_test_res[[i]]$reduced <- 
            sw_test_res[[i]]$original[
                , object[[1]]$res$selected_features[[i]]]
        
        # Align the factor level in test set identical to that of train set
        for(j in 1 : length(factor_features)){
            if(! is.null(sw_test_res[[i]]$reduced[, factor_features[j]])){
                sw_test_res[[i]]$reduced[
                    which(sw_test_res[[i]]$reduced[, factor_features[j]] %in% 
                              unique(object[[i]]$data$balanced[
                                  , factor_features[j]]) == FALSE), 
                    factor_features[j]] <- 
                    DescTools::Mode(object[[i]]$data$balanced[
                        , factor_features[j]])[1]
                levels(sw_test_res[[i]]$reduced[, factor_features[j]]) <- 
                    levels(object[[i]]$data$balanced[, factor_features[j]])
            }
        }
        
        # predict the results using trained model
        sw_test_res[[i]]$res$reduced_clf <- 
            predict(object[[i]]$res$balanced_res, sw_test_res[[i]]$data$reduced)
        
        if(i < (length(categories) + 1)){
            # store the unclassified data
            sw_test_res[[i]]$data$unclf <- 
                droplevels(sw_test_res[[i]]$data$original[
                    which(sw_test_res[[i]]$res$reduced_clf == 0), ])
            
            # Add predicted labels to initial data in each step
            sw_test_res[[i]]$data$original[
                which(sw_test_res[[i]]$res$reduced_clf == 1), 
                'step_res'] <- categories[i]
            clf_res <- rbind(
                clf_res, sw_test_res[[i]]$data$original[
                    which(sw_test_res[[i]]$res$reduced_clf == 1), 
                    c('ID', 'step_res')])
        } else{
            sw_test_res[[i]]$data$original[, 'step_res'] <- 
                sw_test_res[[i]]$res$reduced_clf
            clf_res <- rbind(
                clf_res, sw_test_res[[i]]$data$original[, c('ID', 'step_res')])
        }
    }
    
    colnames(clf_res)[2] <- 'clf_res'
    
    sw_test_res[[1]]$data$original <- merge(sw_test_res[[1]]$data$original, 
                                            clf_res, by.x = 'ID', by.y = 'ID',
                                            sort = FALSE)
    
    sw_test_res[[1]]$data$original$clf_res <- factor(
        sw_test_res[[1]]$data$original$clf_res, levels = categories)
    
    sw_test_res[[1]]$res$confusion <- table(sw_test_res[[1]]$data$original[
        , c('labels', 'clf_res')])
    
    sw_test_res[[1]]$res$accuracy <- sum(diag(sw_test_res[[1]]$res$confusion)) / 
        dim(sw_test_res[[1]]$data$original)[1]
    
    return(sw_test_res)
}


# 
if(i == (length(categories) + 1) &
   !is.null(sw_test_res[[i]]$data$reduced$construction_zone)){
    levels(sw_test_res[[i]]$data$reduced$construction_zone) <- 
        levels(object[[i]]$data$reduced$construction_zone)
}


# res_9srf_2balanced <- stepwise_clf(data, categories, selected_features, 
#                                   par_adasyn = list(
#                                       N_per_run = 10000, baseClass = NULL,
#                                       beta = 1, k = 5, dist = "HEOM"))
# save(res_9srf_2balanced,file = '3_output/stepwise_rf_res/res_9srf_2balanced.RData')

#### nine step random forest (82.6%) ----

train_9srf <- list(s1 = train_rf, s2 = 0, s3 = 0, s4 = 0, s5 = 0, 
                   s6 = 0, s7 = 0, s8 = 0, s9 = 0)
res_9srf <- list(ifhome = 0, ifwork = 0, ifleisure = 0, ifeducation = 0, 
                 ifshopping = 0, ifother = 0, iferrand = 0, 
                 ifassistance = 0, rest = 0)

train_9srf$s1$athome <- factor(ifelse(train_9srf$s1$purpose_new == 'home', 1, 0))
train_9srf$s1$atwork <- factor(ifelse(train_9srf$s1$purpose_new == 'work', 1, 0))
train_9srf$s1$atleisure <- factor(ifelse(train_9srf$s1$purpose_new == 'leisure', 1, 0))
train_9srf$s1$ateducation <- factor(ifelse(train_9srf$s1$purpose_new == 'education', 1, 0))
train_9srf$s1$atshopping <- factor(ifelse(train_9srf$s1$purpose_new == 'shopping', 1, 0))
train_9srf$s1$atother <- factor(ifelse(train_9srf$s1$purpose_new == 'other', 1, 0))
train_9srf$s1$aterrand <- factor(ifelse(train_9srf$s1$purpose_new == 
                                            'business_travel', 1, 0))
train_9srf$s1$atassistance <- factor(ifelse(train_9srf$s1$purpose_new == 
                                                'giving_people_lifts', 1, 0))

res_9srf$ifhome <- randomForest(x = train_9srf$s1[, c(activities_attr, 
                                                      cluster_attr,
                                                      landuse)], 
                                y = train_9srf$s1[, 'athome'],
                                ntree = 100, mtry = 4, importance = TRUE, 
                                do.trace = 5, keep.forest = TRUE)
train_9srf$s2 <- train_9srf$s1[which(res_9srf$ifhome$predicted == 0), ]

res_9srf$ifwork <- randomForest(x = train_9srf$s2[, c(activities_attr, 
                                                      cluster_attr,
                                                      landuse)],
                                y = train_9srf$s2[, 'atwork'],
                                ntree = 100, mtry = 4, importance = TRUE,
                                do.trace = 5, keep.forest = TRUE)
train_9srf$s3 <- train_9srf$s2[which(res_9srf$ifwork$predicted == 0), ]

res_9srf$ifleisure <- randomForest(x = train_9srf$s3[, c(activities_attr, 
                                                         cluster_attr,
                                                         landuse)],
                                   y = train_9srf$s3[, 'atleisure'],
                                   ntree = 100, mtry = 4, importance = TRUE,
                                   do.trace = 5, keep.forest = TRUE)
train_9srf$s4 <- train_9srf$s3[which(res_9srf$ifleisure$predicted == 0), ]

res_9srf$ifeducation <- randomForest(x = train_9srf$s4[, c(activities_attr, 
                                                           cluster_attr,
                                                           landuse)],
                                     y = train_9srf$s4[, 'ateducation'],
                                     ntree = 100, mtry = 4, importance = TRUE,
                                     do.trace = 5, keep.forest = TRUE)
train_9srf$s5 <- train_9srf$s4[which(res_9srf$ifeducation$predicted == 0), ]

res_9srf$ifshopping <- randomForest(x = train_9srf$s5[, c(activities_attr, 
                                                          cluster_attr,
                                                          landuse)],
                                    y = train_9srf$s5[, 'atshopping'],
                                    ntree = 100, mtry = 4, importance = TRUE,
                                    do.trace = 5, keep.forest = TRUE)
train_9srf$s6 <- train_9srf$s5[which(res_9srf$ifshopping$predicted == 0), ]

res_9srf$ifother <- randomForest(x = train_9srf$s6[, c(activities_attr, 
                                                       cluster_attr,
                                                       landuse)],
                                 y = train_9srf$s6[, 'atother'],
                                 ntree = 100, mtry = 4, importance = TRUE,
                                 do.trace = 5, keep.forest = TRUE)
train_9srf$s7 <- train_9srf$s6[which(res_9srf$ifother$predicted == 0), ]

res_9srf$iferrand <- randomForest(x = train_9srf$s7[, c(activities_attr, 
                                                        cluster_attr,
                                                        landuse)],
                                  y = train_9srf$s7[, 'aterrand'],
                                  ntree = 100, mtry = 4, importance = TRUE,
                                  do.trace = 5, keep.forest = TRUE)
train_9srf$s8 <- train_9srf$s7[which(res_9srf$iferrand$predicted == 0), ]

res_9srf$ifassistance <- randomForest(x = train_9srf$s8[, c(activities_attr, 
                                                            cluster_attr,
                                                            landuse)],
                                      y = train_9srf$s8[, 'atassistance'],
                                      ntree = 100, mtry = 4, importance = TRUE,
                                      do.trace = 5, keep.forest = TRUE)
train_9srf$s9 <- train_9srf$s8[which(res_9srf$ifassistance$predicted == 0), ]

res_9srf$rest <- randomForest(x = train_9srf$s9[, c(activities_attr, 
                                                    cluster_attr,
                                                    landuse)],
                              y = train_9srf$s9[, 'purpose_new'],
                              ntree = 100, mtry = 4, importance = TRUE,
                              do.trace = 5, keep.forest = TRUE)

save(train_9srf,file = '3_output/rf_res/train_9srf.RData')
save(res_9srf,file = '3_output/rf_res/res_9srf.RData')
load('3_output/rf_res/res_9srf.RData')
# Overall accuracy %

# (res_9srf[[1]]$confusion[2, 2] + res_9srf[[2]]$confusion[2, 2] +
#     res_9srf[[3]]$confusion[2, 2] + res_9srf[[4]]$confusion [2, 2] + 
#     res_9srf[[5]]$confusion[2, 2] + res_9srf[[6]]$confusion [2, 2] + 
#     res_9srf[[7]]$confusion[2, 2] + res_9srf[[8]]$confusion [2, 2] + 
#     res_9srf[[9]]$confusion[1, 1] + res_9srf[[9]]$confusion[2, 2] + 
#     res_9srf[[9]]$confusion[3, 3] + res_9srf[[9]]$confusion[4, 4] + 
#     res_9srf[[9]]$confusion[5, 5] + res_9srf[[9]]$confusion[6, 6] + 
#     res_9srf[[9]]$confusion[7, 7] + res_9srf[[9]]$confusion[8, 8]) / 
#     dim(train_9srf$s1)[1]


#### (ok) nine step random forest with feature selection (83.8%) ----

train_9srf_fs <- list(s1 = train_rf, s2 = 0, s3 = 0, s4 = 0, s5 = 0, 
                      s6 = 0, s7 = 0, s8 = 0, s9 = 0)
res_9srf_fs <- list(ifhome = 0, ifwork = 0, ifleisure = 0, ifeducation = 0, 
                    ifshopping = 0, ifother = 0, iferrand = 0, 
                    ifassistance = 0, rest = 0)

train_9srf_fs$s1$athome <- factor(ifelse(train_9srf_fs$s1$purpose_new == 'home', 1, 0))
train_9srf_fs$s1$atwork <- factor(ifelse(train_9srf_fs$s1$purpose_new == 'work', 1, 0))
train_9srf_fs$s1$atleisure <- factor(ifelse(train_9srf_fs$s1$purpose_new == 'leisure', 1, 0))
train_9srf_fs$s1$ateducation <- factor(ifelse(train_9srf_fs$s1$purpose_new == 'education', 1, 0))
train_9srf_fs$s1$atshopping <- factor(ifelse(train_9srf_fs$s1$purpose_new == 'shopping', 1, 0))
train_9srf_fs$s1$atother <- factor(ifelse(train_9srf_fs$s1$purpose_new == 'other', 1, 0))
train_9srf_fs$s1$aterrand <- factor(ifelse(train_9srf_fs$s1$purpose_new == 
                                               'business_travel', 1, 0))
train_9srf_fs$s1$atassistance <- factor(ifelse(train_9srf_fs$s1$purpose_new == 
                                                   'giving_people_lifts', 1, 0))

res_9srf_fs$ifhome <- randomForest(x = train_9srf_fs$s1[, c(
    "mean_duration", 'sd_duration', 'mean_starttime', 'sd_endtime', 
    'act_prt', 'daily_occr', 'dist_center', 'sd_starttime', 'wday_prt', 
    'inside_day')], 
    y = train_9srf_fs$s1[, 'athome'],
    ntree = 100, mtry = 4, importance = TRUE, 
    do.trace = 5, keep.forest = TRUE)
train_9srf_fs$s2 <- train_9srf_fs$s1[which(res_9srf_fs$ifhome$predicted == 0), ]

res_9srf_fs$ifwork <- randomForest(x = train_9srf_fs$s2[, c(
    "mean_duration", 'sd_duration', 'mean_starttime', 'act_prt', 
    'daily_occr', 'daily_activities', 'wday_prt', 'dist_center', 'duration_min', 
    'mean_endtime')],
    y = train_9srf_fs$s2[, 'atwork'],
    ntree = 100, mtry = 4, importance = TRUE,
    do.trace = 5, keep.forest = TRUE)
train_9srf_fs$s3 <- train_9srf_fs$s2[which(res_9srf_fs$ifwork$predicted == 0), ]

res_9srf_fs$ifleisure <- randomForest(x = train_9srf_fs$s3[, c(
    "mean_duration", 'dist_center', 'act_prt', 'daily_occr', 
    'daily_activities', 'duration_min', 'mean_starttime', 'construction_zone', 'wday_prt', 
    'mean_endtime')],
    y = train_9srf_fs$s3[, 'atleisure'],
    ntree = 100, mtry = 4, importance = TRUE,
    do.trace = 5, keep.forest = TRUE)
train_9srf_fs$s4 <- train_9srf_fs$s3[which(res_9srf_fs$ifleisure$predicted == 0), ]

res_9srf_fs$ifeducation <- randomForest(x = train_9srf_fs$s4[, c(
    "mean_duration", 'sd_duration', "daily_occr", "dist_center", 
    "mean_starttime", "act_prt", "wday_prt", "daily_activities", 
    "duration_min", "sd_endtime")],
    y = train_9srf_fs$s4[, 'ateducation'],
    ntree = 100, mtry = 4, importance = TRUE,
    do.trace = 5, keep.forest = TRUE)
train_9srf_fs$s5 <- train_9srf_fs$s4[which(res_9srf_fs$ifeducation$predicted == 0), ]

res_9srf_fs$ifshopping <- randomForest(x = train_9srf_fs$s5[, c(
    "dist_center", "mean_duration", "daily_activities", "act_prt", "daily_occr", 
    "duration_min", "construction_zone", "sd_duration", "mean_endtime", 
    "mean_starttime")],
    y = train_9srf_fs$s5[, 'atshopping'],
    ntree = 100, mtry = 4, importance = TRUE,
    do.trace = 5, keep.forest = TRUE)
train_9srf_fs$s6 <- train_9srf_fs$s5[which(res_9srf_fs$ifshopping$predicted == 0), ]

res_9srf_fs$ifother <- randomForest(x = train_9srf_fs$s6[, c(
    "mean_duration", "dist_center", "daily_occr", "act_prt", 
    "duration_min", "daily_activities", "construction_zone", 
    "sd_duration", "mean_starttime", "weekday")],
    y = train_9srf_fs$s6[, 'atother'],
    ntree = 100, mtry = 4, importance = TRUE,
    do.trace = 5, keep.forest = TRUE)
train_9srf_fs$s7 <- train_9srf_fs$s6[which(res_9srf_fs$ifother$predicted == 0), ]

res_9srf_fs$iferrand <- randomForest(x = train_9srf_fs$s7[, c(
    "act_prt", "dist_center", "daily_occr", "daily_activities", 
    "mean_duration", "duration_min", "construction_zone", "sd_duration", 
    "weekday", "mean_starttime")],
    y = train_9srf_fs$s7[, 'aterrand'],
    ntree = 100, mtry = 4, importance = TRUE,
    do.trace = 5, keep.forest = TRUE)
train_9srf_fs$s8 <- train_9srf_fs$s7[which(res_9srf_fs$iferrand$predicted == 0), ]

res_9srf_fs$ifassistance <- randomForest(x = train_9srf_fs$s8[, c(
    "act_prt", "daily_occr", "dist_center", "daily_activities", 
    "mean_duration", "duration_min", "sd_duration", "sd_endtime", 
    "sd_starttime", "construction_zone")],
    y = train_9srf_fs$s8[, 'atassistance'],
    ntree = 100, mtry = 4, importance = TRUE,
    do.trace = 5, keep.forest = TRUE)
train_9srf_fs$s9 <- train_9srf_fs$s8[which(res_9srf_fs$ifassistance$predicted == 0), ]

res_9srf_fs$rest <- randomForest(x = train_9srf_fs$s9[, c(
    "dist_center", "daily_activities", "mean_duration", "act_prt", 
    "duration_min", "daily_occr", "construction_zone", "weekday", 
    "mean_starttime", "mean_endtime")],
    y = train_9srf_fs$s9[, 'purpose_new'],
    ntree = 100, mtry = 4, importance = TRUE,
    do.trace = 5, keep.forest = TRUE)

save(train_9srf_fs,file = '3_output/rf_res/train_9srf_fs.RData')
save(res_9srf_fs,file = '3_output/rf_res/res_9srf_fs.RData')
load('3_output/rf_res/train_9srf_fs.RData')
load('3_output/rf_res/res_9srf_fs.RData')
# rownames(res_9srf$rest$importance[order(res_9srf$rest$importance[, 10], decreasing = TRUE),])[1:10]
# Overall accuracy %

# (res_9srf_fs[[1]]$confusion[2, 2] + res_9srf_fs[[2]]$confusion[2, 2] +
#         res_9srf_fs[[3]]$confusion[2, 2] + res_9srf_fs[[4]]$confusion [2, 2] +
#         res_9srf_fs[[5]]$confusion[2, 2] + res_9srf_fs[[6]]$confusion [2, 2] +
#         res_9srf_fs[[7]]$confusion[2, 2] + res_9srf_fs[[8]]$confusion [2, 2] +
#         res_9srf_fs[[9]]$confusion[1, 1] + res_9srf_fs[[9]]$confusion[2, 2] +
#         res_9srf_fs[[9]]$confusion[3, 3] + res_9srf_fs[[9]]$confusion[4, 4] +
#         res_9srf_fs[[9]]$confusion[5, 5] + res_9srf_fs[[9]]$confusion[6, 6] +
#         res_9srf_fs[[9]]$confusion[7, 7] + res_9srf_fs[[9]]$confusion[8, 8]) /
#     dim(train_9srf_fs$s1)[1]



#### three step random forest (82%) ----

train_msrf <- list(s1 = train_rf, s2 = 0, s3 = 0)
res_msrf <- list(ifhome = 0, ifwork = 0, rest = 0)

train_msrf$s1$athome <- factor(ifelse(train_msrf$s1$purpose_new == 'home', 1, 0))
train_msrf$s1$atwork <- factor(ifelse(train_msrf$s1$purpose_new == 'work', 1, 0))

res_msrf$ifhome <- randomForest(x = train_msrf$s1[, c(activities_attr, 
                                                      cluster_attr,
                                                      landuse)], 
                                y = train_msrf$s1[, 'athome'],
                                ntree = 100, mtry = 4, importance = TRUE, 
                                do.trace = 5, keep.forest = TRUE)
train_msrf$s2 <- train_msrf$s1[which(res_msrf$ifhome$predicted == 0), ]

res_msrf$ifwork <- randomForest(x = train_msrf$s2[, c(activities_attr, 
                                                      cluster_attr,
                                                      landuse)],
                                y = train_msrf$s2[, 'atwork'],
                                ntree = 100, mtry = 4, importance = TRUE,
                                do.trace = 5, keep.forest = TRUE)
train_msrf$s3 <- train_msrf$s2[which(res_msrf$ifwork$predicted == 0), ]

res_msrf$rest <- randomForest(x = train_msrf$s3[, c(activities_attr, 
                                                    cluster_attr,
                                                    landuse)],
                              y = train_msrf$s3[, 'purpose_new'],
                              ntree = 100, mtry = 4, importance = TRUE,
                              do.trace = 5, keep.forest = TRUE)

save(train_msrf,file = '3_output/rf_res/train_msrf.RData')
save(res_msrf,file = '3_output/rf_res/res_msrf.RData')
load('3_output/rf_res/train_msrf.RData')
load('3_output/rf_res/res_msrf.RData')
# Overall accuracy 82%
# (res_msrf$ifhome$confusion[2, 2] + res_msrf$ifwork$confusion[2, 2] + 
#     res_msrf$rest$confusion[1, 1] + res_msrf$rest$confusion[2, 2] +
#     res_msrf$rest$confusion[3, 3] + res_msrf$rest$confusion[4, 4] +
#     res_msrf$rest$confusion[5, 5] + res_msrf$rest$confusion[6, 6] +
#     res_msrf$rest$confusion[7, 7] + res_msrf$rest$confusion[8, 8]) / 
#     dim(train_msrf$s1)[1]


#### (ok) three step random forest with feature selection (83.9%) ----

# "duration_min", "starttime", "endtime", "weekday", "inside_day", "daily_activities"
# 'mean_duration', 'sd_duration', 'mean_starttime', 'sd_starttime', 'mean_endtime',
# 'sd_endtime', 'wday_prt','act_prt', 'daily_occr', 'dist_center'

train_msrf_fs <- list(s1 = train_rf, s2 = 0, s3 = 0)
res_msrf_fs <- list(ifhome = 0, ifwork = 0, rest = 0)

train_msrf_fs$s1$athome <- factor(ifelse(train_msrf_fs$s1$purpose_new == 'home', 1, 0))
train_msrf_fs$s1$atwork <- factor(ifelse(train_msrf_fs$s1$purpose_new == 'work', 1, 0))

res_msrf_fs$ifhome <- randomForest(x = train_msrf_fs$s1[, c(
    "daily_activities", 'mean_duration', 'sd_duration', 'mean_starttime', 
    'sd_endtime', 'sd_starttime', 'wday_prt', 'act_prt', 'daily_occr', 
    'dist_center')], 
    y = train_msrf_fs$s1[, 'athome'],
    ntree = 100, mtry = 4, importance = TRUE, 
    do.trace = 5, keep.forest = TRUE)
train_msrf_fs$s2 <- train_msrf_fs$s1[which(res_msrf_fs$ifhome$predicted == 0), ]

res_msrf_fs$ifwork <- randomForest(x = train_msrf_fs$s2[, c(
    "mean_duration", 'sd_duration', 'mean_starttime', 'act_prt', 
    'daily_occr', 'daily_activities', 'wday_prt', 'dist_center', 'duration_min', 
    'sd_endtime')],
    y = train_msrf_fs$s2[, 'atwork'],
    ntree = 100, mtry = 4, importance = TRUE,
    do.trace = 5, keep.forest = TRUE)
train_msrf_fs$s3 <- train_msrf_fs$s2[which(res_msrf_fs$ifwork$predicted == 0), ]

res_msrf_fs$rest <- randomForest(x = train_msrf_fs$s3[, c(
    "duration_min", 'daily_activities', 'mean_duration', 'sd_duration', 
    'mean_starttime', 'act_prt', 'daily_occr', 'dist_center', 'construction_zone', 
    'mean_endtime')],
    y = train_msrf_fs$s3[, 'purpose_new'],
    ntree = 100, mtry = 4, importance = TRUE,
    do.trace = 5, keep.forest = TRUE)

save(train_msrf_fs,file = '3_output/rf_res/train_msrf_fs.RData')
save(res_msrf_fs,file = '3_output/rf_res/res_msrf_fs.RData')
load('3_output/rf_res/res_msrf_fs.RData')
load('3_output/rf_res/train_msrf_fs.RData')
# Overall accuracy %
# (res_msrf_fs$ifhome$confusion[2, 2] + res_msrf_fs$ifwork$confusion[2, 2] +
#     res_msrf_fs$rest$confusion[1, 1] + res_msrf_fs$rest$confusion[2, 2] +
#     res_msrf_fs$rest$confusion[3, 3] + res_msrf_fs$rest$confusion[4, 4] +
#     res_msrf_fs$rest$confusion[5, 5] + res_msrf_fs$rest$confusion[6, 6] +
#     res_msrf_fs$rest$confusion[7, 7] + res_msrf_fs$rest$confusion[8, 8]) /
#     dim(train_msrf_fs$s1)[1]


#### nine step random forest, use adasyn to balance dateset(99.5%) ----

activities_cat <- c('home', 'work', 'leisure', 'education', 'shopping', 
                    'other', 'business_travel', 'giving_people_lifts')

data_list <- list(original_data = 0, original_data_reduced = 0, 
                  balanced_data_reduced = 0, unclassified_data = 0)
train_9srf_adasyn <- list(s1 = data_list, s2 = data_list, s3 = data_list,
                          s4 = data_list, s5 = data_list, s6 = data_list, 
                          s7 = data_list, s8 = data_list, s9 = data_list)

res_list <- list(original_res = 0, balanced_res = 0, 
                 original_clf = 0, confusion = 0)
res_9srf_adasyn <- list(s1 = res_list, s2 = res_list, s3 = res_list,
                        s4 = res_list, s5 = res_list, s6 = res_list,
                        s7 = res_list, s8 = res_list, s9 = res_list)


for(i in 1:length(activities_cat)){
    train_rf[, activities_cat[i]] <- 
        factor(ifelse(train_rf$purpose_new == activities_cat[i], 1, 0))
}

for(i in 1:9){
    if(i == 1){
        train_9srf_adasyn[[i]]$original_data <- train_rf
    } else {
        train_9srf_adasyn[[i]]$original_data <- 
            train_9srf_adasyn[[i-1]]$unclassified_data
    }
    
    if(i < 9){
        train_9srf_adasyn[[i]]$original_data_reduced <- 
            train_9srf_adasyn[[i]]$original_data[
                , c(activities_cat[i], activities_attr, cluster_attr, landuse)]
        colnames(train_9srf_adasyn[[i]]$original_data_reduced)[1] <- 'objective'
    } else{
        train_9srf_adasyn[[i]]$original_data_reduced <- 
            train_9srf_adasyn[[i]]$original_data[
                , c('purpose_new', activities_attr, cluster_attr, landuse)]
        colnames(train_9srf_adasyn[[i]]$original_data_reduced)[1] <- 'objective'
    }
    
    train_9srf_adasyn[[i]]$balanced_data_reduced <- 
        adasyn(objective ~ .,  train_9srf_adasyn[[i]]$original_data_reduced)
    
    res_9srf_adasyn[[i]]$original_res <- randomForest(
        objective ~ ., data = train_9srf_adasyn[[i]]$original_data_reduced, 
        ntree = 100, mtry = 4, importance = TRUE, do.trace = 5, 
        keep.forest = TRUE)
    
    res_9srf_adasyn[[i]]$balanced_res <- randomForest(
        objective ~ ., data = train_9srf_adasyn[[i]]$balanced_data_reduced, 
        ntree = 100, mtry = 4, importance = TRUE, do.trace = 5, 
        keep.forest = TRUE)
    
    res_9srf_adasyn[[i]]$original_clf <- predict(
        res_9srf_adasyn[[i]]$balanced_res, 
        train_9srf_adasyn[[i]]$original_data_reduced)
    
    res_9srf_adasyn[[i]]$confusion <- table(
        res_9srf_adasyn[[i]]$original_clf,
        train_9srf_adasyn[[i]]$original_data_reduced$objective)
    
    if(i < 9){
        train_9srf_adasyn[[i]]$unclassified_data <- 
            train_9srf_adasyn[[i]]$original_data[
                which(res_9srf_adasyn[[i]]$original_clf == 0), ]
    }
}


save(train_9srf_adasyn,file = '3_output/rf_res/train_9srf_adasyn.RData')
save(res_9srf_adasyn,file = '3_output/rf_res/res_9srf_adasyn.RData')
load('3_output/rf_res/res_9srf_adasyn.RData')

# a = 0
# for(i in 1:8){
#     a = a + res_9srf_adasyn[[i]]$confusion[2, 2]
#     a = a + res_9srf_adasyn[[9]]$confusion[i, i]
# }


#### multi stage random forest ----

train_rf$athome <- factor(ifelse(train_rf$purpose_new == 'home', 1, 0))
train_rf$atwork <- factor(ifelse(train_rf$purpose_new == 'work', 1, 0))
train_rf$atleisure <- factor(ifelse(train_rf$purpose_new == 'leisure', 1, 0))
train_rf$ateducation <- factor(ifelse(train_rf$purpose_new == 'education', 1, 0))
train_rf$atshopping <- factor(ifelse(train_rf$purpose_new == 'shopping', 1, 0))
train_rf$atother <- factor(ifelse(train_rf$purpose_new == 'other', 1, 0))
train_rf$atbusiness_travel <- factor(ifelse(train_rf$purpose_new == 
                                                'business_travel', 1, 0))
train_rf$atgiving_people_lifts <- factor(ifelse(train_rf$purpose_new == 
                                                    'giving_people_lifts', 1, 0))

res_home_noprn <- randomForest(x = train_rf[, c(activities_attr, 
                                                cluster_attr,
                                                landuse)], 
                               y = train_rf[, 'athome'],
                               ntree = 100, mtry = 5, importance = TRUE, 
                               do.trace = 5, keep.forest = TRUE)
train_rf <- train_rf[which(res_home_noprn$predicted == 0), ]
save(res_home_noprn,file = '3_output/res_home_noprn.RData')
rm(res_home_noprn)

res_work_noprn <- randomForest(x = train_rf[, c(activities_attr, 
                                                cluster_attr,
                                                landuse)],
                               y = train_rf[, 'atwork'],
                               ntree = 100, mtry = 5, importance = TRUE,
                               do.trace = 5, keep.forest = TRUE)
train_rf <- train_rf[which(res_work_noprn$predicted == 0), ]
save(res_work_noprn,file = '3_output/res_work_noprn.RData')
rm(res_work_noprn)

res_leisure_noprn <- randomForest(x = train_rf[, c(activities_attr, 
                                                   cluster_attr,
                                                   landuse)],
                                  y = train_rf[, 'atleisure'],
                                  ntree = 100, mtry = 5, importance = TRUE,
                                  do.trace = 5, keep.forest = TRUE)
train_rf <- train_rf[which(res_leisure_noprn$predicted == 0), ]
save(res_leisure_noprn,file = '3_output/res_leisure_noprn.RData')
rm(res_leisure_noprn)

res_education_noprn <- randomForest(x = train_rf[, c(activities_attr, 
                                                     cluster_attr,
                                                     landuse)],
                                    y = train_rf[, 'ateducation'],
                                    ntree = 100, mtry = 5, importance = TRUE,
                                    do.trace = 5, keep.forest = TRUE)
train_rf <- train_rf[which(res_education_noprn$predicted == 0), ]
save(res_education_noprn,file = '3_output/res_education_noprn.RData')
rm(res_education_noprn)

res_shopping_noprn <- randomForest(x = train_rf[, c(activities_attr, 
                                                    cluster_attr,
                                                    landuse)],
                                   y = train_rf[, 'atshopping'],
                                   ntree = 100, mtry = 5, importance = TRUE,
                                   do.trace = 5, keep.forest = TRUE)
train_rf <- train_rf[which(res_shopping_noprn$predicted == 0), ]
save(res_shopping_noprn,file = '3_output/res_shopping_noprn.RData')
rm(res_shopping_noprn)

res_other_noprn <- randomForest(x = train_rf[, c(activities_attr, 
                                                 cluster_attr,
                                                 landuse)],
                                y = train_rf[, 'atother'],
                                ntree = 100, mtry = 5, importance = TRUE,
                                do.trace = 5, keep.forest = TRUE)
train_rf <- train_rf[which(res_other_noprn$predicted == 0), ]
save(res_other_noprn,file = '3_output/res_other_noprn.RData')
rm(res_other_noprn)

res_business_travel_noprn <- randomForest(x = train_rf[, c(activities_attr, 
                                                           cluster_attr,
                                                           landuse)],
                                          y = train_rf[, 'atbusiness_travel'],
                                          ntree = 100, mtry = 5, importance = TRUE,
                                          do.trace = 5, keep.forest = TRUE)
train_rf <- train_rf[which(res_business_travel_noprn$predicted == 0), ]
save(res_business_travel_noprn,file = '3_output/res_business_travel_noprn.RData')
rm(res_business_travel_noprn)

res_giving_people_lifts_noprn <- randomForest(x = train_rf[, c(activities_attr, 
                                                               cluster_attr,
                                                               landuse)],
                                              y = train_rf[, 'atgiving_people_lifts'],
                                              ntree = 100, mtry = 5, importance = TRUE,
                                              do.trace = 5, keep.forest = TRUE)
train_rf <- train_rf[which(res_giving_people_lifts_noprn$predicted == 0), ]
save(res_giving_people_lifts_noprn,file = '3_output/res_giving_people_lifts_noprn.RData')
rm(res_giving_people_lifts_noprn)


load('3_output/res_home_noprn.RData')
load('3_output/res_work_noprn.RData')
load('3_output/res_leisure_noprn.RData')
load('3_output/res_education_noprn.RData')
load('3_output/res_shopping_noprn.RData')
load('3_output/res_other_noprn.RData')
load('3_output/res_business_travel_noprn.RData')
load('3_output/res_giving_people_lifts_noprn.RData')


#### k means clustering leisure activities ----

train_leisure <- train_rf[which(train_rf$purpose_new == 'leisure'), 
                          c(activities_attr, cluster_attr)]

train_leisure_std <- BBmisc::normalize(train_leisure, method = 'range', margin = 2)

train_leisure_cls <- kmeans(train_leisure_std[, - c(4, 5)], centers = 3, 
                            nstart = 3, algorithm = "Hartigan-Wong")
train_rf$purpose_cls <- as.character(train_rf$purpose_new)
train_rf$purpose_cls[which(train_rf$purpose_cls == 'leisure')] <- 
    sapply(train_leisure_cls$cluster, function(x) if(x == 1) 
    {return ('leisure1')
    } else if (x == 2){
        return ('leisure2')
    } else if (x == 3){
        return ('leisure3')
    })
train_rf$purpose_cls <- factor(train_rf$purpose_cls, levels = c(
    "home", "work", "leisure1", "leisure2", "leisure3", "shopping", "other", 
    "business_travel", "giving_people_lifts", "education"))


res_cls <- randomForest(x = train_rf[, c(activities_attr, 
                                         cluster_attr,
                                         landuse)], 
                        y = train_rf[, 'purpose_cls'],
                        ntree = 100, mtry = 4, importance = TRUE, 
                        do.trace = 5, keep.forest = TRUE)
save(train_leisure_cls, file = '3_output/rf_res/train_leisure_cls.RData')
save(res_cls, file = '3_output/rf_res/res_cls.RData')
rm(res_cls)
load('3_output/rf_res/res_cls.RData')


#### using cutoff to balance leisure activities classification ----

res_cutoff0_16 <- randomForest(x = train_rf[, c(activities_attr, 
                                                cluster_attr,
                                                landuse)], 
                               y = train_rf[, 'purpose_new'],
                               cutoff = c(0.12, 0.12, 0.16, 0.12, 0.12, 0.12, 0.12, 0.12), 
                               ntree = 100, mtry = 4, importance = TRUE, 
                               do.trace = 5, keep.forest = TRUE)
save(res_cutoff0_16,file = '3_output/rf_res/res_cutoff0_16.RData')
rm(res_cutoff0_16)
# load('3_output/rf_res/res_cutoff0_16.RData')

res_cutoff0_09 <- randomForest(x = train_rf[, c(activities_attr, 
                                                cluster_attr,
                                                landuse)], 
                               y = train_rf[, 'purpose_new'],
                               cutoff = c(0.13, 0.13, 0.09, 0.13, 0.13, 0.13, 0.13, 0.13), 
                               ntree = 100, mtry = 4, importance = TRUE, 
                               do.trace = 5, keep.forest = TRUE)
save(res_cutoff0_09,file = '3_output/rf_res/res_cutoff0_09.RData')
rm(res_cutoff0_09)
# load('3_output/rf_res/res_cutoff1_2.RData')


#### using strata ----

res_strata <- randomForest(x = train_rf[, c(activities_attr, 
                                            cluster_attr,
                                            landuse)], 
                           y = train_rf[, 'purpose_new'],
                           ntree = 100, mtry = 4, importance = TRUE, 
                           do.trace = 5, keep.forest = TRUE, 
                           strata = train_rf[, 'purpose_new'])
save(res_strata,file = '3_output/rf_res/res_strata.RData')
rm(res_strata)
load('3_output/rf_res/res_strata.RData')

#### using classwt ----

res_classwt1_25 <- randomForest(x = train_rf[, c(activities_attr, 
                                                 cluster_attr,
                                                 landuse)], 
                                y = train_rf[, 'purpose_new'],
                                ntree = 100, mtry = 4, importance = TRUE, 
                                do.trace = 5, keep.forest = TRUE, 
                                classwt = c(1, 1, 1.25, 1, 1, 1, 1, 1))
save(res_classwt1_25,file = '3_output/rf_res/res_classwt1_25.RData')
rm(res_classwt1_25)
# load('3_output/rf_res/res_classwt1_25.RData')


res_classwt0_75 <- randomForest(x = train_rf[, c(activities_attr, 
                                                 cluster_attr,
                                                 landuse)], 
                                y = train_rf[, 'purpose_new'],
                                ntree = 100, mtry = 4, importance = TRUE, 
                                do.trace = 5, keep.forest = TRUE, 
                                classwt = c(1, 1, 0.75, 1, 1, 1, 1, 1))
save(res_classwt0_75,file = '3_output/rf_res/res_classwt0_75.RData')
rm(res_classwt0_75)
# load('3_output/rf_res/res_classwt0_75.RData')




for(i in 1:9){
    print(paste(i,'------------------------------------------'))
    print(res_9srf[[i]]$res$balanced_res$confusion)
    print(res_9srf_balanced[[i]]$res$balanced_res$confusion)
    print(res_9srf_2balanced[[i]]$res$balanced_res$confusion)
}



if(i == 1){
    adasyn_res <- UBL::AdasynClassif(
        form = formular, 
        dat = data[1:10000, ],
        baseClass = baseClass, beta = beta, k = k, dist = dist)
} else{
    adasyn_res <- rbind(
        adasyn_res, UBL::AdasynClassif(
            form = formular, 
            dat = data[((i-1)*10000 + 1) : 
                           min(i*10000, dim(data)[1]), ],
            baseClass = baseClass, beta = beta,
            k = k, dist = dist))
}




# train_data <- train_9srf_adasyn$s1[, c('athome', activities_attr, cluster_attr, landuse)]
# adasyn_data <- adasyn(athome ~ .,  train_data)
# try_res <- randomForest(athome ~ ., data = adasyn_data,
#                         ntree = 100, mtry = 4, importance = TRUE, 
#                         do.trace = 5, keep.forest = TRUE)
# test_res <- predict(try_res, train_data)


smoted_minor <- SMOTE(athome ~ .,  train_data, k = 5, perc.under = 0, 
                      perc.over = (max(table(train_data$athome))  /  
                                       min(table(train_data$athome)) - 1) * 100)
comb_data <- rbind(train_data[which(train_data$athome = 0), ], smoted_minor)
try_res <- randomForest(athome ~ ., data = comb_data,
                        ntree = 100, mtry = 4, importance = TRUE, 
                        do.trace = 5, keep.forest = TRUE)
test_res <- predict(try_res, train_data)

#### Handling missing value in participants 1. mode/mean ----

participants1 <- participants
participants1[, -1] <- na.roughfix(participants1[, -1])
# apply(participants1, 2, function(x) length(which(is.na(x))))


#### Handling missing value in participants 2. mode/mean + kNN----

participants2 <- participants
participants2[, c(2, 4, 8:14)] <- na.roughfix(participants2[, c(2, 4, 8:14)])
# apply(participants2, 2, function(x) length(which(is.na(x))))

# \begin{enumerate}
# \item Given a training set $T = \{U, V\}$, where $U$ are predictors and $V$ are labels.
# \item Estimate the distance between a test object $w = \{u', v'\}$ and all training objects $\{u, v\} \in \{U, V\}$ to find its $k$ nearest neighbors.
# \item Determine the label $v'$ for this test object $w$ as median of $v$ of its $k$ nearest neighbors in case of numerical variables and mode in case of categorical variables
# \end{enumerate}


# table(participants$household_size, useNA = 'ifany')
# replace 1 NA with mode
participants$household_size[which(is.na(participants$household_size))] <- 
    DescTools::Mode(participants$household_size[
        which(!is.na(participants$household_size))])[1]

# table(participants$education, useNA = 'ifany')
# replace 5 NA with mode
participants$education[which(is.na(participants$education))] <- 
    DescTools::Mode(participants$education[
        which(!is.na(participants$education))])[1]

# table(participants$employment_1, useNA = 'ifany')
# replace 5 NA with mode
participants$employment_1[which(is.na(participants$employment_1))] <- 
    DescTools::Mode(participants$employment_1[
        which(!is.na(participants$employment_1))])[1]

# not consider table(participants$work_type, useNA = 'ifany')
# 762 NA

# table(participants$sex, useNA = 'ifany') # , no NA

# table(participants$income, useNA = 'ifany')
# 361 NA

# table(participants$age, useNA = 'ifany')
# 361 NA


data_rf$weekday <- as.factor(data_rf$weekday)
data_rf$purpose_new <- as.factor(data_rf$purpose_new)

data_rf$income <- as.factor(data_rf$income)
data_rf$education <- as.factor(data_rf$education)
data_rf$sex <- as.factor(data_rf$sex)
data_rf$work_type <- as.factor(data_rf$work_type)
data_rf$employment_1 <- as.factor(data_rf$employment_1)
data_rf$construction_zone <- as.factor(data_rf$construction_zone)




#### Two ways to handle missing data - 2. proximity prediction ----

train_rf_2px <- train_rf

# apply(train_rf_2px, 2, function(x) length(which(is.na(x))))

train_rf_2px[, c('household_size', 'income', 'education', 'age', 
                 'work_type', 'employment_1', 'construction_zone')] <- 
    rfImpute(train_rf_2px[, c('household_size', 'income', 'education',
                              'age','work_type', 'employment_1',
                              'construction_zone')], 
             train_rf_2px$purpose_new, keep.forest = FALSE
             , ntree = 200)[, -1]


res_rf2 <- randomForest(x = train_rf_2px[, c(participants_attr[-1],
                                             activities_attr, cluster_attr,
                                             "construction_zone")], 
                        y = train_rf_2px[, 'purpose_new'],
                        ntree = 200, mtry = 2, importance = TRUE, 
                        do.trace = 1, keep.forest = TRUE)



ggplot(cluster_data_cl, aes(x=lon, y = lat, color = factor(cluster))) + geom_point()

max(distm(cluster_data_cl[which(cluster_data_cl$cluster == 1), c('lon', 'lat')]) )


# data_rf[, participants_attr] <- as.data.frame(t(sapply(
#     activities$user_id, function(x) participants[which(
#         participants$participant_ID == x), participants_attr]) ) )


# five people give less information in the survey, but generate 1697 activities in total
# participants[which(is.na(participants$employment_1)), 
#              'participant_ID']
# activities[, 'activity_id']
# sum(activities$user_id %in% participants[
#     which(is.na(participants$employment_1)), 'participant_ID'][1] )


# separate data to training set and test set
activities_sf_training <- activities_sf[
    which(activities_sf$purpose_new != 'unknown'), ]
activities_sf_test <-  activities_sf[
    which(activities_sf$purpose_new == 'unknown'), ]



# Check file information between original folder and local folder
# file.info(list.files("/data/mobis/data/csv", full.names = T))$size
# file.info(list.files('~/Transport-Project-SS2020-ETH/1-Original-Data/project_data', full.names = T))$size


# Function to plot shapefile (depreciated) ------------------------------


library(ggplot2)
library(sp)

shapefile <- function(shapefile, lon_spacing = 1, lat_spacing = 1){
    
    # shapefile : .shp file, read by rgdal::readOGR.
    # lon_/lat_spacing : spacing of longitude_latitude breaks & labels
    
    lon_lat_attribute <- get_lon_lat_attribute(shapefile, 
                                               lon_spacing, lat_spacing)
    
    shapefile_ggplot <- {
        ggplot() + 
            geom_polygon(aes(long, lat, group = group), fill = NA,
                         colour = 'black', size = 0.1, data = shapefile) +
            scale_x_continuous(limits = c(lon_lat_attribute$lon_breaks[1], 
                                          tail(lon_lat_attribute$lon_breaks
                                               , 1) + 0.001),
                               expand = c(0, 0), 
                               breaks = lon_lat_attribute$lon_breaks, 
                               labels = lon_lat_attribute$lon_labels) +
            scale_y_continuous(limits = c(lon_lat_attribute$lat_breaks[1], 
                                          tail(lon_lat_attribute$lat_breaks
                                               ,1)), 
                               expand = c(0, 0), 
                               breaks = lon_lat_attribute$lat_breaks,
                               labels = lon_lat_attribute$lat_labels) +
            theme(panel.border = element_rect(colour = "black", 
                                              fill=NA, size=0.5), 
                  panel.background = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_line(size = 0.1),
                  panel.spacing.x = unit(0, "cm"),
                  plot.margin = unit(c(1, 1, 1, 1), "lines"))
    }
    
    return(shapefile_ggplot)
}

# swiss <- rgdal::readOGR('1-Original-Data/GIS_data/swiss/CHE_Kantone.shp')
# shapefile(swiss, 0.5, 0.5)

get_lon_lat_attribute <- function(shapefile, lon_spacing, lat_spacing){

    # shapefile : .shp file used to derive longitude_latitude breaks & labels, read by rgdal::readOGR.
    # lon_/lat_spacing : spacing of longitude_latitude breaks & labels
    
    # Get spatial range and round them according to lon_spacing and lat_spacing
    lon_lat_range <- sp::bbox(shapefile)
    lon_lat_range_rounded <- c(lon_lat_range[1, 1] %/% lon_spacing *
                                   lon_spacing, 
                              ceiling(lon_lat_range[1, 2] / lon_spacing - 
                                          0.001) * lon_spacing, 
                              lon_lat_range[2, 1] %/% lat_spacing * 
                                  lat_spacing, 
                              ceiling(lon_lat_range[2, 2] / lat_spacing) * 
                                  lat_spacing)
    
    # As maximum lat in world shapefile is only 83.6, we increase it to 90
    if(lon_lat_range_rounded[4] == 85){
        lon_lat_range_rounded[4] <- 90
    }
    
    # Create longitude_latitude breaks used for x/y axis ticks
    lon_breaks <- seq(lon_lat_range_rounded[1], 
                     lon_lat_range_rounded[2], lon_spacing)
    lat_breaks <- seq(lon_lat_range_rounded[3], 
                     lon_lat_range_rounded[4], lat_spacing)
    
    # Associate the breaks with '°W', '°E', '°N', '°S' or 0
    lon_labels <- sapply (lon_breaks, function(x) if(x == 0){
        x
    }else if (x < 0){
        paste(as.character(x), '°W', sep = '')
    }else {
        paste(as.character(x), '°E', sep = '')
    })
    
    lat_labels <- sapply (lat_breaks, function(x) if(x == 0){
        x
    }else if (x < 0){
        paste(as.character(x), '°S', sep = '')
    }else {
        paste(as.character(x), '°N', sep = '')
    })
    
    return ( list(lon_breaks = lon_breaks, lat_breaks = lat_breaks, 
                  lon_labels = lon_labels, lat_labels = lat_labels) )
}

# get_lon_lat_attribute(swiss_kanton, 0.5, 0.5)




