

# three functions to do stepwise classification ---------------------------
# stepwise_clf: model traning
# stepwise_test: model test
# stepwise_model: combine of above two


suppressPackageStartupMessages(library('randomForest'))
suppressPackageStartupMessages(library('DescTools'))
suppressPackageStartupMessages(library('caret'))


stepwise_model <- function(
    traindata, testdata = NULL, categories, selected_features, 
    factor_features = c("weekday", "inside_day", "construction_zone"), 
    stepwise = TRUE, 
    balance_methods = 'adasyn', cls_methods = 'randomForest',
    par_adasyn = list(N_per_run = 5000, baseClass = NULL, beta = 1, k = 5, 
                      dist = "HEOM"),
    par_rf = list(ntree = 100, mtry = 4, importance = TRUE,do.trace = 5, 
                  keep.forest = TRUE)){
    # Input ----
    # 
    # see description in stepwise_clf and stepwise_test respectively
    # 
    # stepwise: If true by default, then stepwise classification will be 
    # conducted. If false, only one step will be excuted
    # 
    # Output ----
    # 
    # a list containing results from stepwise_clf and stepwise_test
    # 
    # 
    # Function ----
    
    if(is.null(categories)){
        categories <- levels(traindata$labels)
    }
    
    # remove features that has only one levels
    selected_features[[1]] = selected_features[[1]][
        selected_features[[1]] %in% names(which(
            apply(traindata, 2, function(x) length(unique(x))) < 2)) == FALSE]
    
    traindata <- as.data.frame(traindata)
    traindata$labels <- factor(traindata$labels, levels = categories)
    
    if(! is.null(testdata)){
        testdata <- as.data.frame(testdata)
        testdata$labels <- factor(testdata$labels, levels = categories)
    }

    classification_res <- list(train = 0)
    
    if(stepwise == TRUE){
        classification_res$train <- stepwise_clf(
            traindata, categories, selected_features, 
            balance_methods = balance_methods, cls_methods = cls_methods,
            par_adasyn = par_adasyn, par_rf = par_rf)
        if(! is.null(testdata)){
            classification_res$test <- stepwise_test(
                classification_res$train, testdata, categories, 
                selected_features, factor_features = factor_features)
        }
    } else if(cls_methods == 'randomForest'){
        if(is.null(testdata)){
            classification_res$train <- randomForest::randomForest(
                labels ~ ., 
                data = traindata[, c('labels', selected_features[[1]])],
                ntree = par_rf$ntree, mtry = par_rf$mtry, 
                importance = par_rf$importance, do.trace = par_rf$do.trace, 
                keep.forest = par_rf$keep.forest
            )
        } else{
            classification_res$train <- randomForest::randomForest(
                x = traindata[, selected_features[[1]]],
                y = traindata$labels,
                xtest = testdata[, selected_features[[1]]],
                ytest = testdata$labels, 
                ntree = par_rf$ntree, mtry = par_rf$mtry, 
                importance = par_rf$importance, do.trace = par_rf$do.trace, 
                keep.forest = par_rf$keep.forest
            )
        }
    }
    
    return(classification_res)
}


# traindata = purpose_labeled[1:10000, ]
# testdata = purpose_labeled[10001:20000, ]
# categories = activity_categories
# selected_features = tp_sf_all
# factor_features = c(participants_factorfeatures, activities_factorfeatures)
# stepwise = TRUE
# balance_methods = NULL
# cls_methods = 'randomForest'
# par_adasyn = list(N_per_run = 5000, baseClass = NULL, beta = 1, k = 5,
#                   dist = "HEOM")
# par_rf = list(ntree = 100, mtry = 6, importance = TRUE,
#               do.trace = 5, keep.forest = TRUE)


stepwise_clf <- function(traindata, categories, selected_features, 
                         balance_methods = 'adasyn', 
                         cls_methods = 'randomForest',
                         par_adasyn = list(N_per_run = 5000, baseClass = NULL, 
                                           beta = 1, k = 5, dist = "HEOM"),
                         par_rf = list(ntree = 100, mtry = 4, importance = TRUE, 
                                       do.trace = 5, keep.forest = TRUE)){
    # Input ----
    # 
    # traindata: data used to do classification. 
    # The labeled column should have colname 'labels'.
    # The instance id column must have colname 'ID'.
    # 
    # categories: categories in labels, its order will be the order to train 
    # the model.
    # 
    # selected_features: selected features used to be predictors, each list
    # elements accounts for one step
    # 
    # balance_methods: methods used to balance imbalanced data set. Can be 
    # 'adasyn' or NULL
    # 
    # cls_methods: methods used to do classification
    # 
    # par_adasyn: parameters used for function 'adasyn'
    # par_rf: parameters used for function 'random forest'
    # 
    # 
    # Output ----
    # 
    # sw_clf_res: a list containing length(categories) + 1 lists for each 
    # classification step. Each list containing following elements:
    # $original: the original data used for classification in current time step.
    # $reduced: columns selected from original necessary for prediction.
    # $balanced: imbalanced data preprocessing output.
    # $unclf: unclassified data until current time step.
    # $balanced_res: classification results from 'balanced' data
    # $reduced_clf: store predicted value for 'reduced' data
    # 
    # 
    # Function ----
    
    # Add one column for each category as binary classification
    for(i in 1:length(categories)){
        traindata[, categories[i]] <- 
            factor(ifelse(traindata$labels == categories[i], 1, 0))
    }
    
    # create a list to store classification results 
    sw_clf_res <- NULL
    for(i in 1 : (length(categories) + 1)){
        sw_clf_res[[paste('s', i, sep = '')]] <- list(
            original = 0, reduced = 0, balanced = 0, unclf = 0, 
            balanced_res = 0, reduced_clf = 0)
    }
    
    for(i in 1 : (length(categories) + 1)){
        # specify original data as the input traindata (i=1) or unclassified data 
        # from last step (i>1)
        if(i==1){
            sw_clf_res[[i]]$original = traindata
        } else{
            sw_clf_res[[i]]$original = sw_clf_res[[i-1]]$unclf
        }
        
        # Extract data used for prediction
        sw_clf_res[[i]]$reduced <- 
            sw_clf_res[[i]]$original[
                , c(ifelse(i < (length(categories) + 1), 
                           categories[i], 'labels'), 
                    selected_features[[i]])]
        colnames(sw_clf_res[[i]]$reduced)[1] <- 'objective'
        
        # imbalanced data pre processing
        if(is.null(balance_methods)){
            sw_clf_res[[i]]$balanced <- sw_clf_res[[i]]$reduced
        } else if(balance_methods == 'adasyn'){
            sw_clf_res[[i]]$balanced <- 
                adasyn(objective ~ .,  sw_clf_res[[i]]$reduced, 
                       N_per_run = par_adasyn$N_per_run, 
                       baseClass = par_adasyn$baseClass, beta = par_adasyn$beta, 
                       k = par_adasyn$k, dist = par_adasyn$dist)
        }
        
        # classification based on selected algorithm
        if(cls_methods == 'randomForest'){
            sw_clf_res[[i]]$balanced_res <- 
                randomForest::randomForest(
                    objective ~ ., data = sw_clf_res[[i]]$balanced, 
                    ntree = par_rf$ntree, mtry = par_rf$mtry, 
                    importance = par_rf$importance, do.trace = par_rf$do.trace, 
                    keep.forest = par_rf$keep.forest)
        }
        
        # predict the results using trained model
        sw_clf_res[[i]]$reduced_clf <- 
            predict(sw_clf_res[[i]]$balanced_res, sw_clf_res[[i]]$reduced)
        
        if(i < (length(categories) + 1)){
            # store the unclassified data
            sw_clf_res[[i]]$unclf <- 
                droplevels(sw_clf_res[[i]]$original[
                    which(sw_clf_res[[i]]$reduced_clf == 0), ])
        }
    }
    return(sw_clf_res)
}


stepwise_test <- function(
    object, testdata, categories, selected_features, 
    factor_features = c("weekday", "inside_day", "construction_zone")){
    # Input ----
    # 
    # object: results returned by function stepwise_clf
    # 
    # testdata: same structure as in the input 'traindata' of stepwise_clf.
    # 
    # categories, selected_features: same as in stepwise_clf
    # 
    # factor_features: used when the levels of factors in test set is larger 
    # than the train set, assign extra factors to the most frequent factors in 
    # train set
    # 
    # 
    # Output ----
    # 
    # sw_test_res: a list containing (length(categories) + 1) elements for 
    # each classification step. Each list containing following elements:
    # $original: the original data used for classification in current time step.
    # In the first list, 'original' also contains column $clf_res of predicted 
    # labels.
    # $reduced: columns selected from original necessary for prediction.
    # $unclf: unclassified data until current time step.
    # The 1st list for 1st step contains another 2 elements:
    # $confusion: confusion matrix of predicted labels
    # 
    # Function ----
    
    # create a list to store classification results 
    sw_test_res <- NULL
    for(i in 1 : (length(categories) + 1)){
        sw_test_res[[paste('s', i, sep = '')]] <- list(
            original = 0, reduced = 0, unclf = 0)
    }
    
    # Add two column to store classification results in each step
    # $reduced_clf: classification results returned by randomForest model
    # $step_res: predicted labels
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
        
        
        if(i != 9 & dim(sw_test_res[[i]]$original)[1] != 0 ){
            # Extract data used for prediction
            sw_test_res[[i]]$reduced <- 
                sw_test_res[[i]]$original[ ,selected_features[[i]]]
            
            # Align the factor level in test set identical to that of train set
            for(j in 1 : length(factor_features)){
                if(! is.null(sw_test_res[[i]]$reduced[, factor_features[j]])){
                    sw_test_res[[i]]$reduced[
                        which(sw_test_res[[i]]$reduced[, factor_features[j]] %in%
                                  unique(object[[i]]$balanced[
                                      , factor_features[j]]) == FALSE), 
                        factor_features[j]] <- 
                        DescTools::Mode(object[[i]]$balanced[
                            , factor_features[j]])[[1]]

                    sw_test_res[[i]]$reduced[, factor_features[j]] <- 
                        factor(sw_test_res[[i]]$reduced[, factor_features[j]], 
                               levels = levels(object[[i]]$balanced[
                                   , factor_features[j]]))
                }
            }
            
            # predict the results using trained model
            sw_test_res[[i]]$original$reduced_clf <- 
                predict(object[[i]]$balanced_res, sw_test_res[[i]]$reduced)
            
            if(i < (length(categories) + 1)){
                # store the unclassified data
                sw_test_res[[i]]$unclf <- 
                    droplevels(sw_test_res[[i]]$original[
                        which(sw_test_res[[i]]$original$reduced_clf == 0), ])
                
                # Add predicted labels to initial data in each step
                sw_test_res[[i]]$original[
                    which(sw_test_res[[i]]$original$reduced_clf == 1), 
                    'step_res'] <- categories[i]
                clf_res <- rbind(
                    clf_res, sw_test_res[[i]]$original[
                        which(sw_test_res[[i]]$original$reduced_clf == 1), 
                        c('ID', 'step_res')])
            } else{
                sw_test_res[[i]]$original[, 'step_res'] <- 
                    sw_test_res[[i]]$original$reduced_clf
                clf_res <- rbind(
                    clf_res, sw_test_res[[i]]$original[, c('ID', 'step_res')])
            }
        }
    }
    
    colnames(clf_res)[2] <- 'clf_res'
    
    sw_test_res[[1]]$original <- merge(sw_test_res[[1]]$original, 
                                           clf_res, by.x = 'ID', by.y = 'ID',
                                           sort = FALSE)
    
    sw_test_res[[1]]$original$clf_res <- factor(
        sw_test_res[[1]]$original$clf_res, levels = categories)
    
    sw_test_res[[1]]$confusion <- caret::confusionMatrix(
        table(sw_test_res[[1]]$original[ , c('labels', 'clf_res')]) )
    
    return(sw_test_res)
}



