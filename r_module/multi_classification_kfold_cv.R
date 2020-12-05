

# multi-algorithms for classification using k-fold cross validation -------


suppressPackageStartupMessages(library('data.table'))
suppressPackageStartupMessages(library('caret'))
suppressPackageStartupMessages(library('randomForest'))
library('C50')
library('e1071')
suppressPackageStartupMessages(library('earth'))
suppressPackageStartupMessages(library('DescTools'))


# use function to obtain the modes of predicted class
calculate_mode <- function(x) {
    uniqx <- unique(na.omit(x))
    count <- uniqx[which.max(tabulate(match(x, uniqx)))]
    return(count)
}


multi_clf_kfold <- function(
    x, y, ID, group = NULL, 
    k = 10, 
    x_predictors = NULL, y_responses = NULL, ID_predict = NULL,
    multi_algorithms = list(
        rf = TRUE, c50 = TRUE, nb = TRUE, mars = TRUE),
    par_rf = list(
        ntree = 100, importance = FALSE, do.trace = 1), 
    par_c50 = list(trials = 10), 
    par_nb = list(laplace = 1)
    ){
    # Input ----
    # 
    # x: a data frame of predictors, including numerical or factor variables
    # 
    # y: a response vector
    # 
    # ID: ID of each instances, 
    # used for output results and generate k fold indices
    # 
    # group: if not NULL, it will be used to generate k fold indices
    # e.g. user ID
    # 
    # k: number of folders used for cross validation, 
    # if k = 1, no cross validation, x_predictors must be specified
    # 
    # x_predictors: if k = 1, x_predictors should be specified as predictors
    # 
    # y_responses: if k = 1, and x_predictors is specified, 
    # y_responses can be specified.
    # 
    # ID_predict: ID of each instances for data to be predicted
    # 
    # multi_algorithms: specify if each algorithm should be conducted.
    # rf, Random forest; 
    # c50, C5.0;
    # nb, Naive Bayesian; 
    # mars, Multivariate adaptive regression splines
    # 
    # par_rf: parameters used for randomForest::randomForest
    # par_c50: parameters used for C50::C5.0
    # par_nb: parameters used for e1071::naiveBayes
    # 
    # Output ----
    # clf_res: 
    # kfold_indices: indices for split train test set
    # prediction: prediction results from trained model
    # accuracy: prediction accuracy for each algorithms
    # rf, trained model using Random forest; 
    # c50, trained model using C5.0;
    # nb, trained model using Naive Bayesian; 
    # mars, trained model using Multivariate adaptive regression splines
    # 
    # 
    # Function ----
    
    
    # Adjust data types of inputs
    x <- as.data.frame(x)
    if(k == 1){
        x_predictors <- as.data.frame(x_predictors)
    }
    # change the levels of y according to their frequency
    y <- factor(y, levels = names(sort(table(y), decreasing = TRUE)))
    if(!is.null(y_responses)){
        y_responses <- factor(
            y_responses, levels = names(sort(table(y), decreasing = TRUE)))
    }
    
    # list to store results
    clf_res <- list(
        kfold_indices = NULL, 
        prediction = NULL, 
        accuracy = NULL,
        rf = list(0), 
        c50 = list(0),
        nb = list(0), 
        mars = list(0)
    )
    
    # train test data split, based on 'group' or 'ID'
    if(!is.null(group)){
        clf_res$kfold_indices <- caret::groupKFold(group, k = k)
    } else if(k != 1){
        clf_res$kfold_indices <- caret::groupKFold(ID, k = k)
    }
    
    for(i in 1 : k){
        # get train test data for each loop
        if(k != 1){
            # with cross validation
            xtrain <- x[clf_res$kfold_indices[[i]], ]
            ytrain <- y[clf_res$kfold_indices[[i]]]
            xtest <- x[-clf_res$kfold_indices[[i]], ]
            ytest <- y[-clf_res$kfold_indices[[i]]]
        } else{
            # without cross validation
            xtrain <- x
            ytrain <- y
            xtest <- x_predictors
            
            if(!is.null(y_responses)){
                ytest <- y_responses
            }
        }
        
        if(k != 1){
            clf_prediction <- data.frame(
                ID = ID[- clf_res$kfold_indices[[i]]],
                labels = ytest, 
                all_predicted = 0
            )
        }else if(!is.null(y_responses)){
            clf_prediction <- data.frame(
                ID = ID_predict, 
                labels = ytest, 
                all_predicted = 0
            )
        }else {
            clf_prediction <- data.frame(
                ID = ID_predict
            )
        }
        
        # Classification results using different algorithm ----
        
        # Random forest
        if(multi_algorithms$rf == TRUE){
            
            # train the model
            clf_res$rf[[i]] <- randomForest::randomForest(
                x = xtrain,
                y = ytrain,
                ntree = par_rf$ntree, 
                mtry = ceiling(log2(dim(xtrain)[2]) + 1), 
                importance = par_rf$importance, 
                do.trace = par_rf$do.trace, 
                keep.forest = TRUE
            )
            
            # predict
            clf_prediction$rf_prediction <- predict(
                clf_res$rf[[i]], 
                xtest
            )
            
            # count accurately predicted
            if(k != 1 | !is.null(y_responses)){
                clf_prediction$all_predicted <- clf_prediction$all_predicted + 
                    ifelse(
                        clf_prediction$labels == clf_prediction$rf_prediction, 
                        1, 0
                    )
            }

            print(paste('Random forest', 
                        i, '/', k, ' ', Sys.time(), sep = ''))
        }
        
        # C5.0
        if(multi_algorithms$c50 == TRUE){
            
            # train the model
            clf_res$c50[[i]] <- C50::C5.0(
                x = xtrain,
                y = ytrain, 
                trials = par_c50$trials
            )

            # predict
            clf_prediction$c50_prediction <- predict(
                clf_res$c50[[i]], 
                xtest
            )
            
            # count accurately predicted
            if(k != 1 | !is.null(y_responses)){
                clf_prediction$all_predicted <- clf_prediction$all_predicted + 
                    ifelse(
                        clf_prediction$labels == clf_prediction$c50_prediction,
                        1, 0
                    )
            }
            
            print(paste('C5.0', 
                        i, '/', k, ' ', Sys.time(), sep = ''))
        }
        
        # Naive Bayesian
        if(multi_algorithms$nb == TRUE){
            
            # train the model
            clf_res$nb[[i]] <- e1071::naiveBayes(
                x = xtrain, 
                y = ytrain,
                laplace = par_nb$laplace
            )
            
            # predict
            clf_prediction$nb_prediction <- predict(
                clf_res$nb[[i]], 
                xtest
            )
            
            # count accurately predicted
            if(k != 1 | !is.null(y_responses)){
                clf_prediction$all_predicted <- clf_prediction$all_predicted + 
                    ifelse(
                        clf_prediction$labels == clf_prediction$nb_prediction,
                        1, 0
                    )
            }
            
            print(paste('Naive Bayesian', 
                        i, '/', k, ' ', Sys.time(), sep = ''))
        }
        
        # Multivariate adaptive regression splines
        if(multi_algorithms$mars == TRUE){
            
            # train the model
            clf_res$mars[[i]] <- earth::earth(
                x = xtrain,
                y = ytrain
            )
            
            # predict
            clf_prediction$mars_prediction <- factor(as.vector(predict(
                clf_res$mars[[i]], 
                xtest, 
                type = 'class')), 
                levels = names(sort(table(y), decreasing = TRUE))
            )
            
            # count accurately predicted
            if(k != 1 | !is.null(y_responses)){
                clf_prediction$all_predicted <- clf_prediction$all_predicted + 
                    ifelse(
                        clf_prediction$labels == clf_prediction$mars_prediction,
                        1, 0
                    )
            }
            
            print(paste('Multivariate adaptive regression splines',
                        i, '/', k, ' ', Sys.time(), sep = ''))
        }
        
        clf_res$prediction <- rbind(
            clf_res$prediction, 
            clf_prediction
        )
    }
    
    # get votes for final classification
    if(k != 1 | !is.null(y_responses)){
        clf_res$prediction$modes <- factor(apply(
            as.matrix(clf_res$prediction[, -c(1:3)]), 
            1, calculate_mode), 
            levels = names(sort(table(y), decreasing = TRUE)))
    }else{
        clf_res$prediction$modes <- factor(apply(
            as.matrix(clf_res$prediction[, -1]),
            1, calculate_mode), 
            levels = names(sort(table(y), decreasing = TRUE)))
    }
    
    # Calculate the overall accuracy
    if(k != 1 | !is.null(y_responses)){
        clf_res$accuracy <- data.frame(
            rf = ifelse(
                multi_algorithms$rf == TRUE, 
                (caret::confusionMatrix(table(
                    clf_res$prediction$labels,
                    clf_res$prediction$rf_prediction)))$overall[1], 
                NA
            ),
            c50 = ifelse(
                multi_algorithms$c50 == TRUE, 
                (caret::confusionMatrix(table(
                    clf_res$prediction$labels,
                    clf_res$prediction$c50_prediction)))$overall[1], 
                NA
            ), 
            nb = ifelse(
                multi_algorithms$nb == TRUE, 
                (caret::confusionMatrix(table(
                    clf_res$prediction$labels,
                    clf_res$prediction$nb_prediction)))$overall[1], 
                NA
            ), 
            mars = ifelse(
                multi_algorithms$mars == TRUE, 
                (caret::confusionMatrix(table(
                    clf_res$prediction$labels,
                    clf_res$prediction$mars_prediction)))$overall[1], 
                NA
            ), 
            modes = (caret::confusionMatrix(table(
                clf_res$prediction$labels,
                clf_res$prediction$modes)))$overall[1]
        )
    }

    return(clf_res)
}



# source('2_code/r_module/namelist.R', chdir = TRUE)
# load('3_output/labeled_activity.RData')
# labeled_activity <- labeled_activity[order(user_id)]

# x = labeled_activity[(1:10000) * 2 - 1, .SD, .SDcol = tp_sf_all]
# y = labeled_activity$labels[(1:10000) * 2 - 1]
# ID = labeled_activity$ID[(1:10000) * 2 - 1]
# k = 1
# group = NULL
# x_predictors = labeled_activity[(1:10000) * 2, .SD, .SDcol = tp_sf_all]
# y_responses = labeled_activity$labels[(1:10000) * 2]
# ID_predict = labeled_activity$ID[(1:10000) * 2]
# multi_algorithms = list(
#     rf = TRUE, c50 = TRUE, nb = TRUE, mars = TRUE)
# par_rf = list(
#     ntree = 100, importance = FALSE, do.trace = 1, keep.forest = TRUE)
# par_c50 = list(trials = 10)
# par_nb = list(laplace = 1)

# system.time(
#     clf_activity1 <- multi_clf_kfold(
#         x = labeled_activity[(1:5000) * 2 - 1, .SD, .SDcol = tp_sf_all],
#         y = labeled_activity$labels[(1:5000) * 2 - 1],
#         ID = labeled_activity$ID[(1:5000) * 2 - 1],
#         k = 5
#     )
# )
# 
# system.time(
#     clf_activity3 <- multi_clf_kfold(
#         x = labeled_activity[(1:10000) * 2 - 1, .SD, .SDcol = tp_sf_all],
#         y = labeled_activity$labels[(1:10000) * 2 - 1],
#         ID = labeled_activity$ID[(1:10000) * 2 - 1],
#         k = 1,
#         x_predictors = labeled_activity[(1:10000) * 2, .SD, .SDcol = tp_sf_all],
#         ID_predict = labeled_activity$ID[(1:10000) * 2]
#     )
# )
# y_responses = labeled_activity$labels[(1:10000) * 2]
# caret::confusionMatrix(y_responses, clf_activity3$prediction$rf_prediction)





# Investigate new algorithms

# suppressPackageStartupMessages(library('data.table'))
# source('2_code/r_module/namelist.R', chdir = TRUE)
# 
# load('3_output/labeled_activity.RData')
# labeled_activity <- labeled_activity[order(user_id)]
# labeled_activity <- labeled_activity[1:10000, ]
# 
# x <- labeled_activity[, .SD, .SDcol = tp_sf_all]
# x <- as.data.frame(x)
# y <- labeled_activity$labels
# y <- factor(y, levels = names(sort(table(y), decreasing = TRUE)))
# ID <- labeled_activity$ID
# k <- 10
# group = NULL
# y_responses = NULL
# multi_algorithms = list(bayesian = TRUE)
# par_nb = list(laplace = 1)
# 
# # list to store results
# clf_res <- list(
#     kfold_indices = NULL, 
#     prediction = NULL, 
#     accuracy = NULL,
#     bayesian = list(0)
# )
# 
# # train test data split, based on 'group' or 'ID'
# if(!is.null(group)){
#     clf_res$kfold_indices <- caret::groupKFold(group, k = k)
# } else if(k != 1){
#     clf_res$kfold_indices <- caret::groupKFold(ID, k = k)
# }
# 
# for(i in 1 : k){
#     # get train test data for each loop
#     if(k != 1){
#         # with cross validation
#         xtrain <- x[clf_res$kfold_indices[[i]], ]
#         ytrain <- y[clf_res$kfold_indices[[i]]]
#         xtest <- x[-clf_res$kfold_indices[[i]], ]
#         ytest <- y[-clf_res$kfold_indices[[i]]]
#     } else{
#         # without cross validation
#         xtrain <- x
#         ytrain <- y
#         xtest <- x_predictors
#         
#         if(!is.null(y_responses)){
#             ytest <- y_responses
#         }
#     }
#     
#     if(k != 1){
#         clf_prediction <- data.frame(
#             ID = ID[- clf_res$kfold_indices[[i]]],
#             labels = ytest, 
#             all_predicted = 0
#         )
#     }else if(!is.null(y_responses)){
#         clf_prediction <- data.frame(
#             ID = ID_predict, 
#             labels = ytest, 
#             all_predicted = 0
#         )
#     }else {
#         clf_prediction <- data.frame(
#             ID = ID_predict
#         )
#     }
#     
#     # Classification results using different algorithm ----
#     
#     if(multi_algorithms$bayesian == TRUE){
#         
#         clf_res$bayesian[[i]] <- e1071::naiveBayes(
#             x = xtrain, 
#             y = ytrain,
#             laplace = par_nb$laplace
#         )
#         
#         # predict
#         clf_prediction$bayesian_prediction <- predict(
#             clf_res$bayesian[[i]], 
#             xtest
#         )
#         
#         # count accurately predicted
#         if(k != 1 | !is.null(y_responses)){
#             clf_prediction$all_predicted <- clf_prediction$all_predicted + 
#                 ifelse(
#                     clf_prediction$labels == clf_prediction$bayesian_prediction,
#                     1, 0
#                 )
#         }
#         
#         print(paste('Naive Bayesian', 
#                     i, '/', k, ' ', Sys.time(), sep = ''))
#     }
#     
# 
#     clf_res$prediction <- rbind(
#         clf_res$prediction, 
#         clf_prediction
#     )
# }
# 
# 
# # Calculate the overall accuracy
# if(k != 1 | !is.null(y_responses)){
#     clf_res$accuracy <- data.frame(
#         nb = ifelse(
#             multi_algorithms$bayesian == TRUE, 
#             (caret::confusionMatrix(table(
#                 clf_res$prediction$labels,
#                 clf_res$prediction$bayesian_prediction)))$overall[1], 
#             NA
#         )
#     )
# }


