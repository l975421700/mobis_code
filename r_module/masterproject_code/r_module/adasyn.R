

# Function to generate ADASYN balanced dataset ----------------------------


# This function handles unbalanced classification problems using the 
# ADASYN algorithm. For large data set, it runs per each subset.

suppressPackageStartupMessages(library('UBL'))

adasyn <- function(formular, data, N_per_run = 5000,
                   baseClass = NULL, beta = 1, k = 5, dist = "HEOM"){
    
    # Input description ----
    # data: the predicted value should have column name 'objective'
    # N_per_run: As the algorithm used in function UBL::AdasynClassif requires
    # too large memory storage, we run it for each small subset with N_per_run 
    # elements
    # 
    # Others see ?UBL::AdasynClassif

    # Value ----
    # res: a balanced dataset
    
    
    res <- NULL
    
    if(dim(data)[1] <= N_per_run){
        if(min(table(data$objective)) < k | 
           min(table(data$objective)) / max(table(data$objective)) > 0.9){
            res <- data
        } else {
            res <- UBL::AdasynClassif(
                form = formular, 
                dat = data, 
                k = k, baseClass = baseClass, beta = beta, dist = dist)
        }
    } else{
        for(i in 1:ceiling(dim(data)[1]/N_per_run)){
            if(min(table(data[((i-1)*N_per_run + 1) : 
                              min(i*N_per_run, dim(data)[1]), 
                              'objective'])) < k | 
               min(table(data[((i-1)*N_per_run + 1) :  
                              min(i*N_per_run, dim(data)[1]), 
                              'objective'])) / 
               max(table(data[((i-1)*N_per_run + 1) :  
                              min(i*N_per_run, dim(data)[1]), 
                              'objective'])) > 0.9){
                res <- rbind(res, data[((i-1)*N_per_run + 1) : 
                                           min(i*N_per_run, dim(data)[1]), ])
            } else {
                res <- rbind(
                    res, UBL::AdasynClassif(
                        form = formular, 
                        dat = data[((i-1)*N_per_run + 1) : 
                                       min(i*N_per_run, dim(data)[1]), ],
                        k = k, baseClass = baseClass, beta = beta, dist = dist))
            }
        }
    }
    return(res)
}


# Example
# train_data <- train_rf[, c('labels', activities_attr, cluster_attr, landuse)]
# colnames(train_data)[1] <- 'objective'
# adasyn_data <- adasyn(objective ~ .,  train_data[1:100, ])
# system.time(adasyn(objective ~ .,  train_data[1:15000, ]))
# smoted_data <- SmoteClassif(objective ~ .,  train_data[1:10000, ], dist = 'HEOM')



