
library('readr')
suppressPackageStartupMessages(library('dplyr'))
suppressPackageStartupMessages(library('stringi'))
suppressPackageStartupMessages(library('lubridate'))
suppressPackageStartupMessages(library('data.table'))
suppressPackageStartupMessages(library('VIM'))
suppressPackageStartupMessages(library('rgdal'))
suppressPackageStartupMessages(library('caret'))
library('sp') 
library('parallel')

source('/data/students/qigao/mobis_analysis/r/eth_purpose_imputation/r_module/namelist.R')
load("/data/mobis/data/enrichments/all_activities.RData")

labeled_activity <- droplevels(
    all_activities[all_activities$labels != 'Unknown', ])
labeled_activity$labels <- factor(labeled_activity$labels, 
                                  levels = activity_categories)
labeled_activity <- labeled_activity[order(user_id)]

xtrain <- labeled_activity[(1 : 10000) * 2, .SD, .SDcol = c( tp_sf_all )]
ytrain <- labeled_activity$labels[(1 : 10000) * 2]
xtest <- labeled_activity[(1 : 10000) * 2 - 1, .SD, .SDcol = c( tp_sf_all )]
ytest <- labeled_activity$labels[(1 : 10000) * 2 - 1]



# mars --------------------------------------------------------------------


suppressPackageStartupMessages(library('earth')) # 0.758 # 0.7531
system.time(
    mars_res <- earth::earth(
        x = xtrain,
        y = ytrain
    )
)
mars_pred1 <- factor(as.vector(predict(mars_res, xtrain, type = 'class')), 
                     levels = activity_categories)
mars_pred2 <- factor(as.vector(predict(mars_res, xtest, type = 'class')), 
                     levels = activity_categories)
caret::confusionMatrix(table(ytrain, mars_pred1))
caret::confusionMatrix(table(ytest, mars_pred2))



# rf ----------------------------------------------------------------------


suppressPackageStartupMessages(library('randomForest')) # 0.9867 # 0.8858
system.time(
    rf_res <- randomForest::randomForest(
        x = xtrain, 
        y = ytrain
    )
)
rf_pred1 <- predict(rf_res, xtrain)
rf_pred2 <- predict(rf_res, xtest)
caret::confusionMatrix(table(ytrain, rf_pred1))
caret::confusionMatrix(table(ytest, rf_pred2))



# c50 ---------------------------------------------------------------------


library('C50') # 0.9837 # 0.8789
system.time(
    c50_res <- C50::C5.0(
        x = xtrain, 
        y = ytrain, 
        trials = 10
    )
)
c50_pred1 <- predict(c50_res, xtrain)
c50_pred2 <- predict(c50_res, xtest)
caret::confusionMatrix(table(ytrain, c50_pred1))
caret::confusionMatrix(table(ytest, c50_pred2))



# nb ----------------------------------------------------------------------


suppressPackageStartupMessages(library('e1071')) # 0.5872 # 0.5812
system.time(
    nb_res <- e1071::naiveBayes(
        x = xtrain, 
        y = ytrain, 
        laplace = 1
    )
)
nb_pred1 <- predict(nb_res, xtrain)
nb_pred2 <- predict(nb_res, xtest)
caret::confusionMatrix(table(ytrain, nb_pred1))
caret::confusionMatrix(table(ytest, nb_pred2))



# bayespolr ---------------------------------------------------------------

library(arm) # # 

arm_res <- arm::bayespolr(
    labels ~ .,
    data = labeled_activity[1:6000, .SD, .SDcol = c('labels', tp_sf_all )]
)
# train
arm_pred1 <- predict(
    arm_res,
    labeled_activity[1:6000, .SD, .SDcol = c(tp_sf_all )])
caret::confusionMatrix(table(arm_pred1,
                             labeled_activity$labels[1:6000])
)
# test
arm_pred2 <- predict(
    arm_res,
    labeled_activity[6001:7000, .SD, .SDcol = c(tp_sf_all )])
caret::confusionMatrix(table(arm_pred2,
                             labeled_activity$labels[6001:7000])
)














