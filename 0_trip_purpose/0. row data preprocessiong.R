# Raw data cleaning -------------------------------------------------------


source('~/mobis_code/source file and import packages.R')

load("/nas/qigao/scratch/run_gao/all_activities.RData")
tp_inswiss <- all_activities[
    in_switzerland == TRUE, 
    -c('all_predicted', 'rf_prediction', 'c50_prediction', 
       'mars_prediction', 'modes', 'imputed_purpose')]
tp_inswiss_label <- droplevels(tp_inswiss[labels != 'Unknown', ])
tp_inswiss_label$labels <- factor(tp_inswiss_label$labels, 
                                  levels = activity_categories)

participants <- readr::read_delim(
    '/data/mobis/data/csv/participants.csv', delim = ';',
    guess_max = 100000, n_max = 100000)
# remove duplicate participants info
participants <- participants %>% dplyr::group_by(participant_ID) %>%
    filter(row_number()==1)
# remove participants without record: #95502 to #3689
participants <- participants[
    participants$participant_ID %in% tp_inswiss$user_id, ]
# Re-code surveyed results
participants$income[which(participants$income == '99')] <- NA
participants$own_car[which(participants$own_car == 9)] <- 0
participants$own_motorbike[which(participants$own_motorbike == 9)] <- 0
participants$own_bike_ebike[which(participants$own_bike_ebike == 9)] <- 0
participants <- as.data.table(participants)

save(participants, file = '/data/students/qigao/scratch/participants.RData')
save(tp_inswiss, file = '/data/students/qigao/scratch/tp_inswiss.RData')
save(tp_inswiss_label, file = '/data/students/qigao/scratch/tp_inswiss_label.RData')


# Original Data Statistics ------------------------------------------------

load("/data/mobis/data/enrichments/scratch/all_activities.RData")
load('/data/students/qigao/scratch/tp_inswiss.RData')

# all activities, act in swiss, ratio
c(dim(all_activities)[1], dim(tp_inswiss)[1], 
  dim(tp_inswiss)[1] / dim(all_activities)[1])

# act in swiss, labeled, ratio
c(dim(tp_inswiss)[1], length(which(tp_inswiss$labels != 'Unknown')), 
  length(which(tp_inswiss$labels != 'Unknown')) / dim(tp_inswiss)[1])

sort(table(tp_inswiss$labels, useNA = 'ifany'), decreasing = TRUE)


min(tp_inswiss$started_at)
max(tp_inswiss$started_at)
(dim(tp_inswiss)[1] - 1041409)/dim(tp_inswiss)[1]

# Daily distribution
ggplot(aes(started_at), data = tp_inswiss) + 
    geom_freqpoly(binwidth = 86400)

hist(tp_inswiss$duration[which(tp_inswiss$duration < 6*3600)], 
     breaks = seq(0, 21600, 600))
plot(table(tp_inswiss$starttime[tp_inswiss$weekday %in% c(1:5)])/5, type = 'l')
lines(table(tp_inswiss$starttime[tp_inswiss$weekday %in% c(6:7)])/2, 
     type = 'l', col = 'red')





# Feature importance using random forests -------------------------------

load('/data/students/qigao/scratch/tp_inswiss_label.RData')

tp_train <- randomForest::randomForest(
    x = tp_inswiss_label[, .SD, .SDcol = tp_sf_all],
    y = tp_inswiss_label$labels,
    ntree = 100, mtry = ceiling(log2(length(tp_sf_all))+1 ) ,
    importance = TRUE, do.trace = 1,
    keep.forest = FALSE
)

save(tp_train, file = '/data/students/qigao/scratch/tp_train.RData')


# Dependence on duration and Number of participants -----------------------

load("/data/mobis/data/enrichments/scratch/all_activities.RData")

# Filter unfully labeled user days

all_activities$dayth <- floor(as.numeric(difftime(
    all_activities$started_at_local, as.Date("2019-09-02", tz="UTC"), units = 'days'
)))

user_alllabelled_days <- all_activities[
    , .(unlabeled_activities = length(which(labels == 'Unknown'))), 
    by = .(user_id, dayth)
]

user_alllabelled_days <- user_alllabelled_days[
    which(user_alllabelled_days$unlabeled_activities == 0),
    ]

alllabelled_activities <- data.table::merge.data.table(
    all_activities, user_alllabelled_days, by = c("user_id", "dayth")
)

alllabelled_purpose <- data.table::merge.data.table(
    purpose_data, activities[, .SD, .SDcols = c('ID', 'yday')], by = 'ID'
)


load('3_output/28_Apr/purpose_data.RData')
load('3_output/28_Apr/activities.RData')



alllabelled_purpose <- alllabelled_purpose[
    alllabelled_purpose$ID %in% alllabelled_activities$ID, ]

save(alllabelled_purpose, file = '3_output/28_Apr/alllabelled_purpose.RData')



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


# tp section --------------------------------------------------------------

# Currently the model lacks power for across participants imputation
tp_inswiss_label <- tp_inswiss_label[order(user_id)]

clf_tp_label <- multi_clf_kfold(
    x = tp_inswiss_label[, .SD, .SDcol = tp_sf_all],
    y = tp_inswiss_label$labels,
    ID = tp_inswiss_label$ID,
    k = 10
)

save(clf_tp_label, file = '/data/students/qigao/scratch/clf_tp_label.RData')






all_activities <- data.table::merge.data.table(
    all_activities, 
    clf_tp_labeled$prediction[, c("ID", "all_predicted")], 
    by = 'ID',
    all.x = TRUE, 
    sort = FALSE
)

system.time(
    clf_tp_all <- multi_clf_kfold(
        x = all_activities[all_predicted > 0, .SD, .SDcol = tp_sf_all],
        y = droplevels(all_activities$labels[
            which(all_activities$all_predicted > 0)]),
        ID = all_activities[all_predicted > 0, .SD, .SDcol = 'ID'],
        k = 1,
        multi_algorithms = list(
            rf = TRUE, c50 = TRUE, nb = FALSE, mars = TRUE),
        # all_predicted == 0 | is.na(all_predicted)
        x_predictors = all_activities[, .SD, .SDcol = tp_sf_all],
        ID_predict = all_activities[, .SD, .SDcol = 'ID']
    )
)

all_activities <- data.table::merge.data.table(
    all_activities, 
    clf_tp_all$prediction, 
    by = 'ID',
    all.x = TRUE, 
    sort = FALSE
)


all_activities$imputed_purpose <- all_activities$rf_prediction
all_activities$imputed_purpose[which(all_activities$all_predicted > 0)] <- 
    all_activities$labels[which(all_activities$all_predicted > 0)]


imputed_activity <- all_activities[
    , .SD, .SDcol = c("ID", "labels", "imputed_purpose")]
save(imputed_activity, file = '/data/mobis/data/enrichments/scratch/imputed_activity.RData')

save(participants, file = '/data/mobis/data/enrichments/scratch/participants.RData')

if(i_saveact == 1){
    save(all_activities, file = '/data/mobis/data/enrichments/scratch/all_activities.RData')
}

if(i_savemodel == 1){
    save(clf_tp_all, file = '/data/mobis/data/enrichments/scratch/clf_tp_all.RData')
}

if(i_savemodel == 1){
    save(clf_tp_labeled, file = '/data/mobis/data/enrichments/scratch/clf_tp_labeled.RData')
}



# ssh qigao@pulusuk.ivt.ethz.ch


