

# Before you run --------

# You can run incremental trip purpose imputation based on following steps:

# Step1: You should specify the path to the git repository mobis_analysis like:
# mobis_analysis_directory = '/data/students/qigao/mobis_analysis'

# Step2: It's suggested to specify a output path that you have write access 
# for imputed purpose like:
# imputed_activity_incremental_output_path = '/data/mobis/data/enrichments/imputed_activity_incremental.RData'

# Step3: You have two choice:
# 
# option1: run incremental imputation based on the newest run of full trip purpose imputation:
# imputed_activity_path = '/data/mobis/data/enrichments/imputed_activity.RData'
# It's not suggested:
# In case of no new data, it will stop with error msg 'No new activities found!'
# In case of little new data, the model traning accuracy might be not enough
# But you can try.
# 
# option2: based on the last run of Qinggang Gao:
# imputed_activity_path = '/data/mobis/data/enrichments/run_gao/imputed_activity.RData'
# It's suggested:
# The main advantage is there is always enough new data to train the model.

# Step4: Now you can run
# source(paste(
#     mobis_analysis_directory,
#     '/r/eth_purpose_imputation/r_module/tp_incremental_impute_function.R',
#     sep=''))
# tp_increment_imputation(
#     mobis_analysis_directory = mobis_analysis_directory,
#     imputed_activity_incremental_output_path =
#         imputed_activity_incremental_output_path,
#     imputed_activity_path = imputed_activity_path
# )


# Functions to do incremental trip purpose imputation --------

tp_increment_imputation <- function(
    mobis_analysis_directory,
    imputed_activity_incremental_output_path = 
        '/data/mobis/data/enrichments/run_gao/imputed_activity_incremental.RData',
    imputed_activity_path = 
        '/data/mobis/data/enrichments/run_gao/imputed_activity.RData',
    activities_path = '/data/mobis/data/csv/activities.csv',
    covid_activities_glob = '/data/mobis/data/covid/*[0-9]',
    participants_path = 
        '/data/mobis/data/enrichments/run_gao/participants.RData',
    clf_tp_all_path = 
        '/data/mobis/data/enrichments/run_gao/clf_tp_all.RData',
    hc_all = 0,
    new_training = 1,
    par_rf = list(ntree = 100, importance = FALSE, do.trace = FALSE)
){
    begin_time = Sys.time()
    # Input packages, functions, and namelist for all subsequent sections ----
    
    source(paste(mobis_analysis_directory, 
                 '/r/eth_purpose_imputation/r_module/install_packages.R',
                 sep = ''), chdir = TRUE)
    source(paste(mobis_analysis_directory, 
                 '/r/eth_purpose_imputation/r_module/wkb2lonlat.R',
                 sep = ''), chdir = TRUE)
    source(paste(mobis_analysis_directory, 
                 '/r/eth_purpose_imputation/r_module/utc2local_time.R',
                 sep = ''), chdir = TRUE)
    source(paste(mobis_analysis_directory, 
                 '/r/eth_purpose_imputation/r_module/hierarchical_clustering.R',
                 sep = ''), chdir = TRUE)
    source(paste(mobis_analysis_directory, 
                 '/r/eth_purpose_imputation/r_module/namelist.R',
                 sep = ''), chdir = TRUE)
    source(paste(
        mobis_analysis_directory, 
        '/r/eth_purpose_imputation/r_module/multi_classification_kfold_cv.R',
        sep = ''), chdir = TRUE)
    
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
    
    # data 'activities' cleaning ----
    
    # analyze only the activities that has not been imputed
    load(imputed_activity_path)
    load(participants_path)
    
    activities <- readr::read_delim(
        activities_path, delim = ',', n_max = Inf)
    
    # combine original mobis and covid activities
    covid_activities_paths <- Sys.glob(covid_activities_glob)
    for(i in 1:length(covid_activities_paths)){
        covid_activities <- readr::read_delim(
            paste(covid_activities_paths[i], '/covid_activities.csv', sep=''),
            delim = ',', n_max = Inf)
        if(i == 1){
            old_activities <- rbind(activities, covid_activities)
        }else{
            old_activities <- rbind(old_activities, covid_activities)
        }
    }
    
    old_activities <- as.data.table(old_activities)
    colnames(old_activities)[colnames(old_activities) == "activity_id"] <- 'ID'
    
    # remove redundant imputed activities
    imputed_activity = imputed_activity[
        imputed_activity$ID %in% old_activities$ID,
        ]
    
    if(hc_all == 1){
        # 1st: transform wkb geometry data into longitude+latitude
        old_activities <- cbind(old_activities,
                                wkb2lonlat(old_activities$geometry))
        # 2nd: conduct spatial hierarchical clustering
        old_activities <- data.table::merge.data.table(
            old_activities, old_activities[, h_clustering(.SD), by = 'user_id', 
                                           .SDcols = c('ID', 'lon', 'lat')][, -1], 
            by = 'ID', sort = FALSE
        )
        
        # 3rd: extract dependent features
        
        # transform UTC time to local time, and infer time info.
        old_activities$started_at_local <- 
            utc2local_time(old_activities$started_at, old_activities$sf)
        old_activities$finished_at_local <- 
            utc2local_time(old_activities$finished_at, old_activities$sf)
        old_activities$starttime <- 
            lubridate::hour(old_activities$started_at_local)
        old_activities$endtime <- 
            lubridate::hour(old_activities$finished_at_local)
        old_activities$weekday <- 
            lubridate::wday(old_activities$started_at_local, week_start = 1)
        old_activities$yday <- 
            lubridate::yday(old_activities$started_at_local)
        old_activities$inside_day <- 
            ifelse(old_activities$yday == lubridate::yday(
                old_activities$finished_at_local), 1, 0)
        old_activities$duration_min <- round(old_activities$duration/60, 1)
        # infer how many activities for each participants
        old_activities <- data.table::merge.data.table(
            old_activities, old_activities[
                , .(activities_per_user = length(ID),
                    surveyed_day = length(unique(yday))),
                by = 'user_id'], 
            by = c('user_id'), sort = FALSE
        )
        old_activities <- data.table::merge.data.table(
            old_activities, old_activities[, .(
                activities_per_cluster = length(ID),
                mean_duration = mean(duration_min), 
                mean_starttime = mean(starttime), 
                mean_endtime = mean(endtime),
                sd_duration = sd(duration_min), 
                sd_starttime = sd(starttime),
                sd_endtime = sd(endtime),
                weekday_freq = length(which(weekday < 6)) / length(weekday)),
                by = c('user_id', 'hc_cat')], 
            by = c('user_id', 'hc_cat'), sort = FALSE
        )
        old_activities$cluster_freq <- 
            old_activities$activities_per_cluster / 
            old_activities$activities_per_user
        old_activities$daily_cluster_freq <- 
            old_activities$activities_per_cluster / old_activities$surveyed_day
        old_activities$daily_activities <- 
            old_activities$activities_per_user / old_activities$surveyed_day
        
        # 4th: extract new data
        all_activities = old_activities[
            ! old_activities$ID %in% imputed_activity$ID, ]
        if(dim(all_activities)[1] == 0){
            stop('No new activities found!')
        }
        
    } else if(hc_all == 0){
        # 1st: extract new data
        all_activities = old_activities[
            ! old_activities$ID %in% imputed_activity$ID, ]
        if(dim(all_activities)[1] == 0){
            stop('No new activities found!')
        }
        
        # 2nd: transform wkb geometry data into longitude+latitude
        all_activities <- cbind(all_activities,
                                wkb2lonlat(all_activities$geometry))
        # 3rd: conduct spatial hierarchical clustering
        all_activities <- data.table::merge.data.table(
            all_activities, all_activities[, h_clustering(.SD), by = 'user_id', 
                                           .SDcols = c('ID', 'lon', 'lat')][, -1], 
            by = 'ID', sort = FALSE
        )
        
        # 4th: extract dependent features
        
        # transform UTC time to local time, and infer time info.
        all_activities$started_at_local <- 
            utc2local_time(all_activities$started_at, all_activities$sf)
        all_activities$finished_at_local <- 
            utc2local_time(all_activities$finished_at, all_activities$sf)
        all_activities$starttime <- 
            lubridate::hour(all_activities$started_at_local)
        all_activities$endtime <- 
            lubridate::hour(all_activities$finished_at_local)
        all_activities$weekday <- 
            lubridate::wday(all_activities$started_at_local, week_start = 1)
        all_activities$yday <- 
            lubridate::yday(all_activities$started_at_local)
        all_activities$inside_day <- 
            ifelse(all_activities$yday == lubridate::yday(
                all_activities$finished_at_local), 1, 0)
        all_activities$duration_min <- round(all_activities$duration/60, 1)
        # infer how many activities for each participants
        all_activities <- data.table::merge.data.table(
            all_activities, all_activities[
                , .(activities_per_user = length(ID),
                    surveyed_day = length(unique(yday))),
                by = 'user_id'], 
            by = c('user_id'), sort = FALSE
        )
        all_activities <- data.table::merge.data.table(
            all_activities, all_activities[, .(
                activities_per_cluster = length(ID),
                mean_duration = mean(duration_min), 
                mean_starttime = mean(starttime), 
                mean_endtime = mean(endtime),
                sd_duration = sd(duration_min), 
                sd_starttime = sd(starttime),
                sd_endtime = sd(endtime),
                weekday_freq = length(which(weekday < 6)) / length(weekday)),
                by = c('user_id', 'hc_cat')], 
            by = c('user_id', 'hc_cat'), sort = FALSE
        )
        all_activities$cluster_freq <- 
            all_activities$activities_per_cluster / 
            all_activities$activities_per_user
        all_activities$daily_cluster_freq <- 
            all_activities$activities_per_cluster / all_activities$surveyed_day
        all_activities$daily_activities <- 
            all_activities$activities_per_user / all_activities$surveyed_day
    }
    
    # transform trip purpose to our classification
    all_activities$labels <- sapply(all_activities$purpose, function(x) {
        if(x %in% c('unknown', "leisure", "shopping", "home", "work", "errand", 
                    "assistance")){
            return(stringi::stri_trans_totitle(x))
        } else if(x == 'homeoffice'){
            return('Work')
        } else if(x == 'coworking'){
            return('Work')
        } else if(x == 'study'){
            return('Education')
        } else if(x == 'wait'){
            return('Other')
        }
    })
    
    # Handling missing value
    # apply(all_activities, 2, function(x) length(which(is.na(x))))
    all_activities[is.na(all_activities)] = 0
    
    # Code categorical variables as factor
    all_activities[, c('labels', 'weekday', 'inside_day')] <- 
        lapply(all_activities[,  c('labels', 'weekday', 'inside_day')], factor)
    
    # Combine activity data with participants information ----
    all_activities <- data.table::merge.data.table(
        all_activities, participants, 
        by.x = "user_id", by.y = "participant_ID", all.x = TRUE
    )
    all_activities$dist2home <- geosphere::distGeo(
        p1 = all_activities[, .(lon, lat)], 
        p2 = all_activities[, .(home_lon, home_lat)])
    
    # trip purpose imputation  ----
    if(new_training == 0){
        load(clf_tp_all_path)
        all_activities$imputed_purpose = predict(
            clf_tp_all$rf[[1]], 
            all_activities[, .SD, .SDcol = tp_sf_all]
        )
        # caret::confusionMatrix(
        #     table(
        #         factor(all_activities[which(
        #             all_activities$labels != 'Unknown')]$imputed_purpose,
        #             levels = activity_categories),
        #         factor(all_activities[which(
        #             all_activities$labels != 'Unknown')]$labels,
        #             levels = activity_categories)
        #     )
        # )
        all_activities$imputed_purpose[
            which(all_activities$labels != 'Unknown')] <- 
            all_activities$labels[which(all_activities$labels != 'Unknown')]
        imputed_activity_incremental <- all_activities[
            , .SD, .SDcol = c("ID", "labels", "imputed_purpose")]
        imputed_activity_incremental = rbind(
            imputed_activity, imputed_activity_incremental
        )
        
    } else if(new_training == 1){
        labeled_activity <- droplevels(
            all_activities[all_activities$labels != 'Unknown', ])
        labeled_activity$labels <- factor(labeled_activity$labels, 
                                          levels = activity_categories)
        
        # Currently the model lacks power for across participants imputation
        labeled_activity <- labeled_activity[order(user_id)]
        
        clf_tp_labeled <- multi_clf_kfold(
            x = labeled_activity[, .SD, .SDcol = tp_sf_all],
            y = labeled_activity$labels,
            ID = labeled_activity$ID,
            k = 2,
            par_rf = par_rf
        )
        
        all_activities <- data.table::merge.data.table(
            all_activities, 
            clf_tp_labeled$prediction[, c("ID", "all_predicted")], 
            by = 'ID',
            all.x = TRUE, 
            sort = FALSE
        )
        
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
            ID_predict = all_activities[, .SD, .SDcol = 'ID'],
            par_rf = par_rf
        )
        
        all_activities <- data.table::merge.data.table(
            all_activities, 
            clf_tp_all$prediction, 
            by = 'ID',
            all.x = TRUE, 
            sort = FALSE
        )
        
        print("Imputation accuracy: ")
        print(1 - clf_tp_all$rf[[1]]$err.rate[100, ])
        # caret::confusionMatrix(
        #     table(
        #         factor(all_activities[which(
        #             all_activities$labels != 'Unknown')]$rf_prediction,
        #             levels = activity_categories),
        #         factor(all_activities[which(
        #             all_activities$labels != 'Unknown')]$labels,
        #             levels = activity_categories)
        #     )
        # )
        
        all_activities$imputed_purpose <- all_activities$rf_prediction
        all_activities$imputed_purpose[
            which(all_activities$all_predicted > 0)] <- 
            all_activities$labels[which(all_activities$all_predicted > 0)]
        
        imputed_activity_incremental <- all_activities[
            , .SD, .SDcol = c("ID", "labels", "imputed_purpose")]
        
        imputed_activity_incremental = rbind(
            imputed_activity, imputed_activity_incremental
        )
        
    }
    
    if(dim(all_activities)[1] != 0){
        save(imputed_activity_incremental,
             file = imputed_activity_incremental_output_path)
    }
    print(Sys.time() - begin_time)
}


# mobis_analysis_directory = '/data/students/qigao/mobis_analysis'
# imputed_activity_incremental_output_path = '/data/mobis/data/enrichments/run_gao/imputed_activity_incremental.RData'
# imputed_activity_path = '/data/mobis/data/enrichments/run_gao/imputed_activity.RData'
# activities_path = '/data/mobis/data/csv/activities.csv'
# covid_activities_glob = '/data/mobis/data/covid/*[0-9]'
# participants_path =
#     '/data/mobis/data/enrichments/run_gao/participants.RData'
# clf_tp_all_path = '/data/mobis/data/enrichments/run_gao/clf_tp_all.RData'
# hc_all = 0
# new_training = 1
# par_rf = list(ntree = 100, importance = FALSE, do.trace = 1)





