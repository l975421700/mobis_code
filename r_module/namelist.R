

# namelist about participants ----

participants_features <- c(
    "household_size", "income", "age", 
    "employment_1", 'ifworker', 'ifstudent')

participants_features_meaning <- c(
    'Household size', 'Annual income', 'Age', 
    'Employment', 'If a worker', 'If a student')

participants_factorfeatures <- c(
    "income", 'employment_1', 
    'ifworker', 'ifstudent')


# namelist about activities ----

activity_categories <- c(
    "Home", "Work", "Leisure", "Shopping", "Errand", "Other", "Assistance",
    "Education"
)


activities_features <- c(
    "duration_min", "starttime", "endtime", "weekday", 
    "daily_activities"
)

activities_features_meaning <- c(
    "Duration", "Start time", "End time", "Day of week", 
    "Activities per day"
)


activities_clusterfeatures <- c(
    'mean_duration', 'sd_duration', 'mean_starttime', 'sd_starttime', 
    'mean_endtime', 'sd_endtime', 'weekday_freq', 
    "cluster_freq", "daily_cluster_freq", 'dist2center'
)

activities_clusterfeatures_meaning <- c(
    'm(duration)', 'std(duration)', 'm(start time)',
    'std(start time)', 'm(end time)', 
    'std(end time)', 'Percentage of weekdays', 
    "Percentage of activities per cluster", "Daily occurrence", 
    'Distance to most often visited cluster'
)

activities_factorfeatures <- c('weekday')


# namelist about modes and trips ----

mode_categories <- c("Walk", "Car", "Train", "Bicycle", "Bus", 'Tram', "Others")

legs_features <- c("length", "starttime", "endtime", "weekday",
                   "duration_min", "speed")

legs_features_meaning <- c(
    "Length", "Start time", 'End time', "Day of week", 
    "Duration", 'Speed'
)

legs_clusterfeatures <- c(
    "mean_starttime_startcluster", "sd_starttime_startcluster",
    "mean_starttime_endcluster", "sd_starttime_endcluster",
    "mean_endtime_startcluster", "sd_endtime_startcluster", 
    "mean_endtime_endcluster", "sd_endtime_endcluster",
    
    "mean_duration_startcluster", "sd_duration_startcluster",
    "mean_duration_endcluster", "sd_duration_endcluster", 
    "mean_speed_startcluster", "sd_speed_startcluster", 
    "mean_speed_endcluster", "sd_speed_endcluster",
    
    "start_dist2center", "end_dist2center",
    
    "startcluster_freq", "endcluster_freq",
    
    "daily_freq_start", "daily_freq_end", 
    
    "weekday_freq")

legs_clusterfeatures_meaning <- c(
    'm(start time at starting cluster)', 
    'std(start time at starting cluster)', 
    'm(start time at ending cluster)', 
    'std(start time at ending cluster)', 
    'm(end time at starting cluster)', 
    'std(end time at starting cluster)', 
    'm(end time at ending cluster)', 
    'std(end time at ending cluster)', 
    
    'm(duration at starting cluster)', 
    'std(duration at starting cluster)', 
    'm(duration at ending cluster)', 
    'std(duration at ending cluster)', 
    'm(speed at starting cluster)', 
    'std(speed at starting cluster)', 
    'm(speed at ending cluster)', 
    'std(speed at ending cluster)', 
    
    'Distance from start point to most often visited cluster', 
    'Distance from end point to most often visited cluster', 
    
    "Percentage of activities per starting cluster",
    "Percentage of activities per ending cluster",
    
    "Daily occurrence of starting cluster", 
    "Daily occurrence of ending cluster", 
    
    'Percentage of weekdays'
)

legs_factorfeatures <- c('weekday')

legs_pointsinfo <- c(
    "speed_0%", "speed_5%", "speed_10%", "speed_25%", "speed_50%", 
    "speed_75%", "speed_90%", "speed_95%", "speed_100%", "speed_std", 
    "acceleration_0%", "acceleration_5%", "acceleration_10%", 
    "acceleration_25%", "acceleration_50%", "acceleration_75%", 
    "acceleration_90%", "acceleration_95%", "acceleration_100%", 
    "acceleration_std")

# "speed_0%", "speed_5%", "speed_10%", "speed_25%", "speed_50%", "speed_75%", "speed_90%", "speed_95%", "speed_100%", "speed_std", "acceleration_0%", "acceleration_5%", "acceleration_10%", "acceleration_25%", "acceleration_50%", "acceleration_75%", "acceleration_90%", "acceleration_95%", "acceleration_100%", "acceleration_std"



# selected features for trip purpose----

tp_sf_all <- c(participants_features, 
                        activities_features, 
                        activities_clusterfeatures)

# selected features for trip mode----

tm_sf_all1 <- c(participants_features, 
                        legs_features, 
                        legs_clusterfeatures)

tm_sf_all <- c(legs_features, 
               legs_clusterfeatures)

# mapping of variable names to its meanings----

mapping_var <- list(
    variable = c(
        participants_features, activities_features, activities_clusterfeatures),
    meaning = c(
        participants_features_meaning, activities_features_meaning, 
        activities_clusterfeatures_meaning)
)

mapping_var_tm <- list(
    variable = c(
        participants_features, legs_features, legs_clusterfeatures),
    meaning = c(
        participants_features_meaning, legs_features_meaning, 
        legs_clusterfeatures_meaning)
)


