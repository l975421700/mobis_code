
# namelist about google places api ----

# API key for google places api
api_key <- 'AIzaSyDEA__8d-hxOJEsA7dbYxU9RpqDpqFqJMI' 
# register_google(key = "AIzaSyDEA__8d-hxOJEsA7dbYxU9RpqDpqFqJMI", write = TRUE)

poi_types <- c('Leisure_p', 'Food_p', 'Store_p', 'Transport_p', 'Religion_p', 
               'Education_p', 'Health_p', 'Civic_p', 'Business_p', 'Unknown_p')

# namelist about participants ----

participants_features <- c(
    "household_size", "income", "education", "age", "sex", "work_type", 
    "employment_1", "own_car", "own_motorbike", "own_bike_ebike", 
    "has_pt_pass_halffare", "has_pt_pass_regional", "has_pt_pass_ga")

participants_features_meaning <- c(
    'Household size', 'Annual income', 'Education', 'Age', 'Sex', 'Work type', 
    'Employment', 'Own car', 'Own motorbike', 'Own bike or ebike', 
    'Own SBB half fare pass', 'Own SBB regional pass', 'Own SBB GA pass')

participants_factorfeatures <- c(
    "income", "education", "sex", "work_type", 'employment_1', "own_car",
    "own_motorbike", "own_bike_ebike", "has_pt_pass_halffare",
    "has_pt_pass_regional", "has_pt_pass_ga")


# namelist about activities ----

activity_categories <- c(
    "Home", "Work", "Leisure", "Shopping", "Other", "Errand", "Assistance",
    "Education"
)


activities_features <- c(
    "duration_min", "starttime", "endtime", "weekday", "inside_day", 
    "daily_activities", 'building_zone'
)

activities_features_meaning <- c(
    "Duration", "Start time", "End time", "Day of week", "Inside a day", 
    "Activities per day", 'Building zone'
)


activities_clusterfeatures <- c(
    'mean_duration', 'sd_duration', 'mean_starttime', 'sd_starttime', 
    'mean_endtime', 'sd_endtime', 'weekday_freq', 
    "cluster_freq", "daily_cluster_freq", 'dist2center'
)

activities_clusterfeatures_meaning <- c(
    'Mean duration', 'Standard deviation of duration', 'Mean start time',
    'Standard deviation of start time', 'Mean end time', 
    'Standard deviation of end time', 'Percentage of weekdays', 
    "Percentage of activities per cluster", "Daily occurrence", 
    'Distance to most often visitied cluster'
)

activities_factorfeatures <- c('weekday', 'inside_day', 'building_zone')


# namelist about modes and trips ----

mode_categories <- c("Walk", "Car", "Train", "Bus", "Bicycle", 'Tram', "Others")

legs_features <- c("length", "starttime", "endtime", "weekday", "inside_day",
                   "duration_min", "speed")

legs_features_meaning <- c(
    "Length", "Start time", 'End time', "Day of week", "Inside a day", 
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
    'Mean start time at starting cluster', 
    'Standard deviation of start time at starting cluster', 
    'Mean start time at ending cluster', 
    'Standard deviation of start time at ending cluster', 
    'Mean end time at starting cluster', 
    'Standard deviation of end time at starting cluster', 
    'Mean end time at ending cluster', 
    'Standard deviation of end time at ending cluster', 
    
    'Mean duration at starting cluster', 
    'Standard deviation of duration at starting cluster', 
    'Mean duration at ending cluster', 
    'Standard deviation of duration at ending cluster', 
    'Mean speed at starting cluster', 
    'Standard deviation of speed at starting cluster', 
    'Mean speed at ending cluster', 
    'Standard deviation of speed at ending cluster', 
    
    'Distance to most often visitied starting cluster', 
    'Distance to most often visitied ending cluster', 
    
    "Percentage of activities per starting cluster",
    "Percentage of activities per ending cluster",
    
    "Daily occurrence of starting cluster", 
    "Daily occurrence of ending cluster", 
    
    'Percentage of weekdays'
)


legs_factorfeatures <- c('weekday', 'inside_day')


# selected features for trip purpose----

tp_sf_pca11 <- rep(list(c(participants_factorfeatures, 
                        activities_factorfeatures, 
                        paste('PC', 1:11, sep = '') )), 
                 (length(activity_categories) + 1))

tp_sf_all <- rep(list(c(participants_features, 
                        activities_features, 
                        activities_clusterfeatures)), 
                 (length(activity_categories) + 1))

tp_sf_noprn <- rep(list(c(activities_features, 
                          activities_clusterfeatures)), 
                   (length(activity_categories) + 1))

tp_sf_nocluster <- rep(list(c(participants_features, 
                              activities_features)), 
                   (length(activity_categories) + 1))

tp_sf_all_poi <- rep(list(c(participants_features, 
                        activities_features, 
                        activities_clusterfeatures, 
                        poi_types)), 
                 (length(activity_categories) + 1))

tp_sf_all_poi9 <- rep(list(c(participants_features, 
                             activities_features, 
                             activities_clusterfeatures, 
                             poi_types[-10])), 
                      (length(activity_categories) + 1))

# selected features for trip mode----

tm_sf_all <- rep(list(c(participants_features, 
                        legs_features, 
                        legs_clusterfeatures)), 
                 (length(mode_categories) + 1))
tm_sf_noprn <- rep(list(c(legs_features, 
                          legs_clusterfeatures)), 
                   (length(mode_categories) + 1))

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


