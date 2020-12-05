library(dplyr)
library(sf)
library(parallel)
library(reshape2)
library(lubridate)
library(ggplot2)

zoning <- read_sf('/data/mobis/data/geodata/ARE/BauzonenSchweiz_ZonesabatirSuisse_2017/ch_are_bauzonen.shp') %>% mutate(zoning_id = row_number())

get_nearest_zoning <- function(activities_df1, zoning, max_dist) {
    buffed_activities <- activities_df1 %>% 
        st_buffer(max_dist) %>% 
        mutate(row_num = row_number())
    
    a <- st_join(buffed_activities, zoning, largest=F) %>% filter(!is.na(zoning_id))
    
    aa  <- activities_df1 %>% 
        select(activity_id) %>%
        slice(match(a$activity_id, activity_id)) 
    ll  <- zoning %>% slice(match(a$zoning_id, zoning_id))  #match(a_not_na$id, zoning$id )
    
    aa$dist_to_zoning <- st_distance(aa, ll, by_element = T)
    aa_ll <- bind_cols(aa, ll %>% st_drop_geometry()) %>%
        group_by(activity_id) %>% 
        slice(which.min(dist_to_zoning))
    
    matched_activities <- aa_ll %>% st_drop_geometry() 
    
    return (matched_activities)
}

detect_activity_zoning <- function(activities_df, radius=100, n_cores=8) {
    radius <- radius
    partitioned_activities <- activities_df %>% 
        ungroup() %>%
        mutate(
            partition_id = floor(row_number()/n() * (n_cores*2-1))
        )  %>% 
        group_by(partition_id) %>% group_split()
    
    lu_short <- zoning %>% select(zoning_id, CH_CODE_HN, CH_BEZ_D)
    
    default_get_nearest_zoning <- function(df) get_nearest_zoning(df, lu_short, radius)
    
    #cl <- makeCluster(n_cores)
    # n = number of cores (I'd recommend one less than machine capacity)
    #clusterExport(cl, c('lu_short', 'radius'), envir=environment()) #export input data to all cores
    #clusterExport(cl, c('get_nearest_zoning')) #export input data to all cores
    
    #clusterEvalQ(cl, {
    #  library(dplyr)
    #  library(sf)
    #  library(reshape2)
    #})
    
    #activity_zoning_list <- parLapply(cl, partitioned_activities, default_get_nearest_zoning )
    #stopCluster(cl) # close cluster when complete (particularly on shared machines)
    
    activity_zoning_list <- mclapply(partitioned_activities, default_get_nearest_zoning, mc.cores = n_cores)
    activity_zoning <- data.table::rbindlist(activity_zoning_list)
    return (activity_zoning)
}

