
# Function to do hierarchical clustering for longitude & latitude ---------


library('geosphere')
suppressPackageStartupMessages(library('DescTools'))


h_clustering <- function(geolocation,
                         hc_distance = 30,
                         suffix = NULL,
                         col_names = c('hc_cat', 'center_lon',
                                       'center_lat', 'dist2center')){
    
    # Input description ----
    # 
    # geolocation: location information
    # $longitude: 
    # $latitude: 
    # 
    # hc_distance: distance used to separate each cluster. will be around 
    # radius of generated cluater
    # 
    # suffix: suffix used to add to col_names
    # 
    # col_names: colnames used for output
    # 
    # 
    # Output ----
    # 
    # h_clustering_res: results contaning following columns
    # $hc_cat: in which cluster each instance is
    # $center_lon: 
    # $center_lat: 
    # 
    # Function ----
    
    
    # distance matrix
    hc_dist <- as.dist(geosphere::distm(geolocation) )
    
    # hierarchical clustering results
    hclust_res <- hclust(hc_dist, method = "average")
    
    # hierarchical clustering categories
    hc_cat <- cutree(hclust_res, h = hc_distance)
    
    # center of the most often visited cluster
    cluster_center <- apply(geolocation[which(
        hc_cat == DescTools::Mode(hc_cat)[1]), ], 2, mean)
    
    # distance to center
    dist2center <- geosphere::distm(geolocation, cluster_center)
    
        
    h_clustering_res <- data.frame(
        hc_cat = hc_cat, 
        center_lon = cluster_center[1], 
        center_lat = cluster_center[2], 
        dist2center = dist2center, 
        row.names = NULL)
    
    if(!is.null(suffix)){
        col_names <- paste(suffix, col_names, sep = '_')
    }
    colnames(h_clustering_res) <- col_names
    
    return(h_clustering_res)
}


# geolocation <- legs[1:1000, c('start_lon', 'start_lat')]
# hc_distance = 30
# h_clustering(legs[1:1000, c('start_lon', 'start_lat')])
# ddd = h_clustering(legs[which(legs$user_id == "ZZYIC"), c('start_lon', 'start_lat')])
# ccc = legs[which(legs$user_id == "ZZYIC"), c('start_hc_cat', 'start_center_lon', 
#                                        "start_center_lat", "start_dist2center")]


