
# Functions to install pkg when it doesn't exist --------------------------


install_packages <- function(pkg){
    if (! is.element(pkg, installed.packages()[,1])){
        install.packages(pkg)
    }
} 


# Install the necessary packages ------------------------------------------


data_format <- c("readr", "dplyr", "rgdal", "sf", "sp", 'lutz')
data_analysis <- c('ggplot2', 'viridisLite', 'googleway', 'parallel',
                   'geosphere', 'DescTools', 'randomForest', 'VIM',
                   'viridis', 'UBL', 'BBmisc', 'caret', 'stringi', 
                   'lubridate', 'data.table', 'kernlab', 'keras', 'raster')
others <- c()

# c("googleAuthR", 'ggmap', "plyr", "gstat", "maps", "mapproj", ...
# "pso", "CaDENCE", "zoo", "corrplot", "abind", "data.table")
# c("spacetime", "lattice", "Rmisc", "MASS", "survival")

lapply (c(data_format, data_analysis, others), install_packages)
# cat("\f")
rm(data_format, data_analysis, others, install_packages)
