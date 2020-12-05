
# load files and import packages ------------------------------------------
source('mobis_code/r_module/install_packages.R', chdir = TRUE) 
source('mobis_code/r_module/wkb2lonlat.R', chdir = TRUE) 
source('mobis_code/r_module/utc2local_time.R', chdir = TRUE) 
source('mobis_code/r_module/hierarchical_clustering.R', chdir = TRUE) 
source('mobis_code/r_module/namelist.R', chdir = TRUE) 
source('mobis_code/r_module/multi_classification_kfold_cv.R', chdir = TRUE) 

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
library('Boruta')


# load("/data/mobis/data/enrichments/scratch/clf_tp_all.RData")
# load("/data/mobis/data/enrichments/scratch/imputed_activity.RData")
# load("/data/mobis/data/enrichments/scratch/all_legs.RData")
# load("/data/mobis/data/enrichments/scratch/clf_tm_all.RData")
# load("/data/mobis/data/enrichments/scratch/imputed_mode.RData")
# load("/data/mobis/data/enrichments/scratch/participants.RData")
