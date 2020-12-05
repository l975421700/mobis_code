
# Function to use python in r ------------------------


library(reticulate)
use_condaenv("mobis", required = TRUE)
conda_install("mobis", "ggplot")

np <- import("geopandas")
npz <- list(0)
npz[[1]] <- np$load("2_output/1.3.8_original_7d_dry_nab43.npz")





# Function to query information from OpenStreetMap ------------------------


suppressPackageStartupMessages(library(osmdata))
suppressPackageStartupMessages(library(osmar))

# Make sure to use kumi URL, description: https://wiki.openstreetmap.org/wiki/Overpass_API#Public_Overpass_API_instances
# get_overpass_url()
# set_overpass_url("https://overpass.kumi.systems/api/interpreter")

bb <- osmar::center_bbox(8.518702, 47.381040, 60, 60)

system.time(
    dat <- opq(bbox = bb) %>% 
        add_osm_feature(key = 'highway') %>%
        osmdata_sf ()
)

system.time(
    dat1 <- opq(bbox = bb) %>% 
        osmdata_sf ()
)

system.time(
    dat2 <- opq(bbox = bb) %>% 
        osmdata_sp ()
)









