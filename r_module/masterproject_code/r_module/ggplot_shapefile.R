
# Function to ggplot shapefile with color and legend  ---------------------


suppressPackageStartupMessages(library('ggplot2'))
library('sp')
library('viridisLite')
library('viridis')
suppressPackageStartupMessages(library('rgdal'))


ggplot_shapefile <- function(
    shapefile, 
    plot_axis = TRUE, 
    spacing = c(1, 1),
    xylimits = c(NA, NA, NA, NA), 
    fill_shp = NA, 
    lon_lat_attribute = list(lon_breaks = NULL, lat_breaks = NULL),
    plot_margin = c(0.25, 0.25, 0.2, 0.3),  # c(top, right, bottom, left )
    plot_legend = FALSE,
    specify_legend = list(
        position = c(0.5,-0.05), name = NULL, direction ='horizontal',
        barheight = 1, barwidth = 30), 
    plot_breaks = FALSE,
    specify_breaks = list(breaks = c(10^2, 10^3, 10^4, 10^5, 10^6), 
                          labels = expression(10^3, 10^4, 10^5, 10^6))
    ){
    
    # Input ----
    
    # shapefile: polygon shapefile, read by rgdal::readOGR; CRS arguments:
    # '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0';
    
    # plot_axis: if TRUE, plot axes; else blank.
    
    # xylimits: axis limits, (xmin, xmax, ymin, ymax). If not given, 
    # it will be inferred based on lon_lat_attribute (which requires
    # plot_axis to be TRUE).
    
    # fill_shp: NA by default, no filled color; 
    # or specify like swiss_kanton@data$Shape__Are
    
    # spacing: spacing of longitude and latitude breaks
    
    # lon_lat_attribute: if plot_axis is TRUE, it can be specified or
    # inferred from shapefile, it can be specified like 
    # lon_lat_attribute = list(lon_breaks = seq(6, 10, 1), 
    # lat_breaks = seq(46, 47.5, 0.5))
    
    # plot_margin: distance between figure box to figure margin 
    # (top, right, bottom, and left )
    
    # plot_legend: If FALSE, don't plot legend
    
    # specify_legend: legend characteristics
    
    # plot_breaks: If FALSE, don't add breaks to values used to fill color
    
    # specify_breaks: add breaks and corresponding labels to color scale?
    # If NULL, no specification. 
    
    # Function ----
    
    # incase the unit of shapefile is m.
    shapefile <- spTransform(shapefile, CRS("+proj=longlat"))
    
    # infer breaks, labels, limits ----
    if(plot_axis == TRUE){ 
        
        # if not given, infer longitude/latitude breaks ----
        if(is.null(lon_lat_attribute$lon_breaks)){ 
            lon_lat_range <- sp::bbox(shapefile)
            lon_lat_range_rounded <- c(
                lon_lat_range[1, 1] %/% spacing[1] * spacing[1], 
                ceiling(lon_lat_range[1, 2] / spacing[1] - 0.001) * 
                    spacing[1], 
                lon_lat_range[2, 1] %/% spacing[2] * spacing[2], 
                ceiling(lon_lat_range[2, 2] / spacing[2]) * spacing[2])
            
            # Maximum lat in world shapefile is only 83.6
            if(lon_lat_range_rounded[4] == 85){ 
                lon_lat_range_rounded[4] <- 90
            }
            
            # Create longitude_latitude breaks used for x/y axis ticks
            lon_lat_attribute$lon_breaks <- seq(
                lon_lat_range_rounded[1], lon_lat_range_rounded[2],
                spacing[1])
            lon_lat_attribute$lat_breaks <- seq(
                lon_lat_range_rounded[3], lon_lat_range_rounded[4],
                spacing[2])
        } 
        
        # Add breaks with '°W', '°E', '°N', '°S' or 0 for labels ----
        lon_lat_attribute$lon_labels <- sapply(
            lon_lat_attribute$lon_breaks, function(x) if(x == 0){
                x
            }else if (x < 0){
                paste(as.character(abs(x)), '°W', sep = '')
            }else {
                paste(as.character(abs(x)), '°E', sep = '')
            })
        
        lon_lat_attribute$lat_labels <- sapply(
            lon_lat_attribute$lat_breaks, function(x) if(x == 0){
                x
            }else if (x < 0){
                paste(as.character(abs(x)), '°S', sep = '')
            }else {
                paste(as.character(abs(x)), '°N', sep = '')
            })
        
        if(is.na(xylimits[1])){ # if xylimits is not given
            xylimits[1:2] <- c(lon_lat_attribute$lon_breaks[1], 
                               tail(lon_lat_attribute$lon_breaks, 1) + 
                                   0.001)
        }
        
        if(is.na(xylimits[3])){
            xylimits[3:4] <- c(lon_lat_attribute$lat_breaks[1], 
                               tail(lon_lat_attribute$lat_breaks, 1))
        }
    }
    
    # ggplot the shapefile ----
    shapefile_ggplot <- ggplot() + 
        geom_polygon(aes(long, lat, group = group), fill = NA, 
                     color = 'black', size = 0.2, data = shapefile) +
        # coord_equal(ratio = 1) + but even worse?
        scale_x_continuous(limits = xylimits[1:2], expand = c(0, 0)) +
        scale_y_continuous(limits = xylimits[3:4], expand = c(0, 0)) +
        theme(panel.border = element_blank(), 
              panel.background = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.spacing.x = unit(0, "cm"),
              plot.margin = unit(plot_margin, "lines"))
    
    # add axes info if required ----
    if(plot_axis == TRUE){
        shapefile_ggplot <- shapefile_ggplot +
            theme(panel.border = element_rect(
                colour = "black", fill=NA, size=0.2),
                axis.text.x = element_text(
                    size = 16, family = 'Times New Roman', vjust = 0),
                axis.text.y = element_text(
                    size = 16, family = 'Times New Roman', hjust = 1), 
                axis.ticks = element_line(size = 0.2)) + 
            scale_x_continuous(
                limits = xylimits[1:2], expand = c(0, 0),
                breaks = lon_lat_attribute$lon_breaks, 
                labels = lon_lat_attribute$lon_labels) +
            scale_y_continuous(
                limits = xylimits[3:4], expand = c(0, 0),
                breaks = lon_lat_attribute$lat_breaks,
                labels = lon_lat_attribute$lat_labels)
    }
    
    # add breaks to value used for fill ----
    if(plot_breaks == TRUE){
        fill_shp <- cut(
            fill_shp, 
            breaks = specify_breaks$breaks, 
            labels = specify_breaks$labels)
    }
    
    # add filled color ----
    if(! is.na(fill_shp)[1]){
        shapefile@data$fill_shp <- fill_shp
        shapefile_ggplot <- shapefile_ggplot + 
            geom_sf(data = st_as_sf(shapefile), 
                    aes(fill = fill_shp), size = 0)
    }
    
    # specify legend ----
    if(plot_legend == TRUE){
        shapefile_ggplot <- shapefile_ggplot + 
            theme(
                legend.position = specify_legend$position,
                legend.direction = specify_legend$direction, 
                legend.background = element_blank(),
                legend.text = element_text(size = 16, 
                                           family = 'Times New Roman', 
                                           margin = margin(t = -4))) +
            scale_fill_manual(
                values = magma(length(specify_breaks$labels)+  3)[
                    -c(1, 2, length(specify_breaks$labels))], 
                name = specify_legend$name, drop = FALSE, 
                labels = rev(specify_breaks$labels),
                breaks = rev(levels(shapefile@data$fill_shp)), 
                guide = guide_legend(
                    keyheight = unit(specify_legend$barheight, 
                                     units = "mm"),
                    keywidth = unit(specify_legend$barwidth / 
                                        length(specify_breaks$labels),
                                    units = "mm"),
                    nrow = 1, byrow = T, title.position = 'top', 
                    label.position = "bottom", 
                    reverse = T, label.hjust = 1.5 # some shifting around
                ))
    }
    
    return(shapefile_ggplot)
}


# swiss_kanton <- rgdal::readOGR('3_output/28_Apr/geo/swiss_kanton.shp')
# png('4_visualization/swiss.png', width = 15, height = 9,
#     units = 'cm', res = 600)
# ggplot_shapefile(swiss_kanton, xylimits = c(5.9, 10.55, 45.78, 47.85),
#                  spacing = c(0.5, 0.5), lon_lat_attribute =
#                      list(lon_breaks = seq(6, 10, 1),
#                           lat_breaks = seq(46, 47.5, 0.5)) )
# dev.off()
# 
# 
# world_cntr <- rgdal::readOGR('1_data/geo_data/world/world_cntr.shp')
# png('4_visualization/world.png', width = 15, height = 9,
#     units = 'cm', res = 600)
# ggplot_shapefile(
#     world_cntr, spacing = c(60, 30), plot_margin = c(1, 1.5, 0.2, 0.3))
# dev.off()

# swiss_hoheitsgebiet <- rgdal::readOGR(
#     '1_data/geo_data/swiss/ch_boundaries/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp')
# swiss_hoheitsgebiet <- sp::spTransform(
#     swiss_hoheitsgebiet, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )

# swiss_hoheitsgrenze <- rgdal::readOGR(
#     '1_data/geo_data/swiss/ch_boundaries/swissBOUNDARIES3D_1_3_TLM_HOHEITSGRENZE.shp')
# swiss_hoheitsgrenze <- sp::spTransform(
#     swiss_hoheitsgrenze, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )
# 
# png('4_visualization/swiss.png', width = 15, height = 9,
#     units = 'cm', res = 600)
# ggplot_shapefile(swiss_hoheitsgrenze, xylimits = c(5.9, 10.55, 45.78, 47.85),
#                  spacing = c(0.5, 0.5), lon_lat_attribute =
#                      list(lon_breaks = seq(6, 10, 1),
#                           lat_breaks = seq(46, 47.5, 0.5)) )
# dev.off()





