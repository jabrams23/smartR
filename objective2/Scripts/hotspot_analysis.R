######################
# Code for hotspot analysis of SMART patrol data
# by Jesse F. Abrams
######################

# Load libraries
library(spdep)
library(rgdal)
library(lattice)
library(raster)
library(rgeos)


library(hexbin)
library(colorspace)
library(viridis)
library(raster)
library(rgdal)
library(ggthemes) # theme_map()
library(ggpubr)
library(grid)
library(gridExtra)
library(lattice)
library(ggplot2)


# Set working directory
setwd("~/Documents/GitHub/smartR/objective2")

# Load data
threat <- raster("~/Dropbox (ScreenForBio)/Jesse's stuff/smartpatrol/objective2/Data/AllTraps_weighted_mean_2014_19.tif")
#threat <- flip(threat, 1)

new_extent <- extent(threat)
new_extent@xmin <- 107.4
new_extent@xmax <- 107.8
new_extent@ymin <- 15.9
new_extent@ymax <- 16.2

threat <- crop(threat,new_extent)

#cost <- raster("/Users/jesse/Desktop/wwf_optim/CC_slope_roads_river_lc_elevation.tif") 
cost <- raster("/Users/jesse/Desktop/wwf_optim/roads_LCP_mean_200m_masked.tif") 
cost <- resample(cost,threat)
cost <- crop(cost,new_extent)
cost <- mask(cost,threat)

neighborhoods <- readOGR(dsn="~/Dropbox (ScreenForBio)/Jesse's stuff/smartpatrol/objective2/Data/GIS_data", layer='HQNSLPAsnew', stringsAsFactors = F)
neighborhoods <- spTransform(neighborhoods, proj4string(threat))
plot(neighborhoods, col=terrain.colors(nrow(neighborhoods)))

stations <- readOGR(dsn="/Users/jesse/Desktop/wwf_optim", layer='Ranger Station', stringsAsFactors = F)
stations <- spTransform(stations, proj4string(threat))
plot(stations, add=TRUE)

roads <- readOGR(dsn="/Users/jesse/Desktop/wwf_optim", layer='roads', stringsAsFactors = F)
roads <- spTransform(roads, proj4string(threat))
plot(roads,add=TRUE)

stations <- crop(stations, neighborhoods)
roads <- crop(roads, neighborhoods)
roads = as(roads, "SpatialPointsDataFrame")

getisgrid <- rasterToPolygons(threat)

# Create the list of neighbors

neighbors <- poly2nb(getisgrid)
weighted_neighbors <- nb2listw(neighbors, zero.policy=T)

vals <- values(threat)
vals <- vals[!is.na(vals)]

# Perform the local G analysis (Getis-Ord GI*)
getisgrid$HOTSPOT = as.vector(localG(vals, weighted_neighbors))

#hotspot <- getisgrid
#hotspot@data$Detections_cumul_AllTraps <- NULL

hotspots <- rasterize(getisgrid,threat,"HOTSPOT")

range01 <- function(x){(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}

values(hotspots) <- range01(values(hotspots))

stations <- as.data.frame(stations@coords)
colnames(stations) <- c("X","Y","Z")
stations$Z <- NULL
#stations2 <- stations[c(6,5,7,8,4),]

roads <- as.data.frame(roads@coords)
colnames(roads) <- c("X","Y")
#stations$Z <- NULL

rangers <- threat
values(rangers) <- 0
rangers <- mask(rangers,threat)
station_cells <- cellFromXY(rangers,stations)

start <- rangers

street <- threat
values(street) <- 0
street <- mask(street,threat)
road_cells <- cellFromXY(street,roads)

start_cells <- append(road_cells,station_cells)

start[start_cells] <- 1
start[-start_cells] <- NA

plot(start)

#plot(hotspots)

plot(cost)
plot(hotspots,add=TRUE,col = rev(rainbow(10, alpha = 0.1)))





# Color the grid cells based on the z-score
breaks = c(-20, -1.96, -1, 1, 1.96, 20)
palette=c("#0000FF80", "#8080FF80", "#FFFFFF80", "#FF808080", "#FF000080")
col = palette[cut(getisgrid$HOTSPOT, breaks)]

rwb <- colorRampPalette(colors = c("red", "white", "blue"))

n2plot <- fortify(neighborhoods)

plot_map <- function(input){
  test_spdf <- as(input, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  colnames(test_df) <- c("value", "x", "y")
  p <- ggplot() +  
    geom_tile(data=test_df, aes(x=x, y=y, fill=value)) +
    scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
    geom_map(data=n2plot, map=n2plot, aes(long, lat, map_id=id),
             color="black", fill="transparent") +
    coord_equal() +
    theme_bw() +
    theme(legend.position="bottom",
          #legend.text=element_text(size=26),
          #legend.title=element_text(size=30),
          axis.title=element_blank())
  
  return(p)
}

png(file="hotspots.png", width = 6, height = 4, units = 'in', res = 300)
plot_map(hotspots)
dev.off()

png(file="threat.png", width = 6, height = 4, units = 'in', res = 300)
plot_map(threat)
dev.off()

png(file="cost.png", width = 6, height = 4, units = 'in', res = 300)
plot_map(cost)
dev.off()

png(file="start.png", width = 6, height = 4, units = 'in', res = 300)
plot_map(start)
dev.off()

