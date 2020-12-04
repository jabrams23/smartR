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

# Set working directory
setwd("~/Documents/GitHub/smartR/objective2")

# Load data
threat <- raster("~/Dropbox (ScreenForBio)/Jesse's stuff/smartpatrol/objective2/Data/cumulative_rasters/Detections_cumul_AllTraps.tif")
#threat <- flip(threat, 1)

new_extent <- extent(threat)
new_extent@xmin <- 107.4
new_extent@xmax <- 107.8
new_extent@ymin <- 15.9
new_extent@ymax <- 16.2

threat <- crop(threat,new_extent)

cost <- raster("/Users/jesse/Desktop/wwf_optim/CC_slope_roads_river_lc_elevation.tif") 
cost <- resample(cost,threat)
cost <- crop(cost,new_extent)
cost <- mask(cost,threat)

neighborhoods <- readOGR(dsn="~/Dropbox (ScreenForBio)/Jesse's stuff/smartpatrol/objective2/Data/GIS_data", layer='HQNSLPAsnew', stringsAsFactors = F)
neighborhoods <- spTransform(neighborhoods, proj4string(threat))
plot(neighborhoods, col=terrain.colors(nrow(neighborhoods)))

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
plot(hotspots)



plot(cost)
plot(hotspots,add=TRUE,col = rev(rainbow(10, alpha = 0.1)))




# Color the grid cells based on the z-score
breaks = c(-20, -1.96, -1, 1, 1.96, 20)
palette=c("#0000FF80", "#8080FF80", "#FFFFFF80", "#FF808080", "#FF000080")
col = palette[cut(getisgrid$HOTSPOT, breaks)]

# Plot
png(file="hotspot.png", width = 10, height = 15, units = 'in', res = 300)
plot(getisgrid, col=col, border=NA)
plot(neighborhoods, border="black", add=T)
dev.off()




