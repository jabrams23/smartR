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
threat <- raster("~/Documents/GitHub/smartR/objective2/Data/cumulative_rasters/Detections_cumul_AllTraps.tif")

plot(threat)

neighborhoods <- readOGR(dsn='~/Documents/GitHub/smartR/objective2/Data/GIS_data', layer='HQNSLPAsnew', stringsAsFactors = F)
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

# Color the grid cells based on the z-score
breaks = c(-20, -1.96, -1, 1, 1.96, 20)
palette=c("#0000FF80", "#8080FF80", "#FFFFFF80", "#FF808080", "#FF000080")
col = palette[cut(getisgrid$HOTSPOT, breaks)]

# Plot
plot(getisgrid, col=col, border=NA)
plot(neighborhoods, border="black", add=T)




