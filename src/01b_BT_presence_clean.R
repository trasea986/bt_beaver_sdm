library(tidyverse)
library(rgbif)
library(sp)
library(maptools)
library(dismo)
library(hexbin)
library(patchwork)
library(RSQLite)
library(rnaturalearthdata)
library(CoordinateCleaner)
library(rgdal)
library(maps)

#This mimics 01a, which was developed by Autumn Harrington


#Upload brook trout presence shapefile (rgdal package needed to do this)

BullTrout <- read.csv("./data/BullTrout_IFWIS_SSS_Exp2020.csv")

#remove records without coordinates
BullTrout <- BullTrout %>%
  filter(!is.na(NewLong))%>%
  filter(!is.na(NewLat))

#Retain only unique LAT/LON locations (removes duplicates)
BullTrout <- BullTrout %>%
  distinct(NewLong, NewLat, .keep_all=TRUE)

#Keep only Long and Lat data
BullTrout <- BullTrout %>%
  dplyr::select(c('NewLong', 'NewLat'))

###Correcting for bias#### (May not have to do this step. Check against environmental rasters)

#set projection as USA Contiguous Albers Equal Area Conic

projection <- "ESRI:102003"

#make a copy of the brook trout dataframe from above
BullTrout_spatial <- BullTrout

#need to specify as spatial points
coordinates(BullTrout_spatial ) <- ~NewLong+NewLat
projection(BullTrout_spatial ) <- CRS('+proj=longlat +datum=NAD83')
BullTrout_spatial  <- spTransform(BullTrout_spatial , projection)


#create a raster with the same extent from spatial points file
r <- raster(BullTrout_spatial)

# set the resolution of the cells to 4500 meters (this seemed to reduce most of the points to one per raster cell when I checked in ArcMap)
res(r) <- 4500

# expand (extend the extent) of the RasterLayer a little. We are saying to take this raster, and add 10000 meters to its extent. Again, this will vary with species and how representative you think the point file is.
r <- extend(r, extent(r)+10000)

# sample random points
BullTrout_sample <- gridSample(BullTrout_spatial, r, n=1)

#now take those sampled points, turn them into a dataframe
BullTrout_df_final <- as.data.frame(BullTrout_sample)

#export points as csv to check out in ArcMap
write.csv(BullTrout_df_final, file = "./data/BullTrout_df_final4500.csv", row.names = FALSE)
