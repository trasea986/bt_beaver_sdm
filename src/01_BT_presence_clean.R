#install packages (not sure if all these are actually used)
install.packages(c("tidyverse","rgbif","sp","maptools","dismo"))
library(tidyverse)
library(rgbif)
library(sp)
library(maptools)
library(dismo)
install.packages(c("hexbin","patchwork","RSQLite"))
library(hexbin)
library(patchwork)
library(RSQLite)
install.packages("rnaturalearthdata")
library(rnaturalearthdata)
install.packages("CoordinateCleaner")
library(CoordinateCleaner)
install.packages("maps")
library(maps)

setwd("E:/SARE/RStudio/SARE_learning")




#Upload brook trout presence shapefile (rgdal package needed to do this)
library(rgdal)
BTrout <- readOGR("E:/SARE/Original_Data/Maxent_shapefiles", "IDFG_BrookTrout")

#check out first 6 rows of the shapefile (make sure it was uploaded correctly)
head(BTrout)

#Convert shapefile to a data frame (aka a table)
BTrout_df <- as.data.frame(BTrout)

#remove records without coordinates
BTrout_df <- BTrout_df %>%
  filter(!is.na(LON))%>%
  filter(!is.na(LAT))

#Retain only unique LAT/LON locations (removes duplicates)
BTrout_df <- BTrout_df %>%
  distinct(LON, LAT, .keep_all=TRUE)




###Correcting for bias#### (May not have to do this step. Check against environmental rasters)

#set projection as USA Contiguous Albers Equal Area Conic

projection <- "ESRI:102003"
#make a copy of the brook trout dataframe from above
BTrout_df_cor <- BTrout_df
#need to specify as spatial points
coordinates(BTrout_df_cor) <- ~LON+LAT
projection(BTrout_df_cor) <- CRS('+proj=longlat +datum=NAD83')
BTrout_df_cor <- spTransform(BTrout_df_cor, projection)


#create a raster with the same extent from spatial points file
r <- raster(BTrout_df_cor)

# set the resolution of the cells to 4500 meters (this seemed to reduce most of the points to one per raster cell when I checked in ArcMap)
res(r) <- 4500

# expand (extend the extent) of the RasterLayer a little. We are saying to take this raster, and add 10000 meters to its extent. Again, this will vary with species and how representative you think the point file is.
r <- extend(r, extent(r)+10000)

# sample random points
BTrout_df1 <- gridSample(BTrout_df_cor, r, n=1)

#turn the raster to a polygon for plotting
p <- rasterToPolygons(r)

#plot the polygon
plot(p, border='gray')

#add the points
points(BTrout_df_cor)

# compare to the selected points in green
points(BTrout_df1, cex=1, col='darkgreen', pch='x')

#now take those sampled points, turn them into a dataframe
BTrout_df1 <- as.data.frame(BTrout_df1)

#this is now our final points
BTrout_df_final <- BTrout_df1

#export points as csv to check out in ArcMap
write.csv(BTrout_df_final, file = "E:/SARE/RStudio/SARE_learning/Outputs/BTrout_df_final4500.csv")
