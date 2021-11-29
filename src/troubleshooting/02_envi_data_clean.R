library(tidyverse)
library(maptools)
library(rgbif)
library(dismo)
library(corrplot)
library(caret)
library(rJava)
library(sp)
library(hexbin)
library(patchwork)
library(RSQLite)
library(maps)
library(raster)
#load packages

#the projection will be USA Contiguous Albers Equal Area Conic
projection <- "ESRI:102003"

#Bring in the final brook trout point file
pointdata <- read.csv("./data/BTrout_df_final4500.csv")

#Look at the extent of the brook trout data
max(pointdata$LON)
min(pointdata$LON)

max(pointdata$LAT)
min(pointdata$LAT)

#set the extent WITH A BUFFER (add some)
ext <- extent(-1770000,-1140000,400000,1600000)

#list all the files that we have unzipped (pulls out the .tif files)
files <- list.files(path ='./data/Maxent_Raster_SameExtent/', pattern='*.tif$',all.files = TRUE, full.names = TRUE)

files

#load the files
predictors <- raster::stack(files)

#view the raster info for 1 file
predictors$annu_velo

#view the rasters (NOTE: frag3to10 and fraggt10 look weird but their values seem correct)
plot(predictors)

#take care of correlated layers (only lat and long)
extracted_vals <- terra::extract(predictors, pointdata[2:3])

#convert to a data frame
extracted_points <- as.data.frame(extracted_vals)

#calculate the correlation among our variable at our points
mydata.cor <- cor(extracted_points, method = 'pearson', use = 'complete.obs')
corrplot(mydata.cor, method = "number")

#iteratively remove correlated (normally cut-off is 0.8)
hc <- caret::findCorrelation(mydata.cor, cutoff = 0.80, exact = TRUE)
hc = sort(hc)
predictors_final_list = extracted_points[,-c(hc)]


#now we have our list and we will cut out from the raster stack

predictors_final <- subset(predictors, names(predictors_final_list))
#plot(predictors_final)


#double check correlation of plots
extracted_vals2 <- raster::extract(predictors_final, pointdata[2:3])
extracted_points2 <- as.data.frame(extracted_vals2)
mydata.cor2 <- cor(extracted_points2, method = 'spearman', use = 'complete.obs')
corrplot(mydata.cor2, method = "number")



#Note: annual_flow, Dam_BldCap, frag3to10, and TotDASqKm were cut out

#how to crop if needed
#annu_velo_crop <- crop(predictors_final$annu_velo, ext)
#check it
#plot(annu_velo_crop)
#continue for each variable

#project into USA Contiguous Albers Equal Area Conic
annu_velo_final <- projectRaster(predictors_final$annu_velo, crs = projection)
Aug_temp_avg_final <- projectRaster(predictors_final$Aug_temp_avg, crs=projection)
brockdepmin_final <- projectRaster(predictors_final$brockdepmin, crs = projection)
Dam_CompSz_final <- projectRaster(predictors_final$Dam_CompSz, crs=projection)
fraggt10_final <- projectRaster(predictors_final$fraggt10, crs=projection)
ksat_avg_final <- projectRaster(predictors_final$ksat_avg, crs = projection)
LandUse_final <- projectRaster(predictors_final$LandUse, crs=projection)
MAXELEVSMO_final <- projectRaster(predictors_final$MAXELEVSMO, crs = projection)
slope_final <- projectRaster(predictors_final$slope, crs=projection)
VC_final <- projectRaster(predictors_final$ValleyCon, crs=projection)

#once done cropping and reprojecting put into raster stack
predictors_maxent <- stack(annu_velo_final,Aug_temp_avg_final,brockdepmin_final,Dam_CompSz_final,fraggt10_final,ksat_avg_final,LandUse_final,MAXELEVSMO_final,slope_final,VC_final)

