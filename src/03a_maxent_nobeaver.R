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
files <- list.files(path ='./Data_112821/Maxent_Raster_SameExtent_NoBeaver/Maxent_Raster_SameExtent_NoBeaver', pattern='*.tif$',all.files = TRUE, full.names = TRUE)

files

#load the files
predictors <- stack(files)

#view the raster info for 1 file
#predictors$annu_velo

#view the rasters (NOTE: frag3to10 and fraggt10 look weird but their values seem correct)
#plot(predictors)

#take care of correlated layers (only lat and long)
extracted_vals <- extract(predictors, pointdata[2:3])

#convert to a data frame
extracted_points <- as.data.frame(extracted_vals)

#calculate the correlation among our variable at our points
mydata.cor <- cor(extracted_points, method = 'spearman', use = 'complete.obs')
corrplot(mydata.cor)

#iteratively remove correlated (normally cut-off is 0.8)
hc <- findCorrelation(mydata.cor, cutoff = 0.8)
hc = sort(hc)
predictors_final_list = extracted_points[,-c(hc)]

#now we have our list and we will cut out from the raster stack

predictors_final <- subset(predictors, names(predictors_final_list))
#plot(predictors_final)

#Note: annual_flow, Dam_BldCap, frag3to10, and TotDASqKm were cut out

#project into USA Contiguous Albers Equal Area Conic
annu_velo_final <- projectRaster(predictors_final$annu_velo, crs = projection)
Aug_temp_avg_final <- projectRaster(predictors_final$Aug_temp_avg, crs=projection)
brockdepmin_final <- projectRaster(predictors_final$brockdepmin, crs = projection)
frag3to10_final <- projectRaster(predictors_final$frag3to10, crs=projection)
fraggt10_final <- projectRaster(predictors_final$fraggt10, crs=projection)
ksat_avg_final <- projectRaster(predictors_final$ksat_avg, crs = projection)
LandUse_final <- projectRaster(predictors_final$LandUse, crs=projection)
MAXELEVSMO_final <- projectRaster(predictors_final$MAXELEVSMO, crs = projection)
slope_final <- projectRaster(predictors_final$slope, crs=projection)
VC_final <- projectRaster(predictors_final$Valley_Con, crs=projection)

#once done cropping and reprojecting put into raster stack
predictors_maxent <- stack(annu_velo_final,Aug_temp_avg_final,brockdepmin_final,frag3to10_final,fraggt10_final,ksat_avg_final,LandUse_final,MAXELEVSMO_final,slope_final,VC_final)


#next up is to remove any points with NA predictor variable values.
coordinates(pointdata) <- ~LON+LAT
rast_values <- raster::extract(predictors_maxent, pointdata)
pointdata <- as.data.frame(pointdata)
pointdata <- cbind(pointdata,rast_values)

#625 points need to be dropped
sum(!complete.cases(pointdata))

pointdata <- pointdata[complete.cases(pointdata), ]

#maxent only wants points
pointdata <- pointdata %>% dplyr::select('LON','LAT')

#find explaination of these values in the help file of the maxent .jar file
model <- maxent(x=predictors_maxent, p=pointdata, factors='Valley_Con', args=c(
  'maximumbackground=10000',
  'defaultprevalence=0.5',
  'betamultiplier=1',
  'plots=true',
  'pictures=true',
  'linear=true',
  'quadratic=true',
  'product=false',
  'threshold=false',
  'hinge=true',
  'threads=4',
  'responsecurves=true',
  'jackknife=true',
  'askoverwrite=false',
  'replicates=10',
  'replicatetype=crossvalidate'),
  path = './outputs/maxent_outputs_NoBeaver')

#threads = 4 was used to match 4 core of my home computer
#defaultprevalence=0.5 used but unsure if correct

model

#determine which model had the best AUC, note that first in sequence is 0
colnames(as.data.frame(model@results))[max.col(as.data.frame(model@results)[c("Test.AUC"),],ties.method="first")]

#species_5 model is best
model_5 <- model@models[[6]]

#plots variable contribution
plot(model_5)

#shows response curves
response(model_5)

#how to predict distribution across the landscape (not sure if i need this to answer my question)
predict_all <- predict(predictors_maxent, model_5, progress = 'text')

#view map
plot(predict_all)

#write the maxent model and the raster
saveRDS(model, file = "./outputs/model_nobeaver.RDS")
saveRDS(predict_all, file = './outputs/model_nobeaver_predict.RDS')