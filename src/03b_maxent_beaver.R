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
files <- list.files(path ='./Data_112821/Maxent_Raster_SameExtent_OnlyBeaver/Maxent_Raster_SameExtent_OnlyBeaver', pattern='*.tif$',all.files = TRUE, full.names = TRUE)

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

#really cor picking 1

predictors_final <- predictors[[1]]
plot(predictors_final)


#project into USA Contiguous Albers Equal Area Conic
Dam_BldCa_final  <- projectRaster(predictors_final, crs = projection)

#once done cropping and reprojecting put into raster stack
predictors_maxent <- stack(Dam_BldCa_final)


#next up is to remove any points with NA predictor variable values.
coordinates(pointdata) <- ~LON+LAT
rast_values <- raster::extract(predictors_maxent, pointdata)
pointdata <- as.data.frame(pointdata)
pointdata <- cbind(pointdata,rast_values)

#17 points need to be dropped
sum(!complete.cases(pointdata))

pointdata <- pointdata[complete.cases(pointdata), ]

#maxent only wants points
pointdata <- pointdata %>% dplyr::select('LON','LAT')

#find explaination of these values in the help file of the maxent .jar file
model <- maxent(x=predictors_maxent, p=pointdata, args=c(
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
  'jackknife=false',
  'askoverwrite=false',
  'replicates=10',
  'replicatetype=crossvalidate'),
  path = './outputs/maxent_outputs_OnlyBeaver')

#threads = 4 was used to match 4 core of my home computer
#defaultprevalence=0.5 used but unsure if correct

model

#determine which model had the best AUC, note that first in sequence is 0
colnames(as.data.frame(model@results))[max.col(as.data.frame(model@results)[c("Test.AUC"),],ties.method="first")]

#species_8 model is best
model_8 <- model@models[[9]]

#plots variable contribution
plot(model_8)

#shows response curves
response(model_8)

#how to predict distribution across the landscape (not sure if i need this to answer my question)
predict_all <- predict(predictors_maxent, model_8, progress = 'text')

#view map
plot(predict_all)

#write the maxent model and the raster
saveRDS(model, file = "./outputs/model_onlybeaver.RDS")
saveRDS(predict_all, file = './outputs/model_onlybeaver_predict.RDS')