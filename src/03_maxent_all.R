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
library(ENMeval)
library(ecospat)
library(rangeModelMetadata)

#the projection will be USA Contiguous Albers Equal Area Conic
projection <- "ESRI:102003"


# brook trout -------------------------------------------------------------

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
files <- list.files(path ='./Data_112821/Maxent_Raster_SameExtent/Maxent_Raster_SameExtent', pattern='*.tif$',all.files = TRUE, full.names = TRUE)

#there should be 14 variables
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
Dam_CompSz_final <- projectRaster(predictors_final$Dam_CompSz, crs=projection)
fraggt10_final <- projectRaster(predictors_final$fraggt10, crs=projection)
ksat_avg_final <- projectRaster(predictors_final$ksat_avg, crs = projection)
LandUse_final <- projectRaster(predictors_final$LandUse, crs=projection)
MAXELEVSMO_final <- projectRaster(predictors_final$MAXELEVSMO, crs = projection)
slope_final <- projectRaster(predictors_final$slope, crs=projection)
VC_final <- projectRaster(predictors_final$Valley_Con, crs=projection)

#once done cropping and reprojecting put into raster stack
predictors_maxent <- stack(annu_velo_final,Aug_temp_avg_final,brockdepmin_final,Dam_CompSz_final,fraggt10_final,ksat_avg_final,LandUse_final,MAXELEVSMO_final,slope_final,VC_final)


#next up is to remove any points with NA predictor variable values.
coordinates(pointdata) <- ~LON+LAT
rast_values <- raster::extract(predictors_maxent, pointdata)
pointdata <- as.data.frame(pointdata)
pointdata <- cbind(pointdata,rast_values)

#brockdepmin and ksat avg are NA for most points, so removing. They are both soil related

pointdata <- pointdata[,-c(1,6,9)]

#625 points need to be dropped if you keep those two. this looks better
sum(!complete.cases(pointdata))

pointdata <- pointdata[complete.cases(pointdata), ]

#maxent only wants points
pointdata <- pointdata %>% dplyr::select('LON','LAT')

#remove these two from the predictors_maxent stack
predictors_maxent <- predictors_maxent[[-c(3, 6)]]


#Next is to run ENMeval to check regularization multiplier values
#set up model list to test
tune_args_list  <- list(fc = c("L","Q","LQ","LQH", "H"), rm = 1:5)

#running enmeval independently to make watching progress easier
enmeval_results <- ENMevaluate(pointdata, predictors_maxent, n.bg=10000, tune.args = tune_args_list, partitions='checkerboard2', algorithm='maxnet')

eval <- eval.results(enmeval_results)

write.csv(eval, "./outputs/brook_trout_enmeval.csv")

#LQH rm value of 1 was the best

#model 4 is the best

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
  path = './outputs/maxent_outputs_all')

#threads = 4 was used to match 4 core of my home computer
#defaultprevalence=0.5 used but unsure if correct

model

#determine which model had the best AUC, note that first in sequence is 0
colnames(as.data.frame(model@results))[max.col(as.data.frame(model@results)[c("Test.AUC"),],ties.method="first")]

#species_6 model is best
model_6 <- model@models[[7]]

#plots variable contribution
plot(model_6)

#shows response curves
response(model_6)

#how to predict distribution across the landscape (not sure if i need this to answer my question)
predict_all <- predict(predictors_maxent, model_6, progress = 'text')

#view map
plot(predict_all)

#write the maxent model and the raster
saveRDS(model, file = "./outputs/model_all.RDS")
saveRDS(predict_all, file = './outputs/model_all_predict.RDS')



# bull trout --------------------------------------------------------------

#Now to do again with bull trout
#Bring in the final brook trout point file
pointdata <- read.csv("./data/BullTrout_df_final4500.csv")

#next up is to remove any points with NA predictor variable values.
coordinates(pointdata) <- ~NewLong+NewLat
rast_values <- raster::extract(predictors_maxent, pointdata)
pointdata <- as.data.frame(pointdata)
pointdata <- cbind(pointdata,rast_values)

#625 points need to be dropped if you keep those two. this looks better
sum(!complete.cases(pointdata))

pointdata <- pointdata[complete.cases(pointdata), ]

#maxent only wants points
pointdata <- pointdata %>% dplyr::select('NewLong','NewLat')

#Next is to run ENMeval to check regularization multiplier values
#set up model list to test
tune_args_list  <- list(fc = c("L","Q","LQ","LQH", "H"), rm = 1:5)

#running enmeval independently to make watching progress easier
enmeval_results <- ENMevaluate(pointdata, predictors_maxent, n.bg=10000, tune.args = tune_args_list, partitions='checkerboard2', algorithm='maxnet')

eval <- eval.results(enmeval_results)

write.csv(eval, "./outputs/bull_trout_enmeval.csv")

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
  path = './outputs/maxent_outputs_all_bull')

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
saveRDS(model, file = "./outputs/model_all_bull.RDS")
saveRDS(predict_all, file = './outputs/model_all_predict_bull.RDS')
