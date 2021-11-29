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
#load packages (not all needed)

#next up is to remove any points with NA predictor variable values.
coordinates(pointdata) <- ~LON+LAT
rast_values <- raster::extract(predictors_maxent, pointdata)
pointdata <- as.data.frame(pointdata)
pointdata <- cbind(pointdata,rast_values)

#625 points need to be dropped
sum(!complete.cases(pointdata))

pointdata <- pointdata[complete.cases(pointdata), ]


###if you want to export points and raster

#pointdata_df <- as.data.farm(pointdata)
#write.csv(pointdata_df, "./folder name/pointsfilename.csv, row.names = FALSE)
#writeRaster(predictors_maxent, filename = paste('.folder/, names(predictors_maxent)), bylayer = TRUE, format = 'raster)


#double check everything looks good and lines up
plot(predictors_maxent$annu_velo)
points(pointdata, pch = 8, col = "black", cex = 1)


#maxent only wants points
pointdata <- pointdata %>% dplyr::select('LON','LAT')

#find explaination of these values in the help file of the maxent .jar file
model <- maxent(x=predictors_maxent, p=pointdata, factors='ValleyCon', args=c(
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
  path = 'E:/SARE/RStudio/SARE_learning/Outputs/maxent_outputs')

#threads = 4 was used to match 4 core of my home computer
#defaultprevalence=0.5 used but unsure if correct

model

#determine which model had the best AUC, note that first in sequence is 0
colnames(as.data.frame(model@results))[max.col(as.data.frame(model@results)[c("Test.AUC"),],ties.method="first")]

#species_5 model is best

#plots variable contribution (not working?)
plot(model)

#shows response curves (not working?)
response(model)

#how to predict distribution across the landscape (not sure if i need this to answer my question)
predict <- predict(predictors_maxent, model, progress = 'text')

#view map
plot(predict)

#save output out of temporary location