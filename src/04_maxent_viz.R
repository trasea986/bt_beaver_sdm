# create csv files for the tables and then create image files of the maps
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
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)

#load model and prediction raster
climate_model <- readRDS(file = "./outputs/model_nobeaver.RDS")
climate_raster <- readRDS(file = './outputs/model_nobeaver_predict.RDS')
climate_results <- as.data.frame(climate_model@results)
beaver_model <- readRDS(file = "./outputs/model_onlybeaver.RDS")
beaver_raster <- readRDS(file = './outputs/model_onlybeaver_predict.RDS')
beaver_results <- as.data.frame(beaver_model@results)
combo_model <- readRDS(file = "./outputs/model_all.RDS")
combo_raster <- readRDS(file = './outputs/model_all_predict.RDS')
combo_results <- as.data.frame(combo_model@results)

#first is a csv table of the AUC values for each best model and worst model
#need custom mincol
min.col <- function(m, ...) max.col(-m, ...)
#now figure out which columns to keep from results df
colnames(as.data.frame(climate_model@results))[max.col(as.data.frame(climate_model@results)[c("Test.AUC"),],ties.method="first")]
colnames(as.data.frame(climate_model@results))[min.col(as.data.frame(climate_model@results)[c("Test.AUC"),],ties.method="first")]

colnames(as.data.frame(beaver_model@results))[max.col(as.data.frame(beaver_model@results)[c("Test.AUC"),],ties.method="first")]
colnames(as.data.frame(beaver_model@results))[min.col(as.data.frame(beaver_model@results)[c("Test.AUC"),],ties.method="first")]

colnames(as.data.frame(combo_model@results))[max.col(as.data.frame(combo_model@results)[c("Test.AUC"),],ties.method="first")]
colnames(as.data.frame(combo_model@results))[min.col(as.data.frame(combo_model@results)[c("Test.AUC"),],ties.method="first")]

#row 8 is AUC
AUC_table <- cbind(combo_results$species_5[8], combo_results$species_9[8], climate_results$species_5[8], climate_results$species_9[8], beaver_results$species_8[8], beaver_results$species_1[8])

colnames(AUC_table) <- c("Combination_Max", "Combination_Min", "Climate_Max", "Climate_Min", "Beaver_Max", "Beaver_Min")

AUC_table <- t(AUC_table)

Model <- row.names(AUC_table)

AUC_table <- cbind(Model, AUC_table)

colnames(AUC_table) <- c("Model", "AUC")
write.csv(AUC_table, './outputs/AUC_table.csv', row.names = FALSE)

#next is the table of the best variable contribution, combo model and species 5
best_overall <- cbind(row.names(combo_results), combo_results$species_5)

contribution <- best_overall[21:30,]
colnames(contribution) <- c("Variable", "Permuatation Importance")
write.csv(contribution, './outputs/contribution_table.csv', row.names = FALSE)

#last step then is visualization
#load in the counties for mapping
#convert to spatial to change the projection, then go back to dataframe for ggplot
states_plot <- c("idaho")
dmap <- map("state", regions=states_plot, col="transparent", plot=FALSE, fill = TRUE)
area_poly <- map2SpatialPolygons(dmap, IDs=dmap$names, , proj4string=CRS("+proj=longlat +datum=WGS84"))

counties <- map_data("county")
county_sub <- subset(counties, region %in% c("idaho"))

#equal area looks a little funky when only looking at ID
combo_raster <- projectRaster(combo_raster, crs = area_poly)
combo_df <- as.data.frame(combo_raster, xy=TRUE)

combo_plot <- ggplot() + 
  geom_raster(data = combo_df, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradient(name = "ENM Value", low = "grey",high = "darkblue",
                      guide = "colourbar",
                      aesthetics = "fill",
                      na.value = "white") +
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + #darkgrey county lines
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Combination") +
  theme_classic(base_size = 16)

ggsave('./outputs/combo_plot.tiff', plot = combo_plot, height = 8, width = 8, units = "in")


beaver_raster <- projectRaster(beaver_raster, crs = area_poly)
beaver_df <- as.data.frame(beaver_raster, xy=TRUE)

beaver_plot <- ggplot() + 
  geom_raster(data = beaver_df, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradient(name = "ENM Value", low = "grey",high = "darkblue",
                      guide = "colourbar",
                      aesthetics = "fill",
                      na.value = "white") +
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + #darkgrey county lines
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Beaver Only") +
  theme_classic(base_size = 16)

ggsave('./outputs/beaver_plot.tiff', plot = beaver_plot, height = 8, width = 8, units = "in")


climate_raster <- projectRaster(climate_raster, crs = area_poly)
climate_df <- as.data.frame(climate_raster, xy=TRUE)

climate_plot <- ggplot() + 
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradient(name = "ENM Value", low = "grey",high = "darkblue",
                      guide = "colourbar",
                      aesthetics = "fill",
                      na.value = "white") +
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + #darkgrey county lines
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("No Beaver") +
  theme_classic(base_size = 16)

ggsave('./outputs/climate_plot.tiff', plot = climate_plot, height = 8, width = 8, units = "in")
