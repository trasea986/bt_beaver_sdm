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
library(cowplot)

projection <- "ESRI:102003"

#load model and prediction rasters
climate_model <- readRDS(file = "./outputs/model_nobeaver.RDS")
climate_raster <- readRDS(file = './outputs/model_nobeaver_predict.RDS')
climate_results <- as.data.frame(climate_model@results)
beaver_model <- readRDS(file = "./outputs/model_onlybeaver.RDS")
beaver_raster <- readRDS(file = './outputs/model_onlybeaver_predict.RDS')
beaver_results <- as.data.frame(beaver_model@results)
combo_model <- readRDS(file = "./outputs/model_all.RDS")
combo_raster <- readRDS(file = './outputs/model_all_predict.RDS')
combo_results <- as.data.frame(combo_model@results)

#bull trout
climate_model_bull <- readRDS(file = "./outputs/model_nobeaver_bull.RDS")
climate_raster_bull <- readRDS(file = './outputs/model_nobeaver_predict_bull.RDS')
climate_results_bull <- as.data.frame(climate_model_bull@results)
beaver_model_bull <- readRDS(file = "./outputs/model_onlybeaver_bull.RDS")
beaver_raster_bull <- readRDS(file = './outputs/model_onlybeaver_predict_bull.RDS')
beaver_results_bull <- as.data.frame(beaver_model_bull@results)
combo_model_bull <- readRDS(file = "./outputs/model_all_bull.RDS")
combo_raster_bull <- readRDS(file = './outputs/model_all_predict_bull.RDS')
combo_results_bull <- as.data.frame(combo_model_bull@results)

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
AUC_table <- cbind(combo_results$species_6[8], combo_results$species_0[8], climate_results$species_6[8], climate_results$species_0[8], beaver_results$species_8[8], beaver_results$species_1[8])

colnames(AUC_table) <- c("Combination_Max", "Combination_Min", "Climate_Max", "Climate_Min", "Beaver_Max", "Beaver_Min")

#transpose and add names
AUC_table <- t(AUC_table)
Model <- row.names(AUC_table)
AUC_table <- cbind(Model, AUC_table)
colnames(AUC_table) <- c("Model", "AUC")
AUC_table <- as.data.frame(AUC_table)
AUC_table$Species <- c('Brook Trout')

#now repeat for bull trout and end with rbind
colnames(as.data.frame(climate_model_bull@results))[max.col(as.data.frame(climate_model_bull@results)[c("Test.AUC"),],ties.method="first")]
colnames(as.data.frame(climate_model_bull@results))[min.col(as.data.frame(climate_model_bull@results)[c("Test.AUC"),],ties.method="first")]

colnames(as.data.frame(beaver_model_bull@results))[max.col(as.data.frame(beaver_model_bull@results)[c("Test.AUC"),],ties.method="first")]
colnames(as.data.frame(beaver_model_bull@results))[min.col(as.data.frame(beaver_model_bull@results)[c("Test.AUC"),],ties.method="first")]

colnames(as.data.frame(combo_model_bull@results))[max.col(as.data.frame(combo_model@results)[c("Test.AUC"),],ties.method="first")]
colnames(as.data.frame(combo_model_bull@results))[min.col(as.data.frame(combo_model@results)[c("Test.AUC"),],ties.method="first")]

#row 8 is AUC
AUC_table_bull <- cbind(combo_results_bull$species_6[8], combo_results_bull$species_0[8], climate_results_bull$species_8[8], climate_results_bull$species_4[8], beaver_results_bull$species_2[8], beaver_results_bull$species_7[8])

colnames(AUC_table_bull) <- c("Combination_Max", "Combination_Min", "Climate_Max", "Climate_Min", "Beaver_Max", "Beaver_Min")

#transpose and add names
AUC_table_bull <- t(AUC_table_bull)
Model_bull <- row.names(AUC_table_bull)
AUC_table_bull <- cbind(Model_bull, AUC_table_bull)
colnames(AUC_table_bull) <- c("Model", "AUC")
AUC_table_bull <- as.data.frame(AUC_table_bull)
AUC_table_bull$Species <- c('Bull Trout')

AUC_table_final <- rbind(AUC_table, AUC_table_bull)
write.csv(AUC_table_final, './outputs/AUC_table.csv', row.names = FALSE)



#next is the table of the best variable contribution, combo model and species 6
best_overall <- cbind(row.names(combo_results), combo_results$species_6)
contribution <- best_overall[19:26,]
colnames(contribution) <- c("Variable", "Permuatation Importance")
contribution <- as.data.frame(contribution)
contribution$Species <- c('Brook Trout')

#next is the table of the best variable contribution for combo model and species 6
best_overall_bull <- cbind(row.names(combo_results_bull), combo_results_bull$species_6)
contribution_bull <- best_overall_bull[19:26,]
colnames(contribution_bull) <- c("Variable", "Permuatation Importance")
contribution_bull <- as.data.frame(contribution_bull)
contribution_bull$Species <- c('Bull Trout')

contribution_final <- rbind(contribution, contribution_bull)

write.csv(contribution_final, './outputs/contribution_table.csv', row.names = FALSE)

#one last check of fit: extract cell values from the prediction rasters
brook_points <- read.csv("./data/BTrout_df_final4500.csv")
bull_points <- read.csv("./data/BullTrout_df_final4500.csv")

#extract from prediction raster
combo_point_val <- raster::extract(combo_raster, brook_points[,2:3])
climate_point_val <- raster::extract(climate_raster, brook_points[,2:3])
beaver_point_val <- raster::extract(beaver_raster, brook_points[,2:3])
combo_point_val_bull <- raster::extract(combo_raster_bull, bull_points)
climate_point_val_bull <- raster::extract(climate_raster_bull, bull_points)
beaver_point_val_bull <- raster::extract(beaver_raster_bull, bull_points)

#next, add in species column and column with "type" to each one, then rbind
combo_point_df <- cbind(combo_point_val, "Brook Trout", "Combo")
climate_point_df <- cbind(climate_point_val, "Brook Trout", "Climate")
beaver_point_df <- cbind(beaver_point_val, "Brook Trout", "Beaver")
combo_point_df_bull <- cbind(combo_point_val_bull, "Bull Trout", "Combo")
climate_point_df_bull <- cbind(climate_point_val_bull, "Bull Trout", "Climate")
beaver_point_df_bull <- cbind(beaver_point_val_bull, "Bull Trout", "Beaver")

enm_vals <- rbind(combo_point_df, climate_point_df, beaver_point_df, combo_point_df_bull, climate_point_df_bull, beaver_point_df_bull)

enm_vals <- as.data.frame(enm_vals)

colnames(enm_vals) <- c('ENM', 'Species', 'Type')

enm_vals$ENM <- as.numeric(enm_vals$ENM)
enm_vals$Species <- as.factor(enm_vals$Species)
enm_vals$Type <- as.factor(enm_vals$Type)

enm_summary <- enm_vals %>%
  group_by(Species, Type) %>%
  summarise(mean = mean(ENM, na.rm = TRUE),
            sd = sd(ENM, na.rm = TRUE))

#fit the two-way ANOVA model to see if significance difference in ENM values. Nope.
model <- aov(ENM ~ Species * Type, data = enm_vals)
#view the model output
summary(model)  
#tukey
TukeyHSD(model, conf.level=.95) 

#plot ENM values
ggplot(enm_summary, aes(x=Type, y=mean, fill=Species)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  labs(x="Model", y = "ENM Value")+  
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  theme_classic() +
  scale_fill_manual(values=c('#3a5a79','#a1a2a0'))
#calculate difference raster

#last step then is visualization
#going to plot contribution

#going to plot Idaho combination maps across two species

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
  ggtitle("Brook Trout") +
  coord_fixed()+
  theme_classic(base_size = 16) + theme (legend.position = "none")

combo_raster_bull <- projectRaster(combo_raster_bull, crs = area_poly)
combo_df_bull <- as.data.frame(combo_raster_bull, xy=TRUE)

combo_plot_bull <- ggplot() + 
  geom_raster(data = combo_df_bull, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradient(name = "ENM Value", low = "grey",high = "darkblue",
                      guide = "colourbar",
                      aesthetics = "fill",
                      na.value = "white") +
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + #darkgrey county lines
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Bull Trout") +
  coord_fixed()+
  theme_classic(base_size = 16) + theme (legend.position = "none")

legend <- get_legend(
  combo_plot + 
    scale_fill_gradient(name = "ENM Value", low = "grey",high = "darkblue",
                        guide = "colourbar",
                        aesthetics = "fill",
                        na.value = "white") +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "right"))
  
combo_plot_final <- plot_grid(combo_plot, combo_plot_bull, legend, ncol = 3, rel_widths = c(1,1,.25))


ggsave('./outputs/combo_plot.tiff', plot = combo_plot_final, height = 8, width = 11, units = "in")



#ENM values + points used in the model
#need to turn points to lat/lon
brook_points <- read.csv("./data/BTrout_df_final4500.csv")
bull_points <- read.csv("./data/BullTrout_df_final4500.csv")
coordinates(brook_points) <- ~LON+LAT
projection(brook_points) <- CRS(projection)
#reproject to the CRS of raster image
brook_points <- spTransform(brook_points, crs(combo_raster))
brook_points <- as.data.frame(brook_points)

coordinates(bull_points) <- ~NewLong+NewLat
projection(bull_points) <- CRS(projection)
#reproject to the CRS of raster image
bull_points <- spTransform(bull_points, crs(combo_raster))
bull_points <- as.data.frame(bull_points)

combo_plot <- ggplot() + 
  geom_raster(data = combo_df, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradient(name = "ENM Value", low = "grey",high = "darkblue",
                      guide = "colourbar",
                      aesthetics = "fill",
                      na.value = "white") +
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = brook_points, aes(x = LON, y = LAT), color = "yellow", shape = 1) +
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Brook Trout") +
  coord_fixed()+
  theme_classic(base_size = 16) + theme (legend.position = "none")

#one bull trout point is likely wrong
bull_points <- subset(bull_points, NewLong < max(bull_points1$NewLong))

combo_plot_bull <- ggplot() + 
  geom_raster(data = combo_df_bull, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradient(name = "ENM Value", low = "grey",high = "darkblue",
                      guide = "colourbar",
                      aesthetics = "fill",
                      na.value = "white") +
  geom_point(data = bull_points, aes(x = NewLong, y = NewLat), color = "yellow", shape = 1) +
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + #darkgrey county lines
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Bull Trout") +
  coord_fixed()+
  theme_classic(base_size = 16) + theme (legend.position = "none")

legend <- get_legend(
  combo_plot + 
    scale_fill_gradient(name = "ENM Value", low = "grey",high = "darkblue",
                        guide = "colourbar",
                        aesthetics = "fill",
                        na.value = "white") +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "right"))

combo_plot_final <- plot_grid(combo_plot, combo_plot_bull, legend, ncol = 3, rel_widths = c(1,1,.25))


ggsave('./outputs/combo_plot_points.tiff', plot = combo_plot_final, height = 8, width = 11, units = "in")



#calculate differences when you add in beaver
climate_raster <- projectRaster(climate_raster, crs = area_poly)
beaver_dif <- combo_raster - climate_raster
climate_raster_bull <- projectRaster(climate_raster_bull, crs = area_poly)
beaver_dif_bull <- combo_raster_bull - climate_raster_bull
dif_df <- as.data.frame(beaver_dif, xy=TRUE)
dif_df_bull <- as.data.frame(beaver_dif_bull, xy=TRUE)

dif_plot <- ggplot() + 
  geom_raster(data = dif_df, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradient2(name = "ENM Change", low = "darkred",high = "darkblue",
                      guide = "colourbar",
                      aesthetics = "fill",
                      na.value = "white", midpoint = 0, mid = "white") +
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + #darkgrey county lines
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Brook Trout") +
  coord_fixed()+
  theme_classic(base_size = 16) + theme (legend.position = "none")

dif_plot_bull <- ggplot() + 
  geom_raster(data = dif_df_bull, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradient2(name = "ENM Change", low = "darkred",high = "darkblue",
                       guide = "colourbar",
                       aesthetics = "fill",
                       na.value = "white", midpoint = 0, mid = "white") +
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + #darkgrey county lines
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Bull Trout") +
  coord_fixed()+
  theme_classic(base_size = 16) + theme (legend.position = "none")

legend <- get_legend(
  dif_plot + 
    scale_fill_gradient2(name = "ENM Change", low = "darkred",high = "darkblue",
                         guide = "colourbar",
                         aesthetics = "fill",
                         na.value = "white", midpoint = 0, mid = "white") +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "right"))

dif_plot_final <- plot_grid(dif_plot, dif_plot_bull, legend, ncol = 3, rel_widths = c(1,1,.25))


ggsave('./outputs/dif_plot.tiff', plot = dif_plot_final, height = 8, width = 12, units = "in")

#plot with the points
dif_plot <- ggplot() + 
  geom_raster(data = dif_df, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradient2(name = "ENM Change", low = "darkred",high = "darkblue",
                       guide = "colourbar",
                       aesthetics = "fill",
                       na.value = "white", midpoint = 0, mid = "white") +
  geom_point(data = brook_points, aes(x = LON, y = LAT), color = "yellow", shape = 1) +
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + #darkgrey county lines
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Brook Trout") +
  coord_fixed()+
  theme_classic(base_size = 16) + theme (legend.position = "none")

dif_plot_bull <- ggplot() + 
  geom_raster(data = dif_df_bull, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradient2(name = "ENM Change", low = "darkred",high = "darkblue",
                       guide = "colourbar",
                       aesthetics = "fill",
                       na.value = "white", midpoint = 0, mid = "white") +
  geom_point(data = bull_points, aes(x = NewLong, y = NewLat), color = "yellow", shape = 1) +
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + #darkgrey county lines
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Bull Trout") +
  coord_fixed()+
  theme_classic(base_size = 16) + theme (legend.position = "none")

legend <- get_legend(
  dif_plot + 
    scale_fill_gradient2(name = "ENM Change", low = "darkred",high = "darkblue",
                         guide = "colourbar",
                         aesthetics = "fill",
                         na.value = "white", midpoint = 0, mid = "white") +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "right"))

dif_plot_final <- plot_grid(dif_plot, dif_plot_bull, legend, ncol = 3, rel_widths = c(1,1,.25))


ggsave('./outputs/dif_plot_points.tiff', plot = dif_plot_final, height = 8, width = 12, units = "in")