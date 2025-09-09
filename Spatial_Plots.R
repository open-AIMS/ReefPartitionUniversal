#Spatial plots

#Results section for PartitionPaper
setwd("C:/Users/vhaller/Documents/GitHub/ReefPartitionAllenAtlas")
# Source required scripts

library(tidyverse)
library(ggplot2)
library(sf)
library(terra)


annastheme = theme_bw()+
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.key.width=unit(1.2,"cm"),
        axis.title=element_text(size=14),
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        strip.text = element_text(size = 14, colour = "black"),
        strip.background = element_rect("white"))



# Output directory for saving results
directory <- "C:/Users/vhaller/OneDrive - Australian Institute of Marine Science/DecisionSupport/BiodiversityReefPartition/Test62500_noDepth2_2025_09_08" 
output_directory<-paste0(directory,"/plots")


#Load data
sites<-readRDS(paste(directory, "Spatial_withpolygonGeometry_PostProcessing.Rdata", sep="/"))

sites$habitat<-as.factor(sites$habitat)
sites%>%
  ggplot() + 
  geom_sf(aes(fill = habitat), linewidth = 0.02)+
  annastheme 
ggsave(paste(output_directory,"PolygonMap.png",sep="/"), width = 30, height = 12, units = "cm")


# Basic histogram reef area
mean_size = mean(sites$area)
ggplot(sites, aes(x=area)) + geom_histogram() +
  xlab("Polygon area (km2)") +
  ylab("Count of polygons") +
  geom_vline(xintercept = mean_size, linetype = 2, colour = "red") +
  annastheme
ggsave(paste(output_directory,"MeanPolygonSize.png",sep="/"), width = 30, height = 12, units = "cm")
