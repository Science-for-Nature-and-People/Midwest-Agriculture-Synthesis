#Midwestern Agriculture Synthesis
#Figures

library("dplyr", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("tidyverse", lib.loc="~/R/win-library/3.4")
library("stringr", lib.loc="~/R/win-library/3.4")
library("stringi", lib.loc="~/R/win-library/3.4")
library("forcats", lib.loc="~/R/win-library/3.4")
library("ggplot2", lib.loc="~/R/win-library/3.4")

setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")


#cover Crop Review
#import data -> summary files
covercrops <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_data.csv", header=TRUE, row.names = "X")
    cc_yield_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Yield_Summary.csv", header=TRUE, row.names = "X")
    cc_soil_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Soil_Summary.csv", header=TRUE, row.names = "X")
    cc_pest_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Pest_Summary.csv", header=TRUE, row.names = "X")
    cc_water_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Water_Summary.csv", header=TRUE, row.names = "X")
 
   
    ###Figure for Cover Crop Review: Soils####
    df <- cc_water_summary
    df$Cover_crop_diversity <- as.factor( df$Cover_crop_diversity)

    #Forest plot  
    j <- ggplot(df, aes(group_metric, mean_per_change, ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change)) +
          geom_pointrange() +
          geom_hline(yintercept=0, lty=2) +# add a dotted line at x=0 after flip
          coord_flip() + # flip coordinates (puts labels on y axis)
          #xlab("group metric") + ylab("percent difference (%)") + # use a white background
          labs(title ="Cover Crop Review", subtitle = "Water Movement", x = "", y = "percent difference (%)") + 
              theme_bw() +
          geom_point(aes(colour = Cover_crop_diversity)) + #color labeling of fine level groupings
          scale_fill_gradientn(colours=topo.colors(6)) +
          facet_grid(main_group ~ .,scales = "free")
          #+ geom_label(aes(label = num_papers), nudge_x = 10, nudge_y = 0) #add labels that include number of papers for each response and number of comparisons included in statistic
    
    #need to add labels for each cover crop diversity grouping. color the range of percent difference.
    #investigate extrodinarily large ranges
    #add num_comparisons and num_papers to the figure.
    #organize by soil metric type
    
    print(j)
    
    ggsave("CC_water.png")
    
     
