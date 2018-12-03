#Midwestern Agriculture Synthesis
#Figures

library("dplyr", lib.loc="~/R/win-library/3.5")
library("readxl", lib.loc="~/R/win-library/3.5")
library("tidyverse", lib.loc="~/R/win-library/3.5")
library("stringr", lib.loc="~/R/win-library/3.5")
library("stringi", lib.loc="~/R/win-library/3.5")
library("forcats", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")
library("colorRamps", lib.loc= "~/R/win-library/3.5")
library("colorspace", lib.loc= "~/R/win-library/3.5")

setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")


#cover Crop Review
#import data -> summary files
covercrops <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_data.csv", header=TRUE, row.names = "X")
    cc_yield_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Yield_Summary.csv", header=TRUE, row.names = "X")
    cc_soil_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Soil_Summary.csv", header=TRUE, row.names = "X")
    cc_pest_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Pest_Summary.csv", header=TRUE, row.names = "X")
    cc_water_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Water_Summary.csv", header=TRUE, row.names = "X")
 
   
    ###Figure for Cover Crop Review: Soils####
    df <- 
      #cc_soil_summary
      #cc_water_summary 
      cc_yield_summary
      #cc_pest_summary[cc_pest_summary$mean_per_change < 1000,]
    df$Cover_crop_diversity <- as.factor( df$Cover_crop_diversity)
    
    
    
   
   #Forest plot  
    j <- ggplot(df, aes(group_metric, mean_per_change, ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change)) +
          geom_pointrange() +
      geom_errorbar(aes(ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change, width=.1)) +
          geom_hline(yintercept=0, lty=2) +# add a dotted line at x=0 after flip
          coord_flip() + # flip coordinates (puts labels on y axis)
          #xlab("group metric") + ylab("percent difference (%)") + # use a white background
          labs(title ="Cover Crop Review", subtitle = "Crop Production", x = "", y = "percent difference (%)") + 
      #scale_fill_discrete(breaks=c("Monoculture","Mixture (2 Spp.)","Mixture (3+ Spp.)")) +        
      theme_bw() +
      #theme(legend.position = "right") +
          geom_point( aes(colour = Cover_crop_diversity)) + #color labeling of fine level groupings
          facet_grid(main_group ~ .,scales = "free")
    
 print(j)  
    
    
    
    
    ggsave("CC_yield.png")
    
    