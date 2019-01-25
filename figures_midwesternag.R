#test
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

#setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")
setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/PestMgmt Review")


#cover Crop Review
#import data -> summary files
CC_summary_all <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/CoverCrop_Summary.csv", header=TRUE, row.names = "X")
    

    #cc_yield_summary <- read.csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Yield_Summary.csv", header=TRUE, row.names = "X")
    #cc_soil_summary <- read.csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Soil_Summary.csv", header=TRUE, row.names = "X")
    #cc_pest_summary <- read.csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Pest_Summary.csv", header=TRUE, row.names = "X")
    #cc_water_summary <- read.csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Water_Summary.csv", header=TRUE, row.names = "X")
 
   
    ###Figures for Cover Crop Review####
    df <- CC_summary_all[CC_summary_all$Group_RV == "Pest Regulation",]
        df<- drop_na(df)
      
              # for Cover Crop, Pest Regulation with one very large outlier (>1000%)      
            #df <- df[df$group_metric != "Seedcorn Maggot (#)",]
    levels(df$Cover_crop_diversity2) <- as.factor( df$Cover_crop_diversity2)
    df$group_metric <- as.factor(df$group_metric)
    df <- df[with(df, order(Cover_crop_diversity, group_metric)),]
    
    
    
   
   #Forest plot  
    j <- ggplot(df, aes(group_metric, mean_per_change, ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change)) +
          geom_pointrange() +
      geom_errorbar(aes(ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change, width=.1)) +
          geom_hline(yintercept=0, lty=2) +# add a dotted line at x=0 after flip
          coord_flip() + # flip coordinates (puts labels on y axis)
          #xlab("group metric") + ylab("percent difference between control and treatment (%)") + # use a white background
          labs(title ="Cover Crop Review", subtitle = "Pest Regulation", x = "", y = "percent difference between control and treatment (%)") + 
      #scale_fill_discrete(breaks=c("Monoculture","Mixture (2 Spp.)","Mixture (3+ Spp.)")) +        
      theme_bw() +
      #theme(legend.position = "right") +
          geom_point( aes(colour = Cover_crop_diversity2)) + #color labeling of fine level groupings
          facet_grid(main_group ~., scales = "free", space = "free") +
      theme(strip.text.y = element_text(angle = 0))
    
 print(j)  
    
    
    
    
    ggsave("Cover Crop_pest.png")
    
    