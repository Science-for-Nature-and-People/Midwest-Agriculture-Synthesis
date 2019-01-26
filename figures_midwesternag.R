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


setwd(".")
datapath <- "/Users/LWA/Desktop/github/midwesternag_synthesis/" 


#cover Crop Review
#import data -> summary files
summary_all <- read.csv("PestMgmt Review/PestMgmt_FULL_Summary.csv", header=TRUE, row.names = "X")
              #Cover Crop data
              #read.csv("www/data/CoverCrop_Summary.csv", header=TRUE, row.names = "X")
    

    #cc_yield_summary <- read.csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Yield_Summary.csv", header=TRUE, row.names = "X")
    #cc_soil_summary <- read.csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Soil_Summary.csv", header=TRUE, row.names = "X")
    #cc_pest_summary <- read.csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Pest_Summary.csv", header=TRUE, row.names = "X")
    #cc_water_summary <- read.csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Water_Summary.csv", header=TRUE, row.names = "X")
 
   
    ###Figures ####
    df <- summary_all[summary_all$Group_RV == "Pest Regulation",]
        df<- drop_na(df)
      
              # for Cover Crop, Pest Regulation with one very large outlier (>1000%)      
            #df <- df[df$group_metric != "Seedcorn Maggot (#)",]
    levels(df$Legend_1) <- as.factor( df$Legend_1)
    df$group_metric <- as.factor(df$group_metric)
    df <- df[with(df, order(Cover_crop_diversity, group_metric)),]
    
    #Pest Management
    df <- df[!(df$group_metric %in% "Soybean Aphids (Cumulative Aphid Days)"),]
   
   #Forest plot  
    j <- ggplot(df, aes(group_metric, mean_abundance_change, ymin = mean_abundance_change-sem_abundance_change, ymax = mean_abundance_change +sem_per_change)) +
          geom_pointrange() +
      geom_errorbar(aes(ymin = mean_abundance_change-sem_abundance_change, ymax = mean_abundance_change +sem_abundance_change, width=.1)) +
          geom_hline(yintercept=0, lty=2) +# add a dotted line at x=0 after flip
          coord_flip() + # flip coordinates (puts labels on y axis)
          #xlab("group metric") + ylab("percent difference between control and treatment (%)") + # use a white background
          labs(title ="Early Season Pest Management Review", subtitle = "Pest Regulation", x = "", y = "percent difference between control and treatment (%)") + 
      #scale_fill_discrete(breaks=c("Monoculture","Mixture (2 Spp.)","Mixture (3+ Spp.)")) +        
      theme_bw() +
      #theme(legend.position = "right") +
          geom_point( aes(colour = Legend_1)) + #color labeling of fine level groupings
          facet_grid(main_group ~., scales = "free", space = "free") +
      theme(strip.text.y = element_text(angle = 0))
    
 print(j)  
    
    
    
    
    ggsave("Cover Crop_pest.png")
    
    