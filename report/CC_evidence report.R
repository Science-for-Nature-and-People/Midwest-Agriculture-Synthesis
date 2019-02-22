# Build Conservation Evidence Report
#Cover Crop Review Summary file

library("dplyr", lib.loc="~/R/win-library/3.5")
library("readxl", lib.loc="~/R/win-library/3.5")
library("tidyverse", lib.loc="~/R/win-library/3.5")
library("stringr", lib.loc="~/R/win-library/3.5")
library("stringi", lib.loc="~/R/win-library/3.5")



#Filter Cover Crop Review articles and identify Paper_id #'s that we can then use to filter summary dataframe
setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/")

df <- read.csv("CC_report_summary.csv", header=TRUE, row.names = "X")
colnames (df)

#Descriptors for querying dataset and producing report

#"Paper_id" <- unique identification number
#"Year_result" <- total number of years study was conducted
#"Group_finelevel"<- Cover crop diversity classification (monoculture, mixture 2 spp, mixture 3+ spp) 
#"Loc_multi_results" 
#"Trt_id1" <- Treatment Identification #
#"Trt1_interaction" <- Identification # of interacting treatment (if there was a treatment interaction)
#"Trt1_interaction2"     <- Identification # of interacting treatment (if there was a second treatment interaction)
#"Trt_id1description" <- Description of treatment 1
#"Trt_id2" <- Treatment Identification #
#"Trt2_interaction" <- Identification # of interacting treatment (if there was a treatment interaction)
#"Trt2_interaction2"    <- Identification # of interacting treatment (if there was a second treatment interaction)
#"Trt_id2description" <- Description of treatment 2


#Rename grouping columns so that they are more obvious
  #group_level 1, 2, 3...?
#"Response_var" <- name of reponse variable measured
#"Group_RV"
#"group_metric"
#"main_group"             
#"Group_finelevel"

#Add back metric grouping information!

#"introduction" <- introduction statement for report
#"citation_short" <- abridged citation "last name (year)" for report
#"results_short" <- condensed results statement for report
#"methods"  <- methods statement for report             
#"Reviewers_results_long" <- comprehensive results statement for report

#"citation" <- full citation for report

#filter dataset based on group(s) of interest and then print summary
#Loop through all grouping levels and print summary paragraph
#don't forget citations that go with each section!

df$Group_RV <- as.factor(df$Group_RV)
df$main_group <- as.factor(df$main_group)
df$group_metric <- as.factor(df$group_metric)
df$Group_finelevel <- as.factor(df$Group_finelevel)

levels(df$Group_RV)
#"Crop Production" "Pest Regulation" "Soil" "Water" 
levels(df$main_group)
#"Biological"           "Chemical"             "Crop Nitrogen Uptake" "Drainage"             "Environmental"       
# "Invertebrates"        "Pathogens"            "Physical"             "Runoff"               "Stand Count"         
# "Weeds"                "Yields" 
levels(df$group_metric)
levels(df$Group_finelevel)
  
report <- df %>%
  mutate(summary_statement = str_c(df$introduction, " ",df$results_short," ",df$methods," ",df$Reviewers_results_long, sep = "")) %>%
  group_by(Paper_id, Group_RV, main_group, group_metric, Group_finelevel) %>%
  summarise(summary_statement2 = paste(unique(summary_statement), collapse = " "))


#Export final methods summary statement
write.csv(report, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/report/www/data/CC_report.csv")

#use this file for report website and for printing final report
report$summary_statement2[report$Group_RV == "Soil"]

############################################################################################




report_print <- report %>%
              group_by(Group_RV, main_group, group_metric, Group_finelevel) %>%
              print(summary_statement2)
              #print(Group_RV) %>%
              group_by(main_group) %>%
              #print(main_group)
              group_by(group_metric) %>%
              group_by(Group_finelevel) %>%
              



print(report_1)


unique(report[report$Paper_id == 15,]$summary_statement)
(df$citation)
print(report)        
