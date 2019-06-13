#####AgEvidence - SNAPP Managing Soil Carbon#####
#####Citations Datatable######

library("readxl")
library("dplyr")
library("stringr")
library("stringi")
library("tidyverse")

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/www/data")


#import data
ref_cc <- read.csv("CoverCrop_References.csv",row.names = NULL)
ref_pest <- read.csv("PestMgmt_References.csv",row.names = NULL)
ref_nutrient <- read.csv("NutrientMgmt_References.csv",row.names = NULL)
#ref_till <- read.csv("Tillage_References.csv")

ref_cc$DateRead <- NULL
ref_pest$DateRead <- NULL
ref_nutrient$DateRead <- NULL


ref_cc$Notes <- NULL
ref_pest$Notes <- NULL
ref_nutrient$Notes <- NULL

df <- full_join(ref_cc, ref_pest)
df <- full_join(df, ref_nutrient)


#####Build Citations####################################

df$citation <- df %>%
  str_glue_data(
    "{Authors} ({PubYear}). {Title}. {Journal}, {Volume_issue}: {Pages}. DOI: {DOI}"
  )

df$citation_short <- df %>%
  str_glue_data("{Authors_abbrev} ({PubYear})")

references <- df %>%
      select(Paper_id, citation, citation_short)

write.csv(references, "references-for-app.csv", row.names = FALSE)
