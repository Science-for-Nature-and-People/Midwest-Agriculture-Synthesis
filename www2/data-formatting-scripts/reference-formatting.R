#####AgEvidence - SNAPP Managing Soil Carbon#####
#####Citations Datatable######

library("readxl")
library("dplyr")
library("stringr")
library("stringi")
library("tidyverse")

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/www2/data")


#import data
ref_till <- read.csv("Tillage_Ref.csv",row.names = NULL)

ref_till <- ref_till %>% select(Paper_id:Pages)

df <- ref_till
#df <- full_join(ref_cc, ref_pest)
#df <- full_join(df, ref_nutrient)


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
