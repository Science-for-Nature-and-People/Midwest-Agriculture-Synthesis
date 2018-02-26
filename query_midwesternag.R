#Midwestern Ag Synthesis - SNAPP
#Database Querying


library(DBI)
library(dplyr)
library(dbplyr)
library(RSQLite)
library(readxl)


setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")

#import dataframes
Ref <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Reference")

ExpD_Loc <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "ExpD_Location")

CashCrop <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "CashCrop")

CoverCrop <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "CoverCrop")

Results <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Results")


