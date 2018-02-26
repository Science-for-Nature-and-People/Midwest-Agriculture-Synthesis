#Midwestern Ag Synthesis - SNAPP
#Database Querying


library(DBI)
library(dplyr)
library(dbplyr)
library(RSQLite)
library(readxl)


setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")

#import dataframes
CC.div.Ref <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Reference")

CC.div.ExpD_Loc <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "ExpD_Location")

CC.div.CashCrop <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "CashCrop")

CC.div.CoverCrop <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "CoverCrop")

CC.div.Results <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Results")


