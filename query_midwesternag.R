#Midwestern Ag Synthesis - SNAPP
#Database Querying


library(DBI)
library(dplyr)
library(dbplyr)
library(RSQLite)
library(readxl)


setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")

#import dataframes
Ref <- read_excel("Synthesis_Database_Atwood.xlsx", sheet = "Reference")

ExpD_Loc <- read_excel("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview/Synthesis_Database_Atwood.xlsx", sheet = "ExpD_Location")

CashCrop <- read_excel("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview/Synthesis_Database_Atwood.xlsx", sheet = "CashCrop")

CoverCrop <- read_excel("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview/Synthesis_Database_Atwood.xlsx", sheet = "CoverCrop")

Results <- read_excel("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview/Synthesis_Database_Atwood.xlsx", sheet = "Results")


#create empty database with no tables
con = dbConnect(RSQLite::SQLite(), dbname = ":memory:")

#inspect empty database
dbListTables(con)

#load data into the database
dbWriteTable(con, "Ref", Ref)

#inspect loaded dataframe
dbListFields(con, "Ref")