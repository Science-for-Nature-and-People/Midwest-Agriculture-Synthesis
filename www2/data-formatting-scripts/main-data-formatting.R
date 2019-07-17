library(readxl)   # for importing dataframe
library(DescTools)
library(forcats)
library(tidyverse)


#### Load data ####
# setwd(".")
# datapath <- "./data" # using relative path
#setwd("Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www2/data")
setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/www2/data")


# import data -> summary files
tillage <- read.csv("TillageMgmt_All_raw.csv", row.names=NULL, stringsAsFactors = FALSE)
tillage$group_level1 <- as.factor(tillage$group_level1)
tillage$group_level2 <- as.factor(tillage$group_level2)
tillage$group_level3 <- as.factor(tillage$group_level3)
tillage$sample_depth <- as.factor(tillage$sample_depth)
tillage$sample_year <- as.factor(tillage$sample_year)
tillage$Tillage_compare <- as.factor(tillage$Tillage_compare)
tillage$Paper_id <- as.factor(tillage$Paper_id)

#possibly drop a few cols###################################################################################
  #Tillage_1name, Tillage_2name, Tillage_1, Tillage_2, Trt1_name, Trt2_name, finelevel_group)

#covercrop <- read.csv("CC_FULL_Summary.csv", row.names = NULL, stringsAsFactors = FALSE)
#pestmgmt <- read.csv("PestMgmt_FULL_Summary.csv", row.names = NULL, stringsAsFactors = FALSE)
#levels(as.factor(pestmgmt$group_metric))
#pestmgmt <- pestmgmt[!(pestmgmt$group_metric == "Reduction in Vigor (Maize)"),]


#nutrient <- read.csv("NutrientMgmt_FULL_Summary.csv", row.names = NULL, stringsAsFactors = FALSE)
#nutrient$Review <- NULL
#nutrient$Review <- nutrient$Review2
#nutrient$Review2 <- NULL

#nutrient$Legend_1 <- if_else(nutrient$Legend_1 == "In-Row", paste("In-row"), nutrient$Legend_1)

#### Manipulate data ####
summary_all <- full_join(covercrop, pestmgmt)
summary_all <- full_join(summary_all, nutrient)
#

# change columns to factors
collist <- c("Review_id", "main_group", "group_metric", "Legend_1", "Legend_2", "Legend_3", "Group_RV", "Review")
summary_all[collist] <- lapply(summary_all[collist], factor)


# rearrange the data according to the new ordering defined above
summary_all <- summary_all %>% arrange(Legend_1)

# make a temporary new column that just combines the group_metric and the main group.
# This new column has all rows then.
# then we reorder this column, so that it will be organized by both facet(main group)
# and by group_metric
summary_all$group_metric_facet <- with(summary_all, paste(group_metric, main_group, sep = "_"))
# summary_all$group_metric_facet <- reorder.factor(summary_all$group_metric_facet, new.order = sort(unique(summary_all$group_metric_facet), decreasing = TRUE))

summary_all %>%
  group_by(Legend_1, Group_RV, Review) %>%
  mutate(group_metric_facet = fct_reorder(as.factor(group_metric_facet), mean_per_change1)) -> summary_all

summary_all

write.csv(summary_all, "data-for-app.csv", row.names = FALSE)
