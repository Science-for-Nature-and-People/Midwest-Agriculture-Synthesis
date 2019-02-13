library(dplyr)    # for sorting and summarizing data
library(readxl)   # for importing dataframe
library(DescTools)
library(forcats)


#### Load data ####
setwd(".")
datapath <- "./data" # using relative path
#setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/www/data")


# import data -> summary files
covercrop <- read.csv("CC_FULL_Summary.csv", row.names = NULL, stringsAsFactors = FALSE)
pestmgmt <- read.csv("PestMgmt_FULL_Summary.csv", row.names = NULL, stringsAsFactors = FALSE)
nutrient <- read.csv("NutrientMgmt_FULL_Summary.csv", row.names = NULL, stringsAsFactors = FALSE)
nutrient$Review <- NULL
nutrient$Review <- nutrient$Review2
nutrient$Review2 <- NULL

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

write.csv(summary_all, "data-for-app.csv", row.names = FALSE)
