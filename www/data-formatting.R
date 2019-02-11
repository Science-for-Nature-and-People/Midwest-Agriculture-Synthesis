library(dplyr)    # for sorting and summarizing data
library(readxl)   # for importing dataframe
library(DescTools)
library(forcats)


#### Load data ####
setwd(".")
datapath <- "./data" # using relative path


# import data -> summary files
covercrop <- read.csv(file.path(datapath, "CC_FULL_Summary.csv"), stringsAsFactors = FALSE)
pestmgmt <- read.csv(file.path(datapath, "PestMgmt_FULL_Summary2.csv"), stringsAsFactors = FALSE)
nutrient <- read.csv(file.path(datapath, "NutrientMgmt_FULL_Summary.csv"), stringsAsFactors = FALSE)

#### Manipulate data ####
summary_all <- full_join(covercrop, pestmgmt)
  summary_all$num_comparison3s <- NULL
summary_all <- full_join(summary_all, nutrient)
  summary_all$X <- NULL
  summary_all$X.1 <- NULL

# change columns to factors
collist <- c("Review_id", "main_group", "group_metric", "Legend_1", "Legend_2", "Legend_3", "Group_RV", "Review")
summary_all[collist] <- lapply(summary_all[collist], factor)
summary_all$Legend_1 <- recode(summary_all$Legend_1,
                               "Single species"="Monoculture", 
                               "Two species"="Mixture (2 Spp.)",
                               "Three or more species"="Mixture (3+ Spp.)"
)

# reorder the data for the legend
summary_all$Legend_1 <- reorder.factor(summary_all$Legend_1, new.order = c("Monoculture", "Mixture (2 Spp.)", "Mixture (3+ Spp.)", "Soil", "Foliage", "Seed", "Seed & Foliage"))

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
  mutate(group_metric_facet = fct_reorder(group_metric_facet, mean_per_change1)) -> summary_all

write.csv(summary_all,"data/data-for-app.csv")
