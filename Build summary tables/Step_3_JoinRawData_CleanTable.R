###Join all raw data files
library("readr", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")

##join unit datasets####
tillage <- read_csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_Group_units.csv")
pest <- read_csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/PestMgmt Review/Pest_Group_units.csv")
covercrop <- read_csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Cover Crop Review/CC_Group_units.csv")
nutrient <- read_csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Nutrient Review/Nutrient_Group_units.csv")

units <- full_join(tillage, pest)
units <- full_join(units, covercrop)
units <- full_join(units, nutrient)

write.csv(units, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/units.csv", row.names = FALSE)

# Main data sets
tillage_data <- read_csv(file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/TillageMgmt_ALL_raw.csv",
                     col_types = cols(Trt2_int = col_integer(),
                                          Trt1 = col_character(),
                                          Trt2 = col_character(),
                                          Trt1_int2 = col_character(),
                                          Trt2_int2 = col_character(),
                                          Trt1_details = col_character(),
                                          Trt2_details = col_character(),
                                          trt_specifics = col_character(),
                                          Tillage_1 = col_character(),
                                          Tillage_2 = col_character(),
                                          nutrient_groups = col_character()))
tillage_data$Res_key <- NULL

nutrient_data <- read_csv(file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/NutrientMgmt_ALL_raw.csv", 
                          col_types = cols(Res_key = col_integer(),
                                          Trt2_int = col_integer(),
                                           Trt1 = col_character(),
                                           Trt2 = col_character(),
                                           Trt1_int2 = col_character(),
                                           Trt2_int2 = col_character(),
                                           Trt1_details = col_character(),
                                           Trt2_details = col_character(),
                                           #trt_specifics = col_character(),
                                           #Tillage_1 = col_character(),
                                           #Tillage_2 = col_character(),
                                           nutrient_groups = col_character()))
nutrient_data$Res_key <- NULL
nutrient_data$Review_id <- "Nutrient Management"


covercrop_data <- read_csv (file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/CC_All_raw.csv", 
                            col_types = cols(Trt2_int = col_integer(),
                                             Trt1 = col_character(),
                                             Trt2 = col_character(),
                                             Trt1_int2 = col_character(),
                                             Trt2_int2 = col_character(),
                                             Trt1_details = col_character(),
                                             Trt2_details = col_character()))
                                             #trt_specifics = col_character(),
                                             #Tillage_1 = col_character(),
                                             #Tillage_2 = col_character()))
                                             #nutrient_groups = col_character()))

covercrop_data$Res_key <- NULL
covercrop_data$Review_id <- "Cover Crop"


pestmgmt_data <- read_csv (file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/PestMgmt_ALL_raw.csv", 
                            col_types = cols(RV_depth = col_character(),
                                             Trt1_name = col_character(),
                                             Trt2_name = col_character(),
                                             Trt2_int = col_integer(),
                                             Trt1 = col_character(),
                                             Trt2 = col_character(),
                                             Trt1_int2 = col_character(),
                                             Trt2_int2 = col_character(),
                                             Trt1_details = col_character(),
                                             Trt2_details = col_character()))
                                            #trt_specifics = col_character(),
                                            #Tillage_1 = col_character(),
                                            #Tillage_2 = col_character()))
                                            #nutrient_groups = col_character()))


pestmgmt_data$Res_key <- NULL
pestmgmt_data$Review_id <- "Early Season Pest Management"

all_data <- full_join(tillage_data, covercrop_data)

all_data <- full_join(all_data, nutrient_data)

all_data <- full_join(all_data, pestmgmt_data)

all_data$Res_key <- rownames(all_data)



unique(all_data$Review)

all_data2 <- all_data %>% 
              select (Res_key,
                      Review,
                      Paper_id,
                      Duration,
                      Loc_multi_results,
                      Response_var,
                      RV_depth,
                      RV_year,
                      RV_trtspecifics,
                      Response_var_units,
                      Stat_test,
                      Stat_type,
                      Trt_1name,
                      Trt1_details,
                      Trt_2name,
                      Trt2_details,
                      finelevel_group,
                      Trt1,
                      Trt1_int,
                      Trt1_int2,
                      Trt1_value,
                      Trt2,
                      Trt2_int,
                      Trt2_int2,
                      Trt2_value,
                      significance,
                      per_change,
                      actual_diff,
                      group_level1,
                      group_level2,
                      group_level3,
                      sample_depth,
                      sample_year,
                      Trt_compare,
                      Tillage_1,
                      Tillage_2,
                      trt_specifics,
                      nutrient_groups,
                      cc_group1,
                      cc_group2, 
                      pm_group1,
                      pm_group2
              )
              


write.csv(all_data2, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/ALL_raw.csv", row.names = FALSE)
