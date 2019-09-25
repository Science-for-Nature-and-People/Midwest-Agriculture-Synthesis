###Join all raw data files
library("readr", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")

##join unit datasets####
tillage <- read_csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/tillage Review/tillage_Group_units.csv")
pest <- read_csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/PestMgmt Review/Pest_Group_units.csv")
covercrop <- read_csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Cover Crop Review/CC_Group_units.csv")
nutrient <- read_csv(file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Nutrient Review/Nutrient_Group_units.csv")

units <- full_join(tillage, pest)
units <- full_join(units, covercrop)
units <- full_join(units, nutrient)

write.csv(units, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/units.csv", row.names = FALSE)


##join references####
tillage <- read_csv(file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Protocols/Files for protocol/Tillage_Refs.csv")
pest <- read_csv(file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Protocols/Files for protocol/PestMgmt_Refs.csv")
covercrop <- read_csv(file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Protocols/Files for protocol/Covercrop_Refs.csv")
nutrient <- read_csv(file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Protocols/Files for protocol/Nutrient_Refs.csv")

refs <- full_join(tillage, pest)
refs <- full_join(refs, covercrop)
refs <- full_join(refs, nutrient)

write.csv(refs, file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Protocols/Files for protocol/refs_all_expanded.csv", row.names = FALSE)



# Main data sets
tillage_data <- read_csv(file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/Tillage_raw.csv",
                     col_types = cols(    loc_multi_results = col_character(),
                                          trt2_int = col_integer(),
                                          trt1 = col_character(),
                                          trt2 = col_character(),
                                          trt1_int2 = col_character(),
                                          trt2_int2 = col_character(),
                                          trt1_details = col_character(),
                                          trt2_details = col_character(),
                                          effect_norm = col_character(),
                                          tillage_1 = col_character(),
                                          tillage_2 = col_character(), 
                                          sample_depth = col_character()))
 
nutrient_data <- read_csv(file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/Nutrient_raw.csv", 
                          col_types = cols(loc_multi_results = col_character(),
                                           trt2_int = col_integer(),
                                           trt1 = col_character(),
                                           trt2 = col_character(),
                                           trt1_int2 = col_character(),
                                           trt2_int2 = col_character(),
                                           trt1_details = col_character(),
                                           trt2_details = col_character(),
                                           effect_norm = col_character(),
                                           nutrient_groups = col_character(),
                                           sample_depth = col_character()))


covercrop_data <- read_csv (file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/Covercrop_raw.csv", 
                            col_types = cols(loc_multi_results = col_character(),
                                             trt2_int = col_integer(),
                                             trt1 = col_character(),
                                             trt2 = col_character(),
                                             trt1_int2 = col_character(),
                                             trt2_int2 = col_character(),
                                             trt1_details = col_character(),
                                             trt2_details = col_character(),
                                             trt2_int = col_integer(),
                                             effect_norm = col_character(),
                                             sample_depth = col_character()))
                                             


pestmgmt_data <- read_csv (file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/PestMgmt_raw.csv", 
                            col_types = cols(rv_depth = col_character(),
                                             loc_multi_results = col_character(),
                                             trt1_name = col_character(),
                                             trt2_name = col_character(),
                                             trt2_int = col_integer(),
                                             trt1 = col_character(),
                                             trt2 = col_character(),
                                             trt1_int2 = col_character(),
                                             trt2_int2 = col_character(),
                                             trt1_details = col_character(),
                                             trt2_details = col_character(),
                                             effect_norm = col_character(),
                                             sample_depth = col_character()))
                                            



all_data <- full_join(tillage_data, covercrop_data)

all_data <- full_join(all_data, nutrient_data)

all_data <- full_join(all_data, pestmgmt_data)

all_data$res_key <- rownames(all_data)



unique(all_data$Review)

all_data2 <- all_data %>% 
              select (res_key,
                      review,
                      paper_id,
                      duration,
                      loc_multi_results,
                      rv,
                      rv_depth,
                      rv_year,
                      rv_trtspecifics,
                      rv_units,
                      stat_test,
                      stat_type,
                      trt1_name,
                      trt1_details,
                      trt2_name,
                      trt2_details,
                      finelevel_group,
                      trt1,
                      trt1_int,
                      trt1_int2,
                      trt1_value,
                      trt2,
                      trt2_int,
                      trt2_int2,
                      trt2_value,
                      significance,
                      per_change,
                      actual_diff,
                      group_level1,
                      group_level2,
                      group_level3,
                      sample_depth,
                      sample_year,
                      trt_compare,
                      tillage_1,
                      tillage_2,
                      nutrient_groups,
                      cc_group1,
                      cc_group2, 
                      pm_group1,
                      pm_group2
              )
              


write.csv(all_data2, file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/ALL_raw.csv", row.names = FALSE)




