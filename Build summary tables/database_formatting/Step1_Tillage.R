#Synthesis of Midwestern Agriculture#######################
#Managing Soil Carbon Working Group - SNAPP, NCEAS ###############


#This file:
#Adds grouping variables to all response variables collected during the data extraction process
#There are 4 key agro-environmental  groups:
##1. Soil
##2. Crop Production
##3. Water (infield movement only)
##4. Pest Regulation

#Sub groups for each big order group vary by review.

#Use 'df' worksheet with this script.


#libraries#####

library("dplyr", lib.loc = "~/R/win-library/3.5")
library("readxl", lib.loc = "~/R/win-library/3.5")
library("tidyverse", lib.loc = "~/R/win-library/3.5")
library("tibble", lib.loc = "~/R/win-library/3.5")

############Import dataframes######################################################################

df <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_Results.csv", row.names = NULL)



df <- df[df$Paper_id != 209, ] #paper was not peer reviewed


#Remove unneeded columns
df$Year_result <- NULL
df$Effect <- NULL
df$Authors_comments <- NULL
df$Reviewers_results_short <- NULL
df$Reviewers_results_long <- NULL
df$Group_RV <- NULL
df$Response_var_org <- NULL


#Rename column rows to be consistent###
df <- rename(df, paper_id = Paper_id, duration = Duration, loc_multi_results = Loc_multi_results, 
             rv = Response_var, rv_depth = RV_depth, rv_year = RV_year,
             rv_units = Response_var_units, rv_trtspecifics = RV_trtspecifics, stat_test = Stat_test, stat_type = Stat_type,
             finelevel_group = Group_finelevel, review = Review_id, 
             trt1_name = Trt_id1name, trt1_value = Trt_id1value, trt1_details = Trt_id1description,
             trt1 = Trt_id1, trt1_int = Trt1_interaction, trt1_int2 = Trt1_interaction2,
             trt2_name = Trt_id2name, trt2_value = Trt_id2value,
             trt2_details = Trt_id2description, 
             trt2 = Trt_id2, trt2_int = Trt2_interaction, trt2_int2 = Trt2_interaction2,
             significance = Sig_level, rv_depth = RV_depth, effect_norm = Effect_norm 
)

#add surrogate key to df
df$review_key = rownames(df)


########################Tillage Review####################################################
#Bulk groupings####

####Soils####
##Soil Chemical Properties####
chem_som <- c("coarse particulate soil organic matter",
              "coarse partiuclate organic matter associated carbon",
              "soil organic matter content (May)",
              "soil organic matter in soil aggregate size <0.4 mm",
              "soil organic matter in soil aggregate size >19 mm",
              "soil organic matter in soil aggregate size 0.4-0.8 mm",
              "soil organic matter in soil aggregate size 0.8-2.0 mm",
              "soil organic matter in soil aggregate size 2.0-6.0 mm",
              "soil organic matter in soil aggregate size 6-19 mm",
              "soil organic matter, total carbon",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size <0.4 mm",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size >19 mm",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size 0.4-0.8 mm",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size 0.8-2.0 mm",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size 2.0-6.0 mm",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size 6-19 mm",
              "particulate organic matter carbon",
              "particulate organic matter carbon ",
              "particulate organic matter nitrogen",
              "fine particulate organic matter in soil aggregate size <0.4 mm",
              "fine particulate organic matter in soil aggregate size >19 mm",
              "fine particulate organic matter in soil aggregate size 0.4-0.8 mm",
              "fine particulate organic matter in soil aggregate size 0.8-2.0 mm",
              "fine particulate organic matter in soil aggregate size 2.0-6.0 mm",
              "fine particulate organic matter in soil aggregate size 6-19 mm",
              "fine particulate soil organic matter",
              "total soil organic matter (sum of loose and occluded POM) after harvest",
              "total soil organic matter (sum of loose and occluded POM) before planting",
              "loose organic matter isotopic composition and fractional proportion of carbon derived from labeled residues",
              "occluded particulate organic matter isotopic composition and fractional proportion of carbon derived from labeled residues",
              "humified organic matter isotopic composition and fractional proportion of carbon derived from labeled residues",
              "soil organic matter content",
              "soil organic matter",
              "total particulate organic matter",
              "total particulate organic matter in soil aggregate size <0.4 mm",
              "total particulate organic matter in soil aggregate size >19 mm",
              "total particulate organic matter in soil aggregate size 0.4-0.8 mm",
              "total particulate organic matter in soil aggregate size 0.8-2.0 mm",
              "total particulate organic matter in soil aggregate size 2.0-6.0 mm",
              "total particulate organic matter in soil aggregate size 6-19 mm",
              "soil particulate organic matter",
              "soil carbon concentration in particulate organic matter"
)

chem_maize_SOC <- c(
  "average annual carbon input from cash crops",
  "corn-derived carbon (C4-C)",
  "eroded soil carbon stock from maize (C4-C) in topsoil",
  "efficiency of returned carbon from maize root and shoot biomass",
  "efficiency of returned carbon from maize root biomass",
  "carbon in maize root and shoot biomass",
  "carbon in maize root biomass",
  "cob biomass carbon content yield",
  "concentration of carbon in maize residue",
  "concentration of carbon in soybean residue",
  "maize carbon input (aboveground residue)",
  "soybean carbon input (aboveground residue)",
  "stover biomass carbon content yield",
  "total crop carbon input",
  "total crop carbon input (maize and soybean residue)"
  
)

chem_crop_SON <- c(
  "annual nitrogen input from maize",
  "annual nitrogen input from soybean",
  "average annual nitrogen input from cash crops"
)


chem_SOC <-  c("total carbon (C4--C & C3-C)",
               "total carbon concentration",
               "total carbon in soil",
               "soil organic carbon by weight mid-season",
               "soil organic carbon contents ",
               "organic carbon ",
               "organic carbon in particulate organic matter (POM)",
               "mineral associated organic carbon",
               "percent carbon in particulate organic matter (POM) ",
               "volumetric soil organic content",
               "soil organic stock",
               "total soil organic stock",
               "soil organic carbon concentration (after harvest",
               "soil organic carbon storage after harvest",
               "soil organic carbon concentration in spring",
               "soil organic carbon storage in the spring",
               "soil carbon in loose-particulate organic matter after harvest",
               "soil carbon in loose-particulate organic matter before planting",
               "soil carbon in occluded-particulate organic matter after harvest",
               "soil carbon in occluded-particulate organic matter before planting",
               "soil organic carbon concentration",
               "soil organic carbon stocks",
               "macroaggregate protected carbon",
               "macroaggregate protected organic carbon",
               "total soil carbon",
               "carbon content in <0.25 mm aggregate size fraction",
               "carbon content in 0.25-0.5 mm aggregate size fraction",
               "carbon content in 0.5-1 mm aggregate size fraction",
               "carbon content in 1-2 mm aggregate size fraction",
               "carbon content in 2-5 mm aggregate size fraction",
               "carbon content in 5-8 mm aggregate size fraction",
               "% soil organic carbon loss as flux (180 days during fallow period)",
               "active carbon concentration",
               "soil organic carbon",
               "delta 13 C in soil",
               "delta-13C value in soil",
               "cumulative stock of soil organic carbon",
               "soil organic carbon ",
               "soil organic carbon concentration ",
               "soil organic carbon concentration on a mass basis",
               "soil organic carbon concentration on an area basis",
               "soil organic carbon content",
               "soil organic carbon content in Crosby silt loam after harvest",
               "soil organic carbon content in Wooster silt loam after harvest",
               "soil organic carbon derived from maize root and shoot biomass",
               "soil organic carbon derived from maize root biomass",
               "soil organic carbon in g/kg",
               "soil organic carbon in March",
               "soil organic carbon in Mg/ha",
               "soil organic carbon of soil aggregate size <0.4 mm",
               "soil organic carbon of soil aggregate size >19 mm",
               "soil organic carbon of soil aggregate size 0.4-0.8 mm",
               "soil organic carbon of soil aggregate size 0.8-2.0 mm",
               "soil organic carbon of soil aggregate size 2.0-6.0 mm",
               "soil organic carbon of soil aggregate size 6-19 mm",
               "soil organic carbon on area basis",
               "soil organic carbon pool",
               "soil organic carbon pool, C3",
               "soil organic carbon pool, C4",
               "soil organic carbon reserves",
               "soil organic carbon stock",
               "soil organic carbon stock ",
               "soil organic carbon stock (C3-C) in topsoil",
               "soil carbon stock from maize (C4-C) in topsoil",
               "soil carbon stored in heavy fractions ",
               "eroded soil organic carbon stock (C3-C) in topsoil",
               "soil carbon stored in light fractions ",
               "mass of soil organic carbon",
               "maximum mineralizable soil carbon (Michaelis-Menten equation)",
               "old carbon (C3-C)",
               "organic carbon pool in soil",
               "light fraction of carbon",
               "soil total carbon",
               "soil total organic carbon",
               "weighted soil organic carbon",
               "total soil organic carbon",
               "total soil organic carbon pool in soil",
               "half-life of C-soil organic carbon",
               "minearlizable soil carbon",
               "mineral fraction carbon",
               "natural abundance of 13-C in soil (delta 13-C)",
               "equivalent mass of soil organic carbon",
               "total soil carbon ",
               "change in soil organic carbon concentrations",
               "potassium sulfate extractable field-moist soil carbon",
               "potassium sulfate extractable microwaved soil carbon",
               "passive organic carbon concentration",
               "passive organic carbon concentration/active carbon",
               "soil carbon",
               "soil carbon concentration",
               "soil carbon concentration whole soil"
)


chem_CNratio <- c("C:N ratio ",
                  "C:N ratio in particulate organic matter (POM)",
                  "mineral associated C:N ratio",
                  "C:N ratio",
                  "carbon to nitrogen ratio in of soil aggregate size <0.4 mm",
                  "carbon to nitrogen ratio in of soil aggregate size >19 mm",
                  "carbon to nitrogen ratio in of soil aggregate size 0.4-0.8 mm",
                  "carbon to nitrogen ratio in of soil aggregate size 0.8-2.0 mm",
                  "carbon to nitrogen ratio in of soil aggregate size 2.0-6.0 mm",
                  "carbon to nitrogen ratio in of soil aggregate size 6-19 mm",
                  "carbon to nitrogen ratio in soil",
                  "carbon/nitrogen ratio of humic acid in soil aggregate size <0.4 mm",
                  "carbon/nitrogen ratio of humic acid in soil aggregate size 0.4-0.8 mm",
                  "carbon/nitrogen ratio of humic acid in soil aggregate size 0.8-2.0 mm",
                  "carbon/nitrogen ratio of humic acid in soil aggregate size 2.0-6.0 mm",
                  "carbon/nitrogen ratio of humic acid in soil aggregate size 6-19 mm"
                  
)

chem_nitrogen_SOM <-
  c(
    "nitrogen in particulate organic matter (POM)",
    "percent nitrogen in particulate organic matter (POM) ",
    "coarse partiuclate organic matter associated nitrogen",
    "nitrogen content in <0.25 mm aggregate size fraction",
    "nitrogen content in 0.25-0.5 mm aggregate size fraction",
    "nitrogen content in 0.5-1 mm aggregate size fraction",
    "nitrogen content in 1-2 mm aggregate size fraction",
    "nitrogen content in 2-5 mm aggregate size fraction",
    "nitrogen content in 5-8 mm aggregate size fraction",
    "soil carbon to nitrogen ratio",
    "soil carbon to nitrogen ratio ",
    "soil nitrogen concentration in particulate organic matter",
    "soil organic matter, total nitrogen",
    "soil organic nitrogen ",
    "soil organic nitrogen on area basis"
  )

chem_totalN <-
  c("total nitrogen ",
    "total nitrogen",
    "total soil nitrogen",
    "mineral associated nitrogen",
    "nitrogen pool in soil",
    "cumulative stock of nitrogen",
    "soil nitrogen",
    "soil nitrogen concentration",
    "soil nitrogen concentration whole soil",
    "soil nitrogen content",
    "soil nitrogen mineralization",
    "soil nitrogen pool",
    "soil nitrogen stored in heavy fractions ",
    "soil nitrogen stored in light fractions ",
    "soil total nitrogen",
    "soil total nitrogen concentration",
    "soil total nitrogen concentration ",
    "soil total nitrogen pool",
    "soil inorganic nitrogen (early fall)",
    "soil inorganic nitrogen (late fall)",
    "soil inorganic nitrogen (late summer)",
    "soil inorganic nitrogen (mid summer)",
    "light fraction of nitrogen",
    "inorganic nitrogen early August",
    "inorganic nitrogen early July",
    "inorganic nitrogen early September",
    "net N-mineralization",
    "potentially mineralizable nitrogen",
    "total inorganic nitrogen",
    "total nitrogen concentration",
    "total nitrogen concentration in soil",
    "total nitrogen in soil",
    "mass of soil nitrogen",
    "equivalent mass of soil nitrogen",
    "mineralizable nitrogen",
    "soil potentially mineralizable nitrogen",
    "soil profile of inorganic nitrogen",
    "total available inorganic nitrogen pool",
    "total soil nitrogen intensity (nitrate & ammonium)",
    "apparent nitrogen mineralization"
  )

chem_pH <-
  c(
    "soil pH mid-season",
    "soil pH",
    "pH",
    "soil total base saturation"
  )

chem_CEC <- c(
  "CEC mid-season",
  "cation exchange capacity",
  "cation-exchange capacity in Crosby silt loam after harvest",
  "cation-exchange capacity in Wooster silt loam after harvest",
  "electirical conductivity",
  "electrical conductivity",
  "electrical conductivity of soil",
  "soil cation exchange capacity",
  "soil CEC in March",
  "soil electical conductivity",
  "soil electircal conductivity",
  "soil electrical conductivity"
)

chem_phosphorus <- c(
  "soil phosphorus mid-season",
  "soil phosphorus",
  "soil phosphorus [Bray P1]",
  "soil phosphorus with Bray P1 extraction",
  "available phosphorus",
  "available soil phosphorus",
  "Bray-extractable phosphorus",
  "extractable phosphorus",
  "extractable soil phosphorus",
  "extractable soil phosphorus levels (Bray P)",
  "soil phosphorus (Bray)",
  "soil phosphorus (Bray-P1)",
  "soil phosphorus (Olsen P)",
  "soil phosphorus content",
  "soil phosphorus content in interrow after 4 years of broadcast fertilizing",
  "soil phosphorus content in row after 4 years of broadcast fertilizing",
  "water-extractable phosphorus",
  "soil phosphate"
)

chem_potassium <- c(
  "soil potassium mid-season",
  "soil potassium",
  "extractable potassium",
  "exchangeable potassium",
  "exchangeable potassium in soil",
  "exchangeable soil potassium",
  "soil potassium base saturation",
  "soil potassium content",
  "soil potassium content in interrow after 4 years of broadcast fertilizing",
  "soil potassium content in row after 4 years of broadcast fertilizing",
  "soil base saturation-Potassium",
  "soil exchangeable potassium"
)

chem_calcium <- c(
  "soil calcium mid-season",
  "soil calcium",
  "exchangeable calcium",
  "soil base saturation-Calcium",
  "soil Ca concentrations in March",
  "soil calcium base saturation",
  "soil exchangeable calcium"
)

chem_magnesium <- c(
  "soil magnesium mid-season",
  "soil magnesium",
  "exchangeable magnesium",
  "soil Mg concentrations in March",
  "soil magnesium base saturation",
  "soil base saturation-Magnesium",
  "soil exchangeable magnesium"
)


chem_misc_nutrients <- c(
  "soil cobalt",
  "soil copper",
  "soil exchangeable sodium",
  "soil hydrogen base saturation",
  "soil molybdenum",
  "soil nickel",
  "soil zinc",
  "soil sodium",
  "soil sulfate",
  "soil sulfur",
  "soil boron",
  "soil iron",
  "soil aluminium",
  "soil manganese"
)

chem_ammonium <- c(
  "soil ammonium mid-season",
  "soil ammonium",
  "soil ammonium (110 Julian day)",
  "soil ammonium (200 Julian day)",
  "soil ammonium (300 Julian day)",
  "soil ammonium concentration in April",
  "soil ammonium concentration in August",
  "soil ammonium in July",
  "soil ammonium in October"
)

chem_nitrate <- c("soil nitrate mid-season",
                  "soil nitrate 36 days after planting",
                  "soil nitrate 64 days after planting",
                  "soil nitrate 92 days after planting",
                  "soil nitrate 42 days after planting",
                  "soil nitrate 72 days after planting",
                  "soil nitrate 103 days after planting",
                  "after-harvest total residual soil nitrate",
                  "soil nitrate",
                  "soil nitrate (110 Julian day)",
                  "soil nitrate (200 Julian day)",
                  "soil nitrate (300 Julian day)",
                  "soil nitrate 02 October",
                  "soil nitrate 11 May",
                  "soil nitrate 13 November",
                  "soil nitrate 18 June",
                  "soil nitrate 18 September",
                  "soil nitrate 2 October",
                  "soil nitrate 23 June",
                  "soil nitrate 23 September",
                  "soil nitrate 25 September",
                  "soil nitrate 29 April",
                  "soil nitrate 30 May",
                  "soil nitrate 30 October",
                  "soil nitrate concentration",
                  "soil nitrate concentration in April",
                  "soil nitrate concentration in August",
                  "soil nitrate content",
                  "soil nitrate in July",
                  "soil nitrate in October",
                  "soil nitrate-N following maize",
                  "soil nitrate-N following soybean",
                  "soil nitrogen intensity of ammonium",
                  "soil nitrogen intensity of nitrate",
                  "soil inorganic nitrate nitrogen concentration"#,
                  #"overwinter changes in soil nitrate following maize",  ####Excluded -> change in value rather than raw value
                  #"overwinter changes in soil nitrate following soybean" ####Excluded -> change in value rather than raw value
)

##Soil Physical Properties####

phy_silt <- c(
  "percent silt",
  "percent silt soil texture",
  "silt concentrations",
  "silt content",
  "silt particle size distribution in spring"
)

phy_clay <- c(
  "percent clay",
  "clay concentrations",
  "clay content",
  "clay particle size distribution in spring",
  "percent clay soil texture"
)

phy_sand <- c(
  "sand concentrations",
  "sand content",
  "sand particle size distribution in spring",
  "percent sand",
  "percent sand soil texture"
)

phy_waterinfiltration <- c(
  "infiltration rate in crop row",
  "infiltration rate in crop interrow",
  "cumulative infiltration in soil",
  "cumulative infiltration over 190 min",
  "infiltration rate",
  "infiltration rate in soil after 2.5 hrs",
  "infiltration rate in soil after 5 minutes",
  "mean cumulative infiltration rate at 3 h",
  "water drop pentration test",
  "water infiltration rate at 140 mins in August",
  "water infiltration rate at 40 mins in August",
  "water infiltration rate at 5 mins in August",
  "water infiltration time of first 2.5 cm water in soybeans",
  "water infiltration time of second 2.5 cm water",
  "steady-state infiltration of soil",
  "soil water sorptivity",
  "sorptivity of soil",
  "sorptivity rate",
  "cumulative infiltration",
  "water infiltration rate",
  "water percolation through soil during growing season",
  "water percolation through soil during non-growing season",
  "annual drainage",
  "drainage",
  "annual subsurface drainage flow"
)

phy_aggregation_diameter <- c( 
  "soil aggregate dry mean weight diameter ",
  "soil aggregate wet mean weight diameter",
  "geometric mean diameter of aggregates",
  "mean weight diameter of aggregates",
  "mean weight diameter in Crosby silt loam after harvest",
  "mean weight diameter in Wooster silt loam after harvest",
  "mean weight diameter of aggregates",
  "mean weight diameter of aggregates (0 kg N/ha applied)",
  "mean weight diameter of aggregates (160 kg N/ha applied)",
  "mean weight diameter of aggregates (80 kg N/ha applied)",
  "mean weight diameter of water-stable aggregates in spring",
  "geometric mean weight diameter of sand-free water stable aggregates"
)


phy_aggregation_stability <- c(
  "wet soil aggregate stability",
  "water stable aggregates ",
  "water aggregate stability (>0.25 mm) of whole soil in maize",
  "percent water stable aggregates",
  "water aggregate stability",
  "water aggregate stability in crop row",
  "water aggregate stability in crop interrow",
  "water stable aggregates",
  "aggregate mean weight diameter in August",
  "aggregate stability",
  "aggregate stability in August",
  "aggregate tensile strength",
  "density of soil aggregates (10 mm diameter) in soil clod",
  "macro-aggregate (0.25-8 mm) distribution",
  "macroaggregates",
  "soil water stable aggregates < 53 um ",
  "soil water stable aggregates > 2000 um ",
  "soil water stable aggregates 250-2000 um ",
  "soil water stable aggregates 53-250 um ",
  "wet soil macroaggregate (0.5 to 2 mm) stability (240 min of sieving)",
  "wet soil macroaggregate (0.5 to 2 mm) stability (3 min of sieving)",
  "wet soil microaggregate (0.053 to 0.25 mm) stability (240 min of sieving)",
  "wet soil microaggregate (0.053 to 0.25 mm) stability (3 min of sieving)",
  "soil aggregation in August ",
  "soil aggregation in March",
  "percent aggregation > 1 mm",
  "total percent aggregation",
  "water stable aggregates",
  "water stable macroaggregate fraction",
  "water-soluble aggregates in Crosby silt loam after harvest",
  "water-soluble aggregates in Wooster silt loam after harvest",
  "water-stable aggregation size 0.4-0.8 mm",
  "water-stable aggregation size 0.8-2.0 mm",
  "water-stable aggregation size 10 mm",
  "water-stable aggregation size 2.0-6.0 mm",
  "wet aggregate stability",
  "soil stability (slake rating)"
)
phy_compaction <- c(
  "cone penetration resistance adjusted for soil moisture one month after planting",
  "cone penetration resistance one month after planting",
  "cone penetration resistance adjusted for soil moisture at planting",
  "soil penetration resistance",
  "soil compaction",
  "penetration resistance in Crosby silt loam after harvest",
  "penetration resistance in Wooster silt loam after harvest",
  "soil penetration resistance in August",
  "soil penetrometer resistance"
)

phy_watercontent <- c(
  "soil water content one month after planting",
  "soil water content midseason",
  "soil water content at planting",
  "soil water content",
  "soil moisture in crop midrow 36-49 days after planting",
  "soil moisture in crop midrow 43-79 days after planting",
  "soil moisture in crop midrow 49-71 days after planting",
  "soil moisture in crop row 1-36 days after planting",
  "soil moisture in crop row 36-49 days after planting",
  "soil moisture in crop row 36-58 days after planting",
  "soil moisture in crop row 43-79 days after planting",
  "soil moisture in crop row 49-71 days after planting",
  "soil moisture in crop row 58-78 days after planting",
  "soil moisture in crop row 6-36 days after planting",
  "soil water 36 days after planting",
  "soil water 64 days after planting",
  "soil water 92 days after planting",
  "soil water 42 days after planting",
  "soil water 72 days after planting",
  "soil water 103 days after planting",
  "soil moisture at maize emergence",
  "soil moisture at maize mid-silk",
  "available water capacity",
  "available water capacity (AWC)",
  "available water capacity in summer",
  "available water-holding capacity in Crosby silt loam after harvest",
  "available water-holding capacity in Wooster silt loam after harvest",
  "field capacity",
  "gravimetric soil moisture (autumn season)",
  "gravimetric soil moisture (spring season)",
  "gravimetric soil moisture (summer season)",
  "gravimetric soil moisture (winter season)",
  "gravimetric soil moisture early August",
  "gravimetric soil moisture early July",
  "gravimetric soil moisture early September",
  "gravimetric water content of soil",
  "permanent wilting point",
  "plant available soil water",
  "plant available water",
  "soil water content",
  "soil water content in soybean",
  "soil moisture",
  "soil moisture after wetting",
  "soil moisture at post-emergence",
  "soil moisture at preharvest",
  "soil moisture at tasseling",
  "soil moisture content in fall",
  "soil moisture content in summer",
  "soil moisture in April",
  "soil moisture in August",
  "soil moisture retention at 0.03 Mpa",
  "soil moisture retention at 0.033 MPa pressure in August ",
  "soil moisture retention at 1.5 Mpa",
  "volumetric soil water content",
  "volumetric water content",
  "volumetric water content (19 June)",
  "volumetric water content (2 October)",
  "volumetric water content (24 July)",
  "volumetric water content when soil matric potential at -10 kPa",
  "water-filled pore space after 60 min",
  "water holding capacity",
  "water uptake rate after 10 min",
  "water repellency index",
  "soil water holding capacity"
)

phy_bulkdensity <- c(
  "soil bulk density at planting",
  "soil bulk density one month after planting",
  "soil bulk density from soil clod one month after planting",
  "soil bulk density from soil clod one month after planting (6 year average)",
  "soil bulk density one month after planting (6 year average)",
  "soil bulk density after harvest",
  "soil bulk density in the spring",
  "soil mass in the spring",
  "soil bulk density",
  "soil bulk density",
  "dry soil bulk density",
  "soil bulk denity during summer",
  "soil bulk density ",
  "soil bulk density in Crosby silt loam after harvest",
  "soil bulk density in March",
  "soil bulk density in summer",
  "soil bulk density in Wooster silt loam after harvest",
  "soil bulk density stover residue partially left in field",
  "wet soil bulk density",
  "weighted bulk density",
  "particle density in Crosby silt loam after harvest",
  "particle density in Wooster silt loam after harvest",
  "mass of soil "
)

phy_airfilled_pores <- c(
  "air-filled porosity at planting",
  "air-filled porosity one month after planting",
  "air-filled porosity two months after planting",
  "air-filled porosity one month after planting ",
  "air-filled porosity midseason ",
  "air filled pore space",
  "air-filled porosity (fa)",
  "oxygen diffusion rate (12 June)",
  "oxygen diffusion rate (19 June)",
  "oxygen diffusion rate (25 June)",
  "air entry or bubbling pressure",
  "soil air content",
  "volumetric air content",
  "soil relative gas diffusion"
)

phy_waterfilled_pores <- c(
  "water-filled porosity at planting",
  "water-filled porosity one month after planting",
  "water-filled porosity one month after planting (clod sample)",
  "water-filled porosity two months after planting",
  "water-filled porosity one month after planting ",
  "water-filled porosity midseason ",
  "water filled pore space",
  "water filled pore space after irrigation",
  "water filled pore space before irrigation"
)

phy_totalpores <- c(
  "total porosity one month after planting ",
  "total porosity midseason ",
  "total porosity in soil",
  "mean volume of residual pores",
  "mean volume of storage pores",
  "mean volume of transmission pores",
  "median pore radius",
  "saturated hydraulic conductivity",
  "saturated hydraulic conductivity (Ks)",
  "saturated hydraulic conductivity in Crosby silt loam after harvest",
  "saturated hydraulic conductivity in Wooster silt loam after harvest",
  "saturated hydraulic conductivity rate",
  "effective porosity (fe)",
  "soil macropores",
  "pore size distribution parameter",
  "pore space in soil clod",
  "pore tortuosity factor",
  "porosity in Crosby silt loam after harvest",
  "porosity in Wooster silt loam after harvest",
  "relative gas diffusion coefficient",
  "soil aeration porosity (August)",
  "soil aeration porosity (July)",
  "soil aeration porosity (June)",
  "soil aeration porosity (September)",
  "soil micropores",
  "soil fine mesopores",
  "total pore space",
  "total porosity (ft)",
  "transmissiivty of soil",
  "volume of transmission (TrP)",
  "soil coarse mesopores",
  "soil porosity in March",
  "soil total porosity",
  "storage pores (StP)"
)

phy_erosion <- c(
  "calculated soil loss (Universal Soil Loss Equation)",
  "soil loss (calculated using the universal soil loss equation)",
  "soil loss"
)

phy_surfaceresidue <- c(
  "residue cover",
  "residue cover after planting on Readlyn soil",
  "residue cover after planting on Haig soil",
  "residue cover after planting on Webster soil",
  "residue remaining on soil from previous crop",
  "residue percent cover after planting (mean of maize and soybean years)",
  "proportion of soil surface covered by residues immediately after planting",
  "% surface covered with stover",
  "residue cover after planting",
  "residue coverage before planting",
  "percent of soil surface cover by residue",
  "surface residue cover before planting",
  "surface residue cover post-cultivation",
  "surface residue cover post-planting",
  "soil residue",
  "soil surface cover (May)",
  "soil surface cover (September)"
)

phy_surfaceresidue_decomp <- c(
  "first order decay rate (k) of aboveground litter"
)




##Soil Biological Properties####


biol_fungi_abund <- c(
  "abundance of basidiomycetes in soil in soil aggregate size 0.8-2.0 mm",
  "abundance of basidiomycetes in soil in soil aggregate size 2.0-6.0 mm",
  "abundance of basidiomycetes in soil in soil aggregate size 6-19 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size <0.4 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size >19 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size 0.4-0.8 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size 0.8-2.0 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size 2.0-6.0 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size 6-19 mm"
)

biol_enzyme_activity <- c(
  "aspartase activity in soils",
  "change in Amidase enzyme activity/change in soil pH",
  "change in arylamidase enzyme activity/change in soil pH",
  "change in L-Asparaginase enzyme activity/change in soil pH",
  "change in L-Aspartase enzyme activity/change in soil pH",
  "change in L-Glutaminase enzyme activity/change in soil pH",
  "change in Urease enzyme activity/change in soil pH",
  "soil enzyme activity (acid phos)",
  "soil enzyme activity (alk phos)",
  "soil enzyme activity (alpha glu)",
  "soil enzyme activity (arylsulf)",
  "soil enzyme activity (urease)",
  "microbial community utilization of substrates"
)

biol_respiration <- c(
  "soil respiration",
  "soil respiration (CO2-C) in in April",
  "soil respiration (CO2-C) in in April ",
  "soil respiration (CO2-C) in in August",
  "soil respiration (CO2-C) in in August ",
  "soil respiration after irrigation",
  "soil respiration before irrigation",
  "basal respiration rate",
  "specific maintenance respiration rate (qCO2)"
)

biol_microbial_biomass <- c(
  "microbial biomass nitrogen",
  "microbial nitrate",
  "soil microbial biomass carbon",
  "soil microbial biomass carbon ",
  "microbial carbon",
  "microbial biomass carbon",
  "soil microbial biomass",
  "soil microbial biomass on 20 May",
  "soil microbial biomass on 25 July",
  "microbial biomass loss",
  "microbial community utilization of substrates"
)


##Soil Environmental Properties####

envir_temp <- c(
  "soil temperature one month after planting",
  "soil temperature in crop row 6-36 days after planting",
  "soil temperature in crop midrow 6-36 days after planting",
  "soil temperature in crop row 36-58 days after planting",
  "soil temperature in crop midrow 36-58 days after planting",
  "soil temperature in crop row 58-78 days after planting",
  "soil temperature in crop midrow 58-78 days after planting",
  "soil temperature in crop row 13-36 days after planting",
  "soil temperature in crop midrow 13-36 days after planting",
  "soil temperature in crop row 36-49 days after planting",
  "soil temperature in crop midrow 36-49 days after planting",
  "soil temperature in crop row 49-71 days after planting",
  "soil temperature in crop midrow 49-71 days after planting",
  "soil temperature",
  "soil temperature (April-October",
  "soil temperature (autumn season)",
  "soil temperature (spring season)",
  "soil temperature (summer season)",
  "soil temperature (winter season)"
)

###Climate Mitigation####
envir_CO2 <- c(
  "carbon dioxide concentration in soil 41 days after tillage",
  "carbon dioxide concentration in soil 7 days after tillage",
  "carbon dioxide concentration in soil 79 days after tillage",
  "carbon dioxide emisisons",
  "carbon dioxide emissions (180 days during fallow period)",
  "carbon dioxide equivalent emissions (180 days during fallow period)",
  "carbon dioxide flux",
  "carbon dioxide flux (180 days during fallow period)",
  "carbon dioxide flux (autumn season)",
  "carbon dioxide flux (spring season)",
  "carbon dioxide flux (summer season)",
  "carbon dioxide flux (winter season)",
  "carbon dioxide flux from soil 1 hr after tillage",
  "carbon dioxide flux from soil 2 hr after tillage",
  "carbon dioxide flux from soil 25 days after tillage",
  "carbon dioxide flux from soil 3 hr after tillage",
  "carbon dioxide flux from soil 4 hr after tillage",
  "carbon dioxide flux from soil 45 days after tillage",
  "carbon dioxide flux from soil 5 days after tillage",
  "carbon dioxide flux from soil 65 days after tillage",
  "carbon dioxide flux from soil 85 days after tillage",
  "carbon dioxide flux from soil at tillage",
  "carbon dioxide flux from soil before tillage",
  "annual carbon dioxide fluxes",
  "annual carbon input from maize",
  "annual carbon input from soybean",
  "annual soil carbon dioxide flux",
  "average weekly soil carbon dioxide emissions",
  "cumulative carbon dioxide emissions (April-October)",
  "soil surface carbon dioxide flux (Julian day = 140)",
  "soil surface carbon dioxide flux (Julian day = 180)",
  "soil surface carbon dioxide flux (Julian day = 220)",
  "soil surface carbon dioxide flux (Julian day = 260)",
  "total soil carbon dioxide emissions (20 day total)",
  "soil carbon dioxide emissions",
  "total carbon flux (180 days during fallow period)"
)


envir_N2O <- c(
  "annual nitrous oxide fluxes",
  "annual soil nitrous oxide flux",
  "averaged daily nitrous oxide-nitrogen emissions (continuous maize)",
  "averaged daily nitrous oxide-nitrogen emissions (maize-soybean rotation)",
  "cumulative nitrous oxide emissions",
  "cumulative nitrous oxide flux (April 2003-March 2004)",
  "cumulative nitrous oxide flux (April 2004-Feb 2005)",
  "nitrous oxide emisisons",
  "nitrous oxide emissions",
  "nitrous oxide emissions (180 days during fallow period)",
  "cumulative annual nitrous oxide emissions from soil",
  "nitrous oxide emission flux from soil (autumn)",
  "nitrous oxide emission flux from soil (spring)",
  "nitrous oxide emission flux from soil (summer)",
  "nitrous oxide emission flux from soil (winter)",
  "nitrous oxide emissions per unit area",
  "nitrous oxide emissions per unit grain yield",
  "nitrous oxide emissions per unit nitrogen grain yield",
  "nitrous oxide flux"
)

envir_CH4 <- c(
  "annual methane fluxes",
  "annual soil methane flux",
  "average annual soil methane emissions",
  "methane emissions (180 days during fallow period)",
  "cumulative annual methane emissions from soil",
  "methane emission flux from soil (autumn)",
  "methane emission flux from soil (spring)",
  "methane emission flux from soil (summer)",
  "methane emission flux from soil (winter)",
  "methane flux",
  "methane flux (180 days during fallow period)",
  "soil methane emissions",
  "soil methane emissions before fall tilling",
  "soil methane oxidation (CH4-C) in in April",
  "soil methane oxidation (CH4-C) in in April ",
  "soil methane oxidation (CH4-C) in in August",
  "soil methane oxidation (CH4-C) in in August ",
  "soil methane emission flux"
)

envir_globalwarmingpotential <- c(
  "annual global warming potential",
  "global warming impact",
  "greenhouse gas intensity (N2O/grain)"
)

####Pest Regulation####
## Weeds ####
weed_broadleaf <- c("abundance of Canada thistle,  Cirsium arvense",
                    "abundance of Common dandelion, Taraxacum officinale",
                    "abundance of Pennsylvania smartweed, Polygonum pensylvanicum",
                    "abundance of Quackgrass, Agropyron repens",
                    "abundance of broadleaf species",
                    "density of common chickweed seeds",
                    "density of corn speedwell seeds",
                    "density of yellow woodsorrel seeds"
)

weed_community_abundance <-
  c(   
    "number of weed seeds",
    "percent of weed seeds",
    "percent weed cover in maize row",
    "percent weed cover between maize rows",
    "escaped weeds after second and third year of tillage",
    "total abundance of weeds",
    "total number of germinable seeds ",
    "number of weed species ",
    "weed density"
  )

weed_grass <-
  c(   
    "abundance of grass species",
    "density of large crabgrass seeds"
  )

weed_community_diversity <-
  c(   "weed seed diversity (Shanon-Weiner heterogeneity)",
       "weed seed diversity (Simpson's)",
       "mean number of weed species"
  )

weed_lambsquarters <-
  c(
    "abundance of Common Lambsquarters",
    "abundance of Common lambsquarters, Chenopodium album",
    "density of common lambsquarters seeds"
  )


weed_amaranthus <- c(
  "abundance of Amaranthus species",
  "Amaranthus species density (weeds)",
  "abundance of Pigweed, Amaranthus spp.",
  "density of redroot pigweed seeds"
)

weed_fallpanicum <- c(
  "abundance of Fall panicum",
  "abundance of Fall panicum, Panicum dichotomiflorum"
)

weed_velvetleaf <- c(
  "abundance of Velvetleaf"
)

## Invertebrates ####

invert_pests_cornrootworm <-
  c(
    "adult counts of Western corn rootworm during highest infestation"
  )
invert_pests_seedcornmaggot <-
  c(
    "adult seedcorn maggot abundance",
    "seedcorn maggot (pest)"
  )

invert_pests_Aglycines <-
  c( "soybean aphid adult density 24 hr after release",
     "soybean aphid nymph density 24 hr after release",
     "soybean aphid adult density 6 days after release",
     "soybean aphid nymph density 14 days after release",
     "soybean aphid density at peak (13 Aug)"
  )

invert_pests_cornborer <-
  c("mature larval density, O. nubilalis",
    "density of European corn borer larvae (first generation)",
    "density of European corn borer larvae (second generation)",
    "Percent of Ostrinia nubilialis eggs hatched",
    "Density of aphids on maize when pest at first generation larval state (O. nubilalis)",
    "mature larval density per injured plant of first-generation O. nubilalis"
  )

invert_pests_damage <-
  c(  "Western corn rootworm  root damage evaluation",
      "slug damage to plants",
      "armyworm damage to plants",
      "cutworms damage to plants"
      
  )

invert_pests_miscabundance <- c(
  "abundance of Green cloverworm (herbivore)",
  "abundance of Bean leaf beetle (herbivore)",
  "abundance of Japanese beetle (herbivore)",
  "abundance of Grasshoppers (herbivore)",
  "abundance of Striped flea beetles (herbivore)",
  "abundance of Grape colaspis (herbivore)",
  "abundance of Tarnished plant bug (herbivore)",
  "abundance of Homopterans (herbivore)",
  "abundance of potato leafhoppers (pest)",
  "green cloverworm (pest)",
  "potato leafhopper (pest)",
  "bean leaf beetle (pest)",
  "tarnished plant bug (pest)",
  "redlegged grasshopper (pest)",
  "differential grasshopper (pest)",
  "green stink bug (pest)",
  "brown stink bug (pest)"
)

invert_preds_activity <- c(
  "Percent of sentinel eggs preyed upon by Coleomegilla maculata (Coleoptera: Coccinellidae)",
  "Percent of sentinel eggs preyed upon by Chyrsoptera sp. (Neuroptera: Chrusopidae)",
  "Percent of sentinel eggs preyed upon by other sucking predators"
  
)

invert_preds<-
  c(
    "damsel bugs (predator)",
    "ladybird density, coccinella septempuncata, at peak (13 Aug)",
    "ladybird density, Harmonia axyridis, at peak (13 Aug)",
    "ladybird larvae density, Coccinellid larvae, at peak (13 Aug)",
    "insiduous flower bug density, Orius insidiosus, at peak (30 July)",
    "Syrphid fly larvae density at peak (30 July, predators)",
    "abundance of Harpalus pensylvanicus (predator, ground beetle)",
    "abundance of Cyclotrachelus sodalis (predator, ground beetle)",
    "abundance of Pterostichus chalcites (predator, ground beetle)",
    "abundance of Scarites substriatus (predator, ground beetle)",
    "insidious flower bug (predator)",
    "spined soldier bug (predator)",
    "green lacewings (predator)",
    "ladybeetles, 3 species (predator)",
    "wolf spider abundance (Pardosa milvina Hentz)",
    "wolf spider abundance (Hogna helluo (Walckenaer))",
    "Density of Coleomegilla maculata (Coleoptera: Coccinellidae) when pest at first generation larval state (O. nubilalis)"
    
  )


invert_preds_soilcomm_abund<-
  c(
    "total species of predator taxa collected from soil surface",
    "total species of predator taxa collected from soil column",
    "total abundance of invertebrates on soil surface (pitfall traps in maize)",
    "total abundance of invertebrates on soil surface (pitfall traps in soybean)"
  )

invert_nonpredpest <-
  c(
    "prey abundance (all taxa)",
    "prey abundance (diptera only)",
    "prey abundance (hemiptera only)"
  )

invert_earthworms <-
  c("earthworm density (spring)",
    "earthworm density (fall)"
  )


pathogen_soybean <- c(
  "prevelance of brown stem rot in soybean",
  "prevelance of Phytophthora sojae in soybean",
  "prevelance of Heterodera glycines in soybean"
)

pathogen_maize <- c(
  "maize with Trichoderma spp. in subcrown mesoctyl",
  "maize with Fusarium spp. in subcrown mesoctyl",
  "maize with Trichoderma spp. in crown",
  "maize with Fusarium spp. in crown",
  "maize with Trichoderma spp. in stalk",
  "maize with Fusarium spp. in stalk",
  "mean recovery of Phytophythora sojae in soils",
  "percentage of fields with brown stem rot",
  "stems with brown stem rot",
  "percentage of fields with Phytophthora sojae",
  "leaf disks on soil colonized with Phytophthora sojae",
  "maize stalk rot",
  "maize stalk rot occurrence"
)

abuscularmycorrhizae_maize <- c(
  "maize root concentration of arbuscular mycorrhizae (C16:1cis11) at R6 stage"
)

abuscularmycorrhizae_soybean <- c(
  "soybean root concentration of arbuscular mycorrhizae (C16:1cis11) at R6 stage"
)

nematode_herbivores <- c(
  "herbivore nematode density (spring)",
  "herbivore nematode density (fall)"
)

nematode_nonpredpest <- c("omnivore nematode density (fall)",
                          "microbivore nematode density (fall)",
                          "fungivore nematode density (fall)",
                          "omnivore nematode density (spring)",
                          "microbivore nematode density (spring)",
                          "fungivore nematode density (spring)")

nematode_density <- c(
  "nematode density (fall)",
  "nematode density (spring)"
)

nematode_soybeancyst <- c(
  "percentage of fields with Heterodera glycines",
  "Heterodera glycines density",
  "population density of soybean cyst nematode (Heterodera glycine)",
  "annual population changes of soybean cyst nematode (Heterodera glycine)",
  "population density of soybean cyst nematode (Heterodera glycines) in cyst stage (Fall)",
  "population density of soybean cyst nematode (Heterodera glycines) in cyst stage (Spring)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles at planting",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles in midseason",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles at harvest",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles 1 month after planting",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles 2 months after planting",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles 2 weeks after planting (CV1)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles in midseason (CV1)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles at harvest (CV1)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles 2 weeks after planting (CV2)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles in midseason (CV2)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles at harvest (CV2)",
  "population density of soybean cyst nematodes (Heterodera glycines)",
  "distance soybean cyst nematodes (Heterodera glycines) travelled following tillage",
  "Lloyd's index of patchiness for cyst nematodes (Heterodera glycines)",
  "natural log of H. glycines egg population density at planting",
  "natural log of H. glycines egg population density at harvest",
  "natural log of reproductive factor (Pf/Pi) refl ects seasonal H. glycines population change",
  "natural log of H. glycines egg population density at harvest (first year soybean after maize)",
  "natural log of H. glycines egg population density at harvest (second year soybean)",
  "natural log of H. glycines egg population density at harvest (third year soybean)",
  "natural log of H. glycines egg population density at harvest (continuous soybean)",
  "natural log of H. glycines egg population density at harvest (maize-soybean rotation)",
  "mean population of Heterodera glycine eggs"
)


####Crop Production####

## Yields ####

yields_grainsoy <- c(
  "soybean grain yield",
  "soybean seed yield",
  "soybean yield"
)

yields_grainsoy_quality <- c(
  "soybean grain moisture",
  "soybean grain oil",
  "soybean grain oil content",
  "soybean grain oil yield",
  "soybean grain protein",
  "soybean grain protein content",
  "soybean grain protein yield"
)

yields_grainsoy_growth <- c(
  "soybean plant height",
  "soybean plant height 4 weeks after planting",
  "soybean plant height at midseason"
)

yields_grainmaize <- c(
  "maize grain yield",
  "cob biomass yield",
  "cob potential cellulsoic ethanol yield",
  "maize grain harvest index",
  "maize grain yield ",
  "maize grain yield after 2 yr of tillage",
  "maize harvest index",
  "maize yield",
  "maize yields",
  "sweet maize ear biomass",
  "sweet maize grain yield (fresh weight)",
  "stover potential cellulsoic ethanol yield",
  "maize grain yield on Readlyn soil",
  "maize grain yield on Haig soil",
  "maize grain yield on Webster soil"
)

yields_grainmaize_quality <- c(
  "maize ear moisture", 
  "maize grain moisture",
  "maize grain moisture on Readlyn soil",
  "maize grain moisture on Haig soil",
  "maize grain moisture on Webster soil",
  "maize harvest moisture"
)

yields_grainmaize_growth <- c(
  "relative plant height", 
  "maize plant height",
  "maize plant height at 8 weeks after planting",
  "maize plant height at midseason",
  "number of days after planting to 50% silking"
)

yields_maize_wateruse <- c(
  "maize seasonal water use",
  "maize water productivity",
  "maize water use"
)

yields_soy_wateruse <- c(
  "soybean water use"
)

yields_cashcrop <- c(
  "cash crop yields (maize and soybeans)"
)

yields_maize_biomass_abvgrd <- c(
  "maize dry matter biomass",
  "maize residue yield",
  "maize silage yield",
  "maize silage yield ",
  "maize stover yield",
  "maize total mature dry matter",
  "stover biomass yield",
  "sweet maize non-reproductive tissue biomass",
  "maize biomass",
  "maize biomass carbon",
  "maize aboveground production",
  "maize aboveground biomass at tasseling",
  "maize aboveground dry matter"
)

yields_soybean_biomass_abvgrd <- c(
  "soybean aboveground net primary production",
  "soybean dry matter biomass",
  "average annual soybean residue biomass"
)

yields_cashcrop_biomass_abvgrd <- c(
  "average annual cash crop residue biomass",
  "average annual maize residue biomass"
)

yields_biomass_blwgrd <- c(
  "maize belowground dry matter production",
  "maize root diameter between rows",
  "maize root diameter within rows",
  "maize root length density",
  "maize root length density between rows",
  "maize root length density within rows",
  "maize root mass between rows",
  "maize root mass within rows",
  "root length density in crop midrow",
  "root length density in crop row",
  "specific root length of maize between rows",
  "specific root length of maize within rows"
)

##Crop Nutrient Content ####

crop_N_maizestalk <- c(
  "concentration of nitrogen in maize residue",
  "maize aboveground tissue nitrogen concentrations",
  "maize leaf nitrogen",
  "maize leaf nitrogen concentration ",
  "maize stover nitrogen uptake",
  "nitrogen content of maize tissue",
  "total maize nitrogen accumulation",
  "total nitrogen uptake in maize",
  "total nitrogen yield",
  "total per plant mass nitrogen (whole plant)",
  "maize aboveground plant nitrogen accumulation",
  "apparent nitrogen recovery in maize",
  "grain nitrogen recovery",
  "maize grain nitrogen removal ",
  "nitrogen fertilizer recovery efficiency",
  "nitrogen recovery efficiency",
  "apparent nitrogen recovery in maize",
  "maize silage nitrogen removal ",
  "maize total nitrogen uptake",
  "maize canopy normalized difference vegetative index (NDVI)",
  "normalized difference vegetative index (NDVI)",
  "relative leaf chlorophyll ",
  "relative leaf chlorophyll at V10 stage"
)

crop_N_maizeroot <- c(
  "maize belowground plant nitrogen accumulation"
)

crop_N_maizegrain <- c( 
  "maize grain nitrogen concentration", 
  "maize grain nitrogen uptake",
  "maize grain nitrogen yield",
  "maize grain total nitrogen uptake"
)

crop_P_maizeabvgrd <- c(
  "maize grain total phosphorus uptake",
  "maize phosphorus uptake at R6 stage",
  "grain phosphorus recovery",
  "cumulative phosphorus removal (grain + crop resiude)"
)

crop_miscNutrients_maize <- c(
  "maize V6 whole plant boron",
  "maize V6 whole plant calcium",
  "maize V6 whole plant copper",
  "maize V6 whole plant iron",
  "maize V6 whole plant magnesium",
  "maize V6 whole plant manganese",
  "maize V6 whole plant nitrogen",
  "maize V6 whole plant phosphorus",
  "maize V6 whole plant potassium",
  "maize V6 whole plant sulfur",
  "maize V6 whole plant zinc"
)

crop_N_soybean <- c(  
  "concentration of nitrogen in soybean residue",
  "nitrogen content of soybean tissue"
)

crop_P_soybean <- c(
  "soybean phosphorus uptake at R6 stage"
)

##Crop stand count####

standcount_maize <- c(
  "% maize emergence",
  "maize emergence",
  "maize emergence rate index",
  "maize harvest plant density",
  "maize plant population ",
  "maize stand count",
  "stand count (maize)",
  "cash crop emergence rate index on Readlyn soil",
  "cash crop emergence rate index on Haig soil",
  "cash crop emergence rate index on Webster soil",
  "emerged cash crop plant population on Readlyn soil",
  "emerged cash crop plant population on Haig soil",
  "emerged cash crop plant population on Webster soil"
)

standcount_maize_damage <- c(
  "barren stalks (maize)",
  "maize broken stalks",
  "number of maize plants injured"
)


standcount_soy <- c(
  "soybean emergence",
  "soybean lodging",
  "soybean stand count",
  "stand count (soybean)"
)

####Water Movement####
## Runoff ####

runoff_nitrate <-
  c(
    "annual nitrate concentration in 1.2 m shallow groundwater",
    "annual nitrate concentration in 1.8 m shallow groundwater",
    "annual nitrate concentration in 2.4 m shallow groundwater",
    "annual nitrate loss with subsurface drainage flow",
    "annual transport of nitrate in surface runoff (maize years)",
    "annual transport of nitrate in surface runoff (soybean/rye years)",
    "dissolved nitrogen in runoff",
    "flow weighted annual (May-April) concentrations of nitrate in surface runoff (maize years)",
    "flow weighted annual (May-April) concentrations of nitrate in surface runoff (soybean/rye years)",
    "flow-weighted average nitrate concentrations in subsurface drainage",
    "nitrate concentrations in subsurface water",
    "nitrate flow-weighted nitrate concentrations in subsurface drainage",
    "nitrate leaching loses to subsurface drainage",
    "nitrate loss in subsurface water",
    "nitrogen in sediment runoff",
    "soil water nitrate concentrations",
    "volume weighted nitrate anions in leachate",
    "annual inorganic nitrogen leaching losses",
    "average nitrate concentration leached collected after many rainfall events",
    "dissolved organic nitrogen"
  )

runoff_phosphorus <-
  c(
    "bioavailable phosphorus in runoff",
    "dissolved organic phosphorus in runoff",
    "dissolved total phosphorus in runoff",
    "total phosphorus in runoff",
    "total phosphorus in sediment runoff",
    "total soil phosphorus in runoff",
    "volume weighted phosphorus anions in leachate"
  )

runoff_pesticides <- c(
  "atrazine concentrations in runoff (24 June)",
  "atrazine concentrations in runoff (27 May)",
  "Atrazine loss",
  "atrazine mass loss in runoff (24 June)",
  "atrazine mass loss in runoff (27 May)",
  "monensin antibiotic losses in leachate",
  "tylosin antibiotic losses in leachate"
)

runoff_carbon <- c(
  "dissolved organic carbon in runoff",
  "organic carbon in runoff sediment",
  "volume weighted dissolved organic carbon in leachate",
  "dissolved organic carbon"
)

runoff_miscnutrients <- c(
  "volume weighted aluminum cations in leachate",
  "volume weighted ammonium cations in leachate",
  "volume weighted boron anions in leachate",
  "volume weighted calcium cations in leachate",
  "volume weighted chloride anions in leachate",
  "volume weighted copper cations in leachate",
  "volume weighted iron cations in leachate",
  "volume weighted magnesium cations in leachate",
  "volume weighted manganese cations in leachate",
  "volume weighted potassium cations in leachate",
  "volume weighted sodium cations in leachate",
  "volume weighted sulfur anions in leachate",
  "volume weighted zinc cations in leachate"
)

runoff_water <- c(
  "daily runoff volume within 24 hours of rain event (21 June)",
  "daily runoff volume within 24 hours of rain event (24 June)",
  "daily runoff volume within 24 hours of rain event (25 May)",
  "daily runoff volume within 24 hours of rain event (26 May)",
  "daily runoff volume within 24 hours of rain event (27 May)",
  "mean time to runoff",
  "subsurface drainage flow",
  "subsurface drainage water flow",
  "surface runoff",
  "water runoff (May)",
  "water runoff (September)",
  "water runoff after inital rainfall simulation of 9.5 cm/h",
  "water runoff after inital rainfall simulation of 9.5 cm/h within 24 hrs",
  "water runoff after seocond rainfall simulation of 9.5 cm/h within 24 hrs",
  "water runoff during growing season",
  "water runoff during non-growing season",
  "cumulative water loss 5 hrs after tillage event  (12 August)",
  "cumulative water loss 5 hrs after tillage event  (13 August)",
  "cumulative water loss 5 hrs after tillage event  (17 August)",
  "cumulative water loss 5 hrs after tillage event  (18 August)"
)

#"total depth of runoff" <- needs to be added to a group

runoff_sediment <- c(
  "sediment concentration after inital rainfall simulation of 9.5 cm/h",
  "sediment concentration after inital rainfall simulation of 9.5 cm/h within 24 hrs",
  "sediment concentration after seocond rainfall simulation of 9.5 cm/h within 24 hrs",
  "sediment in runoff",
  "sediment loss after inital rainfall simulation of 9.5 cm/h",
  "sediment loss after inital rainfall simulation of 9.5 cm/h within 24 hrs",
  "sediment loss after seocond rainfall simulation of 9.5 cm/h within 24 hrs",
  "total suspended solids",
  "soil loss"
)

runoff_manure <- c(
  "water runoff manures (May)"
)

###Apply metric & grouping labels to dataframe#####################################


grouped_table <- df %>%
  select(rv, review_key) %>%
  
  ################group_level1##################

mutate(
  group_level1 = case_when(
    
    #Soil Nutrients####
    #Nitrogen####
    rv %in% chem_nitrogen_SOM ~ "Soil Nutrients",
    rv %in% chem_crop_SON ~ "Soil Nutrients",
    rv %in% chem_totalN ~ "Soil Nutrients",
    rv %in% chem_ammonium ~ "Soil Nutrients",
    rv %in% chem_nitrate ~ "Soil Nutrients",
    rv %in% chem_CNratio ~ "Soil Nutrients",
    
    #Phosphorus####
    rv %in% chem_phosphorus ~ "Soil Nutrients",
    
    #Potassium####
    rv %in% chem_potassium ~ "Soil Nutrients",
    
    #Micro-nutrients####
    rv %in% chem_calcium ~ "Soil Nutrients",
    rv %in% chem_magnesium ~ "Soil Nutrients",
    rv %in% chem_misc_nutrients ~ "Soil Nutrients",
    
    
    
    ### Other Soil Properties####
    #Chemical Properties####
    rv %in% chem_pH ~ "Other Soil Properties",
    rv %in% chem_CEC ~ "Other Soil Properties",
    
    #Physical Properties
    rv %in% chem_som ~ "Other Soil Properties",
    rv %in% phy_aggregation_stability ~ "Other Soil Properties",
    rv %in% phy_aggregation_diameter ~ "Other Soil Properties",
    
    rv %in% phy_silt ~ "Other Soil Properties",
    rv %in% phy_clay ~ "Other Soil Properties",
    rv %in% phy_sand ~ "Other Soil Properties",
    rv %in% phy_compaction ~ "Other Soil Properties",
    rv %in% phy_bulkdensity ~ "Other Soil Properties",
    
    rv %in% phy_surfaceresidue ~ "Other Soil Properties",
    rv %in% phy_surfaceresidue_decomp ~ "Other Soil Properties",
    rv %in% phy_erosion ~ "Other Soil Properties", 
    rv %in% phy_airfilled_pores ~ "Other Soil Properties",
    rv %in% phy_totalpores ~ "Other Soil Properties",
    
    #Abiotic Factors####
    rv %in% envir_temp ~ "Other Soil Properties",
    rv %in% phy_waterinfiltration ~ "Other Soil Properties",
    rv %in% phy_watercontent ~ "Other Soil Properties",
    rv %in% phy_waterfilled_pores ~ "Other Soil Properties",
    
    #Biotic Factors#####
    rv %in% biol_respiration ~ "Other Soil Properties",
    rv %in% biol_fungi_abund ~ "Other Soil Properties",
    rv %in% biol_enzyme_activity ~ "Other Soil Properties",
    rv %in% biol_microbial_biomass ~ "Other Soil Properties",
    rv %in% abuscularmycorrhizae_soybean ~ "Other Soil Properties",
    rv %in% abuscularmycorrhizae_maize ~ "Other Soil Properties",
    
    
    
    
    ##Climate Mitigation####
    #Carbon Mitigation####
    rv %in% chem_maize_SOC ~ "Climate Mitigation",
    rv %in% chem_SOC ~ "Climate Mitigation",
    rv %in% envir_CO2 ~ "Climate Mitigation",
    rv %in% envir_CH4 ~ "Climate Mitigation",
    
    #Nitrogen Mitigation
    rv %in% envir_N2O ~ "Climate Mitigation",
    
    #Global Warming Potential#####
    rv %in% envir_globalwarmingpotential ~ "Climate Mitigation",
    
    
    ###Pests####
    #Weeds ####
    
    rv %in% weed_broadleaf ~ "Pests",
    rv %in% weed_grass ~ "Pests",
    
    rv %in% weed_community_abundance ~ "Pests",
    rv %in% weed_community_diversity ~ "Pests",
    
    rv %in% weed_lambsquarters ~ "Pests",
    rv %in% weed_amaranthus ~ "Pests",
    rv %in% weed_fallpanicum ~ "Pests",
    rv %in% weed_velvetleaf ~ "Pests",
    
    
    #Invertebrates ####
    
    rv %in% invert_pests_cornrootworm ~ "Pests",
    rv %in% invert_pests_seedcornmaggot ~ "Pests",
    rv %in% invert_pests_Aglycines ~ "Pests",
    rv %in% invert_pests_cornborer ~ "Pests",
    rv %in% invert_pests_miscabundance ~ "Pests",
    
    rv %in% invert_preds ~ "Pests",
    rv %in% invert_preds_activity ~ "Pests",
    rv %in% invert_preds_soilcomm_abund ~ "Pests",
    rv %in% invert_nonpredpest ~ "Pests",
    
    rv %in% invert_earthworms ~ "Pests",
    
    rv %in% pathogen_soybean ~ "Pests",
    rv %in% pathogen_maize ~ "Pests",
    
    
    rv %in% nematode_herbivores ~ "Pests",
    rv %in% nematode_density ~ "Pests",
    rv %in% nematode_soybeancyst ~ "Pests",
    rv %in% nematode_nonpredpest ~ "Pests",
    
    
    
    ####Crop Yields####
    #Grain Production ####
    rv %in% yields_grainsoy ~ "Crop Yields",
    rv %in% yields_cashcrop ~ "Crop Yields",
    rv %in% yields_grainmaize ~ "Crop Yields",
    
    #Grain Quality ####
    rv %in% yields_grainsoy_quality ~ "Crop Yields",
    rv %in% yields_grainmaize_quality ~ "Crop Yields",
    
    #Crop Growth ####
    rv %in% yields_grainmaize_growth ~ "Crop Yields",
    rv %in% yields_maize_biomass_abvgrd ~ "Crop Yields",
    rv %in% yields_grainsoy_growth ~ "Crop Yields",
    rv %in% yields_soybean_biomass_abvgrd ~ "Crop Yields",
    rv %in% yields_cashcrop_biomass_abvgrd ~ "Crop Yields",
    rv %in% yields_biomass_blwgrd ~ "Crop Yields",
    
    #Water Use ####
    rv %in% yields_maize_wateruse ~ "Crop Yields",
    rv %in% yields_soy_wateruse ~ "Crop Yields",
    
    #Crop Nutrient Content ####
    rv %in% crop_N_maizestalk ~ "Crop Yields",
    rv %in% crop_N_maizeroot ~ "Crop Yields",
    rv %in% crop_N_maizegrain ~ "Crop Yields",
    
    rv %in% crop_P_maizeabvgrd ~ "Crop Yields",
    rv %in% crop_miscNutrients_maize ~ "Crop Yields",
    
    rv %in% crop_N_soybean ~ "Crop Yields",
    rv %in% crop_P_soybean ~ "Crop Yields",
    
    #Crop stand count####
    rv %in% standcount_maize ~ "Crop Yields",
    rv %in% standcount_soy ~ "Crop Yields",
    
    
    
    #Crop Damage ####
    rv %in% invert_pests_damage ~ "Crop Yields",
    rv %in% standcount_maize_damage ~ "Crop Yields",
    
    
    ####Water Movement####
    #Nutrient Runoff ####
    rv %in% runoff_nitrate ~ "Water Quality",
    rv %in% runoff_phosphorus ~ "Water Quality",
    rv %in% runoff_pesticides ~ "Water Quality",
    rv %in% runoff_carbon ~ "Water Quality",
    rv %in% runoff_miscnutrients ~ "Water Quality",
    rv %in% runoff_manure ~ "Water Quality",
    
    #Flow quantity ####
    rv %in% runoff_water ~ "Water Quality",
    
    #Sediment ####
    rv %in% runoff_sediment ~ "Water Quality"
  )) %>%
  
  
  
  
  ##############group_level2 ########
mutate(
  group_level2 = case_when(
    
    
    #Soil Nutrients####
    #Nitrogen####
    rv %in% chem_nitrogen_SOM ~ "N",
    rv %in% chem_crop_SON ~ "N",
    rv %in% chem_totalN ~ "N",
    rv %in% chem_ammonium ~ "N",
    rv %in% chem_nitrate ~ "N",
    rv %in% chem_CNratio ~ "N",
    
    #Phosphorus####
    rv %in% chem_phosphorus ~ "P & K",
    
    #Potassium####
    rv %in% chem_potassium ~ "P & K",
    
    #Micro-nutrients####
    rv %in% chem_calcium ~ "Micro-nutrients",
    rv %in% chem_magnesium ~ "Micro-nutrients",
    rv %in% chem_misc_nutrients ~ "Micro-nutrients",
    
    
    
    ### Other Soil Properties####
    #Chemical Properties####
    rv %in% chem_pH ~ "Chemical Properties",
    rv %in% chem_CEC ~ "Chemical Properties",
    
    #Physical Properties
    rv %in% chem_som ~ "Physical Properties",
    rv %in% phy_aggregation_stability ~ "Physical Properties",
    rv %in% phy_aggregation_diameter ~ "Physical Properties",
    
    rv %in% phy_silt ~ "Physical Properties",
    rv %in% phy_clay ~ "Physical Properties",
    rv %in% phy_sand ~ "Physical Properties",
    rv %in% phy_compaction ~ "Physical Properties",
    rv %in% phy_bulkdensity ~ "Physical Properties",
    
    rv %in% phy_surfaceresidue ~ "Physical Properties",
    rv %in% phy_surfaceresidue_decomp ~ "Physical Properties",
    rv %in% phy_erosion ~ "Physical Properties", 
    rv %in% phy_airfilled_pores ~ "Physical Properties",
    rv %in% phy_totalpores ~ "Physical Properties",
    
    #Abiotic Factors####
    rv %in% envir_temp ~ "Abiotic Factors",
    rv %in% phy_waterinfiltration ~ "Abiotic Factors",
    rv %in% phy_watercontent ~ "Abiotic Factors",
    rv %in% phy_waterfilled_pores ~ "Abiotic Factors",
    
    #Biotic Factors#####
    rv %in% biol_respiration ~ "Biotic Factors",
    rv %in% biol_fungi_abund ~ "Biotic Factors",
    rv %in% biol_enzyme_activity ~ "Biotic Factors",
    rv %in% biol_microbial_biomass ~ "Biotic Factors",
    rv %in% abuscularmycorrhizae_soybean ~ "Biotic Factors",
    rv %in% abuscularmycorrhizae_maize ~ "Biotic Factors",
    
    
    
    
    ##Climate Mitigation####
    
    #Carbon Mitigation####
    rv %in% chem_maize_SOC ~ "Carbon Mitigation",
    rv %in% chem_SOC ~ "Carbon Mitigation",
    rv %in% envir_CO2 ~ "Carbon Mitigation",
    rv %in% envir_CH4 ~ "Carbon Mitigation",
    
    #Nitrogen Mitigation
    rv %in% envir_N2O ~ "Nitrogen Mitigation",
    
    #Global Warming Potential#####
    rv %in% envir_globalwarmingpotential ~ "Global Warming Potential",
    
    
    ###Pests####
    #Weeds ####
    
    rv %in% weed_broadleaf ~ "Weeds",
    rv %in% weed_grass ~ "Weeds",
    
    rv %in% weed_community_abundance ~ "Weeds",
    rv %in% weed_community_diversity ~ "Weeds",
    
    rv %in% weed_lambsquarters ~ "Weeds",
    rv %in% weed_amaranthus ~ "Weeds",
    rv %in% weed_fallpanicum ~ "Weeds",
    rv %in% weed_velvetleaf ~ "Weeds",
    
    
    #Invertebrate Pests ####
    
    rv %in% invert_pests_cornrootworm ~ "Invertebrate Pests",
    rv %in% invert_pests_seedcornmaggot ~ "Invertebrate Pests",
    rv %in% invert_pests_Aglycines ~ "Invertebrate Pests",
    rv %in% invert_pests_cornborer ~ "Invertebrate Pests",
    rv %in% invert_pests_miscabundance ~ "Invertebrate Pests",
    
    #Pest Natural Enemies ####
    rv %in% invert_preds ~ "Pest Natural Enemies",
    rv %in% invert_preds_activity ~ "Pest Natural Enemies",
    rv %in% invert_preds_soilcomm_abund ~ "Pest Natural Enemies",
    
    #Non-Predators & Pests #####
    rv %in% invert_nonpredpest ~ "Non-Predators & Pests",
    rv %in% nematode_nonpredpest ~ "Non-Predators & Pests",
    rv %in% invert_earthworms ~ "Non-Predators & Pests",
    
    #Pathogens####
    rv %in% pathogen_soybean ~ "Pathogens",
    rv %in% pathogen_maize ~ "Pathogens",
    
    #Nematodes####
    rv %in% nematode_herbivores ~ "Nematodes",
    rv %in% nematode_density ~ "Nematodes",
    rv %in% nematode_soybeancyst ~ "Nematodes",
    
    
    
    
    ####Crop Yields####
    #Grain Production ####
    rv %in% yields_grainsoy ~ "Grain Yields",
    rv %in% yields_cashcrop ~ "Grain Yields",
    rv %in% yields_grainmaize ~ "Grain Yields",
    
    #Grain Quality ####
    rv %in% yields_grainsoy_quality ~ "Grain Quality",
    rv %in% yields_grainmaize_quality ~ "Grain Quality",
    
    #Crop Growth ####
    rv %in% yields_grainmaize_growth ~ "Crop Growth",
    rv %in% yields_maize_biomass_abvgrd ~ "Crop Growth",
    rv %in% yields_grainsoy_growth ~ "Crop Growth",
    rv %in% yields_soybean_biomass_abvgrd ~ "Crop Growth",
    rv %in% yields_cashcrop_biomass_abvgrd ~ "Crop Growth",
    rv %in% yields_biomass_blwgrd ~ "Crop Growth",
    
    #Water Use ####
    rv %in% yields_maize_wateruse ~ "Water Use",
    rv %in% yields_soy_wateruse ~ "Water Use",
    
    #Corn Nutrient Content ####
    rv %in% crop_N_maizestalk ~ "Corn Nutrient Content",
    rv %in% crop_N_maizeroot ~ "Corn Nutrient Content",
    rv %in% crop_N_maizegrain ~ "Corn Nutrient Content",
    rv %in% crop_P_maizeabvgrd ~ "Corn Nutrient Content",
    rv %in% crop_miscNutrients_maize ~ "Corn Nutrient Content",
    
    #Soybean Nutrient Content ####
    rv %in% crop_N_soybean ~ "Soybean Nutrient Content",
    rv %in% crop_P_soybean ~ "Soybean Nutrient Content",
    
    #Crop stand count####
    rv %in% standcount_maize ~ "Stand Count",
    rv %in% standcount_soy ~ "Stand Count",
    
    #Crop Damage####
    rv %in% standcount_maize_damage ~ "Crop Damage",
    rv %in% invert_pests_damage ~ "Crop Damage",
    
    
    ####Water Movement####
    #Nutrient Runoff ####
    rv %in% runoff_nitrate ~ "Nutrient Runoff",
    rv %in% runoff_phosphorus ~ "Nutrient Runoff",
    rv %in% runoff_pesticides ~ "Agrochemical Runoff",
    rv %in% runoff_carbon ~ "Nutrient Runoff",
    rv %in% runoff_miscnutrients ~ "Nutrient Runoff",
    rv %in% runoff_manure ~ "Nutrient Runoff",
    
    #Flow quantity ####
    rv %in% runoff_water ~ "Flow Quantity",
    
    #Sediment ####
    rv %in% runoff_sediment ~ "Sediment Runoff"
  )) %>%
  
  
  
  #######group_level3 #################

mutate(
  group_level3 = case_when(
    
    
    
    #Soil Nutrients####
    #N####
    rv %in% chem_nitrogen_SOM ~ "N content of soil organic matter",
    rv %in% chem_crop_SON ~ "Crop derived N",
    rv %in% chem_totalN ~ "Total N",
    rv %in% chem_ammonium ~ "Ammonium (NH4)",
    rv %in% chem_nitrate ~ "Nitrate (NO3)",
    rv %in% chem_CNratio ~ "C to N ratio",
    
    #P & K####
    rv %in% chem_phosphorus ~ "Phosphorus",
    
    #Potassium####
    rv %in% chem_potassium ~ "Potassium",
    
    #Micro-nutrients####
    rv %in% chem_calcium ~ "Calcium",
    rv %in% chem_magnesium ~ "Magnesium",
    rv %in% chem_misc_nutrients ~ "Misc. micro-nutrients",
    
    
    
    ### Other Soil Properties####
    #Chemical Properties####
    rv %in% chem_pH ~ "pH",
    rv %in% chem_CEC ~ "CEC",
    
    #Physical Properties
    rv %in% chem_som ~ "Soil organic matter content",
    rv %in% phy_aggregation_stability ~ "Aggregate stability",
    rv %in% phy_aggregation_diameter ~ "Aggregate size",
    
    rv %in% phy_silt ~ "Silt content",
    rv %in% phy_clay ~ "Clay content",
    rv %in% phy_sand ~ "Sand content",
    rv %in% phy_compaction ~ "Compaction",
    rv %in% phy_bulkdensity ~ "Bulk density",
    
    rv %in% phy_surfaceresidue ~ "Surface residue coverage",
    rv %in% phy_surfaceresidue_decomp ~ "Decomposition rate of surface residue",
    rv %in% phy_erosion ~ "Erosion", 
    rv %in% phy_airfilled_pores ~ "Air-filled pore space",
    rv %in% phy_totalpores ~ "Total pore space",
    
    #Abiotic Factors####
    rv %in% envir_temp ~ "Temperature",
    rv %in% phy_waterinfiltration ~ "Water infiltration",
    rv %in% phy_watercontent ~ "Moisture content",
    rv %in% phy_waterfilled_pores ~ "Water-filled pore space",
    
    #Biotic Factors#####
    rv %in% biol_respiration ~ "Respiration",
    rv %in% biol_fungi_abund ~ "Fungal abundance",
    rv %in% biol_enzyme_activity ~ "Enzyme activity",
    rv %in% biol_microbial_biomass ~ "Microbial biomass",
    rv %in% abuscularmycorrhizae_soybean ~ "Soybean mycorrhizal colonization",
    rv %in% abuscularmycorrhizae_maize ~ "Corn mycorrhizal colonization",
    
    
    ##Climate Mitigation####
    #Carbon Mitigation####
    rv %in% chem_maize_SOC ~ "Soil organic carbon derived from corn",
    rv %in% chem_SOC ~ "Soil organic carbon",
    rv %in% envir_CO2 ~ "Carbon dioxide (CO2)",
    rv %in% envir_CH4 ~ "Methane (CH4)",
    
    #Nitrogen Mitigation
    rv %in% envir_N2O ~ "Nitrous oxide (N2O)",
    
    #Global Warming Potential#####
    rv %in% envir_globalwarmingpotential ~ "Methane and carbon dioxide (CH4 + CO2)",
    
    
    ###Pests####
    #Weeds ####
    
    rv %in% weed_broadleaf ~ "Broadleafs",
    rv %in% weed_grass ~ "Grasses",
    
    rv %in% weed_community_abundance ~ "Weed community (#)",
    rv %in% weed_community_diversity ~ "Weed community (diversity)",
    
    rv %in% weed_lambsquarters ~ "Lambsquarters (#)",
    rv %in% weed_amaranthus ~ "Amaranthus (#)",
    rv %in% weed_fallpanicum ~ "Fall Panicum (#)",
    rv %in% weed_velvetleaf ~ "Velvetleaf (#)",
    
    
    #Invertebrate Pests ####
    
    rv %in% invert_pests_cornrootworm ~ "Corn rootworm (#)",
    rv %in% invert_pests_seedcornmaggot ~ "Seedcorn maggot (#)",
    rv %in% invert_pests_Aglycines ~ "Soybean aphid (#)",
    rv %in% invert_pests_cornborer ~ "Corn borer (#)",
    rv %in% invert_pests_miscabundance ~ "General pests (#)",
    
    #Pest Natural Enemies ####
    rv %in% invert_preds ~ "Natural enemy (#)",
    rv %in% invert_preds_activity ~ "Natural enemy (activity)",
    rv %in% invert_preds_soilcomm_abund ~ "Soil-inhabiting natural enemy (#)",
    
    #Non-Predators & Pests #####
    rv %in% invert_nonpredpest ~ "Invertebrates (#)",
    rv %in% nematode_nonpredpest ~ "Nematodes (#)",
    rv %in% invert_earthworms ~ "Earthworms (#)",
    
    #Pathogens####
    rv %in% pathogen_soybean ~ "Soybean infections",
    rv %in% pathogen_maize ~ "Corn infections",
    
    #Nematodes####
    rv %in% nematode_herbivores ~ "Herbivores",
    rv %in% nematode_density ~ "Nematode (#)",
    rv %in% nematode_soybeancyst ~ "Soybean cyst nematode (#)",
    
    
    
    
    ####Crop Yields####
    #Grain Production ####
    rv %in% yields_grainsoy ~ "Soybean",
    rv %in% yields_cashcrop ~ "Corn and soybean",
    rv %in% yields_grainmaize ~ "Corn",
    
    #Grain Quality ####
    rv %in% yields_grainsoy_quality ~ "Soybean",
    rv %in% yields_grainmaize_quality ~ "Corn",
    
    #Crop Growth ####
    rv %in% yields_grainmaize_growth ~ "Corn (height)",
    rv %in% yields_maize_biomass_abvgrd ~ "Corn stover (biomass)",
    rv %in% yields_grainsoy_growth ~ "Soybean (height)",
    rv %in% yields_soybean_biomass_abvgrd ~ "Soybean tissue (biomass)",
    rv %in% yields_cashcrop_biomass_abvgrd ~ "Corn and soybean tissue (biomass) ",
    rv %in% yields_biomass_blwgrd ~ "Corn root (biomass)",
    
    #Water Use ####
    rv %in% yields_maize_wateruse ~ "Corn",
    rv %in% yields_soy_wateruse ~ "Soybean",
    
    #Corn Nutrient Content ####
    rv %in% crop_N_maizestalk ~ "Stover nitrogen",
    rv %in% crop_N_maizeroot ~ "Root nitrogen",
    rv %in% crop_N_maizegrain ~ "Grain nitrogen",
    rv %in% crop_P_maizeabvgrd ~ "Stover phosphorus",
    rv %in% crop_miscNutrients_maize ~ "Stover micro-nutrients",
    
    #Soybean Nutrient Content ####
    rv %in% crop_N_soybean ~ "Tissue nitrogen",
    rv %in% crop_P_soybean ~ "Tissue phosphorus",
    
    #Crop stand count####
    rv %in% standcount_maize ~ "Corn",
    rv %in% standcount_soy ~ "Soybean",
    
    #Crop Damage####
    rv %in% standcount_maize_damage ~ "Corn (stand count)",
    rv %in% invert_pests_damage ~ "Invertebrate damage",
    
    
    ####Water Movement####
    #Nutrient Runoff ####
    rv %in% runoff_nitrate ~ "Nitrate",
    rv %in% runoff_phosphorus ~ "Phosphorus",
    rv %in% runoff_pesticides ~ "Pesticides",
    rv %in% runoff_carbon ~ "Carbon",
    rv %in% runoff_miscnutrients ~ "Micro-nutrients",
    rv %in% runoff_manure ~ "Manure",
    
    #Flow quantity ####
    rv %in% runoff_water ~ "Water",
    
    #Sediment ####
    rv %in% runoff_sediment ~ "Sediment Runoff"
  )) 

#Attach column to df######
df <-
  left_join(df, grouped_table, by = c("review_key", "rv"))


##Standarize ranking of tillage treatments based on Reicoisky 2015##########
# "Conservation tillage is not conservation agriculture", Journal of Soil and Water Conservation
#Sept 2015, doi: 10.2489/jswc.70.5.103A
#See Fig 2 for list and rankings (Conventional tillage - Conservation tillage - No tillage)

#Tillage type (abbreviation)
# 0. Conventional tillage (conventional) # Authors do not explicitly state the type of tillage used
# 1. Moldboard plow (MP)
# 2. Disc plow (disc)
# 3. Deep ripper (deep)
# 4. Subsoil-HD (subsoil)
# 5. Rotary tillage (rotary)
# 6. Chisel plow (CP)
# 6.5 Conservation tillage (conservation) # Authors do not explicitly state the type of tillage used
# 7. Field cultivator (cultivator)
# 7.5 Deep zonal tillage
# 8. Ridge till (RT)
# 9. Subsoil-LD (subsoil_low)
# 10. Vertical tillage (vertical)
# 11. Reduced tillage (reduced)
# 12. Mulch tillage (mulch)
# 13. Stubble mulch (stubble)
# 14. Strip tillage (ST)
# 15. Slot tillage (slot)
# 16. No tillage- LD & HD (NT)


#Add tillage rankings to new columns [ tilltype_1 & tilltype_2]

df <- df %>%
  mutate(tilltype_1 = if_else(str_detect(finelevel_group, "conventional_"), 0,
                              if_else(str_detect(finelevel_group, "MPCP_"), 6, #chisel plow treatment
                                      if_else(str_detect(finelevel_group, "MP_"), 1,
                                              if_else(str_detect(finelevel_group, "MPmini_"), 1,
                                                      if_else(str_detect(finelevel_group, "disc_"), 2, 
                                                              if_else(str_detect(finelevel_group, "deep_"), 3, 
                      if_else(str_detect(finelevel_group, "deeptill_"), 3, 
                              if_else(str_detect(finelevel_group, "deep90_"), 3,
                                      if_else(str_detect(finelevel_group, "deep60_"), 3,
                                              if_else(str_detect(finelevel_group, "deep40_"), 3, 
                                                      if_else(str_detect(finelevel_group, "subsoil_"), 4, 
                                                              if_else(str_detect(finelevel_group, "rotary_"), 5, 
                                                                      if_else(str_detect(finelevel_group, "CP_"), 6,
                                                                              if_else(str_detect(finelevel_group, "CPnew_"), 6,
                                          
                    if_else(str_detect(finelevel_group, "conservation_"), 6.5, 
                          if_else(str_detect(finelevel_group, "cultivator_"), 7,
                                  if_else(str_detect(finelevel_group, "deepzone_"), 7.5,
                                          if_else(str_detect(finelevel_group, "RT_"), 8, 
                                                  if_else(str_detect(finelevel_group, "subsoil_low_"), 9, 
                                                          if_else(str_detect(finelevel_group, "vertical_"), 10, 
                                                                  if_else(str_detect(finelevel_group, "reduced_"), 11,
                    if_else(str_detect(finelevel_group, "mulch_"), 12, 
                            if_else(str_detect(finelevel_group, "stubble_"), 13, 
                                    if_else(str_detect(finelevel_group, "ST_"), 14,
                                            if_else(str_detect(finelevel_group, "slot_"), 15, 
                                                    if_else(str_detect(finelevel_group, "NT_"), 16,
                                                            if_else(str_detect(finelevel_group, "NTnew_"), 16,
                                              99999999)))))))))))))))))))))))))))) %>%
mutate(tilltype_2 = if_else(str_detect(finelevel_group, "_conventional"), 0,
                              if_else(str_detect(finelevel_group, "_MPCP"), 6, #chisel plow treatment
                                      if_else(str_detect(finelevel_group, "_MP"), 1,
                                              if_else(str_detect(finelevel_group, "_MPmini"), 1,
                                                      if_else(str_detect(finelevel_group, "_disc"), 2, 
                                                              if_else(str_detect(finelevel_group, "_deep"), 3, 
                                                                      if_else(str_detect(finelevel_group, "_deeptill"), 3, 
                    if_else(str_detect(finelevel_group, "_deep90"), 3,
                            if_else(str_detect(finelevel_group, "_deep60"), 3,
                                    if_else(str_detect(finelevel_group, "_deep40"), 3, 
                                            if_else(str_detect(finelevel_group, "_subsoil"), 4, 
                                                    if_else(str_detect(finelevel_group, "_rotary"), 5, 
                                                            if_else(str_detect(finelevel_group, "_CP"), 6,
                                                                    
                    if_else(str_detect(finelevel_group, "_CPnew"), 6,
                            if_else(str_detect(finelevel_group, "_conservation"), 6.5, 
                                    if_else(str_detect(finelevel_group, "_cultivator"), 7,
                                          if_else(str_detect(finelevel_group, "_deepzone"),7.5,
                                                  if_else(str_detect(finelevel_group, "_RT"), 8, 
                                                          if_else(str_detect(finelevel_group, "_subsoil_low"), 9, 
                                                                  if_else(str_detect(finelevel_group, "_vertical"), 10, 
                    if_else(str_detect(finelevel_group, "_reduced"), 11,
                            if_else(str_detect(finelevel_group, "_mulch"), 12, 
                                    if_else(str_detect(finelevel_group, "_stubble"), 13, 
                                            if_else(str_detect(finelevel_group, "_ST"), 14,
                                                    if_else(str_detect(finelevel_group, "_slot"), 15, 
                                                            if_else(str_detect(finelevel_group, "_NT"), 16,
                                                                    if_else(str_detect(finelevel_group, "_NTnew"), 16,
                                                                            999999))))))))))))))))))))))))))))


####Use tillage rankings to reorganize comparisons where higher ranking is listed in tilltype_1########################################################################################
df$trt1 <- as.integer(df$trt1)
df$trt2 <- as.integer(df$trt2)
df$finelevel_group <- as.character(df$finelevel_group)
df$tilltype_1 <- as.integer(df$tilltype_1)
df$tilltype_2 <- as.integer(df$tilltype_2)
df$trt1_int <- as.integer(df$trt1_int)
df$trt2_int <- as.integer(df$trt2_int)
df$trt1_int2 <- as.integer(df$trt1_int2)
df$trt2_int2 <- as.integer(df$trt2_int2)
df$significance <- as.character(as.factor(df$significance))
df$trt1_name <- as.character(as.factor(df$trt1_name))
df$trt2_name <- as.character(as.factor(df$trt2_name))
df$trt1_details <- as.character(as.factor(df$trt1_details))
df$trt2_details <- as.character(as.factor(df$trt2_details))
df$trt1_value <- as.numeric(df$trt1_value)
df$trt2_value <- as.numeric(df$trt2_value)


df2 <- df %>%
  mutate(trt1_new = case_when(tilltype_1 < tilltype_2 ~ trt1,
                          tilltype_1 > tilltype_2 ~ trt2,
                          tilltype_1 == tilltype_2 ~ trt1)) %>%
  mutate(trt1_int_new = case_when(tilltype_1 < tilltype_2 ~ trt1_int,
                              tilltype_1 > tilltype_2 ~ trt2_int,
                              tilltype_1 == tilltype_2 ~ trt1_int)) %>%
  mutate(trt1_int2_new = case_when(tilltype_1 < tilltype_2 ~ trt1_int2,
                               tilltype_1 > tilltype_2 ~ trt2_int2,
                               tilltype_1 == tilltype_2 ~ trt1_int2)) %>%
  mutate(trt1_value_new = case_when(tilltype_1 < tilltype_2 ~ trt1_value,
                                tilltype_1 > tilltype_2 ~ trt2_value,
                                tilltype_1 == tilltype_2 ~ trt1_value)) %>%
  mutate(trt2_new = case_when(tilltype_1 < tilltype_2 ~ trt2,
                          tilltype_1 > tilltype_2 ~ trt1,
                          tilltype_1 == tilltype_2 ~ trt2)) %>%
  mutate(trt2_int_new = case_when(tilltype_1 < tilltype_2 ~ trt2_int,
                              tilltype_1 > tilltype_2 ~ trt1_int,
                              tilltype_1 == tilltype_2 ~ trt2_int)) %>%
  mutate(trt2_int2_new = case_when(tilltype_1 < tilltype_2 ~ trt2_int2,
                               tilltype_1 > tilltype_2 ~ trt1_int2,
                               tilltype_1 == tilltype_2 ~ trt2_int2)) %>%
  mutate(trt2_value_new = case_when(tilltype_1 < tilltype_2 ~ trt2_value,
                                tilltype_1 > tilltype_2 ~ trt1_value,
                                tilltype_1 == tilltype_2 ~ trt2_value))%>%
  
  #dropping Normative effect - having difficulties coercing it into the opposite value based on criteria below
  #mutate(norm_effect = if_else(tilltype_1 < tilltype_2, Effect_norm,
  #                           if_else(Effect_norm == 0, Effect_norm,
  #                           if_else(is.na(Effect_norm), Effect_norm,
  #                                  if_else(Effect_norm == "", Effect_norm,
  #                         if_else(tilltype_1 > tilltype_2 && Effect_norm %in% "1", paste("-1"),
  #                        if_else(tilltype_1 > tilltype_2 && Effect_norm %in% "-1", paste("1"), "ALBERT"))))))) %>% 
  mutate(finelevel_group_new = if_else(tilltype_1 < tilltype_2, finelevel_group,
                                   if_else(tilltype_1 > tilltype_2, paste(finelevel_group, " reverse"), 
                                           if_else(tilltype_1 == tilltype_2, finelevel_group, "Albert")))) %>%
  mutate(trt1_name_new = case_when(tilltype_1 < tilltype_2 ~ trt1_name,
                               tilltype_1 > tilltype_2 ~ trt2_name,
                               tilltype_1 == tilltype_2 ~ trt1_name)) %>%
  mutate(trt1_details_new = case_when(tilltype_1 < tilltype_2 ~ trt1_details,
                                      tilltype_1 > tilltype_2 ~ trt2_details,
                                      tilltype_1 == tilltype_2 ~ trt1_details)) %>%
  mutate(trt2_name_new = case_when(tilltype_1 < tilltype_2 ~ trt2_name,
                               tilltype_1 > tilltype_2 ~ trt1_name,
                               tilltype_1 == tilltype_2 ~ trt2_name)) %>%
  mutate(trt2_details_new = case_when(tilltype_1 < tilltype_2 ~ trt2_details,
                                      tilltype_1 > tilltype_2 ~ trt1_details,
                                      tilltype_1 == tilltype_2 ~ trt2_details)) %>%
  mutate(tillage_1 = case_when(tilltype_1 < tilltype_2 ~ tilltype_1,
                               tilltype_1 > tilltype_2 ~ tilltype_2,
                               tilltype_1 == tilltype_2 ~ tilltype_1)) %>%
  mutate(tillage_2 = case_when(tilltype_1 < tilltype_2 ~ tilltype_2,
                               tilltype_1 > tilltype_2 ~ tilltype_1,
                               tilltype_1 == tilltype_2 ~ tilltype_2)) 

#Display rows that should have changed with previous code...higher value tillage number now always on right
df3 <- df2 %>% filter(tilltype_1 > tilltype_2) %>% select(tilltype_1, tilltype_2, tillage_1, tillage_2)
#looks good!


#Now drop columns that these new columns replace####
df3 <- df2 %>%
  select(paper_id:stat_type, significance:effect_norm, review_key:group_level3, trt1_new:tillage_2) 

#rename columns to be consistent with other reviews

df3 <- rename(df3, trt1 = trt1_new, trt1_int = trt1_int_new, trt1_int2 = trt1_int2_new,
             trt1_name = trt1_name_new, trt1_value = trt1_value_new, trt1_details = trt1_details_new,
             trt2_name = trt2_name_new, trt2_value = trt2_value_new,
             trt2_details = trt2_details_new, 
             trt2 = trt2_new, trt2_int = trt2_int_new, trt2_int2 = trt2_int2_new,
             finelevel_group = finelevel_group_new
)

df <- df3

######Organize soil sampling depth and year variables########

unique(levels(df$rv_depth))

#####soil depth groupings#####
#These will display such that it is always displying the df from more shallow sampling depths + deepest depth
#Organized by means of sampling depth

depth_0_30 <- c(
  "0-10 cm",
  "0-15 cm",
  "0-20 cm",
  "0-5 cm",
  "0-25 cm",
  "0-7.5 cm",
  "0-16 cm",
  "0-2 cm",
  "0-2.5 cm",
  "0-20 cm",
  "0-23 cm",
  "0-24 cm",
  "0-25 cm",
  "0-8 cm",
  "16 cm",
  "2-5 cm",
  "2 cm",
  "2.5-5 cm",
  "2.5 cm",
  "23 cm",
  "5-10 cm",
  "5-15 cm",
  "5-17.5 cm",
  "5-20 cm",
  "5-7.5 cm",
  "7.5-15 cm",
  "8-16 cm",
  "9 cm",
  "surface layer",
  "clod",
  "10-15 cm",
  "10-20 cm",
  "10-25 cm",
  "10 cm",
  "15-20 cm",
  "15-22.5 cm",
  "15-25 cm",
  "16-24 cm",
  "20-25 cm",
  "20-40 cm",
  "30 cm",
  "15-30 cm",
  "10-30 cm",
  "25-30 cm",
  "20-30 cm",
  "0-35 cm",
  "0-40 cm",
  "0-50 cm",
  "0-30.5 cm",
  "0-38 cm",
  "0-40 cm",
  "0-45 cm",
  "0-53 cm",
  "0-30.5 cm",
  "0-30 cm"
  
)

depth_0_60 <- c(
  "25-50 cm",
  "30-40 cm",
  "30-45 cm",
  "30-50 cm",
  "40-50 cm",
  "15-45 cm",
  "17.5-30 cm",
  "40-60 cm",
  "45-60 cm",
  "30-60 cm",
  "subsoil layer",
  "50-60 cm",
  "50-70 cm",
  "45-75 cm",
  "subsoil and surface layers",
  "0-60 cm",
  "0-70 cm",
  "0-80 cm",
  "0-100 cm",
  "0-120 cm",
  "0-68 cm",
  "0-75 cm",
  "0-90 cm",
  "20-100 cm")


depth_0_100 <- c(
  "60-100 cm",
  "50-75 cm",
  "60-75 cm",
  "60-80 cm",
  "60-90 cm",
  "70-90 cm",
  "75-100 cm",
  "75-105 cm",
  "80-100 cm")

depth_0_150 <- c(
  "90-120 cm",
  "120-150 cm",
  "0-300 cm",
  "150 cm")




#####Apply soil depth groupings####

df <- df %>%
  mutate(
    sample_depth = case_when(
      # RV_depth %in% NA_real_  ~ "Soil Surface",
      rv_depth %in% depth_0_30 ~ "0-30 cm",
      rv_depth %in% depth_0_60 ~ "0-60 cm",
      rv_depth %in% depth_0_100 ~ "0-100 cm",
      rv_depth %in% depth_0_150 ~ "0-150 cm"))



####Treatment Comparisons#####


#There are very few papers for a majority of the tillage practices x outcomes in this database
#To maximize the amount of data we can display, Tillage practices will be merged based on similar disturbance regimes

#Remove these rows and replace with consistent names
df$trt1_name <- NULL
df$trt2_name <- NULL

df <- df %>%
  mutate(
    trt1_name = case_when(
      
      #Replace tilltype_1 rankings with names of tillages
      
      #Group 1: Conventional Tillage <- Moldboard plow, Disc plow, Deep ripper, Dubsoil deep, Rotary tillage
      tillage_1 %in% 1 ~ "Conventional tillage", #Moldboard plow
      tillage_1 %in% 0 ~ "Conventional tillage", #name given to tillage practice in paper - no further specifications provided
      tillage_1 %in% 2 ~ "Conventional tillage", #"Disc plow"
      tillage_1 %in% 3 ~ "Conventional tillage", #"Deep ripper"
      tillage_1 %in% 4 ~ "Conventional tillage", #"Subsoil deep"
      tillage_1 %in% 5 ~ "Conventional tillage", #"Rotary tillage"
      
      #Group 2: Conservation tillage <- Chisel plow, Field cultivation, Subsoil shalow, Vertical tillage, Reduced tillage, Mulch Tillage
      tillage_1 %in% 6 ~ "Conservation tillage", #Chisel plow
      tillage_1 %in% 6.5 ~ "Conservation tillage", #name given to tillage practice in paper - no further specifications provided
      tillage_1 %in% 7 ~ "Conservation tillage", #"Field cultivator"
      tillage_1 %in% 9 ~ "Conservation tillage", #"Subsoil shallow"
      tillage_1 %in% 10 ~ "Conservation tillage", #"Vertical tillage"
      tillage_1 %in% 11 ~ "Conservation tillage", #"Reduced tillage"
      tillage_1 %in% 12 ~ "Conservation tillage", #"Mulch tillage"
      tillage_1 %in% 13 ~ "Conservation tillage", #"Stubble mulch"
      
      #Group 3: Zonal tillage <- Deep zonal tillage, Ridge tillage, Strip tillage, 
      tillage_1 %in% 7.5 ~ "Zonal tillage" , #"Deep zonal tillage"
      tillage_1 %in% 8 ~ "Zonal tillage", #"Ridge till"
      tillage_1 %in% 14 ~ "Zonal tillage", #"Strip tillage"
      
      #Group 4: No tillage (Slot tillage)
      tillage_1 %in% 15 ~ "No tillage", #"Slot tillage"
      tillage_1 %in% 16 ~ "No tillage",
      
      TRUE ~ "Albert"))

df <- df %>%      
  mutate(
    trt2_name = case_when(
      
      #Replace tilltype_2 rankings with names of tillages
      #Group 1: Conventional Tillage <- Moldboard plow, Disc plow, Deep ripper, Dubsoil deep, Rotary tillage
      tillage_2 %in% 1 ~ "Conventional tillage", #Moldboard plow
      tillage_2 %in% 0 ~ "Conventional tillage", #name given to tillage practice in paper - no further specifications provided
      tillage_2 %in% 2 ~ "Conventional tillage", #"Disc plow"
      tillage_2 %in% 3 ~ "Conventional tillage", #"Deep ripper"
      tillage_2 %in% 4 ~ "Conventional tillage", #"Subsoil deep"
      tillage_2 %in% 5 ~ "Conventional tillage", #"Rotary tillage"
      
      #Group 2: Conservation tillage <- Chisel plow, Field cultivation, Subsoil shalow, Vertical tillage, Reduced tillage, Mulch Tillage
      tillage_2 %in% 6 ~ "Conservation tillage", #Chisel plow
      tillage_2 %in% 6.5 ~ "Conservation tillage", #name given to tillage practice in paper - no further specifications provided
      tillage_2 %in% 7 ~ "Conservation tillage", #"Field cultivator"
      tillage_2 %in% 9 ~ "Conservation tillage", #"Subsoil shallow"
      tillage_2 %in% 10 ~ "Conservation tillage", #"Vertical tillage"
      tillage_2 %in% 11 ~ "Conservation tillage", #"Reduced tillage"
      tillage_2 %in% 12 ~ "Conservation tillage", #"Mulch tillage"
      tillage_2 %in% 13 ~ "Conservation tillage", #"Stubble mulch"
      
      #Group 3: Zonal tillage <- Deep zonal tillage, Ridge tillage, Strip tillage, 
      tillage_2 %in% 7.5 ~ "Zonal tillage" , #"Deep zonal tillage"
      tillage_2 %in% 8 ~ "Zonal tillage", #"Ridge till"
      tillage_2 %in% 14 ~ "Zonal tillage", #"Strip tillage"
      
      #Group 4: No tillage (Slot tillage)
      tillage_2 %in% 15 ~ "No tillage", #"Slot tillage"
      tillage_2 %in% 16 ~ "No tillage",
      
      TRUE ~ "Albert"))

#Lists treatments compared for each row
df <- df %>%      
  mutate(trt_compare = str_c(trt1_name, trt2_name, sep = " - ")) %>%
  mutate(review = paste("Tillage"))

levels(as.factor(df$trt_compare))

#[1] "Conservation tillage - Conservation tillage" "Conservation tillage - No tillage"           "Conservation tillage - Zonal tillage"       
#[4] "Conventional tillage - Conservation tillage" "Conventional tillage - Conventional tillage" "Conventional tillage - No tillage"          
#[7] "Conventional tillage - Zonal tillage"        "No tillage - No tillage"                     "Zonal tillage - Conservation tillage"       
#[10] "Zonal tillage - No tillage"  




###Examine data to see if any group labels are missing####################
missing <- df[is.na(df$group_level1),] #check to see if all rows have an assigned group_level1
missing <- df[is.na(df$group_level2),] #check to see if all rows have an assigned group_level2
missing <- df[is.na(df$group_level3),] #check to see if all rows have an assigned group_level3


#Rearrange column order
df2 <- select(df, review, paper_id, duration, rv_year, loc_multi_results,
              group_level1, group_level2, group_level3, rv, rv_depth, sample_depth,
              rv_units, stat_test, stat_type, trt1, trt1_int, trt1_int2, trt1_value,        
              trt2, trt2_int, trt2_int2, trt2_value,
              significance, effect_norm, finelevel_group, trt1_name, trt1_details, trt2_name,
              trt2_details, tillage_1, tillage_2, trt_compare 
)

###Export CSV####################
write.csv(df2, file = "Tillage_ResultsGrouped_move.csv", row.names = FALSE)
