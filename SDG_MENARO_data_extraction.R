# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: SDG data extraction from the SDG Global Database for MENARO countries
# Author: Sebastian Palmas

rm(list=ls())

# PROFILE ----

USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
USER        <- Sys.getenv("USER")

#file paths for each user of the repository
if (USERNAME == "palma"){
  projectFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/MENARO SDG")) #Output files
  repoFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "code/SDG-MENARO")) #repository files
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "code/SDG-MENARO/source_data/"))  #raw data folder
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))
stopifnot(dir.exists(rawdataFolder))

# PACKAGES  ----
library(dplyr) # mutate select left_join group_by filter ungroup if_else lag case_when n summarise first sym rename # |> mutate select left_join group_by filter ungroup if_else lag case_when n summarise first sym rename
library(httr) # GET status_code content
library(jsonlite) # fromJSON
library(openxlsx) # read.xlsx write.xlsx createWorkbook addWorksheet writeData saveWorkbook insertImage
library(plyr) # mutate summarise rename rbind.fill
library(stringr)
# library(purrr) # map_dbl
# library(tidyr) # unnest


# SOURCE FILES ----
## Functions  ----
source("helpers/api_to_json.R")
source("helpers/calculate_percentile.R")
source("helpers/SDG_series_getp.R")
source("helpers/SDGdata.R")
source("helpers/unlist_columns.R")

## MENARO countries ----
MENARO_metadata <- read.csv(file.path(rawdataFolder,"MENARO_metadata.csv")) 

## SDG indicator series codes ----
#metadata of SDG child indicators. Includes the code in the SDG Global Database
CR_SDG_indicators <- read.xlsx(file.path(rawdataFolder, "child_related_SDG_indicators.xlsx"), sheet = "child_related_SDG_indicators")


# SDGGD DOWNLOAD ----
## Creating list of series to download ----
CR_SDG_series <- CR_SDG_indicators$SDG_SERIES_DESCR |> unique()  #get unique values from table
CR_SDG_series <- CR_SDG_series[!is.na(CR_SDG_series)]  #remove NA values
CR_SDG_series <- CR_SDG_series[!(CR_SDG_series == "SI_POV_DAY1")]  #remove series that will be downloaded from other source

## Download (last update: 2025-01-15)----
sdgdb_world <- SDGdata(CR_SDG_series)  #this some 15 minutes to run

## Cleaning table ----
# Check for NA values in the esadb object
if (any(is.na(sdgdb_world))) {
  # Replace NA values with a suitable placeholder
  sdgdb_world[is.na(sdgdb_world)] <- "NA"
}

#unlisting columns
sdgdb_world$footnotes <- NULL #This list is weird and has less values than rows
sdgdb_world <- unlist_columns(sdgdb_world)

#deleting rows that are not of use (disaggregations not included in our indicators)
sdgdb_world <- sdgdb_world |> filter(!(series == "SE_DEV_ONTRK" & dimensions.Sex %in% c("FEMALE", "MALE")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SE_PRE_PARTN" & dimensions.Sex %in% c("FEMALE", "MALE")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SE_TOT_PRFL" & dimensions.Sex %in% c("FEMALE", "MALE")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SG_REG_BRTH" & dimensions.Age %in% c("M48T59", "M36T47", "M24T35", "M12T23", "<1Y", "<8Y")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SH_DYN_MORT" & dimensions.Sex %in% c("FEMALE", "MALE")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SH_H2O_SAFE" & dimensions.Location %in% c("RURAL", "URBAN")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SH_HIV_INCD" & dimensions.Sex %in% c("FEMALE", "MALE")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SH_HIV_INCD" & dimensions.Age %in% c("15-24", "15-49", "50+", "ALLAGE")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SH_SAN_DEFECT" & dimensions.Location %in% c("RURAL", "URBAN")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SH_SAN_HNDWSH" & dimensions.Location %in% c("RURAL", "URBAN")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SH_SAN_SAFE" & dimensions.Location %in% c("RURAL", "URBAN")))
#sdgdb_world <- sdgdb_world |> filter(!(series == "SI_POV_DAY1" ))
#sdgdb_world <- sdgdb_world |> filter(!(series == "SI_POV_DAY1" & dimensions.Sex %in% c("FEMALE", "MALE")))
#sdgdb_world <- sdgdb_world |> filter(!(series == "SI_POV_DAY1" & dimensions.Age %in% c("ALLAGE", "15-64", "65+")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SI_POV_NAHC" & dimensions.Location %in% c("RURAL", "URBAN")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SL_TLF_CHLDEC" & dimensions.Sex %in% c("FEMALE", "MALE")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SL_TLF_CHLDEC" & dimensions.Age %in% c("5-14","10-17")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SP_DYN_ADKL" & dimensions.Age %in% c("10-14")))
sdgdb_world <- sdgdb_world |> filter(!(series == "VC_VAW_MARR" & dimensions.Age %in% c("15-49")))
sdgdb_world <- sdgdb_world |> filter(!(series == "VC_VAW_PHYPYV" & dimensions.Age %in% c("1-4", "2-14", "5-12")))
sdgdb_world <- sdgdb_world |> filter(!(series == "VC_VAW_SXVLN" & dimensions.Age %in% c("18-74")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SI_COV_CHLD" & dimensions.Sex %in% c("FEMALE", "MALE")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SI_COV_CHLD" & dimensions.Age %in% c("<15Y")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SP_ACS_BSRVH2O" & dimensions.Location %in% c("RURAL", "URBAN")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SP_ACS_BSRVSAN" & dimensions.Location %in% c("RURAL", "URBAN")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SE_TOT_CPLR" & dimensions.Sex %in% c("FEMALE", "MALE")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SE_TOT_CPLR" & dimensions.Location %in% c("RURAL", "URBAN")))
sdgdb_world <- sdgdb_world |> filter(!(series == "SE_TOT_CPLR" & dimensions.Quantile %in% c("Q1", "Q2", "Q3", "Q4", "Q5")))

#Changing the codes for indicators that come from the same series
sdgdb_world$series2 <- sdgdb_world$series

sdgdb_world$series2[sdgdb_world$series2 == "SE_TOT_PRFL" &
                      sdgdb_world$`dimensions.Education level` == "GRAD23" &
                      sdgdb_world$`dimensions.Type of skill` == "SKILL_MATH"]  <- "SE_TOT_PRFL_1"
sdgdb_world$series2[sdgdb_world$series2 == "SE_TOT_PRFL" &
                      sdgdb_world$`dimensions.Education level` == "PRIMAR" &
                      sdgdb_world$`dimensions.Type of skill` == "SKILL_MATH"]  <- "SE_TOT_PRFL_2"
sdgdb_world$series2[sdgdb_world$series2 == "SE_TOT_PRFL" &
                      sdgdb_world$`dimensions.Education level` == "LOWSEC" &
                      sdgdb_world$`dimensions.Type of skill` == "SKILL_MATH"]  <- "SE_TOT_PRFL_3"
sdgdb_world$series2[sdgdb_world$series2 == "SE_TOT_PRFL" &
                      sdgdb_world$`dimensions.Education level` == "GRAD23" &
                      sdgdb_world$`dimensions.Type of skill` == "SKILL_READ"]  <- "SE_TOT_PRFL_4"
sdgdb_world$series2[sdgdb_world$series2 == "SE_TOT_PRFL" &
                      sdgdb_world$`dimensions.Education level` == "PRIMAR" &
                      sdgdb_world$`dimensions.Type of skill` == "SKILL_READ"]  <- "SE_TOT_PRFL_5"
sdgdb_world$series2[sdgdb_world$series2 == "SE_TOT_PRFL" &
                      sdgdb_world$`dimensions.Education level` == "LOWSEC" &
                      sdgdb_world$`dimensions.Type of skill` == "SKILL_READ"]  <- "SE_TOT_PRFL_6"

sdgdb_world$series2[sdgdb_world$series2 == "SH_HIV_INCD"]  <- "SH_HIV_INCD_U15"

sdgdb_world$series2[sdgdb_world$series2 == "VC_VAW_SXVLN" & 
                    sdgdb_world$dimensions.Sex == "FEMALE"]  <- "VC_VAW_SXVLN_F"
sdgdb_world$series2[sdgdb_world$series2 == "VC_VAW_SXVLN" &
                    sdgdb_world$dimensions.Sex == "MALE"]  <- "VC_VAW_SXVLN_M"

sdgdb_world$series2[sdgdb_world$series2 == "SE_TOT_CPLR" & 
                      sdgdb_world$`dimensions.Education level` == "PRIMAR"]  <- "SE_TOT_CPLR_PR"
sdgdb_world$series2[sdgdb_world$series2 == "SE_TOT_CPLR" & 
                      sdgdb_world$`dimensions.Education level` == "LOWSEC"]  <- "SE_TOT_CPLR_LS"
sdgdb_world$series2[sdgdb_world$series2 == "SE_TOT_CPLR" & 
                      sdgdb_world$`dimensions.Education level` == "UPPSEC"]  <- "SE_TOT_CPLR_US"

#Value from character to numeric
sdgdb_world$value <- as.numeric(sdgdb_world$value)

#check if indicators only have one row per disaggregation (i.e. that we deleted all unused rows and correctly assigned series2)
sdgdb_world |> dplyr::group_by(series2, 
                               timePeriodStart, 
                               geoAreaName,
                               dimensions.Age,
                               dimensions.Sex, 
                               `dimensions.Education level`,
                               `dimensions.Type of skill`) |> dplyr::tally() |> dplyr::filter(n>1)

## Exporting table ----
save(sdgdb_world, file = file.path(rawdataFolder, "sdgdb_world.Rdata"))
#load(file = file.path(rawdataFolder, "sdgdb_world.Rdata"))

# DW ----
## download from DW (last update: 2025-01-26)----
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,CME,1.0/.CME_MRM0+CME_MRY0T4..?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "CME.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,ECD,1.0/.ECD_CHLD_36-59M_LMPSL..........?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "ECD.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,EDUCATION,1.0/.ED_ANAR_L02+ED_CR_L1+ED_CR_L2+ED_CR_L3+ED_MAT_G23+ED_MAT_L1+ED_MAT_L2+ED_READ_G23+ED_READ_L1+ED_READ_L2+ED_ROFST_L1+ED_ROFST_L2+ED_ROFST_L3.....?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "EDU.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,GENDER,1.0/.GN_SG_LGL_GENEQEMP....?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "GENDER.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,HIV_AIDS,1.0/.HVA_EPI_INF_RT.....?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "HIV.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,IMMUNISATION,1.0/.IM_DTP3+IM_MCV1..?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "IMMU.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,MNCH,1.0/.MNCH_ABR+MNCH_INFDEC+MNCH_MMR+MNCH_SAB+MNCH_UHC.......?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "MNCH.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,NUTRITION,1.0/.NT_ANT_HAZ_NE2_MOD+NT_ANT_WHZ_NE2+NT_ANT_WHZ_PO2......?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "NUTRITION.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,PT,1.0/.PT_CHLD_1-14_PS-PSY-V_CGVR+PT_CHLD_5-17_LBR_ECON-HC+PT_CHLD_Y0T4_REG+PT_F_18-29_SX-V_AGE-18+PT_F_GE15_PS-SX-EM_V_PTNR_12MNTH+PT_M_18-29_SX-V_AGE-18......?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "PT_CHLD.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,PT_CM,1.0/.PT_F_20-24_MRD_U18_TND..........?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "PT_CM.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,PT_FGM,1.0/.PT_F_15-49_FGM...........?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "PT_FGM.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,CHLD_PVTY,1.0/.PV_CHLD_INCM-PL..?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "CHLD_PVTY.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,SOC_PROTECTION,1.0/.SPP_CHLD_SOC_PROT....?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "SOC_PROTECTION.csv"))
# temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,WASH_HOUSEHOLDS,1.0/.WS_PPL_H-B+WS_PPL_S-ALB+WS_PPL_S-OD+WS_PPL_S-SM+WS_PPL_W-ALB+WS_PPL_W-SM...?format=sdmx-csv&labels=both")
# write.csv(x = temp_file, file = file.path(rawdataFolder, "WASH.csv"))

## Read DW files ----
CME <- read.csv(file.path(rawdataFolder, "CME.csv"))
ECD <- read.csv(file.path(rawdataFolder, "ECD.csv"))
EDU <- read.csv(file.path(rawdataFolder, "EDU.csv"))
GENDER <- read.csv(file.path(rawdataFolder, "GENDER.csv"))
HIV <- read.csv(file.path(rawdataFolder, "HIV.csv"))
IMMU <- read.csv(file.path(rawdataFolder, "IMMU.csv"))
MNCH <- read.csv(file.path(rawdataFolder, "MNCH.csv"))
NUTRITION <- read.csv(file.path(rawdataFolder, "NUTRITION.csv"))
PT_CHLD <- read.csv(file.path(rawdataFolder, "PT_CHLD.csv"))
PT_CM <- read.csv(file.path(rawdataFolder, "PT_CM.csv"))
PT_FGM <- read.csv(file.path(rawdataFolder, "PT_FGM.csv"))
CHLD_PVTY <- read.csv(file.path(rawdataFolder, "CHLD_PVTY.csv"))
SOC_PROTECTION <- read.csv(file.path(rawdataFolder, "SOC_PROTECTION.csv"))
WASH <- read.csv(file.path(rawdataFolder, "WASH.csv"))

# WB, UNICEF 1.1.1. DATA ----
# From Salmeron-Gomez Daylan, Solrun Engilbertsdottir, Jose Antonio Cuesta Leiva, David Newhouse, David Stewart, 
# ‘Global Trends in Child Monetary Poverty According to International Poverty Lines’, Policy Research Working Paper, WPS10525, The World Bank, July 2023.
# https://documents1.worldbank.org/curated/en/099835007242399476/pdf/IDU0965118d1098b8048870ac0e0cb5aeb049f98.pdf 
# Appendix Table 36: Number of children living in monetary poor households in 2022 (thousands) 
SI_POV_DAY1 <- read.csv(file.path(rawdataFolder,"SI_POV_DAY1.csv"))
SI_POV_DAY1 <- SI_POV_DAY1 |> mutate(indicator = "1.1.1",
                                     series2 = "SI_POV_DAY1",
                                     timePeriodStart = 2022,
                                     value = value2.15) |> 
  mutate(geoAreaCode = as.character(geoAreaCode)) |> 
  select(indicator, geoAreaCode, series2, timePeriodStart, value)

# MENARO DW ----
PV_CHLD_DPRV_REG_MOD <- read.csv(file.path(rawdataFolder, "PV_CHLD_DPRV_REG_MOD.csv")) |>
  filter(RESIDENCE.Residence == "_T: Total", WEALTH_QUINTILE.Wealth.Quintile == "_T: Total") |> 
  mutate(indicator = "1.2.2",
         series2 = "PV_CHLD_DPRV_REG_MOD",
         timePeriodStart = TIME_PERIOD.Time.period,
         value = OBS_VALUE.Observation.value,
         iso3=str_sub(REF_AREA.Geographic.area, 1,3)) |>
  left_join(MENARO_metadata, by="iso3") |> 
  mutate(geoAreaCode = as.character(LocID)) |> 
  select(indicator, geoAreaCode, series2, timePeriodStart, value)

# MERGING ALL TABLES ----
cri_db_world <- bind_rows(sdgdb_world,
                          SI_POV_DAY1,
                          PV_CHLD_DPRV_REG_MOD)

# EXPORT TABLE FOR ANALYSIS ----
#save table for quicker analysis later
save(cri_db_world, file = file.path(repoFolder, "output/cri_db_world.Rdata"))
load(file = file.path(repoFolder, "output/cri_db_world.Rdata"))


