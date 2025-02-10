# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: Download SDG Global database data for selected indicators and producing one SDGGD DW file to use in data analysis script
# Author: Sebastian Palmas

# Last run: 2025-02-08


# PROFILE ----
source("SDG_MENARO_profile.R")

## Functions  ----
source("helpers/api_to_json.R")
source("helpers/calculate_percentile.R")
source("helpers/SDG_series_getp.R")
source("helpers/SDGdata.R")
source("helpers/unlist_columns.R")


#DOWNLOAD AND FILTER ----
## Creating list of series to download ----
CR_SDG_series <- CR_SDG_indicators$SDG_SERIES_DESCR |> unique()  #get unique values from table
CR_SDG_series <- CR_SDG_series[!is.na(CR_SDG_series)]  #remove NA values
CR_SDG_series <- CR_SDG_series[!(CR_SDG_series == "SI_POV_DAY1")]  #remove series that will be downloaded from other source

## Download ----
SDGGD <- SDGdata(CR_SDG_series)  #this some 15 minutes to run

## Cleaning table ----
# Check for NA values in the esadb object
if (any(is.na(SDGGD))) {
  # Replace NA values with a suitable placeholder
  SDGGD[is.na(SDGGD)] <- "NA"
}

#unlisting columns
SDGGD$footnotes <- NULL #This list is weird and has less values than rows
SDGGD <- unlist_columns(SDGGD)

#deleting rows that are not of use (disaggregations not included in our indicators)
SDGGD <- SDGGD |> filter(!(series == "SE_DEV_ONTRK" & dimensions.Sex %in% c("FEMALE", "MALE")))
SDGGD <- SDGGD |> filter(!(series == "SE_PRE_PARTN" & dimensions.Sex %in% c("FEMALE", "MALE")))
SDGGD <- SDGGD |> filter(!(series == "SE_TOT_PRFL" & dimensions.Sex %in% c("FEMALE", "MALE")))
SDGGD <- SDGGD |> filter(!(series == "SG_REG_BRTH" & dimensions.Age %in% c("M48T59", "M36T47", "M24T35", "M12T23", "<1Y", "<8Y")))
SDGGD <- SDGGD |> filter(!(series == "SH_DYN_MORT" & dimensions.Sex %in% c("FEMALE", "MALE")))
SDGGD <- SDGGD |> filter(!(series == "SH_H2O_SAFE" & dimensions.Location %in% c("RURAL", "URBAN")))
SDGGD <- SDGGD |> filter(!(series == "SH_HIV_INCD" & dimensions.Sex %in% c("FEMALE", "MALE")))
SDGGD <- SDGGD |> filter(!(series == "SH_HIV_INCD" & dimensions.Age %in% c("15-24", "15-49", "50+", "ALLAGE")))
SDGGD <- SDGGD |> filter(!(series == "SH_SAN_DEFECT" & dimensions.Location %in% c("RURAL", "URBAN")))
SDGGD <- SDGGD |> filter(!(series == "SH_SAN_HNDWSH" & dimensions.Location %in% c("RURAL", "URBAN")))
SDGGD <- SDGGD |> filter(!(series == "SH_SAN_SAFE" & dimensions.Location %in% c("RURAL", "URBAN")))
SDGGD <- SDGGD |> filter(!(series == "SI_POV_NAHC" & dimensions.Location %in% c("RURAL", "URBAN")))
SDGGD <- SDGGD |> filter(!(series == "SL_TLF_CHLDEC" & dimensions.Sex %in% c("FEMALE", "MALE")))
SDGGD <- SDGGD |> filter(!(series == "SL_TLF_CHLDEC" & dimensions.Age %in% c("5-14","10-17")))
SDGGD <- SDGGD |> filter(!(series == "SP_DYN_ADKL" & dimensions.Age %in% c("10-14")))
SDGGD <- SDGGD |> filter(!(series == "VC_VAW_MARR" & dimensions.Age %in% c("15-49")))
SDGGD <- SDGGD |> filter(!(series == "VC_VAW_PHYPYV" & dimensions.Age %in% c("1-4", "2-14", "5-12")))
SDGGD <- SDGGD |> filter(!(series == "VC_VAW_SXVLN" & dimensions.Age %in% c("18-74")))
SDGGD <- SDGGD |> filter(!(series == "SI_COV_CHLD" & dimensions.Sex %in% c("FEMALE", "MALE")))
SDGGD <- SDGGD |> filter(!(series == "SI_COV_CHLD" & dimensions.Age %in% c("<15Y")))
SDGGD <- SDGGD |> filter(!(series == "SP_ACS_BSRVH2O" & dimensions.Location %in% c("RURAL", "URBAN")))
SDGGD <- SDGGD |> filter(!(series == "SP_ACS_BSRVSAN" & dimensions.Location %in% c("RURAL", "URBAN")))
SDGGD <- SDGGD |> filter(!(series == "SE_TOT_CPLR" & dimensions.Sex %in% c("FEMALE", "MALE")))
SDGGD <- SDGGD |> filter(!(series == "SE_TOT_CPLR" & dimensions.Location %in% c("RURAL", "URBAN")))
SDGGD <- SDGGD |> filter(!(series == "SE_TOT_CPLR" & dimensions.Quantile %in% c("Q1", "Q2", "Q3", "Q4", "Q5")))

#Changing the codes for indicators that come from the same series
SDGGD$MENARO.indicator.code <- SDGGD$series

SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "SE_TOT_PRFL" &
                                    SDGGD$`dimensions.Education level` == "GRAD23" &
                                    SDGGD$`dimensions.Type of skill` == "SKILL_MATH"]  <- "SE_TOT_PRFL_1"
SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "SE_TOT_PRFL" &
                                    SDGGD$`dimensions.Education level` == "PRIMAR" &
                                    SDGGD$`dimensions.Type of skill` == "SKILL_MATH"]  <- "SE_TOT_PRFL_2"
SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "SE_TOT_PRFL" &
                                    SDGGD$`dimensions.Education level` == "LOWSEC" &
                                    SDGGD$`dimensions.Type of skill` == "SKILL_MATH"]  <- "SE_TOT_PRFL_3"
SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "SE_TOT_PRFL" &
                                    SDGGD$`dimensions.Education level` == "GRAD23" &
                                    SDGGD$`dimensions.Type of skill` == "SKILL_READ"]  <- "SE_TOT_PRFL_4"
SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "SE_TOT_PRFL" &
                                    SDGGD$`dimensions.Education level` == "PRIMAR" &
                                    SDGGD$`dimensions.Type of skill` == "SKILL_READ"]  <- "SE_TOT_PRFL_5"
SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "SE_TOT_PRFL" &
                                    SDGGD$`dimensions.Education level` == "LOWSEC" &
                                    SDGGD$`dimensions.Type of skill` == "SKILL_READ"]  <- "SE_TOT_PRFL_6"

SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "SH_HIV_INCD"]  <- "SH_HIV_INCD_U15"

SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "VC_VAW_SXVLN" & 
                                    SDGGD$dimensions.Sex == "FEMALE"]  <- "VC_VAW_SXVLN_F"
SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "VC_VAW_SXVLN" &
                                    SDGGD$dimensions.Sex == "MALE"]  <- "VC_VAW_SXVLN_M"

SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "SE_TOT_CPLR" & 
                                    SDGGD$`dimensions.Education level` == "PRIMAR"]  <- "SE_TOT_CPLR_PR"
SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "SE_TOT_CPLR" & 
                                    SDGGD$`dimensions.Education level` == "LOWSEC"]  <- "SE_TOT_CPLR_LS"
SDGGD$MENARO.indicator.code[SDGGD$MENARO.indicator.code == "SE_TOT_CPLR" & 
                                    SDGGD$`dimensions.Education level` == "UPPSEC"]  <- "SE_TOT_CPLR_US"

#Value from character to numeric
SDGGD$value <- as.numeric(SDGGD$value)
SDGGD$geoAreaCode <- as.numeric(SDGGD$geoAreaCode)

# FILTER AND ADD COLUMNS ----
SDGGD <- SDGGD |> 
  filter(geoAreaCode %in% MENARO_metadata$LocID) |> 
  left_join(MENARO_metadata |> select(LocID, iso3), by=c('geoAreaCode'="LocID")) |> 
  select(MENARO.indicator.code,
         iso3,
         SDGGD.indicator.code = series,
         sex = dimensions.Sex,
         age = dimensions.Age,
         time.period = timePeriodStart,
         obs.value = value,
         data.source = source,
         unit.measure = attributes.Units,
         education.level = `dimensions.Education level`,
         type.of.skill = `dimensions.Type of skill`) |> 
  mutate(database.source="SDGGD")
  
  
# CHECK DUPLICATED VALUES ----
#check if indicators only have one row per disaggregation (i.e. that we deleted all unused rows and correctly assigned MENARO.indicator.code)
SDGGD |> dplyr::group_by(MENARO.indicator.code, 
                               time.period, 
                               iso3,
                               age,
                               sex, 
                               education.level,
                               type.of.skill) |> dplyr::tally() |> dplyr::filter(n>1)

# SAVE FILE ----
save(SDGGD, file = "source_data/SDGGD.Rdata")
