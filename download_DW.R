# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: Download UNICEF DW data for selected indicators and producing one DW file to use in data_extraction script
# Author: Sebastian Palmas

# Last run: 2025-02-08


# PROFILE ----
source("SDG-MENARO_profile.R")

# SOURCE FILES ----
## MENARO countries ----
MENARO_metadata <- read.csv(file.path(rawdataFolder,"MENARO_metadata.csv")) 

## SDG indicator series codes ----
#metadata of SDG child indicators. Includes the code in the SDG Global Database
CR_SDG_indicators <- read.xlsx(file.path("C:/Users/palma/OneDrive - UNICEF/MENARO SDG/child_related_SDG_indicators.xlsx"),
                               sheet = "child_related_SDG_indicators")

#DOWNLOAD AND FILTER ----

## Neonatal and U5 mortality rate ----
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,CME,1.0/.CME_MRM0+CME_MRY0T4..?format=sdmx-csv&labels=both")
CME <- temp_file |>
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
         DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |> 
  filter(SEX.Sex == "_T: Total",
         WEALTH_QUINTILE.Wealth.Quintile == "_T: Total",
         iso3 %in% MENARO_metadata$iso3) |> 
  left_join(CR_SDG_indicators |> select(DW.indicator.code, MENARO.indicator.code), by="DW.indicator.code")
write.csv(x = CME, file = file.path(rawdataFolder, "CME.csv"))


## Early childhood development ----
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,ECD,1.0/.ECD_CHLD_LMPSL..........?format=sdmx-csv&labels=both")
ECD <- temp_file |>
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
                       DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |> 
  filter(iso3 %in% MENARO_metadata$iso3,
         SEX.Sex == "_T: Total",
         AGE.Current.age == "M36T59: 36 to 59 months old",
         WEALTH_QUINTILE.Wealth.Quintile == "_T: Total",
         MOTHER_EDUCATION.Mother.s.Education.Level == "_T: Total",
         EC_EDUC_ATTND.Early.Childhood.Education.Attendance == "_T: Total",
         RESIDENCE.Residence == "_T: Total",
         ECD_DOMAIN.ECD.Domain == "_T: Total")  #CONFIRM THAT THIS IS THE ONE WE NEED
write.csv(x = ECD, file = file.path(rawdataFolder, "ECD.csv"))

## Education ----
# 4.1.1 and 4.1.2, 4.2.2, 4.1.4
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,EDUCATION,1.0/.ED_ANAR_L02+ED_CR_L1+ED_CR_L2+ED_CR_L3+ED_MAT_G23+ED_MAT_L1+ED_MAT_L2+ED_READ_G23+ED_READ_L1+ED_READ_L2+ED_ROFST_L1+ED_ROFST_L2+ED_ROFST_L3.....?format=sdmx-csv&labels=both")
EDU <- temp_file |>
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
         DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |> 
  filter(SEX.Sex == "_T: Total",
         WEALTH_QUINTILE.Wealth.Quintile == "_T: Total",
         RESIDENCE.Residence == "_T: Total",
         iso3 %in% MENARO_metadata$iso3) |> 
  left_join(CR_SDG_indicators |> select(DW.indicator.code, MENARO.indicator.code), by="DW.indicator.code")
write.csv(x = EDU, file = file.path(rawdataFolder, "EDU.csv"))


## Gender ----
# (5.1.1)
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,GENDER,1.0/.GN_SG_LGL_GENEQEMP....?format=sdmx-csv&labels=both")
GENDER <- temp_file |> 
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
         DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |> 
  filter(iso3 %in% MENARO_metadata$iso3) |> 
  left_join(CR_SDG_indicators |> select(DW.indicator.code, MENARO.indicator.code), by="DW.indicator.code")
write.csv(x = GENDER, file = file.path(rawdataFolder, "GENDER.csv"))

## HIV  ----
# 3.3.1
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,HIV_AIDS,1.0/.HVA_EPI_INF_RT.....?format=sdmx-csv&labels=both")
HIV <- temp_file |>
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
         MENARO.indicator.code = ifelse(`AGE.Current.age` == "Y0T14: Under 15 years old", "SH_HIV_INCD_U15", "SH_HIV_INCD_15_19")) |> 
  filter(iso3 %in% MENARO_metadata$iso3,
         SEX.Sex == "_T: Total")
HIV$OBS_VALUE.Observation.Value[HIV$OBS_VALUE.Observation.Value == "<0.01"] <- 0 #CONFIRM WHAT TO DO WITH THESE VALUES
HIV$OBS_VALUE.Observation.Value <- as.numeric(HIV$OBS_VALUE.Observation.Value)
HIV$LOWER_BOUND.Lower.Bound[HIV$LOWER_BOUND.Lower.Bound == "<0.01"] <- 0 #CONFIRM WHAT TO DO WITH THESE VALUES
HIV$LOWER_BOUND.Lower.Bound <- as.numeric(HIV$LOWER_BOUND.Lower.Bound)
HIV$UPPER_BOUND.Upper.Bound[HIV$UPPER_BOUND.Upper.Bound == "<0.01"] <- 0 #CONFIRM WHAT TO DO WITH THESE VALUES
HIV$UPPER_BOUND.Upper.Bound <- as.numeric(HIV$UPPER_BOUND.Upper.Bound)
write.csv(x = HIV, file = file.path(rawdataFolder, "HIV.csv"))

## Immunization ----
# (3.b.1 - DTP3 and MCV2)
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,IMMUNISATION,1.0/.IM_DTP3+IM_MCV2..?format=sdmx-csv&labels=both")
IMMU <- temp_file |> 
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+")) |> 
  filter(iso3 %in% MENARO_metadata$iso3)
write.csv(x = IMMU, file = file.path(rawdataFolder, "IMMU.csv"))

## Maternal, newborn and child survival  ----
# 3.1.1, 3.1.2, 3.7.2, 3.8.1, 5.6.1
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,MNCH,1.0/.MNCH_ABR+MNCH_INFDEC+MNCH_MMR+MNCH_SAB+MNCH_UHC.......?format=sdmx-csv&labels=both")
MNCH <- temp_file |> 
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
       DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |>
  filter(iso3 %in% MENARO_metadata$iso3,
         WEALTH_QUINTILE.Wealth.Quintile ==  "_T: Total",
         RESIDENCE.Residence == "_T: Total") |> 
  filter(!(DW.indicator.code == "MNCH_ABR" & AGE.Current.age == "Y10T14: 10 to 14 years old")) |> #ABR also includes 10-14
  left_join(CR_SDG_indicators |> select(DW.indicator.code, MENARO.indicator.code), by="DW.indicator.code") 
write.csv(x = MNCH, file = file.path(rawdataFolder, "MNCH.csv"))

##Nutrition  ----
# 2.2.1 - Stunting, 2.2.2 - Wasting and overweight
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,NUTRITION,1.0/.NT_ANT_HAZ_NE2_MOD+NT_ANT_WHZ_NE2+NT_ANT_WHZ_PO2_MOD......?format=sdmx-csv&labels=both")
NUTRITION <- temp_file |>
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
         DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |>
  filter(iso3 %in% MENARO_metadata$iso3,
         SEX.Sex == "_T: Total",
         AGE.Current.age == "Y0T4: Under 5 years old",
         WEALTH_QUINTILE.Wealth.Quintile ==  "_T: Total",
         RESIDENCE.Residence == "_T: Total",
         MATERNAL_EDU_LVL.Mother.s.Education.Level ==  "_T: Total") |> 
  left_join(CR_SDG_indicators |> select(DW.indicator.code, MENARO.indicator.code), by="DW.indicator.code") 
NUTRITION$TIME_PERIOD.Time.period <- as.numeric(NUTRITION$TIME_PERIOD.Time.period)
write.csv(x = NUTRITION, file = file.path(rawdataFolder, "NUTRITION.csv"))

## Protection ----
# 16.2.1 - physical punishment, 8.7.1 - child labour, 16.9.1 - birth registration, 16.2.3 sexual violence F and M
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,PT,1.0/.PT_CHLD_1-14_PS-PSY-V_CGVR+PT_CHLD_5-17_LBR_ECON-HC+PT_CHLD_Y0T4_REG+PT_F_18-29_SX-V_AGE-18+PT_F_GE15_PS-SX-EM_V_PTNR_12MNTH+PT_M_18-29_SX-V_AGE-18......?format=sdmx-csv&labels=both")
PT_CHLD <- temp_file |> mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
                           DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |>
  filter(iso3 %in% MENARO_metadata$iso3,
         WEALTH_QUINTILE.Wealth.Quintile ==  "_T: Total",
         RESIDENCE.Residence == "_T: Total",
         EDUCATION_LEVEL.Education.Level == "_T: Total") |> 
  filter(!(DW.indicator.code == "PT_CHLD_1-14_PS-PSY-V_CGVR" & SEX.Sex %in% c("F: Female", "M: Male"))) |> # PPmay include M and F, we don't need those
  filter(!(DW.indicator.code == "PT_CHLD_5-17_LBR_ECON-HC" & SEX.Sex %in% c("F: Female", "M: Male"))) |> # Child labour may include M and F, we don't need those
  filter(!(DW.indicator.code == "PT_CHLD_Y0T4_REG" & SEX.Sex %in% c("F: Female", "M: Male"))) |> # BR may include M and F, we don't need those
  filter(!(DW.indicator.code == "PT_CHLD_Y0T4_REG" & AGE.Current.age %in% c("M0T11: Under 12 months old",
                                                                            "M12T23: 12 to 23 months old",
                                                                            "M24T35: 24 to 35 months old",
                                                                            "M36T47: 36 to 47 months old",
                                                                            "M48T59: 48 to 59 months old"))) |> # BR may include other age groups, we don't need those
  left_join(CR_SDG_indicators |> select(DW.indicator.code, MENARO.indicator.code), by="DW.indicator.code") 
write.csv(x = PT_CHLD, file = file.path(rawdataFolder, "PT_CHLD.csv"))

## Child marriage ----
# 5.3.1
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,PT_CM,1.0/.PT_F_20-24_MRD_U18_TND..........?format=sdmx-csv&labels=both")
PT_CM <- temp_file |>
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
          DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |>
  filter(iso3 %in% MENARO_metadata$iso3)
write.csv(x = PT_CM, file = file.path(rawdataFolder, "PT_CM.csv"))

## Female genital mutilation ----
# 5.3.2
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,PT_FGM,1.0/.PT_F_15-49_FGM...........?format=sdmx-csv&labels=both")
PT_FGM <- temp_file |>
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
         DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |>
  filter(iso3 %in% MENARO_metadata$iso3,
         WEALTH_QUINTILE.Wealth.Quintile ==  "_T: Total",
         RESIDENCE.Residence == "_T: Total",
         EDUCATION_LEVEL.Education.Level == "_T: Total",
         AGE.Current.age == "Y15T49: 15 to 49 years old") #for some countries there are other age groups reported
write.csv(x = PT_FGM, file = file.path(rawdataFolder, "PT_FGM.csv"))

## Children living in poor households ----
# 1.2.1
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,CHLD_PVTY,1.0/.PV_CHLD_INCM-PL..?format=sdmx-csv&labels=both")
CHLD_PVTY <- temp_file |> 
  mutate(iso3 = str_extract(REF_AREA.Country, "[^:]+"),
                             DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |>
  filter(iso3 %in% MENARO_metadata$iso3)
write.csv(x = CHLD_PVTY, file = file.path(rawdataFolder, "CHLD_PVTY.csv"))

## Social protection ----
# 1.3.1
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,SOC_PROTECTION,1.0/.SPP_CHLD_SOC_PROT....?format=sdmx-csv&labels=both")
SOC_PROTECTION <- temp_file |> 
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
         DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |>
  filter(iso3 %in% MENARO_metadata$iso3)
write.csv(x = SOC_PROTECTION, file = file.path(rawdataFolder, "SOC_PROTECTION.csv"))

## WASH ----
# 1.4.1 - basic water, basic sanitation; 6.1.1 - safely water; 6.2.1 - open defecation, handwashing,safely sanitation
temp_file <- read.csv("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,WASH_HOUSEHOLDS,1.0/.WS_PPL_H-B+WS_PPL_S-ALB+WS_PPL_S-OD+WS_PPL_S-SM+WS_PPL_W-ALB+WS_PPL_W-SM...?format=sdmx-csv&labels=both")
WASH <- temp_file |> 
  mutate(iso3 = str_extract(REF_AREA.Geographic.area, "[^:]+"),
         DW.indicator.code = str_extract(INDICATOR.Indicator, "[^:]+")) |>
  filter(iso3 %in% MENARO_metadata$iso3,
         RESIDENCE.Residence == "_T: Total")
write.csv(x = WASH, file = file.path(rawdataFolder, "WASH.csv"))

# COMBINE FILE ----
# Combine and write tables into one DW Rdata file for use in SDG_MENARO_data_extraction.R 
#ECD is missing
DW <- bind_rows(CME, 
                ECD, 
                EDU,
                GENDER, 
                HIV |> select(-LOWER_BOUND.Lower.Bound, -UPPER_BOUND.Upper.Bound), 
                IMMU, 
                MNCH, 
                NUTRITION, 
                PT_CHLD, 
                PT_CM, 
                PT_FGM, 
                CHLD_PVTY, 
                SOC_PROTECTION, 
                WASH) |> 
  select(MENARO.indicator.code,  #Reducing number of columns for easier exploration
         indicator = INDICATOR.Indicator, 
         DW.indicator.code,
         iso3,
         sex = SEX.Sex,
         age = AGE.Current.age,
         time.period = TIME_PERIOD.Time.period,
         obs.value = OBS_VALUE.Observation.Value,
         data.source = DATA_SOURCE.Data.Source,
         unit.measure = UNIT_MEASURE.Unit.of.measure,
         ref.period = REF_PERIOD.Reference.Period,
         unit.multiplier = UNIT_MULTIPLIER.Unit.multiplier,
         source.link = SOURCE_LINK.Citation.of.or.link.to.the.data.source,
         series.footnote = SERIES_FOOTNOTE.Series.footnote,
         coverage.time = COVERAGE_TIME.The.period.of.time.for.which.data.are.provided,
         freq.coll = FREQ_COLL.Time.interval.at.which.the.source.data.are.collected,
         obs.footnote = OBS_FOOTNOTE.Observation.footnote) |> 
  mutate(database.source="DW")

# CHECK DUPLICATED VALUES ----
#There should be one value per each of the columns below
DW |> dplyr::group_by(iso3, indicator, sex, time.period, age) |> 
  dplyr::tally() |>
  dplyr::filter(n>1)

# SAVE FILE ----
save(DW, file="source_data/DW.Rdata")
