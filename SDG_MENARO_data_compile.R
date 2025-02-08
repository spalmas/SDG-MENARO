# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: SDG data compile from the different data sources
# Author: Sebastian Palmas

# PROFILE ----
source("SDG-MENARO_profile.R")

# SOURCE FILES ----
load("source_data/sdgdb_world.Rdata")
load("source_data/DW.Rdata")


## WB, UNICEF 1.1.1. data ----
# From Salmeron-Gomez Daylan, Solrun Engilbertsdottir, Jose Antonio Cuesta Leiva, David Newhouse, David Stewart, 
# ‘Global Trends in Child Monetary Poverty According to International Poverty Lines’, Policy Research Working Paper, WPS10525, The World Bank, July 2023.
# https://documents1.worldbank.org/curated/en/099835007242399476/pdf/IDU0965118d1098b8048870ac0e0cb5aeb049f98.pdf 
# Appendix Table 36: Number of children living in monetary poor households in 2022 (thousands) 
SI_POV_DAY1 <- read.csv(file.path(rawdataFolder,"SI_POV_DAY1.csv"))
SI_POV_DAY1 <- SI_POV_DAY1 |> mutate(indicator = "1.1.1",
                                     MENARO_indicator_code = "SI_POV_DAY1",
                                     timePeriodStart = 2022,
                                     value = value2.15) |> 
  mutate(geoAreaCode = as.character(geoAreaCode)) |> 
  select(indicator, geoAreaCode, MENARO_indicator_code, timePeriodStart, value)

## MENARO DW ----
PV_CHLD_DPRV_REG_MOD <- read.csv(file.path(rawdataFolder, "PV_CHLD_DPRV_REG_MOD.csv")) |>
  filter(RESIDENCE.Residence == "_T: Total", WEALTH_QUINTILE.Wealth.Quintile == "_T: Total") |> 
  mutate(indicator = "1.2.2",
         MENARO_indicator_code = "PV_CHLD_DPRV_REG_MOD",
         timePeriodStart = TIME_PERIOD.Time.period,
         value = OBS_VALUE.Observation.value,
         iso3=str_sub(REF_AREA.Geographic.area, 1,3)) |>
  left_join(MENARO_metadata, by="iso3") |> 
  mutate(geoAreaCode = as.character(LocID)) |> 
  select(indicator, geoAreaCode, MENARO_indicator_code, timePeriodStart, value)

# COMPARE SDGGB and DW AND FILL EXCEL----


# BINDING SELECTED DATA ----
INDICATORS_MENARO <- NULL

# MERGING ALL TABLES ----
INDICATORS_MENARO <- bind_rows(INDICATORS_MENARO,
                               SI_POV_DAY1,
                               PV_CHLD_DPRV_REG_MOD)

# EXPORT TABLE FOR ANALYSIS ----
save(INDICATORS_MENARO, 'output/INDICATORS_MENARO.Rdata')
