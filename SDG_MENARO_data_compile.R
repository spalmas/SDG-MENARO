# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: SDG data compile from the different data sources
# Author: Sebastian Palmas

# PROFILE ----
source("SDG_MENARO_profile.R")

# FUNCTIONS  ----
source("helpers/descriptive_df.R")

# SOURCE FILES ----
load("source_data/SDGGD.Rdata")
load("source_data/DW.Rdata")

## WB, UNICEF 1.1.1. data ----
# From Salmeron-Gomez Daylan, Solrun Engilbertsdottir, Jose Antonio Cuesta Leiva, David Newhouse, David Stewart, 
# ‘Global Trends in Child Monetary Poverty According to International Poverty Lines’, Policy Research Working Paper, WPS10525, The World Bank, July 2023.
# https://documents1.worldbank.org/curated/en/099835007242399476/pdf/IDU0965118d1098b8048870ac0e0cb5aeb049f98.pdf 
# Appendix Table 36: Number of children living in monetary poor households in 2022 (thousands) 
SI_POV_DAY1 <- read.csv(file.path(rawdataFolder,"SI_POV_DAY1.csv")) |>
  mutate(indicator = "1.1.1",
         MENARO.indicator.code = "SI_POV_DAY1",
         time.period = 2022,
         obs.value = value2.15) |> 
  filter(iso3 %in% MENARO_metadata$iso3) |> 
  mutate(geoAreaCode = as.character(geoAreaCode)) |> 
  select(MENARO.indicator.code, iso3, time.period, obs.value)

## MENARO DW ----
PV_CHLD_DPRV_REG_MOD <- read.csv(file.path(rawdataFolder, "PV_CHLD_DPRV_REG_MOD.csv")) |>
  filter(RESIDENCE.Residence == "_T: Total", WEALTH_QUINTILE.Wealth.Quintile == "_T: Total") |> 
  mutate(MENARO.indicator.code = "PV_CHLD_DPRV_REG_MOD",
         time.period = TIME_PERIOD.Time.period,
         obs.value = OBS_VALUE.Observation.value,
         iso3=str_sub(REF_AREA.Geographic.area, 1,3)) |>
  left_join(MENARO_metadata, by="iso3") |> 
  filter(iso3 %in% MENARO_metadata$iso3) |> 
  select(MENARO.indicator.code, iso3 , time.period, obs.value)

# DESCRIPTIVE STATISTICS ----
describe_indicator(SI_POV_DAY1)
describe_indicator(PV_CHLD_DPRV_REG_MOD)
SDGGD_descriptive <- describe_indicator(SDGGD)
DW_descriptive <- describe_indicator(DW)


# MERGING ALL TABLES ----
#Combining the selected dataset for each indicator into one table ready for analysis
indicator_data_MENARO <- bind_rows(SDGGD |> filter(MENARO.indicator.code %in% c("SE_DEV_ONTRK", "SE_PRE_PARTN", "SE_TOT_CPLR_LS",
                                                                            "SE_TOT_CPLR_PR", "SE_TOT_CPLR_US", 
                                                                            "SE_TOT_PRFL_2", "SE_TOT_PRFL_3", "SE_TOT_PRFL_4",
                                                                            "SE_TOT_PRFL_5", "SE_TOT_PRFL_6", "SG_LGL_GENEQEMP",
                                                                            "SH_ACS_DTP3", "SH_ACS_MCV2", "SH_ACS_UNHC",
                                                                            "SH_DYN_MORT", "SH_DYN_NMRT", "SH_FPL_INFMRH",
                                                                            "SH_H2O_SAFE", "SH_SAN_DEFECT", "SH_SAN_HNDWSH",
                                                                            "SH_SAN_SAFE", "SH_STA_FGMS", "SH_STA_MORT",
                                                                            "SH_STA_STNT", "SH_STA_WAST", "SI_COV_CHLD",
                                                                            "SI_POV_NAHC", "SL_TLF_CHLDEC", "SN_STA_OVWGT", 
                                                                            "SP_ACS_BSRVH2O", "SP_ACS_BSRVSAN", "SP_DYN_ADKL",
                                                                            "SP_DYN_MRBF18", "VC_VAW_MARR", "VC_VAW_PHYPYV",
                                                                            "VC_VAW_SXVLN_F", "VC_VAW_SXVLN_M")),
                               DW |> filter(MENARO.indicator.code %in% c("ED_ROFST_L1",
                                                                         "ED_ROFST_L2", 
                                                                         "ED_ROFST_L3", 
                                                                         "SG_REG_BRTH", 
                                                                         "SH_HIV_INCD_15_19", 
                                                                         "SH_HIV_INCD_U15", 
                                                                         "SH_STA_BRTC")),
                               SI_POV_DAY1,
                               PV_CHLD_DPRV_REG_MOD) |> 
  select(MENARO.indicator.code, iso3, time.period, obs.value, unit.measure, data.source, obs.footnote,
         source.link, series.footnote, database.source)

# EXPORT TABLE FOR ANALYSIS ----
save(indicator_data_MENARO, file = 'output/indicator_data_MENARO.Rdata')
