# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: Script to check if datasets (SDGDB and DW) can be merged
# Author: Sebastian Palmas

# PROFILE ----
source("profile.R")

# FUNCTIONS  ----
source("helpers/descriptive_df.R")

# SOURCE FILES ----
load("source_data/SDGGD.Rdata")
load("source_data/DW.Rdata")

# SI_POV_NAHC ----
#Yes, they can be merged
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SI_POV_NAHC")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SI_POV_NAHC")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)
SI_POV_NAHC <- bind_rows(SDGGD_MENARO,DW_MENARO)
save(SI_POV_NAHC, file = 'source_data/SI_POV_NAHC.Rdata')

# SI_COV_CHLD ----
#No, they can't be merged. DW is using the latest data point for 2024
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SI_COV_CHLD")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SI_COV_CHLD")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)

# SG_REG_BRTH ----
#No, need. All data from SDGDG and more is in DW
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SG_REG_BRTH")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SG_REG_BRTH")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)

# SH_HIV_INCD_U15 ----
# DJI data from DW has out of scale values. MAR is only in SDGGD (although with previous version: UNAIDS 2023)
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SH_HIV_INCD_U15")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SH_HIV_INCD_U15")
SH_HIV_INCD_U15 <- bind_rows(SDGGD_MENARO |> filter(iso3 == "MAR"),
                             DW_MENARO |> filter(!(iso3 == "DJI")))
g <- ggplot(SH_HIV_INCD_U15, aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)
save(SH_HIV_INCD_U15, file = 'source_data/SH_HIV_INCD_U15.Rdata')


# SE_TOT_PRFL_2 ----
#EGY and LBN only in DW
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SE_TOT_PRFL_2")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SE_TOT_PRFL_2")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)
SE_TOT_PRFL_2 <- bind_rows(SDGGD_MENARO,
                           DW_MENARO |> filter(iso3 %in% c("EGY", "LBN")))
save(SE_TOT_PRFL_2, file = 'source_data/SE_TOT_PRFL_2.Rdata')

# SE_TOT_PRFL_5 ----
# All data from DW is in SDGGD
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SE_TOT_PRFL_5")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SE_TOT_PRFL_5")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)

# SE_TOT_PRFL_6 ----
# All data from DW is in SDGGD
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SE_TOT_PRFL_6")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SE_TOT_PRFL_6")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)

# SE_TOT_CPLR_US ----
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SE_TOT_CPLR_US")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SE_TOT_CPLR_US")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)

# SE_PRE_PARTN ----
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SE_PRE_PARTN")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SE_PRE_PARTN")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)

# SP_DYN_MRBF18 ----
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SP_DYN_MRBF18")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SP_DYN_MRBF18")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)


# SH_FPL_INFMRH ----
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SH_FPL_INFMRH")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SH_FPL_INFMRH")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)


# SL_TLF_CHLDEC ----
SDGGD_MENARO <- SDGGD |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SL_TLF_CHLDEC")
DW_MENARO <- DW |> filter(iso3 %in% MENARO_metadata$iso3, MENARO.indicator.code == "SL_TLF_CHLDEC")
g <- ggplot(bind_rows(SDGGD_MENARO,DW_MENARO), aes(x=time.period, y=obs.value, color = iso3))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = database.source))
print(g)


