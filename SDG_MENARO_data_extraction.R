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
  projectFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/MENARO SDG/output")) #Output files
  repoFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "code/SDG-MENARO")) #repository files
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/MENARO SDG/data/"))  #raw data folder
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))
stopifnot(dir.exists(rawdataFolder))

# PACKAGES  ----
# library(dplyr) # mutate select left_join group_by filter ungroup if_else lag case_when n summarise first sym rename # |> mutate select left_join group_by filter ungroup if_else lag case_when n summarise first sym rename
library(httr) # GET status_code content
library(jsonlite) # fromJSON
library(openxlsx) # read.xlsx write.xlsx createWorkbook addWorksheet writeData saveWorkbook insertImage
library(plyr) # mutate summarise rename rbind.fill
# library(purrr) # map_dbl
# library(tidyr) # unnest


# FUNCTIONS  ----
source("helpers/api_to_json.R")
source("helpers/SDG_series_getp.R")
source("helpers/SDGdata.R")
source("helpers/calculate_percentile.R")

# VARIABLES ----
## SDG indicator codes ----
#metadata of SDG child indicators. Includes the code in the SDG Global Database
CR_SDG_indicators <- read.csv("data/child_related_SDG_indicators.csv")

## MENARO countries ----
MENARO_metadata <- read.csv(file.path(repoFolder,"data/MENARO_metadata.csv")) 

# DOWNLOADING AND CLEANING THE TABLE ----
## Download all child-related SDG for all countries ----

worlddb <- SDGdata(child_indicators$SDG_SERIES_DESCR[c(2)])

## Cleaning the table ----

# Check for NA values in the esadb object
if (any(is.na(worlddb))) {
  # Replace NA values with a suitable placeholder
  worlddb[is.na(worlddb)] <- "NA"
}

unlist_columns <- function(df) {
  for (col_name in names(df)) {
    if (is.list(df[[col_name]])) {
      df[[col_name]] <- unlist(df[[col_name]])
    }
  }
  return(df)
}

worlddb<-unlist_columns(worlddb)

backup_worlddb<-worlddb

write.xlsx(worlddb, paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\4. SDG\\2. SDG R API\\ESAR_progress\\data\\SDG Data all countries CRSDG - ",Sys.Date(),".xlsx", sep=""))
write.csv(worlddb, paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\4. SDG\\2. SDG R API\\ESAR_progress\\data\\esar_sdg",".csv", sep=""), row.names = FALSE)



worlddb$numeric_value<-as.numeric(worlddb$value)

worlddb |> mutate(esa= ifelse(geoAreaName %in% c("Angola"
                                                  ,"Botswana"
                                                  ,"Burundi"
                                                  ,"Comoros"
                                                  ,"Eritrea"
                                                  ,"Eswatini"
                                                  ,"Ethiopia"
                                                  ,"Kenya"
                                                  ,"Lesotho"
                                                  ,"Madagascar"
                                                  ,"Malawi"
                                                  ,"Mozambique"
                                                  ,"Namibia"
                                                  ,"Rwanda"
                                                  ,"Somalia"
                                                  ,"South Africa"
                                                  ,"South Sudan"
                                                  ,"United Republic of Tanzania"
                                                  ,"Uganda"
                                                  ,"Zambia"
                                                  ,"Zimbabwe"),1,0)) -> worlddb

###################### calling regions

# 
# Define the API URL to fetch country information, including region and income classification
url <- "https://api.worldbank.org/v2/country?format=json&per_page=500"

# Make the GET request
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content
  json_data <- content(response, as = "text")
  data <- fromJSON(json_data)
  
  # The second element of the data contains the country list
  country_data <- data[[2]]
  
  # Convert the country data into a dataframe for easier viewing
  country_df <- as.data.frame(country_data)
  
  world_income <-unnest(country_df, c(region, adminregion, incomeLevel, lendingType), names_sep = "_")# Display the relevant columns: id (ISO), name, region, and incomeLevel
  
  world_income <- world_income |> select(id,incomeLevel_value,longitude,latitude)
  
  # Print the resulting country information
  print(world_income)
} else {
  print(paste("API request failed with status code:", status_code(response)))
}

# write.csv(world_income, "data/wi.csv")

wpp<- read.csv("data/wpp.csv")
wpp<- wpp |> select(Location, SDMX_Code,LocTypeName,ISO3_Code,SDGRegName,SubRegName,GeoRegName)
wpp <- wpp |> mutate(SDMX_Code = as.character(SDMX_Code))
#  merge region type
worlddb<- worlddb |> 
  left_join(wpp, by=c("geoAreaCode"="SDMX_Code")) |>
  left_join(world_income, by=c("ISO3_Code"="id")) |> 
  left_join(crsdg_sp_map, by=c("series"= "series")) |> select(-valueType, -time_detail,-geoInfoUrl,-timeCoverage,-upperBound,-lowerBound,-basePeriod, -source,-attributes.Units, -`dimensions.Reporting Type`,-`attributes.Observation Status`)


pops<- read.csv("data/pop.csv")
pops_merg <- pops |> select(-iso3,-location)
pops_merg$locationId <- as.character(pops_merg$locationId)
worlddb |> left_join(pops_merg, by=c("geoAreaCode"="locationId")) -> worlddb

# m49<- read.csv("data/m49.csv")
# # Convert 'code' in 'm49' to character
# m49 <- m49 |> mutate(code = as.character(code))
############################
# Filter out sex #
worlddb |> group_by(series) |>
  filter(
    (dimensions.Sex == "BOTHSEX") | (dimensions.Sex == "NA") |(is.na(dimensions.Sex)) | (dimensions.Sex == "") |
      (
        (series %in% c("SP_DYN_ADKL", "SH_STA_FGMS", "VC_VAW_SXVLN", "SP_DYN_MRBF15", 
                       "SP_DYN_MRBF18", "VC_VAW_MARR", "SH_STA_MORT", "SP_DYN_ADKL")) &
          (dimensions.Sex == "FEMALE")
      )
  ) |> ungroup()  |> 
  mutate(seriesDescription = ifelse(dimensions.Sex %in% c("NA", "", NA) | (is.na(dimensions.Sex)) , seriesDescription, paste(seriesDescription,"-", dimensions.Sex)))-> worlddb1
# filter for age

worlddb1 |>
  group_by(series) |>
  filter(
    (dimensions.Age == "NA") | (is.na(dimensions.Age)) | (length(unique(dimensions.Age)) == 1) |
      (series == "SH_STA_FGMS" & dimensions.Age == "15-49") |
      (series == "SD_MDP_MUHC" & dimensions.Age == "ALLAGE") |
      (series == "SG_REG_BRTH" & dimensions.Age == "<5Y") |
      (series == "SI_POV_DAY1" & dimensions.Age == "ALLAGE") |
      (series == "SL_TLF_CHLDEA" & dimensions.Age == "5-17") |
      (series == "VC_VAW_SXVLN" & dimensions.Age == "18-29") |
      (series == "SL_TLF_CHLDEC" & dimensions.Age == "5-17") |
      (series == "VC_VAW_PHYPYV" & dimensions.Age == "1-14") |
      (series == "SP_DYN_ADKL" & dimensions.Age == "15-19") |
      (series == "SP_DYN_ADKL" & dimensions.Age == "10-14") |
      (series == "SH_HIV_INCD" & dimensions.Age == "15-24") |
      (series == "SE_DEV_ONTRK" & dimensions.Age == "M36T59") |
      (series == "VC_VAW_MARR" & dimensions.Age == "15-49") 
  ) |> ungroup() |> 
  mutate(seriesDescription = ifelse(dimensions.Age %in% c("NA", "",NA), seriesDescription, paste(seriesDescription,"-", dimensions.Age)))->worlddb2


# filter for location
worlddb2 |>
  group_by(series) |>
  filter(
    (dimensions.Location == "NA") | (is.na(dimensions.Location)) | (dimensions.Location == "") | (dimensions.Location == "ALLAREA")
  )|> ungroup() ->worlddb3

worlddb3 |>
  group_by(series) |>
  filter(
    (dimensions.Quantile == "NA") | (is.na(dimensions.Quantile)) | (dimensions.Quantile == "") | (dimensions.Quantile == "_T")
  )|> ungroup() ->worlddb4


worlddb5<- worlddb4 |> mutate(seriesDescription = ifelse(`dimensions.Education level` %in% c("NA", "",NA), seriesDescription, paste(seriesDescription,"-", `dimensions.Education level`))) |>
  mutate(seriesDescription = ifelse(`dimensions.Type of skill` %in% c("NA", "",NA), seriesDescription, paste(seriesDescription,"-", `dimensions.Type of skill`)))|>
  mutate(seriesDescription = ifelse(dimensions.Location %in% c("NA", "",NA), seriesDescription, paste(seriesDescription,"-", dimensions.Location)))|>
  mutate(seriesDescription = ifelse(dimensions.Quantile %in% c("NA", "",NA), seriesDescription, paste(seriesDescription,"-", dimensions.Quantile))) |>
  select(-footnotes,- attributes.Nature,-dimensions.Age, -dimensions.Sex, -`dimensions.Education level`, -`dimensions.Type of skill`, -dimensions.Location, -dimensions.Quantile)
# worlddb<- unlist(worlddb)


# write.xlsx(worlddb5, paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\4. SDG\\2. SDG R API\\ESAR_progress\\data\\all crsdg - total - ",Sys.Date(),".xlsx", sep=""))
# write.csv(worlddb5, paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\4. SDG\\2. SDG R API\\ESAR_progress\\data\\crsdg_total",".csv", sep=""), row.names = FALSE)

# VERIFY the below and add flag for sense of indicator

series_targets <- read.csv("data/ind_targets.csv", header=TRUE)

#   data.frame(
#   series = c(
#     "SI_COV_SOCAST", "SP_DYN_ADKL", "SE_TOT_CPLR", "SH_STA_MORT", "SH_DYN_NMRT",
#     "SH_HIV_INCD", "VC_IHR_PSRC", "SE_PRE_PARTN", "SH_STA_BRTC", "VC_VAW_PHYPYV",
#     "SE_DEV_ONTRK", "SE_TOT_PRFL", "SL_TLF_CHLDEC", "SL_TLF_CHLDEA", "SD_MDP_CSMP",
#     "SN_STA_OVWGT", "SH_STA_STNT", "SH_STA_WAST", "SG_REG_BRTH", "VC_VAW_MARR",
#     "SH_STA_FGMS", "VC_VAW_SXVLN", "SI_POV_DAY1", "SI_POV_NAHC", "SD_MDP_MUHC",
#     "SH_SAN_DEFECT", "VC_VOV_SEXL", "SP_ACS_BSRVH2O", "SP_ACS_BSRVSAN", "SH_H2O_SAFE",
#     "SH_SAN_SAFE", "SH_SAN_HNDWSH", "SE_INF_DSBL", "SE_ACS_H2O", "SE_ACS_SANIT",
#     "SE_ACS_ELECT", "SE_ACC_HNDWSH", "SH_ACS_DTP3", "SH_ACS_MCV2", "SP_DYN_MRBF15",
#     "SP_DYN_MRBF18", "SH_DYN_MORT", "SH_ACS_UNHC", "SI_COV_CHLD", "SD_XPD_MNPO",
#     'SG_XPD_HLTH','SG_XPD_PROT'
#   ),
#   lower_better = c(
#     0, 1, 0, 1, 1,
#     1, 1, 0, 0, 1,
#     0, 0, 1, 1, 1,
#     1, 1, 1, 0, 1,
#     1, 1, 1, 1, 1,
#     1, 1, 0, 0, 0,
#     0, 0, 0, 0, 0,
#     0, 0, 0, 0, 1,
#     1, 1, 0, 0, 0,
#     0,0
#   ),
#   target2030 = c(
#     100, 0, 
#     
#   )
# )


# Join the direction data with the world database
worlddb5 <- worlddb5 |>
  left_join(series_targets[-1], by = c("seriesDescription" = "seriesDescription"), keep= FALSE)

#### remove small island and western sahara from Africa classification
worlddb5 |> mutate(UN_Afr_reg= ifelse(GeoRegName=="Africa" & !(geoAreaCode %in% c("175","638","654","732")),1,0)) ->worlddb6
### remove holy see from the list
worlddb6 |> filter(geoAreaCode!="336")-> worlddb7

stn_targets_worlddb<- worlddb7 |> filter(series=="SH_STA_STNTN", timePeriodStart==2012) |> mutate(stntN_2030target = numeric_value / 2,stntP_2030target = ifelse( !is.na(u5pop2030),(stntN_2030target / u5pop2030) * 100000, NA_real_)) |>
  select(geoAreaCode, stntN_2030target, stntP_2030target)

worlddb7<- worlddb7 |> left_join(stn_targets_worlddb, by= c("geoAreaCode"="geoAreaCode"))



worlddb7 |> dplyr::group_by(geoAreaName, seriesDescription) |> dplyr::mutate(latest_year=max(timePeriodStart, na.rm = TRUE),
                                                                               latest_value = numeric_value[which.max(timePeriodStart)],
                                                                               earliest_year = ifelse(series== "SH_STA_STNT" | series == "SN_STA_OVWGT", 2012, ifelse(any(timePeriodStart == 2015) & latest_year > 2015, 2015,
                                                                                                                                                                      ifelse(any(timePeriodStart < latest_year), max(timePeriodStart[timePeriodStart < latest_year], na.rm = TRUE),
                                                                                                                                                                             NA_real_))),
                                                                               earliest_value = numeric_value[timePeriodStart == earliest_year],
                                                                               n = latest_year - earliest_year,
                                                                               hist_exp_growth_rate = if_else(latest_year != earliest_year, log(latest_value / earliest_value) / n, NA_real_),
                                                                               hist_pwr_growth_rate = if_else(latest_year != earliest_year, exp(log(latest_value / earliest_value) / n) - 1, NA_real_),
                                                                               hist_prc_growth_rate = if_else(latest_year != earliest_year, (latest_value - earliest_value) / earliest_value / n, NA_real_),
                                                                               prev_numeric_value = lag(numeric_value),  # Get the previous value in the sequence
                                                                               prev_timePeriodStart = lag(timePeriodStart),
                                                                               point_change = ifelse(!is.na(numeric_value) & !is.na(prev_numeric_value), numeric_value - prev_numeric_value, NA_real_),  # Difference from previous to current
                                                                               year_diff = timePeriodStart - prev_timePeriodStart,  # Difference in years
                                                                               annual_point_change = ifelse(year_diff>0 & !is.na(point_change), point_change / year_diff, NA_real_),
                                                                               hist_pnt_growth_rate = if_else(any(year_diff>0) & any(!is.na(annual_point_change)), weighted.mean(annual_point_change, w = year_diff, na.rm = TRUE), NA_real_),
                                                                               # ind_target = last(ind_target),  # Assuming 'ind_target' is correctly aligned in your data frame
                                                                               years_to_2030 = 2030 - latest_year,
                                                                               ind_target = case_when(
                                                                                 series == "SH_STA_STNT" ~ stntP_2030target,
                                                                                 series == "SH_STA_STNTN" ~ stntN_2030target,
                                                                                 TRUE ~ ind_target  # Default case if none of the above conditions are met
                                                                               ),
                                                                               # Calculate growth rates needed to achieve the target by 2030
                                                                               fut_exp_growth_rate = if_else(years_to_2030 > 0, log(ind_target / latest_value) / years_to_2030, NA_real_),
                                                                               fut_pwr_growth_rate = if_else(years_to_2030 > 0, (ind_target / latest_value)^(1 / years_to_2030) - 1, NA_real_),
                                                                               fut_prc_growth_rate = if_else(years_to_2030 > 0, (ind_target - latest_value) / latest_value / years_to_2030, NA_real_),
                                                                               fut_pnt_growth_rate = if_else(years_to_2030 > 0, (ind_target - latest_value) / years_to_2030, NA_real_),
                                                                               fut_exp_growth_value = ifelse(series=="SH_STA_MORT" | series=="SH_STA_STNTN",if_else(years_to_2030 > 0, latest_value * exp(hist_exp_growth_rate * years_to_2030), NA_real_), pmax(0, pmin(99, if_else(years_to_2030 > 0, latest_value * exp(hist_exp_growth_rate * years_to_2030), NA_real_)))),
                                                                               fut_pwr_growth_value = ifelse(series=="SH_STA_MORT" | series=="SH_STA_STNTN",if_else(years_to_2030 > 0, latest_value * ((1 + hist_pwr_growth_rate) ^ years_to_2030), NA_real_),pmax(0, pmin(99, if_else(years_to_2030 > 0, latest_value * ((1 + hist_pwr_growth_rate) ^ years_to_2030), NA_real_)))),
                                                                               fut_prc_growth_value = ifelse(series=="SH_STA_MORT" | series=="SH_STA_STNTN",if_else(years_to_2030 > 0, latest_value * (1 + hist_prc_growth_rate * years_to_2030), NA_real_) ,pmax(0, pmin(99, if_else(years_to_2030 > 0, latest_value * (1 + hist_prc_growth_rate * years_to_2030), NA_real_)))),
                                                                               fut_pnt_growth_value = ifelse(series=="SH_STA_MORT" | series=="SH_STA_STNTN", if_else(years_to_2030 > 0, latest_value + hist_pnt_growth_rate * years_to_2030, NA_real_) , pmax(0, pmin(99, if_else(years_to_2030 > 0, latest_value + hist_pnt_growth_rate * years_to_2030, NA_real_))))
)|> dplyr::ungroup()-> worlddb8

# write.xlsx(worlddb8,"data/test8.xlsx")
worlddb8 ->worlddb9


####Global Country ranking

# # Filter for the latest values of each seriesDescription for all countries
# latest_data <- worlddb8 |> filter(LocTypeName=="Country/Area") |>
#   group_by(geoAreaName, seriesDescription) |>
#   filter(timePeriodStart == max(timePeriodStart)) |>
#   grouping()

library(dplyr) # |> mutate select left_join group_by filter ungroup if_else lag case_when n summarise first sym rename # |> mutate select left_join group_by filter ungroup if_else lag case_when n summarise first sym rename
library(purrr) # |> map_dbl # |> map_dbl




# Calculate the global rank of the latest value for each series and adjust for direction
worlddb10_glb <- worlddb9 |> filter(LocTypeName=="Country/Area") |>
  group_by(geoAreaName, seriesDescription) |>
  filter(timePeriodStart == max(timePeriodStart)) |>
  ungroup() |>
  group_by(seriesDescription) |> 
  dplyr::mutate(
    # Adjust the value based on the direction flag
    adjusted_value = ifelse(lower_better == 1, numeric_value, -numeric_value),
    # GlobalRank = rank(adjusted_value, na.last= "keep",ties.method = "max"),
    # Calculate rank percentile
    RankPercentile_LV_global = (rank(-adjusted_value,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_hist_exp_rate = ifelse(lower_better == 1, hist_exp_growth_rate, -hist_exp_growth_rate),
    adj_hist_pwr_rate = ifelse(lower_better == 1, hist_pwr_growth_rate, -hist_pwr_growth_rate),
    adj_hist_prc_rate = ifelse(lower_better == 1, hist_prc_growth_rate, -hist_prc_growth_rate),
    adj_hist_pnt_rate = ifelse(lower_better == 1, hist_pnt_growth_rate, -hist_pnt_growth_rate),
    RankPercentile_HRexp_global = (rank(-adj_hist_exp_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpwr_global = (rank(-adj_hist_pwr_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRprc_global = (rank(-adj_hist_prc_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpnt_global = (rank(-adj_hist_pnt_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_fut_exp_rate = ifelse(lower_better == 1, fut_exp_growth_rate, -fut_exp_growth_rate),
    adj_fut_pwr_rate = ifelse(lower_better == 1, fut_pwr_growth_rate, -fut_pwr_growth_rate),
    adj_fut_prc_rate = ifelse(lower_better == 1, fut_prc_growth_rate, -fut_prc_growth_rate),
    adj_fut_pnt_rate = ifelse(lower_better == 1, fut_pnt_growth_rate, -fut_pnt_growth_rate),
  ) |>
  mutate(
    RankPercentile_Fexp_global = map_dbl(-adj_fut_exp_rate, ~ calculate_percentile(.x, -adj_hist_exp_rate[!is.na(adj_hist_exp_rate)])),
    RankPercentile_Fpwr_global = map_dbl(-adj_fut_pwr_rate, ~ calculate_percentile(.x, -adj_hist_pwr_rate[!is.na(adj_hist_pwr_rate)])),
    RankPercentile_Fprc_global = map_dbl(-adj_fut_prc_rate, ~ calculate_percentile(.x, -adj_hist_prc_rate[!is.na(adj_hist_prc_rate)])),
    RankPercentile_Fpnt_global = map_dbl(-adj_fut_pnt_rate, ~ calculate_percentile(.x, -adj_hist_pnt_rate[!is.na(adj_hist_pnt_rate)]))
  ) |> ungroup() 

#  check flow directions
#  check flow directions
worlddb10_glb |> filter(series=="SH_DYN_MORT") |> plot_ly(x= ~RankPercentile_Fexp_global, y=~adj_fut_exp_rate, color = ~ as.character(SDGRegName), name = ~ geoAreaName, size = ~earliest_value)
# write.xlsx(worlddb10_glb,"data/test2.xlsx")
# worlddb10 |> filter(series=="SH_DYN_MORT") |> plot_ly(x= ~RankPercentile_Fexp_global, y=~adj_fut_exp_rate, color = ~ as.character(SDGRegName))
# #  check flow directions
# worlddb10 |> filter(series=="SH_DYN_NMRT") |> plot_ly(x= ~RankPercentile_HRexp_global, y=~hist_exp_growth_rate, color = ~ as.character(esa))
# 
# 
# worlddb10 |> filter(series=="SE_PRE_PARTN") |> mutate(plot_title= unique(seriesDescription)) |> plot_ly(x= ~RankPercentile_LV_global, y=~numeric_value, color = ~GeoRegName) |> layout(title= ~plot_title)
# 
# worlddb10 |> filter(series=="SE_PRE_PARTN") |> mutate(plot_title= unique(seriesDescription)) |> plot_ly(x= ~RankPercentile_HRpwr_global, y=~hist_pwr_growth_rate, color = ~GeoRegName) |> layout(title= ~plot_title)
# 
# 
# 
# worlddb10 |> filter(series=="SI_COV_CHLD") |> plot_ly(x= ~RankPercentile_LV_global, y=~numeric_value, color = ~GeoRegName)
# worlddb10 |> filter(series=="SI_COV_CHLD") |> plot_ly(x= ~RankPercentile_HRprc_global, y=~hist_perc_growth_rate, color = ~GeoRegName)
# # joing to the main db matching the indicator-country-year
# worlddb10 |> select(geoAreaCode,seriesDescription,timePeriodStart, RankPercentile_LV_global) |> right_join(worlddb9, by = c("geoAreaCode" = "geoAreaCode", "seriesDescription"="seriesDescription", "timePeriodStart"="timePeriodStart"),keep=FALSE) -> worlddb10


####Country ranking within AU_UN
worlddb10_AU<- worlddb9 |> filter(LocTypeName=="Country/Area", UN_Afr_reg==1) |>
  group_by(geoAreaName, seriesDescription) |>
  filter(timePeriodStart == max(timePeriodStart)) |>
  ungroup() |>
  group_by(seriesDescription) |>
  dplyr::mutate(
    # Adjust the value based on the direction flag
    adjusted_value = ifelse(lower_better == 1, numeric_value, -numeric_value),
    # GlobalRank = rank(adjusted_value, na.last= "keep",ties.method = "max"),
    # Calculate rank percentile
    RankPercentile_LV_AU = (rank(-adjusted_value,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_hist_exp_rate = ifelse(lower_better == 1, hist_exp_growth_rate, -hist_exp_growth_rate),
    adj_hist_pwr_rate = ifelse(lower_better == 1, hist_pwr_growth_rate, -hist_pwr_growth_rate),
    adj_hist_prc_rate = ifelse(lower_better == 1, hist_prc_growth_rate, -hist_prc_growth_rate),
    adj_hist_pnt_rate = ifelse(lower_better == 1, hist_pnt_growth_rate, -hist_pnt_growth_rate),
    RankPercentile_HRexp_AU = (rank(-adj_hist_exp_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpwr_AU = (rank(-adj_hist_pwr_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRprc_AU = (rank(-adj_hist_prc_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpnt_AU = (rank(-adj_hist_pnt_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_fut_exp_rate = ifelse(lower_better == 1, fut_exp_growth_rate, -fut_exp_growth_rate),
    adj_fut_pwr_rate = ifelse(lower_better == 1, fut_pwr_growth_rate, -fut_pwr_growth_rate),
    adj_fut_prc_rate = ifelse(lower_better == 1, fut_prc_growth_rate, -fut_prc_growth_rate),
    adj_fut_pnt_rate = ifelse(lower_better == 1, fut_pnt_growth_rate, -fut_pnt_growth_rate),
  ) |>
  mutate(
    RankPercentile_Fexp_AU = map_dbl(-adj_fut_exp_rate, ~ calculate_percentile(.x, -adj_hist_exp_rate[!is.na(adj_hist_exp_rate)])),
    RankPercentile_Fpwr_AU = map_dbl(-adj_fut_pwr_rate, ~ calculate_percentile(.x, -adj_hist_pwr_rate[!is.na(adj_hist_pwr_rate)])),
    RankPercentile_Fprc_AU = map_dbl(-adj_fut_prc_rate, ~ calculate_percentile(.x, -adj_hist_prc_rate[!is.na(adj_hist_prc_rate)])),
    RankPercentile_Fpnt_AU = map_dbl(-adj_fut_pnt_rate, ~ calculate_percentile(.x, -adj_hist_pnt_rate[!is.na(adj_hist_pnt_rate)]))
  ) |> ungroup() 


#  check flow directions
worlddb10_AU |> filter(series=="SH_DYN_MORT") |> plot_ly(x= ~RankPercentile_Fexp_AU, y=~adj_fut_exp_rate, color = ~ as.character(SubRegName), name = ~ geoAreaName, size = ~earliest_value)
#  check flow directions
temp_AU<- worlddb10_AU |> select(seriesDescription,geoAreaName,RankPercentile_LV_AU,RankPercentile_HRexp_AU,RankPercentile_HRpwr_AU,RankPercentile_HRprc_AU,RankPercentile_HRpnt_AU, RankPercentile_Fexp_AU,RankPercentile_Fpwr_AU,RankPercentile_Fprc_AU,RankPercentile_Fpnt_AU)

worlddb10_glb<-worlddb10_glb |> left_join(temp_AU, by=c("geoAreaName"="geoAreaName", "seriesDescription"="seriesDescription")) 

# #  check flow directions
# AU_worlddb10 |> filter(series=="SH_DYN_NMRT") |> plot_ly(x= ~RankPercentile_LV_AU, y=~adjusted_value, color= ~SubRegName)
# 
# AU_worlddb10 |> filter(series=="SE_PRE_PARTN") |> plot_ly(x= ~RankPercentile_LV_AU, y=~adjusted_value ,color= ~SubRegName)
# 
# 
# # joing to the main db matching the indicator-country-year
# AU_worlddb10 |> select(geoAreaCode,seriesDescription,timePeriodStart, RankPercentile_AU) |> right_join(worlddb, by = c("geoAreaCode" = "geoAreaCode", "seriesDescription"="seriesDescription", "timePeriodStart"="timePeriodStart")) -> worlddb
# 

####Country ranking by HIGH income

worlddb10_HI<- worlddb9 |> filter(LocTypeName=="Country/Area") |>
  group_by(geoAreaName, seriesDescription) |>
  filter(timePeriodStart == max(timePeriodStart)) |>
  ungroup() |>
  group_by(seriesDescription) |> filter(incomeLevel_value=="High income") |>
  dplyr::mutate(
    # Adjust the value based on the direction flag
    adjusted_value = ifelse(lower_better == 1, numeric_value, -numeric_value),
    # GlobalRank = rank(adjusted_value, na.last= "keep",ties.method = "max"),
    # Calculate rank percentile
    RankPercentile_LV_HI = (rank(-adjusted_value,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_hist_exp_rate = ifelse(lower_better == 1, hist_exp_growth_rate, -hist_exp_growth_rate),
    adj_hist_pwr_rate = ifelse(lower_better == 1, hist_pwr_growth_rate, -hist_pwr_growth_rate),
    adj_hist_prc_rate = ifelse(lower_better == 1, hist_prc_growth_rate, -hist_prc_growth_rate),
    adj_hist_pnt_rate = ifelse(lower_better == 1, hist_pnt_growth_rate, -hist_pnt_growth_rate),
    RankPercentile_HRexp_HI = (rank(-adj_hist_exp_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpwr_HI = (rank(-adj_hist_pwr_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRprc_HI = (rank(-adj_hist_prc_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpnt_HI = (rank(-adj_hist_pnt_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_fut_exp_rate = ifelse(lower_better == 1, fut_exp_growth_rate, -fut_exp_growth_rate),
    adj_fut_pwr_rate = ifelse(lower_better == 1, fut_pwr_growth_rate, -fut_pwr_growth_rate),
    adj_fut_prc_rate = ifelse(lower_better == 1, fut_prc_growth_rate, -fut_prc_growth_rate),
    adj_fut_pnt_rate = ifelse(lower_better == 1, fut_pnt_growth_rate, -fut_pnt_growth_rate),
  ) |>
  mutate(
    RankPercentile_Fexp_HI = map_dbl(-adj_fut_exp_rate, ~ calculate_percentile(.x, -adj_hist_exp_rate[!is.na(adj_hist_exp_rate)])),
    RankPercentile_Fpwr_HI = map_dbl(-adj_fut_pwr_rate, ~ calculate_percentile(.x, -adj_hist_pwr_rate[!is.na(adj_hist_pwr_rate)])),
    RankPercentile_Fprc_HI = map_dbl(-adj_fut_prc_rate, ~ calculate_percentile(.x, -adj_hist_prc_rate[!is.na(adj_hist_prc_rate)])),
    RankPercentile_Fpnt_HI = map_dbl(-adj_fut_pnt_rate, ~ calculate_percentile(.x, -adj_hist_pnt_rate[!is.na(adj_hist_pnt_rate)]))
  ) |> ungroup() 




#  check flow directions
worlddb10_HI |> filter(series=="SH_DYN_MORT") |> plot_ly(x= ~RankPercentile_HRexp_HI, y=~adj_hist_exp_rate, color = ~ as.character(SubRegName), name = ~ geoAreaName, size = ~earliest_value)
#  check flow directions
temp_HI<- worlddb10_HI |> select(seriesDescription,geoAreaName,RankPercentile_LV_HI,RankPercentile_HRexp_HI,RankPercentile_HRpwr_HI,RankPercentile_HRprc_HI,RankPercentile_HRpnt_HI, RankPercentile_Fexp_HI,RankPercentile_Fpwr_HI,RankPercentile_Fprc_HI,RankPercentile_Fpnt_HI)

worlddb10_glb<-worlddb10_glb |> left_join(temp_HI, by=c("geoAreaName"="geoAreaName", "seriesDescription"="seriesDescription")) 


####Country ranking by upper Middle income

worlddb10_UMC<- worlddb9 |> filter(LocTypeName=="Country/Area") |>
  group_by(geoAreaName, seriesDescription) |>
  filter(timePeriodStart == max(timePeriodStart)) |>
  ungroup() |>
  group_by(seriesDescription) |> filter(incomeLevel_value=="Upper middle income") |>
  dplyr::mutate(
    # Adjust the value based on the direction flag
    adjusted_value = ifelse(lower_better == 1, numeric_value, -numeric_value),
    # GlobalRank = rank(adjusted_value, na.last= "keep",ties.method = "max"),
    # Calculate rank percentile
    RankPercentile_LV_UMC = (rank(-adjusted_value,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_hist_exp_rate = ifelse(lower_better == 1, hist_exp_growth_rate, -hist_exp_growth_rate),
    adj_hist_pwr_rate = ifelse(lower_better == 1, hist_pwr_growth_rate, -hist_pwr_growth_rate),
    adj_hist_prc_rate = ifelse(lower_better == 1, hist_prc_growth_rate, -hist_prc_growth_rate),
    adj_hist_pnt_rate = ifelse(lower_better == 1, hist_pnt_growth_rate, -hist_pnt_growth_rate),
    RankPercentile_HRexp_UMC = (rank(-adj_hist_exp_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpwr_UMC = (rank(-adj_hist_pwr_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRprc_UMC = (rank(-adj_hist_prc_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpnt_UMC = (rank(-adj_hist_pnt_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_fut_exp_rate = ifelse(lower_better == 1, fut_exp_growth_rate, -fut_exp_growth_rate),
    adj_fut_pwr_rate = ifelse(lower_better == 1, fut_pwr_growth_rate, -fut_pwr_growth_rate),
    adj_fut_prc_rate = ifelse(lower_better == 1, fut_prc_growth_rate, -fut_prc_growth_rate),
    adj_fut_pnt_rate = ifelse(lower_better == 1, fut_pnt_growth_rate, -fut_pnt_growth_rate),
  ) |>
  mutate(
    RankPercentile_Fexp_UMC = map_dbl(-adj_fut_exp_rate, ~ calculate_percentile(.x, -adj_hist_exp_rate[!is.na(adj_hist_exp_rate)])),
    RankPercentile_Fpwr_UMC = map_dbl(-adj_fut_pwr_rate, ~ calculate_percentile(.x, -adj_hist_pwr_rate[!is.na(adj_hist_pwr_rate)])),
    RankPercentile_Fprc_UMC = map_dbl(-adj_fut_prc_rate, ~ calculate_percentile(.x, -adj_hist_prc_rate[!is.na(adj_hist_prc_rate)])),
    RankPercentile_Fpnt_UMC = map_dbl(-adj_fut_pnt_rate, ~ calculate_percentile(.x, -adj_hist_pnt_rate[!is.na(adj_hist_pnt_rate)]))
  ) |> ungroup() 



#  check flow directions
worlddb10_UMC |> filter(series=="SH_DYN_MORT") |> plot_ly(x= ~RankPercentile_HRexp_UMC, y=~adj_hist_exp_rate, color = ~ as.character(SubRegName), name = ~ geoAreaName, size = ~earliest_value)
#  check f
temp_UMC<- worlddb10_UMC |> select(seriesDescription,geoAreaName,RankPercentile_LV_UMC,RankPercentile_HRexp_UMC,RankPercentile_HRpwr_UMC,RankPercentile_HRprc_UMC,RankPercentile_HRpnt_UMC, RankPercentile_Fexp_UMC,RankPercentile_Fpwr_UMC,RankPercentile_Fprc_UMC,RankPercentile_Fpnt_UMC)

worlddb10_glb<-worlddb10_glb |> left_join(temp_UMC, by=c("geoAreaName"="geoAreaName", "seriesDescription"="seriesDescription")) 


####Country ranking by lower Middle income

worlddb10_LMC<- worlddb9 |> filter(LocTypeName=="Country/Area") |>
  group_by(geoAreaName, seriesDescription) |>
  filter(timePeriodStart == max(timePeriodStart)) |>
  ungroup() |>
  group_by(seriesDescription) |> filter(incomeLevel_value=="Lower middle income") |>
  dplyr::mutate(
    # Adjust the value based on the direction flag
    adjusted_value = ifelse(lower_better == 1, numeric_value, -numeric_value),
    # GlobalRank = rank(adjusted_value, na.last= "keep",ties.method = "max"),
    # Calculate rank percentile
    RankPercentile_LV_LMC = (rank(-adjusted_value,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_hist_exp_rate = ifelse(lower_better == 1, hist_exp_growth_rate, -hist_exp_growth_rate),
    adj_hist_pwr_rate = ifelse(lower_better == 1, hist_pwr_growth_rate, -hist_pwr_growth_rate),
    adj_hist_prc_rate = ifelse(lower_better == 1, hist_prc_growth_rate, -hist_prc_growth_rate),
    adj_hist_pnt_rate = ifelse(lower_better == 1, hist_pnt_growth_rate, -hist_pnt_growth_rate),
    RankPercentile_HRexp_LMC = (rank(-adj_hist_exp_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpwr_LMC = (rank(-adj_hist_pwr_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRprc_LMC = (rank(-adj_hist_prc_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpnt_LMC = (rank(-adj_hist_pnt_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_fut_exp_rate = ifelse(lower_better == 1, fut_exp_growth_rate, -fut_exp_growth_rate),
    adj_fut_pwr_rate = ifelse(lower_better == 1, fut_pwr_growth_rate, -fut_pwr_growth_rate),
    adj_fut_prc_rate = ifelse(lower_better == 1, fut_prc_growth_rate, -fut_prc_growth_rate),
    adj_fut_pnt_rate = ifelse(lower_better == 1, fut_pnt_growth_rate, -fut_pnt_growth_rate),
  ) |>
  mutate(
    RankPercentile_Fexp_LMC = map_dbl(-adj_fut_exp_rate, ~ calculate_percentile(.x, -adj_hist_exp_rate[!is.na(adj_hist_exp_rate)])),
    RankPercentile_Fpwr_LMC = map_dbl(-adj_fut_pwr_rate, ~ calculate_percentile(.x, -adj_hist_pwr_rate[!is.na(adj_hist_pwr_rate)])),
    RankPercentile_Fprc_LMC = map_dbl(-adj_fut_prc_rate, ~ calculate_percentile(.x, -adj_hist_prc_rate[!is.na(adj_hist_prc_rate)])),
    RankPercentile_Fpnt_LMC = map_dbl(-adj_fut_pnt_rate, ~ calculate_percentile(.x, -adj_hist_pnt_rate[!is.na(adj_hist_pnt_rate)]))
  ) |> ungroup() 



#  check flow directions
worlddb10_LMC |> filter(series=="SH_DYN_MORT") |> plot_ly(x= ~RankPercentile_HRexp_LMC, y=~adj_hist_exp_rate, color = ~ as.character(SubRegName), name = ~ geoAreaName, size = ~earliest_value)
#  check f
#  check f
temp_LMC<- worlddb10_LMC |> select(seriesDescription,geoAreaName,RankPercentile_LV_LMC,RankPercentile_HRexp_LMC,RankPercentile_HRpwr_LMC,RankPercentile_HRprc_LMC,RankPercentile_HRpnt_LMC, RankPercentile_Fexp_LMC,RankPercentile_Fpwr_LMC,RankPercentile_Fprc_LMC,RankPercentile_Fpnt_LMC)

worlddb10_glb<-worlddb10_glb |> left_join(temp_LMC, by=c("geoAreaName"="geoAreaName", "seriesDescription"="seriesDescription")) 



####Country ranking by low income

worlddb10_LI<- worlddb9 |> filter(LocTypeName=="Country/Area") |>
  group_by(geoAreaName, seriesDescription) |>
  filter(timePeriodStart == max(timePeriodStart)) |>
  ungroup() |>
  group_by(seriesDescription) |> filter(incomeLevel_value=="Low income") |>
  dplyr::mutate(
    # Adjust the value based on the direction flag
    adjusted_value = ifelse(lower_better == 1, numeric_value, -numeric_value),
    # GlobalRank = rank(adjusted_value, na.last= "keep",ties.method = "max"),
    # Calculate rank percentile
    RankPercentile_LV_LI = (rank(-adjusted_value,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_hist_exp_rate = ifelse(lower_better == 1, hist_exp_growth_rate, -hist_exp_growth_rate),
    adj_hist_pwr_rate = ifelse(lower_better == 1, hist_pwr_growth_rate, -hist_pwr_growth_rate),
    adj_hist_prc_rate = ifelse(lower_better == 1, hist_prc_growth_rate, -hist_prc_growth_rate),
    adj_hist_pnt_rate = ifelse(lower_better == 1, hist_pnt_growth_rate, -hist_pnt_growth_rate),
    RankPercentile_HRexp_LI = (rank(-adj_hist_exp_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpwr_LI = (rank(-adj_hist_pwr_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRprc_LI = (rank(-adj_hist_prc_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    RankPercentile_HRpnt_LI = (rank(-adj_hist_pnt_rate,na.last= "keep", ties.method = "max") / n() )* 100,
    adj_fut_exp_rate = ifelse(lower_better == 1, fut_exp_growth_rate, -fut_exp_growth_rate),
    adj_fut_pwr_rate = ifelse(lower_better == 1, fut_pwr_growth_rate, -fut_pwr_growth_rate),
    adj_fut_prc_rate = ifelse(lower_better == 1, fut_prc_growth_rate, -fut_prc_growth_rate),
    adj_fut_pnt_rate = ifelse(lower_better == 1, fut_pnt_growth_rate, -fut_pnt_growth_rate),
  ) |>
  mutate(
    RankPercentile_Fexp_LI = map_dbl(-adj_fut_exp_rate, ~ calculate_percentile(.x, -adj_hist_exp_rate[!is.na(adj_hist_exp_rate)])),
    RankPercentile_Fpwr_LI = map_dbl(-adj_fut_pwr_rate, ~ calculate_percentile(.x, -adj_hist_pwr_rate[!is.na(adj_hist_pwr_rate)])),
    RankPercentile_Fprc_LI = map_dbl(-adj_fut_prc_rate, ~ calculate_percentile(.x, -adj_hist_prc_rate[!is.na(adj_hist_prc_rate)])),
    RankPercentile_Fpnt_LI = map_dbl(-adj_fut_pnt_rate, ~ calculate_percentile(.x, -adj_hist_pnt_rate[!is.na(adj_hist_pnt_rate)]))
  ) |> ungroup() 



#  check flow directions
worlddb10_LI |> filter(series=="SH_DYN_MORT") |> plot_ly(x= ~RankPercentile_HRexp_LI, y=~adj_hist_exp_rate, color = ~ as.character(SubRegName), name = ~ geoAreaName, size = ~earliest_value)
#  check f
temp_LI<- worlddb10_LI |> select(seriesDescription,geoAreaName,RankPercentile_LV_LI,RankPercentile_HRexp_LI,RankPercentile_HRpwr_LI,RankPercentile_HRprc_LI,RankPercentile_HRpnt_LI, RankPercentile_Fexp_LI,RankPercentile_Fpwr_LI,RankPercentile_Fprc_LI,RankPercentile_Fpnt_LI)

worlddb10_glb<-worlddb10_glb |> left_join(temp_LI, by=c("geoAreaName"="geoAreaName", "seriesDescription"="seriesDescription")) 


# Define the indicators to exclude from country average rank calculations
exclude_indicators <- c(
  "Proportion of children living in child-specific multidimensional poverty (%) - BOTHSEX - <18Y - ALLAREA",
  "Proportion of government spending in health, direct social transfers and education which benefit the monetary poor (%)",
  "Children moderately or severely stunted (thousands) - <5Y",
  "Adolescent birth rate (per 1,000 women aged 15-19 and 10-14 years) - FEMALE - 10-14"
)

worlddb10_glb |> group_by(geoAreaName)|>
  mutate(
    cnt_avg_rank_glb_LV = mean(RankPercentile_LV_global[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_AU_LV  = mean(RankPercentile_LV_AU[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_glb_HRpnt = mean(RankPercentile_HRpnt_global[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_glb_Fpnt = mean(RankPercentile_Fpnt_global[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_AU_HRpnt = mean(RankPercentile_HRpnt_AU[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_AU_Fpnt = mean(RankPercentile_Fpnt_AU[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_UMC_HRpnt = mean(RankPercentile_HRpnt_UMC[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_UMC_Fpnt = mean(RankPercentile_Fpnt_UMC[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_LMC_HRpnt = mean(RankPercentile_HRpnt_LMC[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_LMC_Fpnt = mean(RankPercentile_Fpnt_LMC[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_LI_HRpnt = mean(RankPercentile_HRpnt_LI[!seriesDescription %in% exclude_indicators], na.rm = TRUE),
    cnt_avg_rank_LI_Fpnt = mean(RankPercentile_Fpnt_LI[!seriesDescription %in% exclude_indicators], na.rm = TRUE)
  ) |> 
  # cnt_avg_rank_glb_LV = mean(RankPercentile_LV_global, na.rm = TRUE),
  # cnt_avg_rank_AU_LV  = mean(RankPercentile_LV_AU, na.rm = TRUE),
  # cnt_avg_rank_glb_HRpnt= mean(RankPercentile_HRpnt_global, na.rm = TRUE),
  # cnt_avg_rank_glb_Fpnt= mean(RankPercentile_Fpnt_global, na.rm=TRUE),
  # cnt_avg_rank_AU_HRpnt= mean(RankPercentile_HRpnt_AU, na.rm = TRUE),
  # cnt_avg_rank_AU_Fpnt= mean(RankPercentile_Fpnt_AU, na.rm=TRUE),
  # cnt_avg_rank_UMC_HRpnt= mean(RankPercentile_HRpnt_UMC, na.rm = TRUE),
  # cnt_avg_rank_UMC_Fpnt= mean(RankPercentile_Fpnt_UMC, na.rm=TRUE),
  # cnt_avg_rank_LMC_HRpnt= mean(RankPercentile_HRpnt_LMC, na.rm = TRUE),
  # cnt_avg_rank_LMC_Fpnt= mean(RankPercentile_Fpnt_LMC, na.rm=TRUE),
  # cnt_avg_rank_LI_HRpnt= mean(RankPercentile_HRpnt_LI, na.rm = TRUE),
  # cnt_avg_rank_LI_Fpnt= mean(RankPercentile_Fpnt_LI, na.rm=TRUE)
  ungroup() |> group_by(esa,seriesDescription)|>
  mutate(
    ind_avg_rank_glb_LV= ifelse(any(!is.na(RankPercentile_LV_global)),mean(RankPercentile_LV_global, na.rm = TRUE),NA_real_),
    ind_avg_rank_AU_lv= ifelse(any(!is.na(RankPercentile_LV_AU)),mean(RankPercentile_LV_AU, na.rm = TRUE),NA_real_),
    ind_avg_rank_glb_HRpnt= ifelse(any(!is.na(RankPercentile_HRpnt_global)),mean(RankPercentile_HRpnt_global, na.rm = TRUE),NA_real_),
    ind_avg_rank_glb_Fpnt= ifelse(any(!is.na(RankPercentile_Fpnt_global)),mean(RankPercentile_Fpnt_global, na.rm = TRUE),NA_real_),
    ind_avg_rank_AU_HRpnt= ifelse(any(!is.na(RankPercentile_HRpnt_AU)),mean(RankPercentile_HRpnt_AU, na.rm = TRUE),NA_real_),
    ind_avg_rank_AU_Fpnt= ifelse(any(!is.na(RankPercentile_Fpnt_AU)),mean(RankPercentile_Fpnt_AU, na.rm = TRUE),NA_real_),
    ind_avg_rank_UMC_HRpnt= ifelse(any(!is.na(RankPercentile_HRpnt_UMC)),mean(RankPercentile_HRpnt_UMC, na.rm = TRUE),NA_real_),
    ind_avg_rank_UMC_Fpnt= ifelse(any(!is.na(RankPercentile_Fpnt_UMC)),mean(RankPercentile_Fpnt_UMC, na.rm = TRUE),NA_real_),
    ind_avg_rank_LMC_HRpnt= ifelse(any(!is.na(RankPercentile_HRpnt_LMC)),mean(RankPercentile_HRpnt_LMC, na.rm = TRUE),NA_real_),
    ind_avg_rank_LMC_Fpnt=  ifelse(any(!is.na(RankPercentile_Fpnt_LMC)),mean(RankPercentile_Fpnt_LMC, na.rm = TRUE),NA_real_),
    ind_avg_rank_LI_HRpnt= ifelse(any(!is.na(RankPercentile_HRpnt_LI)),mean(RankPercentile_HRpnt_LI, na.rm = TRUE),NA_real_),
    ind_avg_rank_LI_Fpnt= ifelse(any(!is.na(RankPercentile_Fpnt_LI)),mean(RankPercentile_Fpnt_LI, na.rm = TRUE),NA_real_)
  ) |> ungroup()-> worlddb10_glb_av




#  check flow directions
worlddb10_glb_av |> filter(SDGRegName=="Sub-Saharan Africa" )|>plot_ly(x= ~cnt_avg_rank_glb_Fpnt, y=~cnt_avg_rank_glb_HRpnt, color = ~ as.character(SDGRegName), name = ~ geoAreaName, size = ~earliest_value)


write.xlsx(worlddb10_glb_av, "data/esar_benchmarks.xlsx")


#Country effort tables
# Quintile labeling functions with NA handling
label_quintile <- function(value) {
  if (is.na(value)) {
    return("NA")
  } else if (value < 20) {
    return("Lowest quintile")
  } else if (value < 40) {
    return("2nd Lowest")
  } else if (value < 60) {
    return("medium quintile")
  } else if (value < 80) {
    return("Above average")
  } else if (value <= 100) {
    return("Top quintile")
  } else {
    return("Above historical efforts")
  }
}

label_effort <- function(value) {
  if (is.na(value)) {
    return("NA")
  } else if (value < 25) {
    return("Low effort")
  } else if (value < 75) {
    return("Medium effort")
  } else if (value <= 100) {
    return("High effort")
  } else {
    return("Above historical efforts")
  }
}

# Apply the quintile labels
worlddb10_glb_av <- worlddb10_glb_av |>
  mutate(
    `Overall SDG Performance` = mapply(label_quintile, cnt_avg_rank_glb_LV),
    `Historical Performance` = mapply(label_quintile, cnt_avg_rank_glb_HRpnt),
    `Needed Performance` = mapply(label_effort, cnt_avg_rank_glb_Fpnt)
  )

# Select the relevant columns and filter for unique geoAreaName
result_df <- worlddb10_glb_av |> 
  filter(esa == 1) |>
  group_by(geoAreaName) |>
  summarise(
    cnt_avg_rank_glb_LV = mean(cnt_avg_rank_glb_LV, na.rm = TRUE),
    cnt_avg_rank_glb_HRpnt = mean(cnt_avg_rank_glb_HRpnt, na.rm = TRUE),
    cnt_avg_rank_glb_Fpnt = mean(cnt_avg_rank_glb_Fpnt, na.rm = TRUE),
    `Overall SDG Performance` = first(`Overall SDG Performance` [!is.na(`Overall SDG Performance` )]),
    `Historical Performance` = first(`Historical Performance`[!is.na(`Historical Performance`)]),
    `Needed Performance` = first(`Needed Performance`[!is.na(`Needed Performance`)])
  ) |>
  ungroup()

# Create an Excel workbook
wb <- createWorkbook()

# Add a worksheet
addWorksheet(wb, "Country Quintiles")

# Write the data to the worksheet
writeData(wb, "Country Quintiles", result_df)

# Save the workbook
saveWorkbook(wb, file = "country_quintile_ranks.xlsx", overwrite = TRUE)




# write.xlsx(worlddb_, "data/esar_benchmarks_cnt_ind_Ranks.xlsx")

# For sym() and !! (bang-bang) to force evaluation

create_growth_report <- function(data, series_description, rate_type) {
  # Validate rate type input
  valid_rate_types <- c("exp", "pwr", "prc", "pnt")
  if (!rate_type %in% valid_rate_types) {
    stop("Invalid rate type. Choose from 'exp', 'pwr', 'prc', 'pnt'")
  }
  
  # Dynamically construct column names
  latest_value_col <- sym("latest_value")
  rank_prc_latest <- sym("RankPercentile_LV_global")
  hist_rate_col <- sym(paste("hist", rate_type, "growth_rate", sep = "_"))
  fut_rate_col <- sym(paste("fut", rate_type, "growth_rate", sep = "_"))
  fut_value_col <- sym(paste("fut", rate_type, "growth_value", sep = "_"))
  hist_rank_perc_glb <- sym(paste("RankPercentile_HR",rate_type,"_global", sep=""))
  fut_RankPercentile_global <- sym(paste("RankPercentile_F",rate_type,"_global", sep=""))
  target<- sym("ind_target")
  fut_RankPercentile_AU<- sym(paste("RankPercentile_F",rate_type,"_AU", sep=""))
  fut_RankPercentile_HI<- sym(paste("RankPercentile_F",rate_type,"_HI", sep=""))
  fut_RankPercentile_UMC<- sym(paste("RankPercentile_F",rate_type,"_UMC", sep=""))
  fut_RankPercentile_LMC<- sym(paste("RankPercentile_F",rate_type,"_LMC", sep=""))
  fut_RankPercentile_LI<- sym(paste("RankPercentile_F",rate_type,"_LI", sep=""))
  country_avg_LV_glb <- sym("cnt_avg_rank_glb_LV")
  country_avg_LV_AU <- sym("cnt_avg_rank_AU_LV")
  country_avg_HR_pnt_glb <- sym("cnt_avg_rank_glb_HRpnt")
  country_avg_HR_pnt_AU <- sym("cnt_avg_rank_AU_HRpnt")
  country_avg_Fpnt_glb <- sym("cnt_avg_rank_glb_Fpnt")
  country_avg_Fpnt_AU <- sym("cnt_avg_rank_AU_Fpnt")
  indicator_avg_in_esa_LV_glb<- sym("ind_avg_rank_glb_LV")
  indicator_avg_in_esa_LV_AU<- sym("ind_avg_rank_AU_lv")
  indicator_avg_in_esa_HRpnt_glb <- sym("ind_avg_rank_glb_HRpnt")
  indicator_avg_in_esa_HRpnt_AU <- sym("ind_avg_rank_AU_HRpnt")
  indicator_avg_in_esa_Fpnt_glb <- sym("ind_avg_rank_glb_Fpnt")
  indicator_avg_in_esa_Fpnt_AU <- sym("ind_avg_rank_AU_Fpnt")
  # # Print unique values for GeoRegName and seriesDescription for debugging
  # print(unique(data$GeoRegName))
  # print(unique(data$seriesDescription))
  
  # Filter and select relevant data
  report <- data |>
    filter(esa == 1, seriesDescription == series_description) |>
    select(
      Country = geoAreaName,
      `Latest Value (2022)` = !!latest_value_col,
      `Glb Rank Percentile of LV` = !!rank_prc_latest,
      AARC = !!hist_rate_col,
      `Hist. Rank Percentile glb` = !!hist_rank_perc_glb,
      `Target 2030` = !!target,
      `Value 2030` = !!fut_value_col,
      `Needed AARC` = !!fut_rate_col,
      `Rank Prc. of needed glb` = !!fut_RankPercentile_global,
      `Rank Prc. of needed AU` = !!fut_RankPercentile_AU,
      `Rank Prc. of needed HI` = !!fut_RankPercentile_HI,
      `Rank Prc. of needed UMC` = !!fut_RankPercentile_UMC,
      `Rank Prc. of needed LMC` = !!fut_RankPercentile_LMC,
      `Rank Prc. of needed LI` = !!fut_RankPercentile_LI,
      `Country avg. Rank LV - glb` = !!country_avg_LV_glb,
      `Country avg. Rank LV - AU` = !!country_avg_LV_AU,
      `Country avg. Rank Hist. - glb` = !!country_avg_HR_pnt_glb,
      `Country avg. Rank Hist. - AU` = !!country_avg_HR_pnt_AU,
      `Country avg. Rank F - glb` = !!country_avg_Fpnt_glb,
      `Country avg. Rank F - AU` = !!country_avg_Fpnt_AU,
      `ESA Indicator avg. Rank LV - glb` = !!indicator_avg_in_esa_LV_glb,
      `ESA Indicator avg. Rank LV - AU` = !!indicator_avg_in_esa_LV_AU,
      `ESA Indicator avg. Rank Hist. - glb` = !!indicator_avg_in_esa_HRpnt_glb,
      `ESA Indicator avg. Rank Hist. - AU` = !!indicator_avg_in_esa_HRpnt_AU,
      `ESA Indicator avg. Rank F - glb` = !!indicator_avg_in_esa_Fpnt_glb,
      `ESA Indicator avg. Rank F - AU` = !!indicator_avg_in_esa_Fpnt_AU,
    ) |>
    mutate(
      AARC = scales::percent(AARC, accuracy = 0.01),
      `Needed AARC` = scales::percent(`Needed AARC`, accuracy = 0.01)
    )
  
  # Debug output
  if (nrow(report) == 0) {
    cat("No data matched the filters.\n")
  }
  
  return(report)
}
# Example usage:
# Assuming 'worlddb10_glb' is your data frame loaded with all columns needed
result_table <- create_growth_report(worlddb10_glb_av, "Proportion of children moderately or severely stunted (%) - <5Y", "pwr")
print(result_table)


# Initialize an Excel workbook
wb <- createWorkbook()

# Iterate through each series
# Helper list to track sheet names and ensure uniqueness
sheet_names <- list()

# Iterate through each series
for (series in unique(worlddb10_glb_av$seriesDescription)) {
  for (rate_type in c("exp", "pwr", "prc", "pnt")) {
    result_table <- create_growth_report(worlddb10_glb_av, series, rate_type)
    
    # Create a short unique sheet name
    short_series_name <- substr(series, 1, 20)  # Reduce the length to 20 characters
    clean_series_name <- gsub("[^A-Za-z0-9]", "", short_series_name)  # Remove special characters
    sheet_name <- paste(clean_series_name, rate_type, sep = "_")
    
    # Ensure the sheet name is unique by appending a numeric suffix if needed
    counter <- 1
    original_sheet_name <- sheet_name
    while (sheet_name %in% sheet_names) {
      sheet_name <- paste(original_sheet_name, counter, sep = "_")
      counter <- counter + 1
    }
    sheet_names[[length(sheet_names) + 1]] <- sheet_name
    
    # Add each result to a new sheet in the workbook
    addWorksheet(wb, sheet_name)
    # Write the full series description at the top of the sheet
    writeData(wb, sheet_name, x = series, startCol = 1, startRow = 1, xy = c(1, 1))
    writeData(wb, sheet_name, result_table, startRow = 3)  # Offset the data to start after the description
  }
}
# Save the workbook
saveWorkbook(wb, paste("SDG_Proj_Benchmark - ", Sys.Date(),".xlsx"), overwrite = TRUE)

cat("Excel workbook created successfully with growth rates reports for each series and rate type.\n")



###################creating quandrand charts

library(ggrepel) # geom_text_repel
# Define the indicators and their short names
indicators <- data.frame(
  seriesDescription = c(
    "Neonatal mortality rate (deaths per 1,000 live births) - BOTHSEX - <1M",
    "Proportion of children moderately or severely stunted (%) - <5Y",
    "Proportion of the target population who received 3 doses of diphtheria-tetanus-pertussis (DTP3) vaccine (%)",
    "Under-five mortality rate, by sex (deaths per 1,000 live births) - BOTHSEX - <5Y",
    "Completion rate, by sex, location, wealth quintile and education level (%) - BOTHSEX - LOWSEC - ALLAREA - _T",
    "Completion rate, by sex, location, wealth quintile and education level (%) - BOTHSEX - PRIMAR - ALLAREA - _T",
    "Participation rate in organized learning (one year before the official primary entry age), by sex (%) - BOTHSEX",
    "Proportion of children and young people achieving a minimum proficiency level in reading and mathematics (%) - BOTHSEX - PRIMAR - SKILL_MATH",
    "Proportion of children and young people achieving a minimum proficiency level in reading and mathematics (%) - BOTHSEX - PRIMAR - SKILL_READ",
    "Proportion of population practicing open defecation, by urban/rural (%) - ALLAREA",
    "Proportion of population using basic drinking water services, by location (%) - ALLAREA",
    "Proportion of population using basic sanitation services, by location (%) - ALLAREA",
    "[ILO] Proportion of children/households receiving child/family cash benefit, by sex (%) - BOTHSEX",
    "Proportion of population below international poverty line (%) - BOTHSEX - ALLAGE - ALLAREA"
  ),
  ind = c(
    "Neonatal", "Stunted", "DTP3", "U5MR", "Completion_lowS",
    "Completion_Prim", "Pre_primary", "learning_math", "Learning_read",
    "Open_def", "Water", "Sanitation", "Cash", "Poverty"
  )
)



# Extract the relevant seriesDescription values
relevant_indicators <- indicators$seriesDescription


# Filter the worlddb10_glb dataframe for relevant indicators and non-NA values
filtered_df <- worlddb10_glb |>
  filter(seriesDescription %in% relevant_indicators,
         !is.na(RankPercentile_LV_global),
         !is.na(RankPercentile_Fpnt_global), esa==1)
# Create the complete table of indicators for reference
complete_table <- data.frame(seriesDescription = relevant_indicators) |>
  mutate(latest_value = NA, RankPercentile_LV_global = NA, RankPercentile_Fpnt_global = NA)

# Filter the worlddb10_glb dataframe for relevant indicators and non-NA values for the table data
table_data <- worlddb10_glb |> 
  filter(seriesDescription %in% relevant_indicators, esa == 1) |>
  select(geoAreaName, seriesDescription, latest_value, RankPercentile_LV_global, RankPercentile_Fpnt_global)


# Create a mapping between seriesDescription and ind
indicator_mapping <- setNames(indicators$ind, indicators$seriesDescription)

# Substitute seriesDescription names with ind names
filtered_df <- filtered_df |>
  mutate(indicator_name = indicator_mapping[seriesDescription])

# Create an Excel workbook
wb <- createWorkbook()

unique_countries <- unique(filtered_df$geoAreaName)

for (country in unique_countries) {
  country_data <- filtered_df |> filter(geoAreaName == country)
  avg_LV <- mean(country_data$RankPercentile_LV_global)
  avg_Fpnt <- mean(country_data$RankPercentile_Fpnt_global)
  
  country_data <- country_data |>
    mutate(color = ifelse(RankPercentile_LV_global < avg_LV & RankPercentile_Fpnt_global > avg_Fpnt, "red", "blue"))
  
  
  p <- ggplot(country_data, aes(x = RankPercentile_LV_global, y = RankPercentile_Fpnt_global)) +
    geom_point(aes(color = color), size = 3) +
    scale_color_identity() +
    labs(title = paste("Country:", country),
         x = "Latest Value - Rank percentile Global",
         y = " Needed effort -Rank Percentile Global") +
    geom_text_repel(aes(label = indicator_name),color = "grey50", max.overlaps = Inf)  +
    geom_vline(xintercept = avg_LV, linetype = "dashed", color = "red") +
    geom_hline(yintercept = avg_Fpnt, linetype = "dashed", color = "red") +
    theme_minimal() +
    theme(axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # Save the plot as an image file
  file_name <- paste0("plot_", gsub(" ", "_", country), ".png")
  ggsave(file_name, plot = p, width = 10, height = 6)
  
  # Add a sheet to the workbook
  addWorksheet(wb, country)
  
  # Insert the image into the Excel sheet
  insertImage(wb, country, file = file_name, width = 10, height = 6)
  
  # Create the table data by joining with the complete table to ensure all indicators are included
  table_data_export <- complete_table |>
    left_join(table_data |> filter(geoAreaName == country), by = "seriesDescription") |>
    select(seriesDescription, latest_value = latest_value.y, RankPercentile_LV_global = RankPercentile_LV_global.y, RankPercentile_Fpnt_global = RankPercentile_Fpnt_global.y) |>
    rename(`Indicator Name` = seriesDescription,
           `Latest Value` = latest_value,
           `Global Performance Score` = RankPercentile_LV_global,
           `Level Acceleration Needed` = RankPercentile_Fpnt_global)
  # Write the table data to the Excel sheet
  writeData(wb, sheet = country, x = table_data_export, startRow = 20)
}

# Save the workbook
saveWorkbook(wb, file = "country_plots.xlsx", overwrite = TRUE)

# Remove the image files
file.remove(list.files(pattern = "plot_.*\\.png"))
