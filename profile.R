# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: Profile, folders to use and packages
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
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "code/SDG-MENARO/00_source_data/"))  #raw data folder
  CR_SDG_indicators_path <- file.path("C:/Users/palma/OneDrive - UNICEF/MENARO SDG/child_related_SDG_indicators.xlsx") #master table with indicator codes. Local folder
} else if (USERNAME == "YOUR_USERNAME"){
  projectFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "YOUR/PATH")) #Output files
  repoFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "YOUR/PATH")) #repository files
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "YOUR/PATH"))  #raw data folder
  CR_SDG_indicators_path <- file.path(Sys.getenv("USERPROFILE"), "YOUR/PATH") #master table with indicator codes. Local folder
  
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))
stopifnot(dir.exists(rawdataFolder))

# PACKAGES  ----
library(countrycode) 
library(plyr) 
library(dplyr)
library(forcats)
library(ggplot2)
library(httr) 
library(jsonlite) 
library(openxlsx) 
library(stringr)

# SOURCE FILES ----
## MENARO countries ----
MENARO_metadata <- read.csv(file.path(rawdataFolder,"MENARO_metadata.csv")) 

## SDG indicator series codes ----
#metadata of SDG child indicators. Includes the code in the SDG Global Database
CR_SDG_indicators <- read.xlsx(CR_SDG_indicators_path,
                               sheet = "child_related_SDG_indicators")
