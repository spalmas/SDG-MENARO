# Trend analysis for SDG child indicators in UNICEF MENARO

## Files in the repository

-   *download_DW.R*: To download and clean data from UNICEF Data Warehouse. Produces *source_data/DW.Rdata*

-   *download_SDGGD.R*: To download and clean data from SDG Global Database. Produces *source_data/SDGGD.Rdata*

-   *SDG_MENARO_check_data_merge.R*: To check if SDGGD and DW data can be merged and produce merged datasets for selected indicators. Produces *SE_TOT_PRFL_2.Rdata*, *SH_HIV_INCD_U15.Rdata* and *SI_POV_NAHC.Rdata*.

-   *SDG_MENARO_data_compile.R*: Code to extract the data from the SDG Global Database

-   *SDG_MENARO_data_description.qmd*: to produce .doc with descriptive analysis by indicator and by country

-   *SDG_MENARO_profile.R*: To establish working directories and packages. Should be run first before any script

-   **helpers/**

    -   *api_to_json.R*:
    -   *calculate_percentile.R*:
    -   *descriptive_df.R*: function to get descriptive statistics of the information for each indicator.
    -   *SDG_series_getp.R*:
    -   *SDGdata.R*:
    -   *unlist_columns.R*: data from SDGDB comes with nested lists. This function unlists the columns.

-   **output/**

    -   *indicator_data_WORLD.Rdata*: final database of indicators used in analysis. All countries included.

-   **source_data/**

    -   *DW.Rdata*: Data from UNICEF Data Warehouse. Already filtered cleaned. Created in *DW_data_download.R*.
    -   *MENARO_metadata.csv*: Metadata of UNICEF MENARO countries, including names, iso3 and ID codes used in SDMX
    -   *PV_CHLD_DPRV_REG_MOD.csv*: Multidimensional poverty data from MENARO DW. The original source was the MDP report.
    -   *SDGGD.Rdata*: Clean data form SDG Database
    -   *SE_TOT_PRFL_2.Rdata*: Merged data for indicator.
    -   *SH_HIV_INCD_U15.Rdata*: Merged data for indicator.
    -   *SI_POV_DAY1.csv*: Data from Salmeron-Gomez, et al., 2023 on % of children living in extreme poverty.
    -   *SI_POV_NAHC.Rdata*: Merged data for indicator.

## Files not in the repository

-   [child_related_SDG_indicators.xlsx](https://unicef-my.sharepoint.com/:x:/r/personal/spalmas_unicef_org/Documents/MENARO%20SDG/child_related_SDG_indicators.xlsx?d=wa4abddb44036478db00fa74ee2a9ab25&csf=1&web=1&e=K1uSLa): Master table of child-related SDG children indicators. Including SDG Global Database codes, UNICEF Data Warehouse codes, comments and issues. (Only people with existing access)

-   [MENARO SDG indicator analysis.docx](https://unicef-my.sharepoint.com/:w:/r/personal/spalmas_unicef_org/Documents/MENARO%20SDG/MENARO%20SDG%20indicator%20analysis.docx?d=we824a41d4492476f8fc85c4a26306185&csf=1&web=1&e=vYMwGe): Document with details of the analysis to use in the final report. Includes reduced tables of indicators, which ones are modeled, if we merged any data, and general description of the analysis so far.

-   [SDG_MENARO_data_description.docx](https://unicef-my.sharepoint.com/:w:/r/personal/spalmas_unicef_org/Documents/MENARO%20SDG/SDG_MENARO_data_description.docx?d=wd8b2268c027d4b54a8a4277e9f7da304&csf=1&web=1&e=tYm9xz): descriptive analysis to check number of points, number of indicators by country, number of countries by indicator. (Only people with existing access)
