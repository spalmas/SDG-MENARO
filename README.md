# Trend analysis for SDG child indicators in UNICEF MENARO

## Files in the repository

-   *download_DW.R*: To download and clean data from UNICEF Data Warehouse. Produces *source_data/DW.Rdata*.

-   *download_SDGGD.R*: To download and clean data from SDG Gloal Database. Produces *source_data/SDGGD.Rdata*.

-   *SDG_MENARO_data_compile.R*: Code to extract the data from the SDG Global Database

-   *SDG_MENARO_profile.R*: To establish working directories and packages. Should be run first before any script.

-   **helpers/**

    -   *api_to_json.R*:
    -   *calculate_percentile.R*:
    -   *SDG_series_getp.R*:
    -   *SDGdata.R*:
    -   *unlist_columns.R*: data from SDGDB comes with nested lists. This function unlists the columns.

-   **output/**

    -   *INDICATORS_MENARO.Rdata*: final database of indicators in MENARO to use for analysis

-   **source_data/**

    -   *DW.Rdata*: Data from UNICEF Data Warehouse. Already filtered cleaned. Created in *DW_data_download.R*.
    -   *MENARO_metadata.csv*: Metadata of UNICEF MENARO countries, including names, iso3 and ID codes used in SDMX
    -   *PV_CHLD_DPRV_REG_MOD.csv*: Multidimensional poverty data from MENARO DW. The original source was the MDP report.
    -   *SDGGD.Rdata*: Clean data form SDG Database
    -   *SI_POV_DAY1.csv*: Data from Salmeron-Gomez, et al., 2023 on % of children living in extreme poverty.

## Files not in the repository

-   [child_related_SDG_indicators.xlsx](https://unicef-my.sharepoint.com/:x:/r/personal/spalmas_unicef_org/Documents/MENARO%20SDG/child_related_SDG_indicators.xlsx?d=wa4abddb44036478db00fa74ee2a9ab25&csf=1&web=1&e=K1uSLa): Master table of child-related SDG children indicators. Including SDG Global Database codes, UNICEF Data Warehouse codes, comments and issues. (Only people with existing access)

-   [child_related_SDG_indicators.docx](https://unicef-my.sharepoint.com/:w:/r/personal/spalmas_unicef_org/Documents/MENARO%20SDG/child_related_SDG_indicators.docx?d=we824a41d4492476f8fc85c4a26306185&csf=1&web=1&e=giRpjV): Reduced version of the master table of indicators. Useful for written documents (Only people with existing access)
