# Trend analysis for SDG child indicators in UNICEF MENARO

## Files in the repository

-   *SDG_MENARO_data_extraction.R*: Code to extract the data from the SDG Global Database

-   **source_data/**

    -   *MENARO_metadata.csv*: Metadata of UNICEF MENARO countries, including names, iso3 and ID codes used in SDMX
    -   *child_related_SDG_indicators.xlsx*: Master list of child-related SDG children indicators. Including SDG Global Database codes, UNICEF Data Warehouse codes, comments and issues. A document with a reduced version of the table can be found [HERE](https://unicef-my.sharepoint.com/:w:/g/personal/spalmas_unicef_org/ER2kJOiSRG9Hj8hcSiYwYYUBHgC4sQlZvyaFiA2eTnL4HQ?e=nL9o7y) (access with UNICEF account).
    -   *PV_CHLD_DPRV_REG_MOD.csv*: Multidimensional poverty data from MENARO DW. The original source was the MDP report.
    -   *SI_POV_DAY1.csv*: Data from Salmeron-Gomez, et al., 2023 on % of children living in extreme poverty.

-   **helpers/**

    -   *api_to_json.R*:
    -   *calculate_percentile.R*:
    -   *SDG_series_getp.R*:
    -   *SDGdata.R*:
    -   *unlist_columns.R*: data from SDGDB comes with nested lists. This function unlists the columns.

-   **output/**

    -   *cri_db_world.Rdata*: final database of indicators to use for analysis
