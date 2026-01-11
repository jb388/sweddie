This directory contains template tables for the Synthesizing Warming Experiments to Depth Data Integration Effort (SWEDDIE).

These templates correspond to the "core" component of the database, i.e., data that is predominantly static. The data tables should be self explanatory; the accompanying 'xxx_dd' tables define the variables for each table. Briefly, the 'experiment' table focuses on infrastructure; 'site' focuses on site characteristics, e.g., long term climate averages, and soil, vegetation, and geological characterization; 'plot' contains experimental design information (plot layout and IDs, treatments).


We have also provided templates for metadata files, i.e., data dictionaries ('datTemplate_dd') and file level metadata ('flmd_SWEDDIE', 'flmd_SWEDDIE_dd'). The file 'Guide_dd.txt' contains instructions for filling out 'datTemplate_dd'. If you use these templates, please replace the "datTemplate" component of the dd table name with the exact string used for the data file name. 

The 'flmd_SWEDDIE' and 'flmd_SWEDDIE_dd' files provided are for reference and internal usage. All current experiments contain a flmd file in the data/experiments/*expName*/meta directory which is appended with a new row/s upon ingestion of new data.

Notes:
1) All observational data (soil temperature, soil moisture, CO2 fluxes, etc.) must contain a column named "plt_name" with values that match exactly with the values in the 'plot' table. This is required to preserve treatment information and situate observations in space.
2) If multiple entries need to be entered in a single cell, please used ";" (semicolon) and no spaces
3) Missing data values preferably "NA". Please specify convention used for missing data values, not a number values (e.g., "NAN"), etc., in FLMD 

Please contact Jeff Beem-Miller (jeffrey.beem.miller[at]gmail.com) with any questions.