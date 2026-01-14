Welcome to the repository for *sweddie*, a companion R package containing data ingestion, query, and reporting tools for the Soil Warming Experiment to Depth Data Integration Effort (SWEDDIE).

Getting started:
1) Install *sweddie* locally:
```
devtools::install_github("https://github.com/jb388/sweddie")
library(sweddie)
```
2) Download the database by visiting <https://doi.org/10.5281/zenodo.18237777>. SWEDDIE v1.0.0 contains all core data (experimental warming infrastructure, site characteristics, plot layout/experimental design) as well as soil temperature and moisture timeseries data. 
3) Once the data are downloaded and unzipped you can deploy the functions in the *sweddie* R package. 

Notes:
- SWEDDIE has many more datasets than soil temperature and moisture timeseries, but not all of them are publically available at this time. Metadata for all datasets in SWEDDIE will be available in the near future (early 2026), and we encourage users to contact the data providers directly if interested in any of the non-public datasets listed.
- The package is still in beta with testing ongoing. Please notify us with any code issues, package concerns, or questions via the [Issues](https://github.com/jb388/sweddie/issues) page.
- The package may be on CRAN eventually but we encourage you to install the most recent (beta) version of the package here for best results. 
