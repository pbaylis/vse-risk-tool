# vse-risk-tool
Code repository for the VSE COVID Risk Tool, hosted online at [https://baylislab.shinyapps.io/risk-tool-v2/](https://baylislab.shinyapps.io/risk-tool-v2/).

# Purpose

VSE COVID-19 Risk Tool can be used to compare some of the risks and benefits of re-opening different sectors of the BC economy. This repository contains the code necessary to produce a Shiny app with two main visualizations:
  1. An interactive scatterplot comparing measures of economic productivity with a risk index for occupations in BC sectors.
  2. A flexible table to show measures of risk, economic productivity, and a range of other relevant measures by disaggregated occupations and subsectors. This table can respond to selections made in the scatterplot.

# Usage

1. Open `vse-risk-tool.Rproj` in RStudio.
2. Execute `Run App` in RStudio.


## File structure

- `data/input/` contains tidy data aggregated to its specific level for each province, for example:
  - `/BC/centraltrality-ind3.csv`: Measures of economic centrality for each subsector
  - `/BC/econ-ind2.csv`: Measures of industry employment for each sector
  - `/BC/risk-occ4.csv`: Occupation measures of risk for each subsector
  - `/BC/risk_index_coefficients.csv`: Risk coefficients used in VSE Risk Tool
- `data/output/` contains the input data merged for each province to be used in the VSE Risk Tool

## Note on `build-data.R`

By default, this tool produces these figures using BC-specific data in `data/`, which are produced by `build-data.R`. `build-data.R` builds the BC-specific on source data on occupation risk and economic value from outside of the repository. Users with access to the BC-specific data can run `build-data.R` by adding their location for the cleaned BC-specific data to the `RAW_POSSIBLE` character vector in `setup.R`.

Most users, however, should consider this repository to be a example of a useful way to visualize the underlying data. However, repurposing this code for other regions will require significant additional data work, as it is only currently designed to handle BC-specific data.

