# vse-risk-tool
Code repository for the VSE COVID Risk Tool, hosted online at [https://baylislab.shinyapps.io/risk-tool-v2/](https://baylislab.shinyapps.io/risk-tool-v2/).

# Purpose

VSE COVID-19 Risk Tool can be used to compare some of the risks and benefits of re-opening different sectors of the BC economy. This repository contains the code necessary to produce a Shiny app with two main visualizations:
  1. An interactive scatterplot comparing measures of economic productivity with a risk index for occupations in BC sectors.
  2. A flexible table to show measures of risk, economic productivity, and a range of other relevant measures by disaggregated occupations and subsectors. This table can respond to selections made in the scatterplot.

# Usage

1. Open `vse-risk-tool.Rproj` in RStudio.
2. Open `app.R`.
3. Execute `Run App` in RStudio.

## File structure

- `data/input/` contains tidy data aggregated to its specific level for each province, for example:
  - `/BC/ind_3_digit.csv`: Measures of centrality for each subsector
  - `/BC/ind_2_digit.csv`: Share of GDP and employment for each sector
  - `/BC/occ_4_digit_job.csv`: Occupation risk levels for each subsector
  - `/BC/occ_4_digit_hh.csv`: Risk levels associated with the household for each subsector
  - `/BC/occ_2_digit_desc.csv`: Two digit occupation code (NOC47) and its description
  - `/BC/risk_index_coefficients.csv`: Risk coefficients used in VSE Risk Tool
- `data/output/` contains the input data merged for each province to be used in the VSE Risk Tool

## Note on `build-data.R`

`build-data.R` collects the data in `data/input` directory and reformats them into the detailed (`/data/output/<PROV>/detailed.csv`) and aggregated (`/data/output/<PROV>/aggregated.csv`) output datasets. It is run before the execution of the Shiny application and only needs to be rerun if the input datasets are changed.

Most users, however, should consider this repository to be a example of a useful way to visualize the underlying data. However, repurposing this code for other regions will require significant additional data work, as it is only currently designed to handle BC-specific data.

