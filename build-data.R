# Build dataset for interactive visualizations of occupation X risk data
# NOTE: To set directory location automatically, first open risk-tool-v2.Rproj in RStudio and then run this file

library(tidyverse)
library(haven)
library(scales)
library(weights)
library(labelled)

# Setup ----
rm(list = ls())
source("setup.R")

# Load economic data ----

# Load GDP share by 2 digit sector
gdp_share <- read_csv(file.path(RAW, "naics2-value.csv")) %>% 
    mutate(ind_2_digit = sprintf("%02d", NAICS2),
           ind_2_digit_label = NAICS2_label)

# Load employment averages and changes by 2 digit sector
employment_change <- read_dta(file.path(RAW, "ind_2_digit_employment_lfs_pumf.dta")) %>% 
    mutate(ind_2_digit = sprintf("%02d", ind_2_digit),
           pct_employment_chg = (chg_employment)/employment_avg_2019) %>%
    select(ind_2_digit,
           employment_avg_2019,
           employment_chg = chg_employment,
           employment_chg_bottom_quartile = chg_employment_bottom,
           employment_chg_at_work = chg_employment_at_work,
           employment_chg_at_work_bottom_quartile = chg_employment_at_work_bottom) %>%
    mutate(employment_chg_pct = employment_chg / employment_avg_2019)

# Temporary fix: adjusting employment for industry 51 and 71
employment_change <- employment_change %>% 
    mutate(employment_avg_2019 = ifelse(ind_2_digit == 51, employment_avg_2019*55250/(55250+54100),
                                        ifelse(ind_2_digit == 71, employment_avg_2019*54100/(55250+54100),
                                               employment_avg_2019))) %>%
    mutate(employment_avg_2019 = round(employment_avg_2019))

# Load 2-digit level employment changes that take account of teachers on spring break
employment_change_vacation <- read_dta(file.path(RAW, "ind_2_digit_employment_vacation_lfs_pumf.dta")) %>% 
    mutate(ind_2_digit = sprintf("%02d", ind_2_digit))

# Load measures of economic centrality (3 digit subsector)
centrality <- read_dta(file.path(RAW, "ind_3_digit_centrality_gdp.dta")) %>% 
    select(ind_3_digit, 
           centrality_norm = norm_influence,
           subsector_gdp_share) 

# Combine 2 digit occupation data
econ <- gdp_share %>% 
    full_join(employment_change) %>% 
    full_join(employment_change_vacation)

# Save for building the 2 digit plots
write_csv(econ, file.path(WORK, "econ-ind2.csv.gz"))
write_csv(centrality, file.path(WORK, "centrality-ind3.csv.gz"))

# Load occupational characteristics data ----

# Load the number of workers 4 digit occupation X and 3 digit sector
num_workers <- read_dta(file.path(RAW, "occ_4_digit_ind_3_digit_n_workers.dta")) %>% 
    rename(n_workers = n_workers_weighted_round) %>% 
    remove_attributes(c("label", "format.stata")) %>% 
    filter(ind_3_digit > 0)

# Load occupational characteristics by 4 digit occupation
risk <- read_dta(file.path(RAW, "occ_4_digit_all_risk_vars.dta")) %>% 
    remove_attributes(c("label", "format.stata")) %>% 
    select(-n_workers_weighted_round) 

# Add occupational and sector descriptions from crosswalks
occ_crosswalk <- read_dta(file.path(RAW, "occ_xwalk_description.dta")) %>%
    remove_attributes(c("label", "format.stata"))

ind_crosswalk <- read_dta(file.path(RAW, "ind_xwalk_description.dta")) %>% 
    remove_attributes(c("label", "format.stata"))  

num_workers <- num_workers %>%
    left_join(occ_crosswalk) %>%
    left_join(ind_crosswalk)

# Combine the number of workers and risk data
# We lack risk data for 9 observations: 7 for "Legislators" and 1 each for commissions and non-commissioned officers in the armed forces
occ <- num_workers %>% inner_join(risk)

# Drop 2-digit sector 55 (Management of Companies and Enterprises), seems to be deprecated
occ <- occ %>% filter(ind_2_digit != 55)

# Pad 2-digits variables with zeroes
occ <- occ %>% 
    mutate(ind_2_digit = sprintf("%02d", ind_2_digit),
           occ_2_digit_40 = sprintf("%02d", occ_2_digit_40))

# Compute the share of workers in this unit group as a fraction of the 2 digit major group occupation
occ <- occ %>% 
    group_by(ind_2_digit) %>% 
    mutate(share_workers_unit = n_workers/sum(n_workers)) %>% 
    ungroup()

# Reverse outdoor variables to create a single indoor variable
occ <- occ %>%
    mutate(outcover_r = 6 - outcover,
           outexposed_r = 6 - outexposed) %>%
    mutate(indoor = rowMeans(select(., outcover_r, outexposed_r))) %>%
    select(-c(outcover_r, outexposed_r))

# Combine with economic data, save 4 digit occuption by 3 digit sector data ----
write_csv(occ, file.path(WORK, "risk-occ4.csv.gz"))

