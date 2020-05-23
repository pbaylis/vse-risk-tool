# Build dataset for VSE risk tool

# Setup ----
rm(list = ls())
source("setup.R")

# Load BC data ----


# Load sector (2 digit industry) and subsector (3 digit industry) data
ind2 <- read_csv(file.path(IN, "BC/ind_2_digit.csv"))
ind3 <- read_csv(file.path(IN, "BC/ind_3_digit.csv"))
ind_crosswalk <- read_dta(file.path(IN, "BC", "ind_xwalk_description.dta")) # TODO: Get from CSV --- ask Maria

ind <- ind3 %>% 
    left_join(ind_crosswalk %>% select(ind_3_digit, ind_2_digit)) %>%
    left_join(ind2)

# Load occupation data at 2 and 4 digit aggregation levels
occ2 <- read_csv(file.path(IN, "BC/occ_2_digit.csv")) # TODO: For consistency, note that I removed _desc from this name.
occ4_job <- read_csv(file.path(IN, "BC/occ_4_digit_job.csv"))
occ4_hh <- read_csv(file.path(IN, "BC/occ_4_digit_hh.csv"))
occ_crosswalk <- read_dta(file.path(IN, "BC", "occ_xwalk_description.dta")) # TODO: Get from CSV --- ask Maria

occ <- occ4_job %>%
    left_join(occ4_hh) %>%
    left_join(occ_crosswalk %>% distinct(occ_4_digit, occ_2_digit_47)) %>%
    left_join(occ2)

# Combine into a dataset where the observation is 4 digit occupation by subsector (3 digit)
occ4_ind3 <- read_csv(file.path(IN, "BC/occ_4_digit_ind_3_digit.csv")) %>%
    rename(n_workers = n_workers_weighted_round)

data <- occ4_ind3 %>% 
    left_join(ind) %>%
    left_join(occ)

# Compute the share of workers in this unit group as a fraction of the 2 digit sector
data <- data %>% 
    group_by(ind_2_digit) %>% 
    mutate(share_workers_unit = n_workers/sum(n_workers)) %>% 
    ungroup()

# Reverse outdoor variables to create a single indoor variable. TODO: Keep this? Or just remove.
# occ <- occ %>%
#     mutate(outcover_r = 6 - outcover,
#            outexposed_r = 6 - outexposed) %>%
#     mutate(indoor = rowMeans(select(., outcover_r, outexposed_r))) %>%
#     select(-c(outcover_r, outexposed_r))

# Compute risk index ----
# Load coefficients from factor analysis and for simple averaging
b <- read_csv(file.path(IN, "BC/risk_index_coefficients.csv")) 

data <- data %>% mutate(
    risk_index_factor = compute_risk(., b, "coef1"),
    risk_index_mean = compute_risk(., b, "coef2"))

# Standardize and rescale risk indices
data <- data %>% 
    mutate_at(vars(starts_with("risk_index")), stdz, weight = occ$n_workers) %>%
    mutate_at(vars(starts_with("risk_index")), rescale, c(0, 100))

# Write disaggreged data for table ----
write_csv(data, file.path(OUT, "BC", "occ4_ind3.csv"))

# TODO: Check on NAs. Where do they come from?

# test <- data[!complete.cases(data),]

# Many are due to having an ind_3_digit == 0. Why does this happen?
# Also many are in sector 55.

# Having computed this aggregated risk index, now we'll aggregate the risk scores to the level displayed in the figure, which is 2 digit occupation by 2 digit sector

temp <- data %>% 
    group_by(occ_2_digit_47, ind_2_digit) %>%
    summarise(n_workers = sum(n_workers))

data_agg <- data %>%
    group_by(occ_2_digit_47, ind_2_digit) %>%
    summarize_at(vars(risk_index_factor, risk_index_mean), weighted.mean, weights = data$n_workers, na.rm = T) %>% 
    filter(complete.cases(occ_2_digit_47, ind_2_digit)) %>%
    left_join(ind2 %>% select(ind_2_digit, employment_avg_2019, chg_employment, chg_employment_bottom, sector_gdp_share)) %>% 
    left_join(temp) %>%
    left_join(occ_crosswalk %>% distinct(occ_2_digit_47, occ_2_digit_47_description)) %>%
    left_join(ind_crosswalk %>% distinct(ind_2_digit, ind_2_digit_description)) %>%
    group_by(ind_2_digit) %>%
    mutate(share_workers_major = n_workers / sum(n_workers))


write_csv(data_agg, file.path(OUT, "BC", "occ2_ind2.csv"))
