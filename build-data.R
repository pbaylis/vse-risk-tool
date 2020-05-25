# Build dataset for VSE risk tool

# Setup ----
rm(list = ls())
source("setup.R")

# Load BC data ----

# Load sector (2 digit industry) and subsector (3 digit industry) data
ind2 <- read_csv(file.path(IN, "BC/ind_2_digit.csv"))

# Temporarily split average employment in sectors 51 and 71 up, until this is fixed in raw data
ind2 <- ind2 %>% 
    mutate(employment_avg_2019 = case_when(
        ind_2_digit == 51 ~ employment_avg_2019*55250/(55250+54100),
        ind_2_digit == 71 ~ employment_avg_2019*54100/(55250+54100),
        TRUE ~ employment_avg_2019
    )) 

ind3 <- read_csv(file.path(IN, "BC/ind_3_digit.csv"))
ind_crosswalk <- read_csv(file.path(IN, "BC", "ind_xwalk_description.csv")) 

ind <- ind_crosswalk %>% select(ind_3_digit, ind_2_digit) %>%
    left_join(ind3) %>%
    left_join(ind2)

# Load occupation data at 2 and 4 digit aggregation levels
occ2 <- read_csv(file.path(IN, "BC/occ_2_digit.csv")) # TODO: For consistency, note that I removed _desc from this name.
occ4_job <- read_csv(file.path(IN, "BC/occ_4_digit_job.csv"))
occ4_hh <- read_csv(file.path(IN, "BC/occ_4_digit_hh.csv"))
occ_crosswalk <- read_csv(file.path(IN, "BC", "occ_xwalk_description.csv"))

occ <- occ_crosswalk %>% distinct(occ_4_digit, occ_2_digit_47) %>%
    left_join(occ4_job) %>%
    left_join(occ4_hh) %>%
    left_join(occ2)

# Combine into a dataset where the observation is 4 digit occupation by subsector (3 digit)
occ4_ind3 <- read_csv(file.path(IN, "BC/occ_4_digit_ind_3_digit.csv")) %>%
    rename(n_workers = n_workers_weighted_round)

data <- occ4_ind3 %>% 
    left_join(ind) %>%
    left_join(occ)

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
# Note: for now we standardize using the weighted means and SDs in the data. Once the team running the factor analysis (in Stata) passes on those means and SDs used by that tool, we'll standardize manually using those instead to make sure that we are computing the index correctly.
data <- data %>% 
    mutate_at(vars(starts_with("risk_index")), stdz, weight = data$n_workers) %>% 
    mutate_at(vars(starts_with("risk_index")), rescale, c(0, 100))

# Where do our NAs come from? TODO: document the sources of these errors. Have asked Sam and Maria to look into it. 
test <- data[!complete.cases(data),] %>%
    select(ind_3_digit, occ_4_digit, ind_2_digit, occ_2_digit_47, n_workers, sector_gdp_share,subsector_gdp_share, assist, lives_with_health_worker)

# Drop bad industry codes or observations without critical 2 digit sector / occupations IDs.
data <- data %>%
    filter(!(ind_2_digit %in% c(0, 55))) %>% # 55 is deprecated, not sure about 0 but seems clearly wrong
    filter(!is.na(ind_2_digit) & !is.na(occ_2_digit_47)) # Throws out observatins without matching 2 digit sector or observation codes

test2 <- data[!complete.cases(data),] %>%
    select(ind_3_digit, occ_4_digit, ind_2_digit, occ_2_digit_47, n_workers, sector_gdp_share,subsector_gdp_share, assist, lives_with_health_worker)

# Compute the share of workers in this unit group as a fraction of the 2 digit sector
# Do this after dropping so everything adds up correctly.
data <- data %>% 
    group_by(ind_2_digit) %>% 
    mutate(share_workers_unit = n_workers/sum(n_workers)) %>% 
    ungroup()

# TEMPORARY: rename 47 to 40 so that this code after this works. But should actually use 40 above once we have the data read.
data <- data %>%
    rename(occ_2_digit_40 = occ_2_digit_47,
           occ_2_digit_40_description = occ_2_digit_47_description)

# Having computed this aggregated risk index, now we'll aggregate the risk scores to the level displayed in the figure, which is 2 digit occupation by 2 digit sector
temp <- data %>% 
    group_by(occ_2_digit_40, ind_2_digit) %>%
    summarise(n_workers = sum(n_workers))

data_agg <- data %>%
    group_by(occ_2_digit_40, ind_2_digit) %>%
    summarize_at(vars(risk_index_factor, risk_index_mean), ~ weighted.mean(., w = n_workers, na.rm = T)) %>% 
    filter(complete.cases(occ_2_digit_40, ind_2_digit)) %>%
    left_join(ind2 %>% select(ind_2_digit, employment_avg_2019, chg_employment, chg_employment_bottom, sector_gdp_share)) %>% 
    left_join(temp) %>%
    left_join(occ_crosswalk %>% distinct(occ_2_digit_40, occ_2_digit_40_description)) %>%
    left_join(ind_crosswalk %>% distinct(ind_2_digit, ind_2_digit_description)) %>%
    group_by(ind_2_digit) %>%
    mutate(share_workers_major = n_workers / sum(n_workers)) %>%
    ungroup()

# Write disaggregated and aggregated data ----
# Now that merging is complete, set IDs to be characters with the appropriate badding
data <- data %>%
    mutate_at(vars(ind_2_digit, occ_2_digit_40), function(x) sprintf("%02d", x)) %>%
    mutate_at(vars(ind_3_digit), function(x) sprintf("%03d", x)) %>% 
    mutate_at(vars(occ_4_digit), function(x) sprintf("%04d", x))

data_agg <- data_agg %>%
    mutate_at(vars(ind_2_digit, occ_2_digit_40), function(x) sprintf("%02d", x))

write_csv(data, file.path(OUT, "BC", "occ4_ind3.csv"))
write_csv(data_agg, file.path(OUT, "BC", "occ2_ind2.csv"))
