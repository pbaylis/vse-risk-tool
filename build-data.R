# Build dataset for VSE risk tool

# Setup ----
rm(list = ls())
source("setup.R")

# Load BC data ----

collect_province_data <- function(this_prov) {
    # this_prov <- "CA" # DEBUG
    # print(this_prov)
    
    # Load sector (2 digit industry) and subsector (3 digit industry) data
    ind2 <- read_csv(file.path(IN, "ind_2_digit.csv")) %>%
        filter(province == this_prov) %>% select(-province) %>%
        mutate(sector_gdp_share = sector_gdp_share / 100)
    
    ind3 <- read_csv(file.path(IN, "ind_3_digit.csv")) %>%
        filter(province == this_prov) %>% select(-province) %>%
        mutate(subsector_gdp_share = subsector_gdp_share / 100) # Technically this is a "proportion", and so are the rest of the "share" variables 
    
    ind_crosswalk <- read_csv(file.path(IN, "ind_xwalk_description.csv")) 
    
    ind <- ind_crosswalk %>% select(ind_3_digit, ind_2_digit) %>%
        left_join(ind3) %>%
        left_join(ind2)
    
    # Load occupation data at 2 and 4 digit aggregation levels
    occ4_job <- read_csv(file.path(IN, "occ_4_digit_job.csv")) # Does not vary by province
    
    occ4_hh <- read_csv(file.path(IN, "occ_4_digit_hh.csv")) %>%
        filter(province == this_prov) %>% select(-province)
    
    occ_crosswalk <- read_csv(file.path(IN, "occ_xwalk_description.csv"))
    
    occ <- occ_crosswalk %>% 
        distinct(occ_4_digit, occ_2_digit_40, .keep_all = TRUE) %>%
        select(occ_4_digit, occ_2_digit_40, occ_2_digit_40_description) %>%
        left_join(occ4_job) %>%
        left_join(occ4_hh)
    
    # Combine into a dataset where the observation is 4 digit occupation by subsector (3 digit)
    occ4_ind3 <- read_csv(file.path(IN, "occ_4_digit_ind_3_digit.csv")) %>%
        filter(province == this_prov) %>% select(-province) %>%
        rename(n_workers = n_workers_weighted_round)
    
    data <- occ4_ind3 %>% 
        left_join(ind) %>%
        left_join(occ)
    
    # Compute risk index ----
    # Load coefficients from factor analysis and for simple averaging
    coef_tbl <- read_csv(file.path(IN, "coefficients_factor_model.csv")) %>%
        filter(province == this_prov) %>% select(-province)
    
    index_vars <- data %>% 
        rename(crowded_dwelling = unsuitable_dwelling) %>%
        select(coef_tbl$var)
    
    # Standardize the variables we'll use
    for (v in coef_tbl$var) {
        mn <- coef_tbl %>% filter(var == v) %>% pull(mean)
        sd <- coef_tbl %>% filter(var == v) %>% pull(sd)
        index_vars[[v]] <- (index_vars[[v]] - mn) / sd
    }
    
    # Select relevant variables and pivot to long
    long <- index_vars %>% 
        mutate(i = 1:nrow(.)) %>%
        pivot_longer(-i, names_to = "var")
    
    # Multiply standardized variables by coefficients and return
    data$risk_index_factor <- long %>% full_join(coef_tbl) %>%
        mutate(prod = value * factor) %>%
        group_by(i) %>%
        summarise(prediction = sum(prod)) %>%
        pull(prediction)
    
    data$risk_index_mean <- long %>% full_join(coef_tbl) %>%
        mutate(prod = value * sign(factor)) %>%
        group_by(i) %>%
        summarise(prediction = sum(prod)) %>%
        pull(prediction)
    
    
    # Standardize and rescale risk indices
    # Note: for now we standardize using the weighted means and SDs in the data. Once the team running the factor analysis (in Stata) passes on those means and SDs used by that tool, we'll standardize manually using those instead to make sure that we are computing the index correctly.
    data <- data %>% 
        mutate_at(vars(starts_with("risk_index")), rescale, c(0, 100))
    
    # Drop bad industry codes or observations without critical 2 digit sector / occupations IDs.
    data <- data %>%
        filter(!(ind_2_digit %in% c(0, 55))) %>% # 55 is deprecated, not sure about 0 but seems clearly wrong
        filter(!is.na(ind_2_digit) & !is.na(occ_2_digit_40)) # Throws out observatins without matching 2 digit sector or observation codes
    
    # Compute the share of workers in this unit group as a fraction of the 2 digit sector
    # Do this after dropping so everything adds up correctly.
    data <- data %>% 
        group_by(ind_2_digit) %>% 
        mutate(share_workers_unit = n_workers/sum(n_workers)) %>% 
        ungroup()
    
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
        left_join(occ_crosswalk %>% distinct(occ_2_digit_40, .keep_all = TRUE) %>% select(occ_2_digit_40, occ_2_digit_40_description)) %>%
        left_join(ind_crosswalk %>% distinct(ind_2_digit, .keep_all = TRUE) %>% select(ind_2_digit,  ind_2_digit_description)) %>%
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
    
    dir.create(file.path(OUT, this_prov))
    
    write_csv(data, file.path(OUT, this_prov, "risk_occ4ind3.csv"))
    write_csv(data_agg, file.path(OUT, this_prov, "risk_occ2ind2.csv"))
}

province_names <- read_csv(file.path(IN, "ind_2_digit.csv")) %>% 
    distinct(province) %>% 
    filter(province != "QC") %>% # Skip QC
    pull(province)
    
lapply(province_names, collect_province_data)

# Load manually created Quebec data and resave as output ---
QC_disagg <- read_csv(file.path(IN, "QC", "quebec_risk_occ_4_ind_3.csv")) %>%
    rename(risk_index_factor = index_factor,
           risk_index_mean = index_mean)
QC_agg <- read_csv(file.path(IN, "QC", "quebec_risk_ind_2_occ_2.csv"))

dir.create(file.path(OUT, "QC"))
write_csv(QC_agg, file.path(OUT, "QC", "risk_occ2ind2.csv"))
write_csv(QC_disagg, file.path(OUT, "QC", "risk_occ4ind3.csv"))
