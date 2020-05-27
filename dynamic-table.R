# Create a table with all variables included

create_tables <- function(data) {
    tab_list <- list() # Create a container for the results
    
    all <- data %>%
        dplyr::arrange(ind_2_digit, occ_2_digit_40, -n_workers) %>%
        mutate(mental_health = mental_health_always + mental_health_sometimes)
    
    # Rename variables for display
    all <- all %>%
        select(`Sector code` = ind_2_digit,
               `Occupation (NOC40)` = occ_2_digit_40,
               Sector = ind_2_digit_description,
               `Subsector` = ind_3_digit_description,
               `Occupation (Major group)` = occ_2_digit_40_description, 
               `Occupation (Unit group)` = occ_4_digit_description,
               `VSE Risk Index (Factor model)` = risk_index_factor,
               `Alt Risk Index (Simple avg)` = risk_index_mean,
               `Occupation (unit group) share of sector workers` = share_workers_unit, # Start economics variables
               Workers = n_workers, 
               `Subsector centrality` = norm_influence,
               `Subsector GDP share` = subsector_gdp_share,
               `Sector employment (average 2019)` = employment_avg_2019,
               `Sector employment change` = chg_employment,
               `Sector employment change (bottom quartile)` = chg_employment_bottom,
               `Sector GDP share` = sector_gdp_share,
               `Physical proximity` = proximity, # Start risk variables
               `Deal with public` = public, 
               `Face to face` = face2face, 
               `Assisting and caring` = assist,
               `Exposed to weather` = outexposed,
               `Contact with others` = contact,
               `Exposed to disease` = disease,
               `External customers` = customer,
               # Start household risk factor variables
               `Lives with health worker (%)` = lives_with_health_worker, 
               `Public transit (%)` = takes_public_transit,
               `Crowded Dwelling (%)` = unsuitable_dwelling,
               `Work from home (%)` = work_from_home, 
               `Low income (%)` = low_income_family,
               `Multi-generational household (%)` = multi_gen_hh,
               `Lives with 60+ (%)` = lives_with_age_60_plus,
               `Lives with kids under 5 (%)` = lives_with_kids_5_under,
               `Lives with kids between 6 and 12 (%)` = lives_with_kids_6_12,
               `Age of worker: age 60 and over (%)` = age_60_plus,
               `Age of worker: 15-24 (%)` = age_15_24,
               `Age of worker: 25-34 (%)` = age_25_34,
               `Age of worker: 35-44 (%)` = age_35_44,
               `Age of worker: 45-54 (%)` = age_45_54,
               `Age of worker: 55-64 (%)` = age_55_64,
               `Age of worker: 65-74 (%)` = age_65_74,
               `Age of worker: age 75 and over (%)` = age_75_plus,
               `Language (%)` = language_spoken_not_english,
               `Immigrant (%)` = immigrant,
               `Lone Parent (%)` = lone_parent,
               `Poor Neighborhood (%)` = low_income_nbhd,
               `Mental Health (%)` = mental_health
        )
    
    tab_list$main <- all %>%
        select(`Sector code`:`Occupation (unit group) share of sector workers`)
    
    tab_list$risk <- all %>%
        select(`Sector code`:`Occupation (unit group) share of sector workers`, 
               `Physical proximity`:`External customers`)
    
    tab_list$econ <- all %>%
        select(`Sector code`:`Occupation (unit group) share of sector workers`, 
               `Workers`:`Sector GDP share`)
    
    tab_list$household <- all %>%
        select(`Sector code`:`Occupation (unit group) share of sector workers`, 
               `Lives with health worker (%)`:`Mental Health (%)`)
    
    # Return the list of tables
    tab_list
}


