# Plot static and interactive scatterplots
# Note: needs to be run by app.R, uses objects created in that file

plot_data <- read_csv(file.path(OUT, "BC", "occ2_ind2.csv")) 

# Wrap label for legend
plot_data <- plot_data %>% 
    mutate(ind_2_digit_label_leg = str_wrap(ind_2_digit_description, width = 20))

# Occupation-industry scatterplotS ("bubble box plots") ----

# Only keep occupations that are at least 1% of a sector
# Also include the number of workers in the industry that this occupation is a part of
plot_data <- plot_data %>% 
    filter(share_workers_major >= 0.01) %>%
    mutate(lab = sprintf("%s: %s", ind_2_digit_description, occ_2_digit_47_description)) %>%
    group_by(ind_2_digit) %>%
    mutate(n_workers_industry = sum(n_workers, na.rm = T)) %>%
    mutate(share_workers = n_workers / n_workers_industry) %>%
    mutate(employment_loss = -1 * chg_employment,
           employment_loss_bottom_quartile = -1 * chg_employment_bottom) %>%
    ungroup() %>% 
    mutate(key = 1:nrow(.))


# Create the tooltip that we'll use for all plots
tooltip <-
    ~ sprintf(
        "Sector: %s<br>Occupation: %s<br>VSE Risk Index (Factor model): %2.0f<br>Sector Feb employment: %s<br>Sector employment loss: Feb-Mar: %s<br>Sector GDP share: %2.2f<br>Workers in occupation: %s<br>Share of workers within sector: %2.2f<br><br><b>Sector Code</b>: %s<br><b>Occupation Code</b>: %s",
        ind_2_digit_description,
        occ_2_digit_47_description,
        risk_index_factor,
        format(employment_avg_2019, big.mark=",", trim=TRUE),
        format(employment_loss, big.mark=",", trim=TRUE),
        sector_gdp_share,
        format(n_workers,big.mark=",", trim=TRUE),
        share_workers,
        ind_2_digit,
        occ_2_digit_47
    )

# Create a base figure to build other figures from
fig_base <- plot_ly(data = plot_data, 
                    y = ~risk_index_factor,
                    customdata = ~key,
                    source = "myClickSource",
                    color = ~factor(ind_2_digit_label_leg),
                    colors = "Paired",
                    size = ~share_workers,
                    sizes = c(1, 500),
                    text = tooltip,
                    hoverinfo = "text") %>%
    layout(yaxis = list(title = "VSE Risk Index (Factor model)", range = expand_range(range(plot_data$risk_index_factor), 0.05), fixedrange = T))

fig3A <- fig_base %>% 
    add_markers(x = ~employment_avg_2019) %>%
    layout(xaxis = list(title = "Sector employment (2019 average)", 
                        range = expand_range(range(plot_data$employment_avg_2019, na.rm = T), 0.05), 
                        fixedrange = T))

fig3B <- fig_base %>% 
    add_markers(x = ~employment_loss) %>%
    layout(xaxis = list(title = "Sector employment loss", 
                        range = expand_range(range(plot_data$employment_loss, na.rm = T), 0.05), 
                        fixedrange = T))

fig3C <- fig_base %>% 
    add_markers(x = ~share_GDP) %>%
    layout(xaxis = list(title = "Sector GDP share", 
                        range = expand_range(range(plot_data$sector_gdp_share, na.rm = T), 0.05), 
                        fixedrange = T))

fig3D <- fig_base %>% 
    add_markers(x = ~employment_loss_bottom_quartile) %>%
    layout(xaxis = list(title = "Sector employment loss: Feb-Mar (bottom quartile income)", 
                        range = expand_range(range(plot_data$employment_loss_bottom_quartile, na.rm = T), 0.05), 
                        fixedrange = T))

