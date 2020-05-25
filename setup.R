library(pacman)

p_load(shiny, tidyverse, DT, haven, RColorBrewer, cowplot, scales, ggrepel, plotly, gtools, stringr, weights, labelled)

# Where to get input files
IN <- "data/input" 

# Where do save output files
OUT <- "data/output"

#' Compute risk index from a dataset, give a table with variable names and weights (coefficients). Just creates a linear combination of the given variables.
#'
#' @param data dataset with variables over which we'll be computing the linear combination.
#' @param coef_tbl table with columns named "var" and coef_name (see below).
#' @param coef_name character vector of the variable name with the weights to use (default: "coef").
#'
#' @return numeric vector of linear combinations with length equal to the number of rows in data.
compute_risk <- function(data, coef_tbl, coef_name = "coef") {
    # DEBUG START
    # data <- risk
    # coef_tbl <- b
    # coef_name <- "coef1"
    # DEBUG END
    
    # Select relevant variables and pivot to long
    long <- data %>% 
        select(coef_tbl$var) %>%
        mutate(i = 1:nrow(.)) %>%
        pivot_longer(-i, names_to = "var")
    
    # Standardize all variables before use, since that's what the factor analysis does
    long <- long %>%
        group_by(var) %>%
        mutate(value_sd = scale(value)) %>%
        ungroup
    
    # Multiply standardized variables by coefficients and return
    long %>% full_join(coef_tbl) %>%
        mutate(prod = value_sd * get(coef_name)) %>%
        group_by(i) %>%
        summarise(prediction = sum(prod)) %>%
        pull(prediction)
}

# Set visual theme
theme_set(theme_cowplot())

