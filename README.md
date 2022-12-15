
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Multivariate pattern analysis - mvpa

Build validated Projection to Latent Structures regression (PLS-R)
models through Monte Carlo resampling. Identify important explanatory
variables / features, using the Target Projection on the validated PLS-R
model. Perform confounder projection to interpret the influence of
potential confounders on a response value.

An overview of included tools: - Principal component analysis -
Univariate correlation - QQ-plot - Projection to latent structures
regression(PLS-R) model generation (using Monte Carlo resampling) -
Target projection / selectivity ratio calculation - Confounder
projection

## Installation

You can install mvpa from [GitHub](https://github.com/) with:

``` r
if (!require("devtools", quietly = TRUE)) {
  install.packages("devtools") 
} else {
  print("Devtools has been already installed.")
}
    
devtools::install_github("tim-b90/mvpa")
```

## Example

Use a multivariate dataset, perform confounder projection in order to
remove confounder influence and use the resulting dataframe for
PLS-regression analysis and subsequent target projection.

``` r
library(mvpa)

# We load the in-built demo dataset
dataset <- PA_Andersen

# Perform confounder projection
result_list <- perform_confounder_projection(dataset,
                                             confounders = c('Age', 'Sex', "Adiposity_score_3"),
                                             standardize = TRUE)
# Plot explained variance by confounders
plot_explained_var_by_confounders(result_list,
                                  rel_font_size = 1,
                                  rotate = FALSE)

# Use the confounder-freed dataset and perform PLS-R
new_data <- remove_variables(result_list$residual_variance_df,
                             vars_to_remove = c("Age", "Sex", "Adiposity_score_3",
                                                "BMI", "WC_to_H", "Skinfold"))
# Enable reproducible Monte-Carlo resampling
set.seed(5)

pls_r_result <- perform_mc_pls_tp(data = new_data,
                                  y_variable = "Andersen",
                                  nr_components = 3,
                                  standardize = FALSE,
                                  mc_resampling = TRUE,
                                  nr_repetitions = 150,
                                  validation_threshold = 0.3)
                                 
# Free current R session from seed                                 
set.seed(NULL)

# We can plot the RMSEP bar plot to see which selected number of components performed the best
plot_cost_function(pls_r_result,
                   validation_threshold = 0.3)

# A different view that shall help to understand how the ideal number of components (1) has been selected
plot_cost_function_values_distribution(pls_r_result)


# Now we can look at the explained (predictive) versus orthogonal variance per variable
plot_variable_variance_distribution(pls_r_result)

# Using the same data, we can plot the Selectivity Ratios, here SR1 and SR2
plot_tp_value(pls_r_result, tp_value_to_plot = "sr1")
plot_tp_value(pls_r_result, tp_value_to_plot = "sr2")

# In case we want to see the distributions of Selectivity Ratios derived from the repeated sampling,
# we can plot that as well
plot_tp_value_mc(pls_r_result,
                 tp_value_to_plot = "sr1",
                 component = 1,
                 confidence_limits = c(0.05, 0.95))
```
