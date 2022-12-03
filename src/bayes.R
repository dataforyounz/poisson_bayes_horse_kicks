rm( list = ls() )

library( tidyverse )
library( gridExtra )
library( ggpubr )

source( "functions.R" )

## Read in data

data_file <- list.files( pattern = ".csv" )
data_long <- read_csv( data_file, col_types = cols() ) %>%
             pivot_longer( -Year, names_to = "Corp", values_to = "Deaths" )


## Fit Bayesian Poisson Model

y <- data_long %>% pull( Deaths )

bayes_fit    <- fit_poisson_gamma( y = y, alpha = 1, beta = 1 )
#post_samples <- sample_posterior( fit = bayes_fit )

bayes_plot( fit = bayes_fit )



y_corp <- data_long %>% filter(Corp == "C11") %>% pull( Deaths )

bayes_fit_corp <- fit_poisson_gamma( y = y_corp, alpha = 1, beta = 1 )
bayes_summary( fit = bayes_fit_corp )

bayes_plot( fit = bayes_fit_corp )


bayes_summary <- function( fit )
{
  
  cat( paste0(
    "--------------------------------------------\n",
    "Poisson-Gamma Model\n--------------------------------------------\n",
    "Data",
    "\nObs: ", fit$n_obs,
    "\nSum: ", sum( fit$data ),
    "\nMLE: ", mean( fit$data ), 
    "\n\nDistributions",
    "\nPrior:\t   ", fit$prior, 
    "\nPosterior: ", fit$posterior,
    "\n\nPosterior Parameters",
    "\nMode: \t", round( fit$posterior_summary["mode"], 3 ),
    "\nMedian: ", round( fit$posterior_summary["median"], 3 ),
    "\nMean: \t", round( fit$posterior_summary["mean"], 3 ),
    "\nCI: \t", "[", round( fit$posterior_ci[1], 3 ), " ", round( fit$posterior_ci[2], 3 ), "]"
    
    ))
  
}






