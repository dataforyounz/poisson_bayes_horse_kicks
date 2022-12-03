rm( list = ls() )

library( tidyverse )
library( gridExtra )
library( ggpubr )
library( R2jags )

## Data load and format --------------------------------------------------------

data_file <- list.files( "data" )
data_long <- read_csv( paste0("data/", data_file), col_types = cols() ) %>%
             pivot_longer( -Year, names_to = "Corp", values_to = "Deaths" )

deaths <- data_long %>% pull(Deaths)
n_obs  <- length( deaths ) 

alpha <- 1
beta  <- 1

data   <- c("n_obs", "deaths", "alpha", "beta")
params <- "lambda"

model_fit <- jags( data, parameters.to.save = params, model.file = "src/model_conjugate.txt",
                  n.chains = 2, n.iter = 5000, n.burnin = 1000, n.thin = 1, DIC = T)

model_fit

posterior <- tibble(
             samples = model_fit$BUGSoutput$sims.list$lambda[,1]
)

ggplot() + geom_histogram( data = posterior, aes(x = samples), bins = 30, fill = "light blue", col = "blue") 

hist( posterior$samples, nclass = 50, freq = F)

lam <- seq(0, 5, by = .01)
lines( lam, dgamma(lam, alpha + sum(deaths), beta + n_obs), col = "red")



