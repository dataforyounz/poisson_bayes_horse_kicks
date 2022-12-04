rm( list = ls() )

library( tidyverse )
library( gridExtra )
library( ggpubr )
library( R2jags )

## Data load and format --------------------------------------------------------

data_file <- list.files( "data" )
data_long <- read_csv( paste0("data/", data_file), col_types = cols() ) %>%
             pivot_longer( -Year, names_to = "Corp", values_to = "Deaths" )

## Pooled model --------------------------------------------------------

deaths <- data_long %>% pull(Deaths)
n_obs  <- length( deaths ) 

# Priors on Gamma distribution - already specified in .txt file.
alpha <- 1
beta  <- 1

# Specify jags model and generate samples
data      <- c( "n_obs", "deaths" )
params    <- "lambda"
model_fit <- jags( data, 
                   parameters.to.save = params, 
                   model.file = "src/model_conjugate.txt",
                   n.chains = 2, 
                   n.iter = 5000, 
                   n.burnin = 1000, 
                   n.thin = 1, 
                   DIC = T)

# Create tibble of posterior samples
posterior <- tibble(
             samples = model_fit$BUGSoutput$sims.list$lambda[,1]
)

# Histogram of posterior with Gamma density overlayed
plot_pooled <- ggplot( data = posterior ) + 
               geom_histogram( aes(x = samples, y = after_stat(density) ), 
                               bins = 35, fill = "sky blue", col = "white" ) +
               geom_function( fun = dgamma, 
                              args = list( shape = alpha + sum(deaths), rate = beta + n_obs), 
                              col = "navy", size = .7) +
               labs( x = "Lambda", y = "Density", subtitle = "Posterior samples") +
               theme_bw() 

ggsave( plot_pooled, file = "figs/plot_pooled.png", units = "cm", width = 8, height = 7)


index <- sample( 1:dim(model_fit$BUGSoutput$sims.list$lambda)[1], 10, replace = T)
samps <- model_fit$BUGSoutput$sims.list$lambda[index,1]

sapply( samps, function(lambda) dpois(0:10, lambda) ) 




## Hierarchical 

deaths <- data_long %>% pull(Deaths)
corps  <- data_long %>% distinct( Corp ) %>% pull()
n_obs  <- data_long %>% distinct( Year ) %>% count() %>% pull()
n_corp <- length( corps )

deaths <- matrix( deaths, ncol = n_corp, nrow = n_obs )

tmp <- read_csv( paste0("data/", data_file), col_types = cols() ) 

death_mat <- as.matrix(tmp)[,-1]

data   <- c("death_mat", "n_obs", "n_corp")
params <- c("lambda", "alpha", "beta")

model_fit <- jags( data, parameters.to.save = params, model.file = "src/model_conjugate_hier.txt",
                   n.chains = 10, n.iter = 5000, n.burnin = 2500, n.thin = 1, DIC = T)

model_fit

hist( model_fit$BUGSoutput$sims.list$alpha, nclass = 50 )
hist( model_fit$BUGSoutput$sims.list$beta, nclass = 50 )

hist( model_fit$BUGSoutput$sims.list$lambda[,3], nclass = 50 )






