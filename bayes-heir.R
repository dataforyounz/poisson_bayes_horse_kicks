rm( list = ls() )

library( tidyverse )
library( gridExtra )
library( ggpubr )
library( R2jags )

## Data load and format --------------------------------------------------------

data_file <- list.files( "data" )
data_load <- read_csv( paste0("data/", data_file), col_types = cols() ) 
death_mat <- as.matrix(data_load)[,-1]

n_obs <- nrow( death_mat )
n_corp <- ncol( death_mat )


n_chains  <- 10
n_burnin  <- 2500
n_samples <- 5000

data   <- c("death_mat", "n_obs", "n_corp")
params <- c("lambda", "alpha", "beta")

model_fit <- jags( data, 
                   parameters.to.save = params, 
                   model.file = "src/model_conjugate_hier.txt",
                   n.chains = n_chains, 
                   n.iter = n_samples, 
                   n.burnin = n_burnin, 
                   n.thin = 1, 
                   DIC = T
                  )

save( model_fit, file = "samples/heir_samples.Rdata" ) 


corp_means <- round( model_fit$BUGSoutput$mean$lambda, 3)
hdi_lower <- round( apply(model_fit$BUGSoutput$sims.list$lambda, 2, quantile, .025), 3)
hdi_upper <- round( apply(model_fit$BUGSoutput$sims.list$lambda, 2, quantile, .975), 3)

posterior_stats <- tibble(
  Corp = colnames( data_load )[-1],
  Mean = corp_means,
  `HDI Lower` = hdi_lower,
  `HDI Upper` = hdi_upper
)


posterior_stats %>% 
  kbl( table.attr = "style='width:50%;'",
       caption = "Posterior rates for each Corp with 95% Highest Density Interval (HDI).") %>% 
  kable_classic( full_width = T ) %>%
  column_spec( 2, color = "white", 
               background = spec_color(posterior_stats$Mean[1:14], end = 0.5)) %>%
  save_kable( "figs/table_posterior_corp.png", zoom = 10 )






model_fit

hist( model_fit$BUGSoutput$sims.list$alpha, nclass = 25 )
hist( model_fit$BUGSoutput$sims.list$beta, nclass = 25 )

hist( model_fit$BUGSoutput$sims.list$lambda[,3], nclass = 50 )


corp_samples <- tibble(
  corp = rep( colnames(death_mat), each = n_chains * (n_samples - n_burnin) ),
  samples = as.numeric( model_fit$BUGSoutput$sims.list$lambda )
)

index <- 15
lam <- model_fit$BUGSoutput$sims.list$lambda[index,]

sapply( lam, function(l) rpois(n_obs, lambda = l))


corp_samples %>%
  mutate( corp = factor(corp, levels = c("GC", paste0("C", 1:11), paste0("C", 14:15)))) %>%
  ggplot( aes(x = samples, y = after_stat(density))) +
  geom_histogram( binwidth = .05, fill = "firebrick", col = "white") +
  facet_wrap(. ~ corp, scales = "fixed", nrow = 2) +
  theme_bw() +
  labs( y = "Density", x = "Rate", subtitle = "Posterior rate distirbution for each corp.")





a <- model_fit$BUGSoutput$sims.list$alpha[5]
b <- model_fit$BUGSoutput$sims.list$beta[5]

ll <- seq(0, 2, by = .01)
plot( ll, dgamma(ll, 10.506, 14.949), type = "l")

lines(ll, dgamma(ll, a, b), col = "lightgrey")

hist( model_fit$BUGSoutput$sims.list$alpha / model_fit$BUGSoutput$sims.list$beta, nclass = 25, freq = F)

lines(ll, dnorm(ll, 10.506 / 14.949, sqrt(10.506 / (14.949^2) / sqrt(48) )) )


