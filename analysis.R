rm( list = ls() )

library( tidyverse )
library( gridExtra )
library( ggpubr )

## -----------------------------------------------------------------------------
## Frequentist analysis of horse kick data
## -----------------------------------------------------------------------------

## Data load and format --------------------------------------------------------

data_file <- list.files( "data" )
data_long <- read_csv( paste0("data/", data_file), col_types = cols() ) %>%
             pivot_longer( -Year, names_to = "Corp", values_to = "Deaths" ) %>%
             mutate( Corp = factor(Corp, levels = c("GC", paste0("C", 1:11), paste0("C", 14:15))))

## Maximum Likelihood Estimates ------------------------------------------------

mle_pooled <- data_long %>% pull( Deaths ) %>% mean()
mle_corps  <- data_long %>% group_by(Corp) %>% summarise( mean = mean(Deaths) )

## Fit to pooled data ----------------------------------------------------------

data_pooled <- data_long %>%
                count( Deaths ) %>%
                mutate( freq = n / sum(n), 
                        exp = dpois(0:4, mle_pooled) )

plot_mle_pooled <- data_pooled %>%
                   ggplot( aes(x = Deaths, y = freq)) +
                   geom_point( aes(col = "Data") ) +
                   geom_line( aes( y = exp)) +
                   geom_point( aes( y = exp, col = "Fit") ) +
                   labs( y = "Density", col = NULL, subtitle = "Poisson approximation of horse kick deaths") +
                   theme_bw() +
                   theme( legend.title = element_blank(), 
                          legend.position = "bottom") +
                   scale_color_manual( values = c("firebrick", "black"))

ggsave( plot_mle_pooled, file = "figs/plot_mle_pooled.png", units = "cm", width = 10, height = 7)

## Fit to each Corp ------------------------------------------------------------




