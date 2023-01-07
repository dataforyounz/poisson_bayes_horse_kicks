rm( list = ls() )

library( tidyverse )
library( gridExtra )
library( ggpubr )
library( kableExtra )

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


data_corp <- data_long %>%
             group_by( Corp ) %>%
             count( Deaths ) %>%
             left_join( mle_corps, by = "Corp") %>%
             mutate( freq = n / sum(n), 
                     exp = dpois(0:max(Deaths), mean), 
                     exp_n = exp * sum(n), 
                     n_obs = sum(n),
                     n_deaths = mean * n_obs ) %>%
             ungroup()

stat_corp <- data_corp %>%
             select( Corp, n_deaths, n_obs, mean ) %>%
             distinct( .keep_all = TRUE ) %>%
             rename( Deaths = n_deaths, 
                     "Total Obs" = n_obs, 
                     Rate = mean )

plot_mle_corp <- data_corp %>%
                 ggplot( aes(x = Deaths, y = freq)) +
                 geom_point( aes(col = "Data") ) +
                 geom_line( aes( y = exp)) +
                 geom_point( aes( y = exp, col = "Fit") ) +
                 facet_wrap(. ~ Corp, scales = "fixed", nrow = 2) +
                 labs( y = "Density", col = NULL, subtitle = "Poisson approximation of horse kick deaths by Corp") +
                 theme_bw() +
                 theme( legend.title = element_blank(), 
                        legend.position = "bottom") +
                 scale_color_manual( values = c("firebrick", "black"))

ggsave( plot_mle_corp, file = "figs/plot_mle_corp.png", units = "cm", width = 20, height = 10)

# Table
stat_corp %>% 
  select(1:4) %>% 
  kbl( table.attr = "style='width:50%;'",
       caption = "Observed deaths across corps and goodness of fit with Poisson distirbution.") %>% 
  kable_classic( full_width = T ) %>%
  column_spec( 4, color = "white", 
               background = spec_color(stat_corp$Rate[1:14], end = 0.5)) %>%
  footnote( general = "GOF = Chi Square Goodness of Fit Statistic.", footnote_as_chunk = T ) %>%
  save_kable( "figs/table_mle_corp.png", zoom = 10 )


