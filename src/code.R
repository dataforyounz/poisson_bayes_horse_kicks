
rm( list = ls() )

library( tidyverse )

data_file <- list.files( pattern = ".csv" )
data_long <- read_csv( data_file, col_types = cols() ) %>%
             pivot_longer( -Year, names_to = "Corp", values_to = "Deaths" )

data_summary <- data_long %>%
                summarise( mean = mean(Deaths),
                           variance = var(Deaths) ,
                           st_dev = sqrt(variance), 
                           fano = variance / mean,
                           n_obs = n() )

data_count <- data_long %>%
              group_by( Deaths ) %>%
              count() %>%
              ungroup() %>%
              mutate( p_obs = n / sum(n), 
                      p_exp = dpois( Deaths, sum( p_obs * Deaths) ),
                      n_exp = sum(n) * p_exp )

plot <- ggplot( data_count, aes( x = Deaths, y = n) ) +
        geom_point() +
        geom_line( aes( y = n_exp), col = "firebrick") +
        labs( subtitle = "Observed number of deaths per year", 
              y = "Frequency", 
              x = "Number of Deaths per Year") +
        theme_bw() +
        theme( panel.grid.minor.x = element_blank(), 
               panel.grid.minor.y = element_blank() )
        


