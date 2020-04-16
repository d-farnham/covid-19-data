rm(list = ls())
package.list <- list("ggplot2", "dplyr", "reshape2", "lubridate")
source('R/load_packages.R') # load packages


# read in the data
US_deaths = read.csv("~/Google Drive/covid-19-data/us-states.csv") %>%
                    dplyr::mutate(date = as.Date(date)) %>%
                    dplyr::group_by(date) %>%
                    dplyr::summarise(US_cases = sum(cases),
                                     US_deaths = sum(deaths)) %>%
                    dplyr::mutate(US_deaths_new = US_deaths - lag(US_deaths, 1))


# start date with 10 total deaths
start_date = US_deaths$date[min(which(US_deaths$US_deaths > 10))]

# let's only look after the date when 10 total deaths had occured
US_deaths = US_deaths %>% dplyr::filter(date > start_date)


# set span such that we fit on the nearest 14 points
span = 14/nrow(US_deaths)

pdf(file = 'figs/US_deaths_and_loess_fit.pdf',
    height = 5,
    width = 8)
print(
ggplot(US_deaths,
       aes(date, US_deaths)) +
  geom_line(size = 1.0) +
  geom_point() +
  geom_smooth(span = span,
              col = 'red',
              linetype = 'dashed') +
  # geom_line(aes(date, US_death_estimated), 
  #           col = 'red', 
  #           linetype = 'dashed',
  #           size = 1.25) +
  labs(x = 'Date',
       y = "Total reported deaths in the US from Covid-19") +
  theme_bw()
)
dev.off()

pdf(file = 'figs/US_deaths_new_and_loess_fit.pdf',
    height = 5,
    width = 8)
print(
  ggplot(US_deaths,
         aes(date, US_deaths_new)) +
    geom_line(size = 1.0) +
    geom_point() +
    geom_smooth(span = span,
                col = 'red',
                linetype = 'dashed') +
    # geom_line(aes(date, US_death_estimated), 
    #           col = 'red', 
    #           linetype = 'dashed',
    #           size = 1.25) +
    labs(x = 'Date',
         y = "New reported deaths in the US from Covid-19") +
    theme_bw()
)
dev.off()

