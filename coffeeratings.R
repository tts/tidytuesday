library(tidytuesdayR)
library(ggplot2)
library(ggthemes)
library(tidyverse)

tt_data <- tt_load("2020-07-07")

coffees <- tt_data$coffee_ratings

species_altitude_points <- coffees %>% 
  select(species, altitude_mean_meters, total_cup_points) %>% 
  filter(!is.na(altitude_mean_meters)) %>% 
  filter(altitude_mean_meters < 10000) # over this this probably bad data

cof_cut <- species_altitude_points %>%
  mutate(interval = cut_interval(altitude_mean_meters, length = 500, dig.lab = 50)) %>%
  group_by(interval, species) %>% 
  summarize(medianValueInInterval = median(total_cup_points)) %>% 
  mutate(medianValueInInterval = ifelse(species == 'Arabica', -1 * medianValueInInterval, medianValueInInterval))

brks <- seq(-90, 90, 10)
lbls <- paste0(as.character(c(seq(90, 0, -10), seq(10, 90, 10))))

ggplot(cof_cut, aes(x = interval, y = medianValueInInterval, fill = species)) + 
  geom_bar(stat = "identity", width = .6) + 
  scale_y_continuous(breaks = brks, labels = lbls) +
  coord_flip() +  
  labs(y = "Median points",
       x = "Altitude interval",
       title = "Altitude as such does not make much difference",
       subtitle = "Still, arabica grown in 2000-2500 m like e.g. in Ethiopia is found tasty (I agree)") +
  theme_tufte() +  
  theme(plot.title = element_text(hjust = .2), 
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  scale_fill_brewer(palette = "Dark2")
