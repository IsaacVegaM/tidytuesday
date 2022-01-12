# Load libraries
library(tidyverse)
library(maps)
library(viridis)
library(ggthemes)

# Obtaining the data
tuesdata <- tidytuesdayR::tt_load("2022-01-11")
stressor <- tuesdata$stressor

# Varroa mites
mites <- stressor %>% 
  filter(stressor == "Varroa mites") %>% 
  group_by(year, state) %>% 
  summarise(stress_pct = mean(stress_pct, na.rm = T))

# Map
us_states <- map_data("state")
mites$state <- tolower((mites$state))
us_states_mites <- left_join(us_states, mites, by = c("region" = "state")) %>% 
  filter(! is.na(year)) %>% 
  filter(! year == 2021)


map <- ggplot(data = us_states_mites,
               mapping = aes(x = long, y = lat,
                             group = group, fill = stress_pct))

map + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_viridis_c(option = "plasma") +
  theme_map() +
  facet_wrap(~ year, ncol = 3, strip.position = "bottom") +
  theme(legend.position = c("bottom"),
        legend.background = element_blank(),
        legend.key.height = unit(0.25, "cm"),
        strip.background = element_blank()) +
  labs(title = "Bees colonies stressed by varroa mites, 2015 - 2020", 
       fill = "% of stressed bees colonies",
       caption = "Data source: USDA. #TidyTuesday by @IsaacVM_")

ggsave("mites_stress.png", width = 8, height = 4.5)
