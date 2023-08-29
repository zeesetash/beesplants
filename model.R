install.packages('tidyverse')
library(tidyverse)


bees <- read_csv("C:/Users/Z/Downloads/plants_and_bees.csv")

#Only include samples that had plants
bees_with_plant <- bees %>%
  filter(plant_species != "None")
View(bees_with_plant)

# How popular is each plant species?

plant_summary <- bees_with_plant %>%
  group_by(nonnative_bee) %>%
  count(plant_species, wt = bees_num, sort = TRUE)

plant_summary_native <- bees_with_plant %>%
  filter(nonnative_bee == 0) %>%
  count(plant_species, wt = bees_num, sort = TRUE)


ggplot(plant_summary_native, 
       aes(x = reorder(plant_species, -n), 
           y = n,
           fill = plant_species)) +
  geom_bar(stat = 'identity',
           show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90))  +
  labs(x = "Plant Species", 
       y = "Total Bees Counted", 
       title = "Native Bee Preferences") 

native_season <- bees_with_plant  %>%
  group_by(season) %>%
  filter(nonnative_bee == 0) %>%
  count(plant_species, wt = bees_num, sort = TRUE)

ggplot(native_season_zoom, 
       aes(x = reorder(plant_species, -n), 
           y = n,
           fill = plant_species)) +
  geom_bar(stat = 'identity',
  ) +
  facet_wrap(~season) +
  theme(axis.text.x = element_text(angle = 90))  +
  labs(x = "Plant Species", 
       y = "Total Bees Counted", 
       title = "Native Bee Preferences") 

pop_species <- c("Leucanthemum vulgare", "Rudbeckia hirta", "Cichorium intybus", "Daucus carota", "Chamaecrista fasciculata", "Asclepias tuberosa")

native_season_zoom <- native_season %>%
  filter(plant_species %in% pop_species)

View(native_season_zoom)

diverse_sample <- bees_with_plant %>%
  group_by(sample_id) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

sample_dist <- bees %>%
  filter( sample_id ==  17473) %>% 
  group_by(plant_species)


ggplot(sample_dist, aes(x = plant_species, y = bees_num, fill = reorder(bee_species, -bees_num))) +
  geom_col() +
  labs(x = "Plant Species", 
       y = "Total Bees Counted", 
       title = "Distribution of a Sample",
       fill = "Bee Species",
       subtitle = "Sample ID 17473, 07/18/2017") +
  theme(axis.text.x = element_text(angle = 90))

native_plant_summary_wt <- bees_with_plant %>%
  group_by(native_or_non) %>%
  count(wt = bees_num, sort = TRUE)

native_plant_summary <- bees_with_plant %>%
  group_by(native_or_non) %>%
  summarize(n = n())

cols <- c("non-native" = "palevioletred2", "native" = "lightskyblue")   
ggplot(plant_summary_native, 
       aes(x = reorder(plant_species, -n), 
           y = n,
           fill = native_or_non)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))  +
  labs(x = "Plant Species", 
       y = "Total Bees Counted", 
       title = "Native Bee Preferences",
       fill = "Plant Origin") + 
  scale_fill_manual(values = cols)

ggplot(plant_summary_non, 
       aes(x = reorder(plant_species, -n), 
           y = n,
           fill = native_or_non)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))  +
  labs(x = "Plant Species", 
       y = "Total Bees Counted", 
       title = "Non-native Bee Preferences",
       fill = "Plant Origin") + 
  scale_fill_manual(values = cols)

ggplot(native_season_zoom, 
       aes(x = reorder(plant_species, -n), 
           y = n,
           fill = plant_species)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~season) +
  theme(axis.text.x = element_text(angle = 90))  +
  labs(x = "Plant Species", 
       y = "Total Bees Counted", 
       title = "Native Bee Preferences",
       fill = "Plant Species") 