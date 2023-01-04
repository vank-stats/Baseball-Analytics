# load packages --- Install these first if you haven't yet
library(Lahman)
library(ggplot2)
library(dplyr)

# look at data frame variables
names(People)
names(battingStats())


# example players ---------------------------------------------------------

## Carlos Santana
# pull player id
santana_id <- People %>% 
  filter(nameFirst == "Carlos", nameLast == "Santana") %>% 
  pull(playerID)

# get their batting stats
santana <- battingStats() %>% filter(playerID == santana_id)

# add in the age variable
santana_update <- santana %>% 
  inner_join(People, by = "playerID") %>% 
  mutate(age = yearID - birthYear)

santana_update %>% 
  ggplot(aes(x = age, y = OPS)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 33, color = "red", size = 1) +
  labs(title = paste(santana_update$nameFirst, santana_update$nameLast)) +
  theme_bw()

## Nelson Cruz
# pull player id
example_id <- People %>% 
  filter(nameFirst == "Nelson", nameLast == "Cruz", birthYear == "1980") %>% 
  pull(playerID)

# get their batting stats
example <- battingStats() %>% filter(playerID == example_id)

# add in the age variable
example_update <- example %>% 
  inner_join(People, by = "playerID") %>% 
  mutate(age = yearID - birthYear)

example_update %>% 
  ggplot(aes(x = age, y = OPS)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 33, color = "red", size = 1) +
  labs(title = paste(example_update$nameFirst, example_update$nameLast)) +
  theme_bw()

## Ian Kinsler
# pull player id
example_id <- People %>% 
  filter(nameFirst == "Ian", nameLast == "Kinsler") %>% 
  pull(playerID)

# get their batting stats
example <- battingStats() %>% filter(playerID == example_id)

# add in the age variable
example_update <- example %>% 
  inner_join(People, by = "playerID") %>% 
  mutate(age = yearID - birthYear)

example_update %>% 
  ggplot(aes(x = age, y = OPS)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 33, color = "red", size = 1) +
  labs(title = paste(example_update$nameFirst, example_update$nameLast)) +
  theme_bw()

## Edgar Martinez
# pull player id
example_id <- People %>% 
  filter(nameFirst == "Edgar", nameLast == "Martinez") %>% 
  pull(playerID)

# get their batting stats
example <- battingStats() %>% filter(playerID == example_id)

# add in the age variable
example_update <- example %>% 
  inner_join(People, by = "playerID") %>% 
  mutate(age = yearID - birthYear)

example_update %>% 
  ggplot(aes(x = age, y = OPS)) +
  geom_point(aes(size = G)) +
  geom_smooth() +
  geom_vline(xintercept = 33, color = "red", size = 1) +
  labs(title = paste(example_update$nameFirst, example_update$nameLast)) +
  theme_bw()

