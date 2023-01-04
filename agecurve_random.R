# load packages --- Install these first if you haven't yet

library(Lahman)
library(ggplot2)
library(dplyr)

# look at data frame variables
names(People)
names(battingStats())


# example player ----------------------------------------------------------

# enter the first name
first <- "Ian"

# enter the last name
last <- "Kinsler"

# pull player id
example_id <- People %>% 
  filter(nameFirst == first, nameLast == last) %>% 
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



# generate random players ----------------------------------------------------------

# Find all players who debuted after 1960 and played more than 5 seasons

ids <- Batting %>% 
  group_by(playerID) %>% 
  mutate(seasons = n(), debut = min(yearID)) %>% 
  filter(seasons > 5, debut > 1960) %>% 
  pull(playerID) %>% 
  unique()


## Everything under this can be run all at once to generate graphs for random
#    players

# pull random player id
example_id <- People %>% 
  filter(playerID == sample(ids, 1)) %>% 
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
  geom_smooth(span = 1) +
  geom_vline(xintercept = 33, color = "red", size = 1) +
  labs(title = paste(example_update$nameFirst, example_update$nameLast),
       subtitle = paste("Career: ", min(example_update$yearID), " - ",
                        max(example_update$yearID))) +
  theme_bw()

