## Choose a Player

firstName <- "Brandon"
lastName<- "Belt"


# Load libraries

library(Lahman)
library(ggplot2)
library(dplyr)


# pull player id

batter_id <- People %>% 
  filter(nameFirst == firstName, nameLast == lastName) %>% 
  pull(playerID)


# Get chosen player's batting stats and calculate age for each season

batter <- battingStats() %>% 
  filter(playerID == batter_id) %>% 
  inner_join(People, by = "playerID") %>% 
  mutate(age = yearID - birthYear)


# Create player summary for comps

batter_summary <- batter %>% 
  summarize(debut = min(age), current = max(age), TotalG = sum(G), 
            avgOPS = mean(OPS), highOPS = max(OPS))


# Graph player's career to date

batter %>% 
  ggplot(aes(x = age, y = OPS)) +
  geom_point(aes(size = AB)) +
  geom_smooth() +
  geom_vline(xintercept = batter_summary$current, color = "red", size = 1) +
  labs(title = paste(batter$nameFirst, batter$nameLast)) +
  theme_bw()



# Find comps for chosen player

Comps <- battingStats() %>%
  inner_join(People, by = "playerID") %>% 
  mutate(age = yearID - birthYear) %>%
  group_by(playerID, nameFirst, nameLast) %>% 
  mutate(debut = min(age), reached30 = max(age) > batter_summary$current) %>%
  filter(debut < batter_summary$debut+1, yearID > 1990, reached30, 
         age < batter_summary$current+1) %>%
  summarize(TotalAB = sum(AB), avgOPS = round(mean(OPS),3), 
            highOPS = max(OPS)) %>%
  filter(TotalAB > 3000, avgOPS > batter_summary$avgOPS - 0.050, 
         avgOPS < batter_summary$avgOPS + 0.050, 
         highOPS > batter_summary$highOPS - 0.050,
         highOPS < batter_summary$highOPS + 0.050)


# Look at first few comps and determine number of players

head(Comps)
nrow(Comps)

# Store player ids for the comps

comp_ids <- Comps$playerID


# Randomly choose ids

example_id <- People %>% 
  filter(playerID %in% sample(comp_ids, 4)) %>% 
  pull(playerID)


# get their batting stats, calculate age, and add name

example <- battingStats() %>% 
  filter(playerID %in% example_id) %>% 
  inner_join(People, by = "playerID") %>% 
  mutate(age = yearID - birthYear, name = paste(nameFirst, nameLast))


# Graph comps for the chosen players

example %>% 
  ggplot(aes(x = age, y = OPS)) +
  geom_point(aes(size = G)) +
  geom_smooth(span = 1) +
  geom_vline(xintercept = batter_summary$current, color = "red", size = 1) +
  labs(title = paste("Comps for", firstName, lastName)) +
  theme_bw() +
  facet_wrap(vars(name))

  