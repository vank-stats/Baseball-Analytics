## Choose A Player and their comparable positions (C, 1B, 2B, 3B, SS, OF)

firstName <- "Kris"
lastName<- "Bryant"
positions <- c("2B", "3B")


# Load libraries

library(Lahman)
library(dplyr)
library(ggplot2)


# pull player id

batter_id <- People %>% 
  filter(nameFirst == firstName, nameLast == lastName) %>% 
  pull(playerID)

# get their batting stats and add in age

batter <- battingStats() %>% 
  filter(playerID == batter_id) %>%
  inner_join(People, by = "playerID") %>% 
  mutate(age = yearID - birthYear)

# Summarize player's stats

batter_summary <- batter %>% 
  summarize(debut = min(age), current = max(age), 
            TotalG = sum(G), 
            CareerAvg = round(sum(H) / sum(AB), 3),
            CareerOBP = round(sum(H + BB + HBP) / sum(AB + BB + HBP + SF), 3),
            CareerSLG = round(sum(H + X2B + 2*X3B + 3*HR) / sum(AB), 3),
            highOPS = max(OPS)) %>%
  mutate(CareerISO = CareerSLG - CareerAvg,
         CareerOPS = CareerOBP + CareerSLG)

batter_summary


# Graph chosen player's OPS per season to date

g <- batter %>% 
  ggplot(aes(x = age, y = OPS)) +
  geom_point(aes(size = AB)) +
  geom_smooth() +
  geom_vline(xintercept = batter_summary$current, color = "red", size = 1) +
  labs(title = paste(batter$nameFirst, batter$nameLast)) +
  theme_bw()

g


# Find players who played comparable position(s) as chosen player (since 1991)

fielders <- Fielding %>%
  filter(POS %in% positions, yearID > 1990)%>%
  group_by(playerID, yearID)%>%
  summarize(games = sum(G)) %>%
  filter(games > 100) %>%
  group_by(playerID) %>%
  summarize(seasons = n()) %>%
  filter(seasons > 2) %>%
  ungroup()


# Add age to batting data

batting <- battingStats() %>%
  inner_join(People, by = "playerID") %>% 
  mutate(age = yearID - birthYear)


# Find players who have similar debut age, OPS up to chosen player's age,
#   and OPS in best season. Then rank players by similarity of OBP and ISO
#   up to chosen player's age.

Comps <- batting %>% 
  group_by(playerID, nameFirst, nameLast) %>% 
  mutate(debut = min(age), reachedage = max(age) > batter_summary$current) %>%
  filter(debut < batter_summary$debut + 2, yearID > 1990, reachedage, 
         age < batter_summary$current + 2) %>%
  summarize(CareerOBP = round(sum(H + BB + HBP) / sum(AB + BB + HBP + SF), 3),
            CareerAvg = round(sum(H) / sum(AB), 3),
            CareerSLG = round(sum(H + X2B + 2*X3B + 3*HR) / sum(AB), 3),
            TotalAB = sum(AB), highOPS = max(OPS)) %>%
  mutate(CareerISO = CareerSLG - CareerAvg,
         CareerOPS = CareerOBP + CareerSLG,
         OBPDiff = round(CareerOBP - batter_summary$CareerOBP, 3),
         ISODiff = round(CareerISO - batter_summary$CareerISO, 3)) %>%
  filter(TotalAB > 1000, CareerOPS > batter_summary$CareerOPS - 0.050, 
         CareerOPS < batter_summary$CareerOPS + 0.050, 
         highOPS > batter_summary$highOPS - 0.050,
         highOPS < batter_summary$highOPS + 0.050) %>%
  ungroup() %>%
  inner_join(fielders, by = "playerID") %>%
  mutate(CompScore = abs(OBPDiff) + abs(ISODiff)) %>%
  arrange(CompScore)

# View top comps

head(Comps)
nrow(Comps)

# Find playerIDs for top 4 Comp

example_id <- Comps %>% 
  slice_head(n = 4) %>% 
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
