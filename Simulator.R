### Load packages
library(Lahman)
library(dplyr)

### Load Batting dataset and filter to 2018
Batting <- Batting
bat18 <- filter(Batting, yearID == 2018, AB > 50) %>%
  mutate(X1B = H - X2B - X3B - HR,
         BBetc = BB + HBP,
         PA = AB + BB + HBP + SH + SF,
         p1B = X1B / PA, p2B = p1B + (X2B / PA), p3B = p2B + (X3B / PA), 
         pHR = p3B + (HR / PA), pBB = pHR + (BBetc / PA))

### Define function to simulate a plate appearance for a specified batter
### Still need to add pitcher functionality
pa <- function(batterID = "troutmi01", pitcherID = NA) {
  x <- runif(1)
  player <- filter(bat18, playerID == batterID)
  outcome <- case_when(x < player$p1B ~ "Single",
                       x < player$p2B ~ "Double",
                       x < player$p3B ~ "Triple",
                       x < player$pHR ~ "Home Run",
                       x < player$pBB ~ "Walk",
                       TRUE ~ "Out")
  outcome
}

### Test the function by simulating 650 PAs of Mike Trout
pas <- rep(NA, 650)
hits <- rep(NA, 650)
onbase <- rep(NA, 650)
for(i in 1:650) {
  pas[i] <- pa("fowlede01")
}

### Calculate the number of hits and times on base
hits <- pas %in% c("Single", "Double", "Triple", "Home Run")
onbase <- hits | (pas == "Walk")

### Show "season" stats and calculate BA and OBP
table(pas)
sum(hits)/(650-sum(pas == "Walk"))
mean(onbase)

