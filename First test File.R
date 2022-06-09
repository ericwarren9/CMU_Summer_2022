# Practice File

# Here is a practice code example to upload to Github ---------------------

library(tidyverse)
nba_stats <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/intro_r/nba_2022_player_stats.csv")


# Bar Chart of Positional Data in NBA -------------------------------------

#Create the bar plot of position:
nba_stats %>%
  ggplot(aes(x = position)) +
  geom_bar(fill = "darkblue") +
  labs(title = "Number of NBA players by position",
       x = "Position", y = "Number of players",
       caption = "Source: Basketball-Reference.com")
