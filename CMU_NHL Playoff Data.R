# Purpose: To look at hockey data and answer questions we have about the data.


# Read in the hockey data -------------------------------------------------

library(tidyverse)
nhl_shots <- read_csv("nhl_playoffs_shots_2022.csv")


# Questions we want to answer ---------------------------------------------
# 1. Left handed shooters vs right handed shooters heat map
# 2. Is there a relationship between the distance and shot type players will exhibit
# 3. Looking at home and away shot data
# 4. See what players are generating the most amount of shot opportunities (cluster)


# See what data is missing ------------------------------------------------

na_nhl_shots <- nhl_shots[!complete.cases(nhl_shots), ] # If data we are investigating uses goalie stats then we need to omit the na's


# Table of the different types of shots... used later to compare ----------

table(nhl_shots$shotType)


# Start looking at the heat map of the shooters positioning ---------------

# Do not use these lines of code for final presentation... just testing some things out
nhl_shots$shooterLeftRight <- factor(nhl_shots$shooterLeftRight, labels = c("Left Handed Shooter", "Right Handed Shooter"))
nhl_shots %>%
  mutate(absArenaAdjustedXCord = -1 *abs(arenaAdjustedXCord)) %>%
  ggplot(aes(x = absArenaAdjustedXCord, y = arenaAdjustedYCord)) + 
  xlim(-95, -15) +
  ylim(-40, 40) +
  # Crease area: rectangle 
  ggplot2::geom_tile(aes(x = -86.75, y = 0, width = 4.5, height = 8)) +
  # Crease area: circular arc
  ggforce::geom_arc_bar(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r0 = 4, r = 6), size = 1 / 12) +
  # net
  ggplot2::geom_tile(aes(x = -90.67, y = 0, width = 3.33, height = 6)) + 
  stat_density2d(aes(fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "white",
                      high = "darkred") +
  facet_wrap(~ shooterLeftRight, ncol = 2) +
  labs(title = "Differences in Position on the Ice Both Handed Players Shoot",
       x = "X coordinate in the offensive zone",
       y = "Y coordinate in the offensive zone",
       caption = "Data Courtesy of MoneyPuck.com") +
  theme_bw() + theme(legend.position = "bottom") +
  coord_fixed() +
  coord_flip()

# Try using the rink in it

# Try to make two seperate plots
par(1, 2)
library(sportyR)
nhl_shots_left <- nhl_shots %>%
  filter(shooterLeftRight == "L") %>% 
  filter(arenaAdjustedYCord < 41, arenaAdjustedYCord > -41) %>% 
  mutate(absXCoord = -abs(arenaAdjustedXCord))
geom_hockey(league = "NHL", full_surf = F) + 
  stat_density2d(data = nhl_shots_left,
                 adjust = 0.5,
                 alpha = 0.5,
                 h = 10,
                 aes(x = absXCoord,
                     y = arenaAdjustedYCord,
                     fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "lightyellow",
                      high = "darkred") +
  labs(title = "Position on the Ice Left Handed Players Shoot",
       caption = "Data Courtesy of MoneyPuck.com") + 
  theme(legend.position = "bottom")
nhl_shots_right <- nhl_shots %>%
  filter(shooterLeftRight == "R")  %>% 
  filter(arenaAdjustedYCord < 41, arenaAdjustedYCord > -41) %>% 
  mutate(absXCoord = -abs(arenaAdjustedXCord))
geom_hockey(league = "NHL", full_surf = F) + 
  stat_density2d(data = nhl_shots_right,
                 adjust = 0.5,
                 alpha = 0.5,
                 h = 10,
                 aes(x = absXCoord,
                     y = arenaAdjustedYCord,
                     fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "lightyellow",
                      high = "darkred") +
  labs(title = "Position on the Ice Right Handed Players Shoot",
       caption = "Data Courtesy of MoneyPuck.com") + 
  theme(legend.position = "bottom")






# Side by side plot
library(sportyR)
nhl_shots$shooterLeftRight <- factor(nhl_shots$shooterLeftRight, labels = c("Left Handed Shooter", "Right Handed Shooter"))
nhl_shots_filter <- nhl_shots %>% filter(arenaAdjustedYCord < 41, arenaAdjustedYCord > -41) %>% mutate(absXCoord = -abs(arenaAdjustedXCord))
geom_hockey(league = "NHL", full_surf = F) + 
  stat_density2d(data = nhl_shots_filter,
                 adjust = 0.5,
                 alpha = 0.5,
                 h = 10,
                 aes(x = absXCoord,
                     y = arenaAdjustedYCord,
                     fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "lightyellow",
                      high = "darkred") +
  facet_wrap(~ shooterLeftRight, ncol = 2) +
  labs(title = "Differences in Position on the Ice Both Handed Players Shoot",
       caption = "Data Courtesy of MoneyPuck.com") + 
  theme(legend.position = "bottom")


# Shot Distance versus type of shot ---------------------------------------

nhl_shots %>%
  ggplot(aes(x = shotDistance)) + 
  geom_histogram(bins = 15, color = "red") +
  labs(title = "How Type of Shot Varies by Distance",
       x = "Distance Away from Goal (in feet)",
       y = "Frequency of Type of Shot",
       caption = "Data Courtesy of MoneyPuck.com") +
  theme_bw() +
  facet_wrap(~ shotType, ncol = 3)


# Examine Home versus Away Shot Data -----------------------------------------

# Try to group the away and home shots by team

# Away Shot Data
nhl_shots_away_teams <- nhl_shots %>% 
  group_by(awayTeamCode) %>% 
  filter(isHomeTeam == 0) %>% 
  summarize(away_shots = sum(shotWasOnGoal),
            away_goals = sum(event == "GOAL"),
            away_games = n_distinct(game_id)) %>%
  mutate(away_shots_per_game = round(away_shots / away_games, 2), 
         away_goals_per_game = round(away_goals / away_games, 2),
         away_shooting_percentage = round(away_goals / away_shots, 4)) %>%
  rename(teamName = awayTeamCode)

# Home Shot Data
nhl_shots_home_teams <- nhl_shots %>% 
  group_by(homeTeamCode) %>%
  filter(isHomeTeam == 1) %>% 
  summarize(home_shots = sum(shotWasOnGoal),
            home_goals = sum(event == "GOAL"),
            home_games = n_distinct(game_id)) %>%
  mutate(home_shots_per_game = round(home_shots / home_games, 2),
         home_goals_per_game = round(home_goals / home_games, 2),
         home_shooting_percentage = round(home_goals / home_shots, 4)) %>%
  rename(teamName = homeTeamCode)

# Merge the Data Sets together into one
nhl_shots_on_goal <- merge(nhl_shots_away_teams,
                           nhl_shots_home_teams,
                           by = "teamName")


# Make cluster models for different shot and scoring data -----------------

# Make cluster for home and away shots on goal data per game
library(flexclust)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_shots_on_goal,
                     away_shots_per_game, 
                     home_shots_per_game), 
       k = 4,
       control = list(initcent = "kmeanspp"))
nhl_shots_on_goal %>%
  mutate(nhl_team_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = away_shots_per_game, 
             y = home_shots_per_game,
             color = nhl_team_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

# Here we can see what teams are in each cluster
nhl_shots_on_goal$cluster_number_total_shots <- init_kmeanspp@cluster

# Here we can look at home and away shooting percentages and if there is a correlation between clusters
library(flexclust)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_shots_on_goal,
                     away_shooting_percentage, 
                     home_shooting_percentage), 
       k = 4,
       control = list(initcent = "kmeanspp"))
nhl_shots_on_goal %>%
  mutate(nhl_team_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = away_shooting_percentage, 
             y = home_shooting_percentage,
             color = nhl_team_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

# Look at which teams are in each cluster
nhl_shots_on_goal$cluster_number_shooting_percentage <- init_kmeanspp@cluster

# Here we can look at who is in cluster 2 (for example)
nhl_shots_on_goal[which(nhl_shots_on_goal$cluster_number_shooting_percentage == 2), ]

# Want to see what clusters the two finalist teams are in for both data sets to see if there is a match or trend
nhl_shots_on_goal %>%
  filter(teamName %in% c("COL", "TBL")) %>%
  select(teamName, cluster_number_total_shots, cluster_number_shooting_percentage)

# How about the two teams that lost in the conference finals and two Stanley Cup Finalists? Maybe more of a pattern?
nhl_shots_on_goal %>%
  filter(teamName %in% c("COL", "TBL", "NYR", "EDM")) %>%
  select(teamName, cluster_number_total_shots, cluster_number_shooting_percentage)