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

# Initial Model 
nhl_shots %>%
  ggplot(aes(x = shotDistance)) + 
  geom_histogram(bins = 15, color = "red") +
  labs(title = "How Type of Shot Varies by Distance",
       x = "Distance Away from Goal (in feet)",
       y = "Frequency of Type of Shot",
       caption = "Data Courtesy of MoneyPuck.com") +
  theme_bw() +
  facet_wrap(~ shotType, ncol = 3)

# Updated Model
nhl_shots %>%
  ggplot(aes(x = shotDistance)) +
  geom_histogram(bins = 15,
                 color = "cornflowerblue",
                 fill = "cornflowerblue",
                 alpha = .22,
                 size = .65) +
  geom_vline(xintercept = mean(nhl_shots$shotDistance),
             linetype = "88",
             color = "darkred") +
  labs(title = "How Type of Shot Varies by Distance",
       x = "Distance Away From Goal (in feet)",
       y = "Frequency of Type of Shot",
       caption = "Data Courtesy of Moneypuck.com") +
  theme_bw() +
  facet_wrap( ~ shotType, ncol = 1)


# Examine Home versus Away Shot Data -----------------------------------------

# Try to group the away and home shots by team

# Away Shot Data
nhl_shots_away_teams <- nhl_shots %>% 
  group_by(awayTeamCode) %>% 
  filter(isHomeTeam == 0) %>% 
  summarize(away_shots = sum(event %in% c("GOAL", "SHOT", "MISS")),
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
  summarize(home_shots = sum(event %in% c("GOAL", "SHOT", "MISS")),
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


# Cluster based on player data --------------------------------------------

# Load the away player shooting data
nhl_shots_away_players <- nhl_shots %>% 
  group_by(shooterName) %>% 
  filter(isHomeTeam == 0) %>% 
  summarize(away_shots = sum(event %in% c("GOAL", "SHOT", "MISS")),
            away_goals = sum(event == "GOAL"),
            away_games = n_distinct(game_id)) %>%
  mutate(away_shots_per_game = round(away_shots / away_games, 2), 
         away_goals_per_game = round(away_goals / away_games, 2),
         away_shooting_percentage = round(away_goals / away_shots, 4))

# Load the home player shooting data
nhl_shots_home_players <- nhl_shots %>% 
  group_by(shooterName) %>%
  filter(isHomeTeam == 1) %>% 
  summarize(home_shots = sum(event %in% c("GOAL", "SHOT", "MISS")),
            home_goals = sum(event == "GOAL"),
            home_games = n_distinct(game_id)) %>%
  mutate(home_shots_per_game = round(home_shots / home_games, 2),
         home_goals_per_game = round(home_goals / home_games, 2),
         home_shooting_percentage = round(home_goals / home_shots, 4))

# Merge the Data Sets together into one
nhl_player_shooting <- merge(nhl_shots_away_players,
                           nhl_shots_home_players,
                           by = "shooterName")

# Make cluster for home and away shots on goal data per game
library(flexclust)
set.seed(12)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_player_shooting,
                     away_shots_per_game, 
                     home_shots_per_game), 
       k = 6,
       control = list(initcent = "kmeanspp"))
nhl_player_shooting %>%
  mutate(nhl_player_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = away_shots_per_game, 
             y = home_shots_per_game,
             color = nhl_player_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_player_shooting$away_shots_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_player_shooting$home_shots_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom")

# Here we can see what teams are in each cluster
nhl_player_shooting$cluster_number_total_shots <- init_kmeanspp@cluster

# Here we can look at the shooting percentages at each spot
set.seed(12)
nhl_player_shooting_filtered <- nhl_player_shooting %>%
  filter(away_shots >= 3,
         home_shots >= 3)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_player_shooting_filtered,
                     away_shooting_percentage, 
                     home_shooting_percentage), 
       k = 4,
       control = list(initcent = "kmeanspp"))
nhl_player_shooting_filtered %>%
  mutate(nhl_player_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = away_shooting_percentage, 
             y = home_shooting_percentage,
             color = nhl_player_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_player_shooting_filtered$away_shooting_percentage), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_player_shooting_filtered$home_shooting_percentage), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom")

# Look at which teams are in each cluster
nhl_player_shooting_filtered$cluster_number_shooting_percentage <- init_kmeanspp@cluster

# Here we can see what cluster people are in for the upper cluster taking the most shots away and at home (these might be the top playoff players generating the most offense)
nhl_player_shooting[which(nhl_player_shooting$cluster_number_total_shots == 6), "shooterName"]


# Look at shooting total instead of home and away breakdown ---------------

nhl_shots_total_players <- nhl_shots %>% 
  group_by(shooterName) %>% 
  summarize(total_shots = sum(event %in% c("GOAL", "SHOT", "MISS")),
            total_goals = sum(event == "GOAL"),
            total_games = n_distinct(game_id)) %>%
  mutate(total_shots_per_game = round(total_shots / total_games, 2), 
         total_goals_per_game = round(total_goals / total_games, 2),
         total_shooting_percentage = round(total_goals / total_shots, 4))

# Cluster of shots on goal versus their shooting percentage
nhl_shots_total_players <- nhl_shots_total_players[complete.cases(nhl_shots_total_players), ]
library(flexclust)
set.seed(12)
nhl_shots_total_players <- nhl_shots_total_players %>%
  mutate(std_total_shots = 
           as.numeric(scale(total_shots, 
                            center = TRUE, 
                            scale = TRUE)),
         std_total_shooting_percentage = 
           as.numeric(scale(total_shooting_percentage, 
                            center = TRUE, 
                            scale = TRUE)))
init_kmeanspp <- 
  kcca(dplyr::select(nhl_shots_total_players,
                     std_total_shots, 
                     std_total_shooting_percentage), 
       k = 6,
       control = list(initcent = "kmeanspp"))
nhl_shots_total_players %>%
  mutate(nhl_player_shot_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = std_total_shots, 
             y = std_total_shooting_percentage,
             color = nhl_player_shot_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_shots_total_players$std_total_shooting_percentage), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_shots_total_players$std_total_shots), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom")

# Make requirement that players need at least 5 shots
nhl_shots_total_players_filtered <- nhl_shots_total_players[complete.cases(nhl_shots_total_players), ] %>% 
  filter(nhl_shots_total_players$total_shots >= 5)
library(flexclust)
set.seed(12)
nhl_shots_total_players <- nhl_shots_total_players_filtered %>%
  mutate(std_total_shots = 
           as.numeric(scale(total_shots, 
                            center = TRUE, 
                            scale = TRUE)),
         std_total_shooting_percentage = 
           as.numeric(scale(total_shooting_percentage, 
                            center = TRUE, 
                            scale = TRUE)))
init_kmeanspp <- 
  kcca(dplyr::select(nhl_shots_total_players,
                     std_total_shots, 
                     std_total_shooting_percentage), 
       k = 6,
       control = list(initcent = "kmeanspp"))
nhl_shots_total_players %>%
  mutate(nhl_player_shot_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = std_total_shots, 
             y = std_total_shooting_percentage,
             color = nhl_player_shot_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_shots_total_players_filtered$std_total_shooting_percentage), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_shots_total_players_filtered$std_total_shots), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom")

# Look at players who are in cluster 6
nhl_shots_total_players_filtered$cluster_number_total_shots <- init_kmeanspp@cluster

nhl_shots_total_players_filtered[which(nhl_shots_total_players_filtered$cluster_number_total_shots == 4), "shooterName"]

nhl_shots_total_players_filtered[which(nhl_shots_total_players_filtered$cluster_number_total_shots == 1), "shooterName"]




# Find the breakdown between expected goals and goals scored --------------

# Make a goal scored category
nhl_shots$goalScored <- ifelse(nhl_shots$event == "GOAL", 1, 0)

# Calculate expected goals based on data given
expected_goals <- lm(goalScored ~ shooterLeftRight + shooterTimeOnIce + shotType + abs(shotAngle) + shotDistance + shotOnEmptyNet + shotRebound + shotRush + shotWasOnGoal + arenaAdjustedShotDistance + arenaAdjustedXCord + arenaAdjustedYCord + isHomeTeam, nhl_shots)
summary(expected_goals)

# Make the fitted values into the data set
nhl_shots$expected_goals <- expected_goals$fitted.values

#Fix rounding error to make all players that have expected goals at negative to zero (this is negative because missing the net technically has a slightly negative value for this model)
nhl_shots$expected_goals <- ifelse(nhl_shots$expected_goals > 0, nhl_shots$expected_goals, 0)

#Make data set with all the things we want
nhl_expected_goal_players <- nhl_shots %>% 
  group_by(shooterName) %>% 
  summarize(total_goals = sum(goalScored),
            total_shots = sum(event %in% c("GOAL", "SHOT", "MISS")),
            total_games = n_distinct(game_id),
            total_expected_goals = sum(expected_goals)) %>%
  mutate(total_shots_per_game = round(total_shots / total_games, 2), 
         total_shooting_percentage = round(total_goals / total_shots, 4),
         goals_per_game = round(total_goals / total_games, 4),
         expected_goals_per_game = round(total_expected_goals / total_games, 4),
         diff_expected_to_actual = total_goals - total_expected_goals,
         diff_expected_to_actual_per_game = round(diff_expected_to_actual / total_games, 4))

# Cluster players by expected goals and amount of goals they scored per game
library(flexclust)
set.seed(12)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_expected_goal_players,
                     expected_goals_per_game, 
                     goals_per_game), 
       k = 6,
       control = list(initcent = "kmeanspp"))
nhl_expected_goal_players %>%
  mutate(nhl_player_goals_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = expected_goals_per_game, 
             y = goals_per_game,
             color = nhl_player_goals_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_expected_goal_players$goals_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_expected_goal_players$expected_goals_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") + 
  coord_fixed()

# See who outlier is
nhl_expected_goal_players$cluster_number <- init_kmeanspp@cluster

nhl_expected_goal_players[which(nhl_expected_goal_players$cluster_number == 3), "shooterName"]

# What if we cluster based on total shots per game and their expected goal value
set.seed(12)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_expected_goal_players,
                     total_shooting_percentage, 
                     diff_expected_to_actual_per_game), 
       k = 4,
       control = list(initcent = "kmeanspp"))
nhl_expected_goal_players %>%
  mutate(nhl_player_shots_goals_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = total_shooting_percentage, 
             y = diff_expected_to_actual_per_game,
             color = nhl_player_shots_goals_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_expected_goal_players$diff_expected_to_actual_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_expected_goal_players$total_shooting_percentage), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom")


# Look at clustering by team based on expected goals and shot perc --------

nhl_expected_goal_team <- nhl_shots %>% 
  group_by(teamCode) %>% 
  summarize(total_goals = sum(goalScored),
            total_shots = sum(event %in% c("GOAL", "SHOT", "MISS")),
            total_games = n_distinct(game_id),
            total_expected_goals = sum(expected_goals)) %>%
  mutate(total_shots_per_game = round(total_shots / total_games, 2), 
         total_shooting_percentage = round(total_goals / total_shots, 4),
         goals_per_game = round(total_goals / total_games, 4),
         expected_goals_per_game = round(total_expected_goals / total_games, 4),
         diff_expected_to_actual = total_expected_goals - total_goals,
         diff_expected_to_actual_per_game = round(diff_expected_to_actual / total_games, 4))

# Here we can look at doing the shot percentage as a team compared to expected goals
set.seed(12)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_expected_goal_team,
                     total_shooting_percentage, 
                     diff_expected_to_actual_per_game), 
       k =5,
       control = list(initcent = "kmeanspp"))
nhl_expected_goal_team %>%
  mutate(nhl_team_shots_goals_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = total_shooting_percentage, 
             y = diff_expected_to_actual_per_game,
             color = nhl_team_shots_goals_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_expected_goal_team$diff_expected_to_actual_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_expected_goal_team$total_shooting_percentage), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom")

# Put each team into a cluster
nhl_expected_goal_team$cluster_for_shooting_percent_to_expected_goals <- init_kmeanspp@cluster

nhl_expected_goal_team %>%
  select(teamCode, cluster_for_shooting_percent_to_expected_goals)




# Here we can look at doing the total shots as a team compared to expected goals
set.seed(12)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_expected_goal_team,
                     total_shooting_percentage, 
                     diff_expected_to_actual_per_game), 
       k = 4,
       control = list(initcent = "kmeanspp"))
nhl_expected_goal_team %>%
  mutate(nhl_team_shots_goals_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = total_shooting_percentage, 
             y = diff_expected_to_actual_per_game,
             color = nhl_team_shots_goals_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_expected_goal_team$diff_expected_to_actual_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_expected_goal_team$total_shooting_percentage), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom")

# Put each team into a cluster
nhl_expected_goal_team$cluster_for_shooting_percent_to_expected_goals <- init_kmeanspp@cluster

nhl_expected_goal_team %>%
  select(teamCode, cluster_for_shooting_percent_to_expected_goals)



# Goaltending Data --------------------------------------------------------

#Goaltending Data set
nhl_goalies <- nhl_shots %>% 
  group_by(goalieNameForShot) %>% 
  summarize(goals_allowed = sum(goalScored),
            total_shots_faced = sum(event %in% c("GOAL", "SHOT")),
            total_games = n_distinct(game_id),
            total_rebounds = sum(shotGeneratedRebound)) %>%
  mutate(total_shots_faced_per_game = round(total_shots_faced / total_games, 2), 
         save_percentage = round((total_shots_faced - goals_allowed) / total_shots_faced, 4),
         goals_allowed_per_game = round(goals_allowed / total_games, 4),
         rebounds_per_game = round(total_rebounds / total_games, 4)) %>%
  na.omit()


# Make cluster of goaltenders: Does allowing more rebounds decrease save percentage?
set.seed(12)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_goalies,
                     rebounds_per_game, 
                     goals_allowed_per_game), 
       k = 4,
       control = list(initcent = "kmeanspp"))
nhl_goalies %>%
  mutate(nhl_goalie_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = rebounds_per_game, 
             y = goals_allowed_per_game,
             color = nhl_goalie_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_goalies$goals_allowed_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_goalies$rebounds_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom")

# Put each team into a cluster
nhl_goalies$save_cluster <- init_kmeanspp@cluster

nhl_goalies %>%
  select(goalieNameForShot, save_cluster)


# How do goalies do on the road versus home for save percentages?

#Put in away stats
nhl_goalies_away <-  nhl_shots %>%
  group_by(goalieNameForShot) %>% 
  filter(isHomeTeam == 1) %>%
  summarize(away_goals_allowed = sum(goalScored),
            away_shots_faced = sum(event %in% c("GOAL", "SHOT")),
            away_games = n_distinct(game_id),
            away_rebounds = sum(shotGeneratedRebound)) %>%
  mutate(away_shots_faced_per_game = round(away_shots_faced / away_games, 2), 
         away_save_percentage = round((away_shots_faced - away_goals_allowed) / away_shots_faced, 4),
         away_goals_allowed_per_game = round(away_goals_allowed / away_games, 4),
         away_rebounds_per_game = round(away_rebounds / away_games, 4)) %>%
  na.omit()

#Put in home stats
nhl_goalies_home <-  nhl_shots %>%
  group_by(goalieNameForShot) %>% 
  filter(isHomeTeam == 0) %>%
  summarize(home_goals_allowed = sum(goalScored),
            home_shots_faced = sum(event %in% c("GOAL", "SHOT")),
            home_games = n_distinct(game_id),
            home_rebounds = sum(shotGeneratedRebound)) %>%
  mutate(home_shots_faced_per_game = round(home_shots_faced / home_games, 2), 
         home_save_percentage = round((home_shots_faced - home_goals_allowed) / home_shots_faced, 4),
         home_goals_allowed_per_game = round(home_goals_allowed / home_games, 4),
         home_rebounds_per_game = round(home_rebounds / home_games, 4)) %>%
  na.omit()

# Only combine and observe goalies with both away and home data
nhl_goalies_starters <- merge(nhl_goalies_away, nhl_goalies_home, by = "goalieNameForShot")

# See how goalies compare on away versus home: Does home ice advantage exist for goalies?
set.seed(12)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_goalies_starters,
                     away_save_percentage, 
                     home_save_percentage), 
       k = 5,
       control = list(initcent = "kmeanspp"))
nhl_goalies_starters %>%
  mutate(nhl_goalie_save_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = away_save_percentage, 
             y = home_save_percentage,
             color = nhl_goalie_save_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_goalies_starters$home_save_percentage), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_goalies_starters$away_save_percentage), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom")

# Put each team into a cluster
nhl_goalies_starters$starter_saves_cluster <- init_kmeanspp@cluster

nhl_goalies_starters %>%
  select(goalieNameForShot, starter_saves_cluster)