# Purpose: To look at hockey data and answer questions we have about the data. This is more updated than the other file as it starts to limit the plots that we plan to use as a group.


# Read in the hockey data -------------------------------------------------

library(tidyverse)
nhl_shots <- read_csv("nhl_playoffs_shots_2022.csv")


# Questions we want to answer ---------------------------------------------
# 1. Left handed shooters vs right handed shooters heat map
# 2. Is there a relationship between the distance and shot type players will exhibit
# 3. Looking at home and away shot data and see if players have a difference of how many shots they can generate at home versus away. We can also see the amount they generate per game and examine who the better offensive players are.



# Start looking at the heat map of the shooters positioning ---------------

# Make the heat map to answer question 1
library(sportyR)
nhl_shots$shooterLeftRight <- factor(nhl_shots$shooterLeftRight, labels = c("Left Handed Shooter", "Right Handed Shooter"))
nhl_shots_filter <- nhl_shots %>% 
  filter(arenaAdjustedYCord < 41, arenaAdjustedYCord > -41) %>% 
  mutate(absXCoord = -abs(arenaAdjustedXCord))
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
       caption = "Data courtesy of MoneyPuck.com") + 
  theme(legend.position = "bottom")


# Examine to see if there is a correlation between shot distance and shot type --------

# Make the labels for the graph
nhl_shots$shotType <- factor(nhl_shots$shotType, labels = c("Backhand", "Deflection", "Slapshot", "Snapshot", "Tip", "Wrap", "Wrist"))

# Model showing the different shot types versus distance
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
       caption = "Data courtesy of Moneypuck.com") +
  theme_bw() +
  facet_wrap( ~ shotType, ncol = 1)


# Number of shots per player plus the difference between the home and away settings --------

# First manipulate the data to answer this question

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


# Doing Hierarchical clustering using home and away shots by player -------


# Compute the Euclidean distance
player_shot_compare_dist <- dist(dplyr::select(nhl_player_shooting, away_shots_per_game, home_shots_per_game))

# Get the complete linkage information for these variables
nhl_shots_hclust_complete <- hclust(player_shot_compare_dist, method = "complete")

# Determine the number of clusters I should use
library(ggdendro)
ggdendrogram(nhl_shots_hclust_complete, 
             theme_dendro = FALSE, 
             labels = FALSE, 
             leaf_labels = FALSE) +
  labs(y = "Dissimilarity between clusters") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())
# From this plot I believe 5 clusters is the best since the drop is not very steep / as much distance away

# Make standard deviations for each of the home and away shots
nhl_player_shooting <- nhl_player_shooting %>%
  mutate(std_away_shots_per_game = 
           as.numeric(scale(away_shots_per_game, 
                            center = TRUE, 
                            scale = TRUE)),
         std_home_shots_per_game = 
           as.numeric(scale(home_shots_per_game, 
                            center = TRUE, 
                            scale = TRUE)))

# Create a new variable to show players who are 3 std's above in both home and away shots. This will be used later for the coloring of the names
shooterNameExtreme <- filter(nhl_player_shooting, std_away_shots_per_game >= 3, std_home_shots_per_game >= 3)

# Make cluster labels and plot for complete linkage
library(ggrepel)  
nhl_player_shooting %>%
  mutate(shooting_clusters =
           as.factor(cutree(nhl_shots_hclust_complete,
                            k = 5))) %>%
  ggplot(aes(x = away_shots_per_game, 
             y = home_shots_per_game, 
             color = shooting_clusters)) +
  #Label all players who are at least 3 standard deviations above the average for either the home and away shots
  geom_label_repel(aes(label = ifelse((std_away_shots_per_game >= 3) & (std_home_shots_per_game >= 3), as.character(shooterName), '')), 
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   size = 3,
                   color = "brown",
                   min.segment.length = 0,
                   max.overlaps = Inf) +
  #Label all players who are at least 3 standard deviations above the average for either the home or away shots but not both
  geom_label_repel(aes(label = ifelse(((std_away_shots_per_game >= 3) | (std_home_shots_per_game >= 3)) & shooterName != shooterNameExtreme$shooterName, as.character(shooterName), '')),
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   size = 3,
                   color = "purple",
                   min.segment.length = 0,
                   max.overlaps = Inf) +
  geom_point(alpha = .7) +
  ggthemes::scale_color_colorblind() +
  labs(title = "Grouping players by their offensive shot output in home and away settings",
       x = "Amount of away shots per game",
       y = "Amount of home shots per game",
       color = "Player Shooting Clusters",
       caption = "Data courtesy of Moneypuck.com") +
  theme_bw() +
  coord_fixed()
  theme(legend.position = "bottom")
