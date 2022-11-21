

####################################

# LOAD PACKAGES AND FUNCTIONS

####################################

library("tidyverse")
library("caret")


'%!in%' <- function(x,y)!'%in%'(x,y)


RMSE <- function(true_value, predicted_value){
  
  sqrt(mean((true_value - predicted_value)^2))
  
}




##############################################

# PARTITION DATA INTO TRAINING AND TEST SETS

##############################################


data <- read_csv("https://raw.githubusercontent.com/bibliotheque-generale/HDS/main/fpl_dataset.csv")


test <- data %>%
  filter(
    season_tag == 2
  ) %>%
  select(
    c(2:6)
  ) %>%
  mutate(
    ppg = total_points/matches_played
  )


training <- data %>%
  filter(
    season_tag == 1
  ) %>%
  select(
    c(2:7, 13, 16)
  ) %>%
  mutate(
    ppg = total_points/matches_played
  ) 


rm(data)



mu <- mean(training$ppg)

RMSE(test$ppg, mu)




######################################################

# ASSESS 'TEAM' EFFECT

######################################################


team_effect <- training %>%
  group_by(
    team
  ) %>%
  summarise(
    b_team = sum(ppg - mu)/n()
  ) %>%
  filter(
    !is.na(team)
  )


ggplot(team_effect, aes(x = reorder(team, b_team), y = b_team, fill = b_team)) + 
  geom_bar(
    stat = "identity", color = "#000000"
  ) +
  ggtitle(
    "Team Effects"
  ) + 
  xlab(
    ""
  ) +
  ylab(
    "Effect"
  ) +
  geom_hline(
    yintercept = 0
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position = "none",
    plot.title = element_text(hjust = .5)
  ) + 
  scale_fill_distiller(
    type = "div",
    palette = 8,
    direction = 1
  )




######################################################

# CLUSTER PLAYER STATS TO REFINE 'POSITION' EFFECT

######################################################


cluster_data <- training %>%
  mutate(
    orientation = as.integer(ifelse(
      position == "GK", 1, ifelse(
        position == "DEF", 2, ifelse(
          position == "MID", 3, 4
  ))))) %>%
  select(
    name,
    orientation
  ) %>%
  left_join(
    training,
    by = "name"
  ) %>%
  filter(
    matches_played >= 10
  ) %>%
  select(
    name,
    orientation,
    minutes, 
    goals_scored,
    assists
  )



### Midfield Clusters 

midfielders <- cluster_data %>%
  filter(
    orientation == 3
  ) %>%
  mutate(
    minutes = scale(minutes),
    goals_scored = scale(goals_scored),
    assists = scale(assists)
  ) %>%
  select(
    minutes,
    goals_scored,
    assists
  )


hclust <- hclust(dist(midfielders), method = "complete")

plot(hclust)



set.seed(1, sample.kind = "Rounding")

midfield_clusters <- kmeans(midfielders, 4, nstart = 15)

midfield_clusters


midfielders_output <-
  bind_cols(
    select(filter(cluster_data, orientation == 3), name),
    cluster = midfield_clusters$cluster
  ) %>%
  mutate(
    category = "Midfielder",
    group = ifelse(
      cluster == 1, 'Sub', ifelse(
        cluster == 2, "Tier 1", ifelse(
          cluster == 3, "Tier 3", "Tier 2"
  )))) %>%
  select(
    -cluster
  )


rm(midfielders, midfield_clusters)



### Defender Clusters 

defenders <- cluster_data %>%
  filter(
    orientation == 2
  ) %>%
  mutate(
    minutes = scale(minutes),
    goals_scored = scale(goals_scored),
    assists = scale(assists)
  ) %>%
  select(
    minutes,
    goals_scored,
    assists
  )


hclust <- hclust(dist(defenders), method = "complete")

plot(hclust)


set.seed(2, sample.kind = "Rounding")

defender_clusters <- kmeans(defenders, 3, nstart = 15)

defender_clusters


defenders_output <-
  bind_cols(
    select(filter(cluster_data, orientation == 2), name),
    cluster = defender_clusters$cluster
  ) %>%
  mutate(
    category = "Defender",
    group = ifelse(
      cluster == 1, 'Tier 1', ifelse(
        cluster == 2, "Sub", "Tier 2"
    ))) %>%
    select(
      -cluster
    )


rm(defenders, defender_clusters)



### Forward Clusters 

forwards <- cluster_data %>%
  filter(
    orientation == 4
  ) %>%
  mutate(
    minutes = scale(minutes),
    goals_scored = scale(goals_scored),
    assists = scale(assists)
  ) %>%
  select(
    minutes,
    goals_scored,
    assists
  )


hclust <- hclust(dist(forwards), method = "complete")

plot(hclust)


set.seed(3, sample.kind = "Rounding")

forward_clusters <- kmeans(forwards, 3, nstart = 15)

forward_clusters


forwards_output <-
  bind_cols(
    select(filter(cluster_data, orientation == 4), name),
    cluster = forward_clusters$cluster
  ) %>%
  mutate(
    category = "Forward",
    group = ifelse(
      cluster == 1, 'Tier2', ifelse(
        cluster == 2, "Sub", "Tier 1"
  ))) %>%
  select(
    -cluster
  )


rm(forwards, forward_clusters, hclust)




### Goalies (no clusters needed)

goalies_output <- cluster_data %>%
  filter(
    orientation == 1
  ) %>%
  select(
    name
  ) %>%
  mutate(
    category = "Goalie",
    group = "Goalie"
  )


rm(cluster_data)



### COMBINE ALL POSITIONS INTO ONE TABLE

new_positions <- midfielders_output %>%
  bind_rows(
    defenders_output,
    forwards_output,
    goalies_output
  )


rm(midfielders_output, defenders_output, forwards_output, goalies_output)



### UPDATE TRAINING AND TEST SETS WITH NEW POSITION CLASSIFICATIONS 

updated_training <- training %>%
  left_join(
    new_positions,
    by = "name"
  ) %>%
  select(
    name,
    category,
    group,
    team,
    total_points,
    matches_played,
    ppg
  )


updated_testing <- test %>%
  left_join(
    new_positions,
    by = "name"
  ) %>%
  select(
    name,
    category,
    group,
    team,
    total_points,
    matches_played,
    ppg
  )


rm(training, test, new_positions)




### Estimate Position Effect

position_effect <- updated_training %>%
  left_join(
    team_effect,
    by = "team"
  ) %>%
  group_by(
    category,
    group
  ) %>%
  summarise(
    b_position = sum(ppg - mu - b_team)/n()
  ) %>%
  filter(
    !is.na(
      group
    )
  )


ggplot(filter(updated_training, category == "Midfielder"), aes(x = ppg, fill = group)) + 
  geom_density(alpha = .75) + 
  scale_fill_manual(
    values = c("#d73027", "#fdae61", "#ffffbf", "#1a9850"),
    limits = c("Sub", "Tier 3", "Tier 2", "Tier 1")
  ) +
  geom_hline(
    yintercept = 0
  ) +
  ggtitle(
    "Midfielder Groups"
  ) +
  xlab(
    "Points Per Game"
  ) +
  ylab(
    element_blank()
  ) +
  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = .5)
  )




######################################################

# ASSESS 'PLAYER' EFFECT

######################################################


player_effect <- updated_training %>%
  left_join(
    team_effect,
    by = 'team'
  ) %>%
  left_join(
    position_effect,
    by = c("category", "group")
  ) %>%
  mutate(
    b_player = ppg - b_team - b_position - mu
  ) %>%
  select(
    name,
    b_player
  ) %>%
  filter(
    !is.na(
      b_player
    )
  )


ggplot(player_effect, aes(x = b_player)) + 
  geom_histogram(
    aes(y = ..density..),
    color = "black", 
    fill = "grey"
  ) +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(player_effect$b_player), sd = sd(player_effect$b_player)),
    color = "#d73027",
    size = 1
  ) + 
  ggtitle(
    "Player Effects"
  ) +
  xlab(
    element_blank()
  ) +
  ylab(
    element_blank()
  ) +
  theme_minimal()




######################################################

# PREDICT 2021-'22 PPG VALUES

######################################################


predictions <- updated_testing %>%
  left_join(
    team_effect,
    by = "team"
  ) %>%
  left_join(
    position_effect,
    by = c("category", "group")
  ) %>%
  left_join(
    player_effect,
    by = "name"
  ) %>%
  replace_na(
    list(
      b_team = 0,
      b_position = 0,
      b_player = 0
    )
  ) %>%
  mutate(
    pred = mu + b_team + b_position + b_player
  ) 


RMSE(updated_testing$ppg, predictions$pred)

RMSE(updated_testing$ppg, mu)


errors <- predictions %>%
  mutate(
    error = pred - ppg
  )




###

#rm(list=ls())

