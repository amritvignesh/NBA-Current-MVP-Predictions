library(tidyverse)
library(nbastatR)
library(nbaTools)
library(ggimage)
library(gt)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(png)
library(paletteer)
library(webshot2)
library(fs)

Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)
stats24_basic <- bref_players_stats(seasons = 2024, tables = "totals", widen = TRUE, assign_to_environment = TRUE) 
stats24_adv <- bref_players_stats(seasons = 2024, tables = "advanced", widen = TRUE, assign_to_environment = TRUE)

stats24_basic <- distinct(stats24_basic, slugPlayerBREF, .keep_all = TRUE)
stats24_adv <- distinct(stats24_adv, slugPlayerBREF, .keep_all = TRUE)

stats24 <- inner_join(stats24_basic, stats24_adv, by = "slugPlayerBREF")


stats24 <- stats24 %>%
  group_by(slugTeamBREF.x) %>%
  filter(countGames.x >= (65/82) * max(countGames.x)) %>%
  ungroup() %>%
  summarize(player = namePlayer.x, team = slugTeamBREF.x, idPlayerNBA = idPlayerNBA.x, ppg = ptsTotals/countGames.x, apg = astTotals/countGames.x, rpg = trbTotals/countGames.x, spg = stlTotals/countGames.x, bpg = blkTotals/countGames.x, per = ratioPER, ws48 = ratioWSPer48, bpm = ratioBPM, vorp = ratioVORP)

pbp_24 <- game_logs(seasons = 2024)
wins <- pbp_24 %>%
  mutate(outcome = ifelse(outcomeGame == "W", 1, 0)) %>%
  filter(minutes >= 20) %>%
  group_by(idPlayer, nameTeam) %>%
  summarize(games = n(), win = sum(outcome)/n()) %>%
  ungroup() %>%
  group_by(nameTeam) %>%
  filter(games >= (65/82) * max(games)) %>%
  ungroup()

stats24 <- inner_join(wins, stats24, by = c("idPlayer" = "idPlayerNBA"))

stats24 <- stats24 %>%
  mutate(mvp = 0) 

stats_new_train <- inner_join(basic_stats, advanced_stats, by = c("idPlayerNBA", "season"))

stats_new_train <- stats_new_train %>%
  select(player = player.x, season, team = team.x, ppg, apg, rpg, spg, bpg, win, per, ws48, bpm, vorp, mvp = mvp.x)

stats_new_train_reg <- glm(mvp ~ ppg + apg + rpg + spg + bpg + win + per + ws48 + bpm + vorp, data = stats_new_train, family = "binomial")

stats24_final <- stats24 %>%
  ungroup() %>%
  mutate(prediction = predict(stats_new_train_reg, stats24, type = "response")) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  ungroup() 

table <- stats24_final %>%
  select(player, team, mvp_prob) %>%
  arrange(-mvp_prob) %>%
  filter(row_number() <= 10) %>%
  mutate(mvp_prob = round(mvp_prob, 3)) %>%
  ungroup()
table %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, team, mvp_prob)
  ) %>%
  data_color(
    columns = mvp_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team = md("**Team**"),
    mvp_prob = md("**MVP Probability**")
  ) %>%
  tab_header(
    title = md("**2023-24 NBA MVP Probability, Updated After Games of 11/13/2023**"),
    subtitle = "Based on NBA MVP Data from 1980 - 2023 Involving Basic and Advanced Statistics"
  )
