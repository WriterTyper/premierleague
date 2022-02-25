

```{r}
library(tidyverse)
library(readr)
```

```{r}
library(gt)
```

```{r}
home <- data %>% filter(Season == "2020-21") %>% filter(HomeTeam == "Fulham") %>% select(HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% mutate(GD = FTHG-FTAG) %>% select(GD)
```

```{r}
away <- data %>% filter(Season == "2020-21") %>% filter(AwayTeam == "Fulham") %>% select(HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% mutate(GD = -(FTHG-FTAG)) %>% select(GD)
```

```{r}
bind_rows(home, away) %>% gt() %>% tab_style(
    style = cell_fill(color = "grey"),
    locations = cells_body(
      columns = GD,
      rows = GD == 0)
  ) %>% tab_style(
    style = cell_fill(color = "green"),
    locations = cells_body(
      columns = GD,
      rows = GD >= 1)
  ) %>% tab_style(
    style = cell_fill(color = "red"),
    locations = cells_body(
      columns = GD,
      rows = GD <= 1)
  )
```


```{r}
install.packages("worldfootballR")
```

```{r}
library(worldfootballR)
```



```{r}
epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2020)
dplyr::glimpse(epl_results)
```

```{r}
h <- epl_results %>% 
  filter(season == "2020/2021") %>%
  select(home_team, away_team, home_goals, away_goals) %>%
  filter(home_team == "Fulham") %>% 
  mutate(goal_diff = home_goals - away_goals)
```

```{r}
a <- epl_results %>% 
  filter(season == "2020/2021") %>%
  select(home_team, away_team, home_goals, away_goals) %>%
  filter(away_team == "Fulham") %>% 
  mutate(goal_diff = -(home_goals - away_goals))
```


```{r}
bind_rows(h, a)
```

```{r}
bind_rows(h, a) %>%  select(goal_diff) %>% rename(GD = goal_diff) %>% gt() %>% tab_style(
    style = cell_fill(color = "grey"),
    locations = cells_body(
      columns = GD,
      rows = GD == 0)
  ) %>% tab_style(
    style = cell_fill(color = "green"),
    locations = cells_body(
      columns = GD,
      rows = GD >= 1)
  ) %>% tab_style(
    style = cell_fill(color = "red"),
    locations = cells_body(
      columns = GD,
      rows = GD <= 1)
  )
```

```{r}
c1 <- bind_rows(home, away)
```


```{r}
c2 <- bind_rows(h, a) %>% select(goal_diff)
```

```{r}
bind_cols(c1, c2) %>% mutate(res = GD - goal_diff)
```

```{r}
glimpse(epl_results)
```


```{r}
epl_results2 <- understat_league_match_results(league = "EPL", season_start_year = 2021)
dplyr::glimpse(epl_results2)
```
```{r}
epl_results2 %>% select(home_abbr, away_abbr, home_goals, away_goals)
```

```{r}
epl_results2 %>% select(home_abbr, away_abbr, home_goals, away_goals) %>% filter(home_abbr == "BRE") %>% mutate(goal_diff = home_goals-away_goals) %>% mutate(team = "BRE") %>% relocate(team)
```

```{r}
epl_results2 %>% select(home_abbr, away_abbr, home_goals, away_goals) %>% filter(away_abbr == "BRE") %>% mutate(goal_diff = away_goals-home_goals) %>% mutate(team = "BRE") %>% relocate(team)
```


```{r}
home <- epl_results2 %>%  select(home_abbr, away_abbr, home_goals, away_goals) %>% filter(home_abbr == "BRE") %>% mutate(goal_diff = home_goals-away_goals) %>% mutate(team = "BRE") %>% relocate(team) %>% select(team, goal_diff)

away <- epl_results2 %>% select(home_abbr, away_abbr, home_goals, away_goals) %>% filter(away_abbr == "BRE") %>% mutate(goal_diff = away_goals-home_goals) %>% mutate(team = "BRE") %>% relocate(team) %>% select(team, goal_diff)

results <- bind_rows(home, away) %>% rownames_to_column("match_num") %>% mutate(match_num = as.numeric(match_num)) %>% arrange(match_num) %>% rowid_to_column("match_order")

oneline <- results %>% 
  select(match_order, goal_diff, team) %>% 
  pivot_wider(names_from = match_order, values_from = goal_diff)

oneline
```

```{r}
oneline_result <- function(.data, team_abbr) {
  
  home <- .data %>%  
    select(home_abbr, away_abbr, home_goals, away_goals) %>% 
    filter(home_abbr == team_abbr) %>% 
    mutate(goal_diff = home_goals-away_goals) %>% 
    mutate(team = team_abbr) %>% 
    relocate(team) %>% 
    select(team, goal_diff)
  
  away <- .data %>% 
    select(home_abbr, away_abbr, home_goals, away_goals) %>% 
    filter(away_abbr == team_abbr) %>% 
    mutate(goal_diff = away_goals-home_goals) %>% 
    mutate(team = team_abbr) %>% 
    relocate(team) %>% 
    select(team, goal_diff)
  
  results <- bind_rows(home, away) %>% 
    rownames_to_column("match_num") %>% 
    mutate(match_num = as.numeric(match_num)) %>% 
    arrange(match_num) %>% 
    rowid_to_column("match_order")
  
  oneline <- results %>% 
    select(match_order, goal_diff, team) %>% 
    pivot_wider(names_from = match_order, values_from = goal_diff)
  
  oneline
}

```



```{r}
oneline_result(epl_results2, "LIV")
```
```{r}
team_list <- list("ARS", "LIV", "MUN", "CHE")
```

```{r}
lapply(team_list, oneline_result(epl_results2))
```

```{r}
oneline_result(epl_results2, "CHE")
```

```{r}
lapply(1:5, mean, na.rm = TRUE)
```

```{r}
team_list <- epl_results2 %>% distinct(home_abbr)
```

```{r}
team_list <- as.list(team_list)
```

```{r}
team_list <- team_list$home_abbr
```


```{r}
map_dfr(team_list, oneline_result, .data = epl_results2) %>% arrange(team)
```
```{r}
map_dfr(team_list, oneline_result, .data = epl_results2)
```


