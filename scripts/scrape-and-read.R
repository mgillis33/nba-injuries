library(tidyverse)
library(rvest)
library(tibble)

base_url <- "http://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=2000-01-01&EndDate=2023-04-12&InjuriesChkBx=yes&Submit=Search"

base_webpage <- read_html(base_url)

new_urls <- "http://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=2000-01-01&EndDate=2023-04-12&InjuriesChkBx=yes&Submit=Search&start=%s"

table_base <- html_table(base_webpage)[[1]] %>% 
  as_tibble(.name_repair = "unique")

tabtable_new <- data.frame()
df <- data.frame()

i <- 0

while (i < 25426) {
  print(i)
  new_webpage <- read_html(sprintf(new_urls,i))
  table_new <- rvest::html_table(new_webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  df<- rbind(df,table_new)
  i=i+25
}

df <- df %>% 
  rename(
    date = X1,
    team = X2,
    acquired = X3,
    name = X4,
    notes = X5
  )

df <- subset(df, df$acquired == "")

df <- df %>% 
  select(date, team, name, notes) %>% 
  mutate(name = gsub("• ", "", name)) %>% 
  mutate(name = gsub("\\s*\\([^\\)]+\\) ", "", name))

injuries <- df %>% 
  count(name, name = "injuries")

injuries <- injuries[-1,]

players <- read.csv("data/all_seasons.csv")

players <- players %>% 
  select(!c(X, team_abbreviation, age)) %>% 
  rename(name = player_name) %>% 
  mutate(player_weight = player_weight * 2.20462) %>% 
  mutate(player_height = player_height * 0.393701) %>% 
  mutate(season = gsub("\\.-*", "", season)) %>% 
  mutate(name = gsub("\\ /.*", "", name))

players <- players %>% 
  group_by(name) %>% 
  summarize(across(c(player_height, player_weight, gp, pts, reb, ast, net_rating, oreb_pct, dreb_pct, usg_pct, ts_pct, ast_pct), mean))

full_table <- inner_join(injuries_freq, players, by = "name")

full_table %>% 
  ggplot(aes(x = player_weight)) +
  geom_histogram() +
  theme_bw()

full_table %>% 
  ggplot(aes(x = injuries)) +
  geom_histogram() +
  theme_bw()

full_table %>% 
  ggplot(aes(x = player_weight, y = injuries)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()
