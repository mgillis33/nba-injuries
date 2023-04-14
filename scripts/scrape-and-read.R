library(tidyverse)
library(rvest)
library(tibble)
library(tidymodels)

### scraping code ###
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
######

# output of scrape on 4/14/23:
df <- read.csv("/Users/mgillis/Desktop/Projects/nba-injuries/data/injuries_scraped.csv")

df <- df %>% 
  rename(
    date = X1,
    team = X2,
    acquired = X3,
    name = X4,
    notes = X5
  )

df <- subset(df, df$acquired == "" &
               df$name != "" &
               !(grepl("head coach", df$notes)) &
               !(grepl("illness", df$notes)) &
               !(grepl("flu", df$notes)) &
               !(grepl("virus", df$notes)) &
               !(grepl("health", df$notes)) &
               !(grepl("personal", df$notes)) &
               !(grepl("personal", df$notes)) &
               !(grepl("child", df$notes))
             )

df <- df %>% 
  select(date, team, name, notes) %>% 
  mutate(name = gsub("• ", "", name)) %>% 
  mutate(name = gsub("\\s*\\([^\\)]+\\) ", "", name))

injuries <- df %>% 
  count(name, name = "injuries")

df2 <- read.csv("data/all_seasons.csv")

df2 <- df2 %>% 
  select(!c(X, team_abbreviation, age)) %>% 
  rename(name = player_name) %>% 
  mutate(player_weight = player_weight * 2.20462) %>% 
  mutate(player_height = player_height * 0.393701) %>% 
  mutate(bmi = (player_weight/(player_height^2))*703) %>% 
  mutate(season = gsub("\\.-*", "", season)) %>% 
  mutate(name = gsub("\\ /.*", "", name))

players <- df2 %>% 
  group_by(name) %>% 
  summarize(across(c(player_height, player_weight, gp, pts, reb, ast, net_rating, oreb_pct, dreb_pct, usg_pct, ts_pct, ast_pct, bmi), mean)) %>% 
  mutate(across(where(is.numeric), round, 2))

full_table <- inner_join(injuries, players, by = "name")

full_table %>% 
  ggplot(aes(x = bmi)) +
  geom_histogram() +
  theme_bw()

full_table %>% 
  ggplot(aes(x = injuries)) +
  geom_histogram() +
  theme_bw()

full_table %>% 
  ggplot(aes(x = bmi, y = injuries)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

injuries_bmi <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(bmi ~ injuries, data = full_table)

parsnip::tidy(injuries_bmi)

glance(injuries_bmi)

lm_stats <- function(variable_of_interest){
  
  response_variable <- full_table %>% 
    pull(injuries)
  
  explanatory_variable <- full_table %>% 
    pull(variable_of_interest)
  
  new_df <- data.frame(response_variable, explanatory_variable)
  
  new_fit <- linear_reg() %>% 
    set_engine("lm") %>% 
    fit(response_variable ~ explanatory_variable, data = new_df )
  
  slope <- tidy(new_fit) %>% 
    filter(term == "explanatory_variable") %>%
    pull(estimate)
  
  r_squared <- glance(new_fit) %>% 
    pull(r.squared)
  
  p_value <- glance(new_fit) %>% 
    pull(p.value)
  
  return(
    list(
      slope = slope, 
      r_squared = r_squared, 
      p_value = p_value
    )
  )
  
}

variable_names <- full_table %>% 
  select_if(is.numeric) %>%
  select(!injuries) %>%
  names()

injury_models <- data.frame(variable_names,slope = NA, r_squared = NA, p_value = NA)

row_index <- 1

for(row_index in 1:nrow(injury_models)){
  
  stats_results <- lm_stats(variable_of_interest = injury_models[row_index, "variable_names"])
  
  injury_models[row_index, "slope"] <- stats_results$slope
  injury_models[row_index,"r_squared"] <- stats_results$r_squared
  injury_models[row_index,"p_value"] <- stats_results$p_value
  
}

injury_models %>% 
  arrange(desc(r_squared))
