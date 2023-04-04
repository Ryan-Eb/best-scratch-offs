library(tidyverse)
library(lubridate)

source("/Users/jennaryan/Documents/GitHub/best-scratch-offs/fl_data_prep.R")

#want to look at tickets sold per day for each game

tickets_sold_df <- clean_df %>% 
  select(Game_number,Game_name,scrape_date,game_age,tickets_sold,ticket_cost,ticket_cost_num) %>% 
  group_by(scrape_date) %>% 
  distinct(Game_number, .keep_all = TRUE) %>% 
  arrange(Game_number,scrape_date) %>% 
  ungroup() %>% 
  group_by(Game_number) %>% 
  mutate(sold_since_last_scrape = tickets_sold - lag(tickets_sold)) %>%
  mutate(days_since_last_scrape = as.numeric(scrape_date - lag(scrape_date))) %>% 
  mutate(tickets_per_day_since_last_scrape = sold_since_last_scrape/days_since_last_scrape) %>% 
  ungroup()

#most popular game today
tickets_sold_df %>% 
  filter(scrape_date == max(scrape_date)) %>% 
  arrange(desc(tickets_per_day_since_last_scrape)) %>% 
  .[1,] %>% view()

#tickets sold per day since last scrape for each ticket amount
tickets_sold_df %>% 
  filter(scrape_date == max(scrape_date)) %>% 
  group_by(ticket_cost_num) %>% 
  summarise(tickets_sold_per_day = sum(tickets_per_day_since_last_scrape)) %>% 
  ungroup() %>% 
  mutate(total_dollar_spend_on_tickets = ticket_cost_num * tickets_sold_per_day)

#tickets sold per day total since last scrape
tickets_sold_df %>% 
  filter(scrape_date == max(scrape_date)) %>% 
  group_by(ticket_cost_num) %>% 
  summarise(tickets_sold_per_day = sum(tickets_per_day_since_last_scrape)) %>% 
  ungroup() %>% 
  mutate(total_dollar_spend_on_tickets = ticket_cost_num * tickets_sold_per_day) %>% 
  summarise(tickets_sold_per_day = sum(tickets_sold_per_day),
            total_dollar_spend_on_tickets = sum(total_dollar_spend_on_tickets))

#total spend on scratch offs per day dataframe
#this will be interesting to track for seasonality, weekends, holidays, etc,...
tickets_sold_df %>% 
  drop_na() %>% 
  mutate(dollar_per_day = tickets_per_day_since_last_scrape * ticket_cost_num) %>% 
  group_by(scrape_date) %>%
  summarise(dollar_spend_per_day = sum(dollar_per_day))

#plot of ticket spend per day for each ticket amount
tickets_sold_df %>% 
  drop_na() %>% 
  mutate(dollar_per_day = tickets_per_day_since_last_scrape * ticket_cost_num) %>% 
  group_by(ticket_cost_num,scrape_date) %>%
  summarise(dollar_spend_per_day = sum(dollar_per_day)) %>% 
  ungroup() %>% 
  ggplot(aes(x = scrape_date, y = dollar_spend_per_day, color = as.factor(ticket_cost_num)))+
  geom_line()+geom_point(aes(shape = as.factor(ticket_cost_num)))+
  scale_y_continuous(labels = scales::dollar_format())

          