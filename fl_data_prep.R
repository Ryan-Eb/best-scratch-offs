library(tidyverse)
library(lubridate)
library(ggrepel)

daily_csv <- list.files("/Users/jennaryan/Documents/R Projects/fl lottery/daily_csv_fl",
                        full.names = TRUE) %>% 
                        lapply(read_csv)

df <- bind_rows(daily_csv)

clean_df <- df %>% 
  select(-`...1`) %>% 
  mutate(start_odds = as.numeric(gsub(",", "",str_split(odds, "-in-", simplify = TRUE) %>% .[,2]))) %>% 
  mutate(number_tickets = start_odds * total_prizes) %>% 
  mutate(prize_amount = as.numeric(gsub(",", "",str_sub(prize_amount, start=2L, end = -1L)))) %>%
  group_by(Game_number,scrape_date) %>% 
  mutate(number_tickets = number_tickets[1]) %>% 
  mutate(start_overall_odds = number_tickets/sum(total_prizes)) %>% 
  mutate(tickets_sold = sum(prizes_paid) * start_overall_odds) %>% 
  ungroup() %>% 
  mutate(tickets_remain = number_tickets - tickets_sold) %>% 
  mutate(current_odds = tickets_remain / prizes_remaining) %>% 
  mutate(start_date = mdy(start_date)) %>% 
  mutate(Game_number = as.factor(Game_number)) %>% 
  mutate(percent_claimed = prizes_paid / total_prizes * 100) %>% 
  mutate(game_age = as.numeric(scrape_date - start_date)) %>% 
  mutate(avg_tickets_sold_per_day = tickets_sold / game_age) %>% 
  mutate(ticket_cost_num = as.numeric(str_sub(ticket_cost, start = 2L, end = -1L)))

#top prize tracker. Track winning tickets for all prizes >= $5mil
#filter out prizes that have been 100% claimed

buy_out_df <- clean_df %>% 
  group_by(Game_number,scrape_date) %>% 
  summarise(prize_amount_start = sum(total_prizes*prize_amount),
            ticket_cost_start = number_tickets[1] * ticket_cost_num[1],
            prize_amount_remain = sum(prizes_remaining*prize_amount),
            ticket_cost_remain = tickets_remain[1] * ticket_cost_num[1],
            cost_to_buy_out_start = ticket_cost_start - prize_amount_start,
            cost_to_buy_out_current = ticket_cost_remain - prize_amount_remain,
            start_roi = (prize_amount_start / ticket_cost_start-1),
            current_roi = (prize_amount_remain / ticket_cost_remain-1),
            game_age = game_age[1],
            Game_name = Game_name[1],
            ticket_cost = ticket_cost[1]) %>% 
  ungroup()

avg_sold_per_day <- clean_df %>% 
  group_by(scrape_date) %>%
  distinct(Game_number, .keep_all = TRUE) %>% 
  summarise(tickets_remain = sum(tickets_remain)) %>% 
  arrange(scrape_date) %>% 
  summarise(days_since_last_scrape = as.numeric(scrape_date[n()]-scrape_date[n()-1]),
            tickets_sold_since_last_scrape = tickets_remain[n()-1] - tickets_remain[n()]) %>% 
  summarise(avg_per_day = tickets_sold_since_last_scrape/days_since_last_scrape) %>% 
  as.numeric() %>% 
  round(0)

total_amount_remain <- buy_out_df %>% 
  filter(scrape_date == max(scrape_date)) %>% 
  drop_na() %>% 
  summarise(prizes_remain = sum(prize_amount_remain)) %>% 
  as.numeric() %>% 
  round(0)

greater_500_remain <- clean_df %>% 
  filter(scrape_date == max(scrape_date)) %>% 
  filter(prize_amount >= 500) %>% 
  summarise(remain_500 = sum(prizes_remaining)) %>% 
  as.numeric() %>% 
  round(0)

best_games_tbl <- buy_out_df %>% 
  filter(scrape_date == max(scrape_date)) %>% 
  drop_na() %>% 
  group_by(ticket_cost) %>% 
  summarise(Game_name = Game_name[1],
            `ROI %` = scales::label_percent()(max(current_roi)),
            `Return on 1$` =scales::label_dollar()(1+1*max(current_roi))) %>% 
  mutate(ticket_cost_num = as.numeric(str_sub(ticket_cost, start = 2L, end = -1L))) %>% 
  arrange(ticket_cost_num) %>% 
  select(-ticket_cost_num) 
