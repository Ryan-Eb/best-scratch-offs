library(tidyverse)
library(lubridate)
library(ggrepel)

daily_csv <- list.files("/Users/jennaryan/Documents/R Projects/tx lottery/daily_csv_tx",
                        full.names = TRUE) %>% 
  lapply(read_csv)

df <- bind_rows(daily_csv)

clean_df <- df %>% 
  select(-`...1`) %>% 
  mutate(Amount = as.numeric(gsub(",", "",str_sub(Amount, start=2L, end = -1L)))) %>%
  rename(prize_amount = Amount) %>% 
  rename(total_prize = `No. in Game*`) %>% 
  rename(prizes_claimed = `No. Prizes Claimed`) %>%
  mutate(total_prize = as.numeric(str_replace_all(total_prize,",",""))) %>% 
  mutate(prizes_claimed = str_replace_all(prizes_claimed,"---", "0")) %>% 
  mutate(prizes_claimed = as.numeric(str_replace_all(prizes_claimed,",",""))) %>% 
  mutate(remaining_prizes = total_prize - prizes_claimed) %>% 
  group_by(game_number,scrape_date) %>% 
  mutate(total_prize_game = sum(total_prize)) %>% 
  mutate(start_overall_odds = number_tickets / total_prize_game) %>% 
  mutate(tickets_sold = sum(prizes_claimed) * start_overall_odds) %>% 
  ungroup() %>% 
  mutate(tickets_remain = number_tickets - tickets_sold) %>% 
  mutate(current_odds = tickets_remain / remaining_prizes) %>% 
  mutate(start_date = mdy(`Start Date`)) %>% 
  mutate(game_number = as.factor(game_number)) %>% 
  mutate(percent_claimed = prizes_claimed / total_prize * 100) %>% 
  mutate(game_age = as.numeric(scrape_date - start_date)) %>% 
  mutate(avg_tickets_sold_per_day = tickets_sold / game_age) %>% 
  mutate(ticket_cost_num = as.numeric(str_sub(`Ticket Price`, start = 2L, end = -1L))) %>% 
  rename(game_name = `Game Name`) %>% 
  rename(ticket_price = `Ticket Price`)


buy_out_df <- clean_df %>% 
  group_by(game_number,scrape_date) %>% 
  summarise(prize_amount_start = sum(total_prize*prize_amount),
            ticket_cost_start = number_tickets[1] * ticket_cost_num[1],
            prize_amount_remain = sum(remaining_prizes*prize_amount),
            ticket_cost_remain = tickets_remain[1] * ticket_cost_num[1],
            cost_to_buy_out_start = ticket_cost_start - prize_amount_start,
            cost_to_buy_out_current = ticket_cost_remain - prize_amount_remain,
            start_roi = (prize_amount_start / ticket_cost_start-1),
            current_roi = (prize_amount_remain / ticket_cost_remain-1),
            game_age = game_age[1],
            game_name = game_name[1],
            ticket_price = ticket_price[1]) %>% 
  ungroup()

##not updated yet for NC
avg_sold_per_day <- clean_df %>% 
  group_by(scrape_date) %>%
  distinct(game_number, .keep_all = TRUE) %>% 
  summarise(tickets_remain = sum(tickets_remain),
            number_tickets = sum(number_tickets)) %>% 
  arrange(scrape_date) %>% 
  summarise(days_since_last_scrape = as.numeric(scrape_date[n()]-scrape_date[n()-1]),
            number_tickets_diff = number_tickets[n()] - number_tickets[n()-1], 
            tickets_sold_since_last_scrape = number_tickets_diff + tickets_remain[n()-1] - tickets_remain[n()]) %>% 
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
  summarise(remain_500 = sum(remaining_prizes)) %>% 
  as.numeric() %>% 
  round(0)

best_games_tbl <- buy_out_df %>% 
  filter(scrape_date == max(scrape_date)) %>% 
  drop_na() %>% 
  group_by(ticket_price) %>% 
  summarise(game_name = game_name[1],
            `ROI %` = scales::label_percent()(max(current_roi)),
            `Return on 1$` =scales::label_dollar()(1+1*max(current_roi))) %>% 
  mutate(ticket_cost_num = as.numeric(str_sub(ticket_price, start = 2L, end = -1L))) %>% 
  arrange(ticket_cost_num) %>%
  mutate(game_name = str_replace_all(game_name, "[^a-zA-Z0-9]", " ")) %>% 
  select(-ticket_cost_num) 

