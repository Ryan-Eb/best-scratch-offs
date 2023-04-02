library(tidyverse)
library(lubridate)
library(kableExtra)
library(DT)

daily_csv <- list.files("/Users/jennaryan/Documents/R Projects/mn lottery/daily_csv_mn",
                        full.names = TRUE) %>% 
  lapply(read_csv)

df <- bind_rows(daily_csv)

#remove end data.  add columns for % claimed.  add game age column
#separate the game $ amount

cleaned_df  <- df%>% 
  select(-End_date, -Game1, -`...1`) %>% 
  mutate(total_prizes_500 = Claimed + Remaining) %>% 
  mutate(percent_claimed_500 = Claimed / total_prizes_500 *100) %>%
  mutate(odds_500 = no_tickets / total_prizes_500 ) %>%
  mutate(total_prizes = no_tickets / overall_odds) %>% 
  mutate(game_age = as.numeric(Date - Start_date)) %>% 
  mutate(tickets_div_avg = no_tickets / game_age) %>% 
  separate_wider_delim(Game, delim = " - ", names = c("game_amount", "Game"))

#create list of all game $ amounts
amount_list <- cleaned_df %>% distinct(game_amount) %>% 
                  mutate(amount_num = as.numeric(str_sub(game_amount, 2L, -1L))) %>% 
                  arrange(amount_num) %>% 
                  select(game_amount)

#create list of dataframes for each game amount
#sums all prizes together
#add loess prediction for %claimed to DF
game_df_list <- list()
for (i in 1:nrow(amount_list)) {
  game_df <- cleaned_df %>% 
    drop_na() %>% 
    group_by(Game, Date) %>% 
    summarise(total_prizes_500 = sum(total_prizes_500),
              Claimed = sum(Claimed),
              Remaining = sum(Remaining),
              game_age = game_age[1],
              game_amount = game_amount[1],
              Date = Date[1],
              no_tickets = no_tickets[1],
              odds_500 = no_tickets / total_prizes_500,
              overall_odds = overall_odds[1],
              tickets_div_age = no_tickets / game_age) %>% 
    ungroup() %>% 
    mutate(percent_claimed_500 = Claimed / total_prizes_500*100) %>% 
    filter(game_amount == as.character(amount_list[i,1])) 
  
  loess <- loess(percent_claimed_500 ~ as.numeric(game_age), data = game_df, span = 1.5)
  loess.predict <- predict(loess, SE = F)
  
  game_df <- bind_cols(game_df, loess.predict) %>% 
    rename(loess_fit = `...13`) %>% 
    mutate(residual = percent_claimed_500 - loess_fit)
  
  game_df_list[[i]] <- game_df
}


#create plot of % tickets claimed vs Game Age for each $ amount
#claimed is a total of all prizes for each game
per_claim_plot_list = list()  
for (i in 1:nrow(amount_list)) {
  
  p <- game_df_list[[i]] %>% 
    ggplot(aes(x = as.numeric(game_age), y = percent_claimed_500)) +
    geom_point(aes(color = Game), size=2)+
    geom_smooth(span = 1.5)+
    theme_bw()+
    labs(x = "Game Age - Days", y = "% Winners Claimed")+
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
  
  per_claim_plot_list[[i]] <- p
}

#create list of best games to play for each $ amount
best_game_list <- list()
for (i in 1:nrow(amount_list)) {
  x<-game_df_list[[i]] %>% 
    arrange(desc(Date)) %>% 
    filter(Date == Date[1]) %>% 
    arrange(residual) %>% 
    select(Game,game_amount, game_age, percent_claimed_500, loess_fit, residual)
  
  best_game_list[[i]] <- x
}

#plot of tickets/age of game vs percent claimed for all game amounts 
tickets_age_per_claimed_plot   <- list()
for(i in 1:nrow(amount_list)){
  x <- game_df_list[[i]] %>% 
    arrange(desc(Date)) %>% 
    filter(Date == Date[1]) %>% 
    ggplot(aes(x = tickets_div_age, y = percent_claimed_500, color = Game))+
    geom_point()+
    geom_rect(aes(xmin = min(tickets_div_age), 
                ymin = min(percent_claimed_500),
                xmax = ((max(tickets_div_age)-min(tickets_div_age))/2+min(tickets_div_age)),
                ymax =((max(percent_claimed_500)-min(percent_claimed_500))/2+min(percent_claimed_500))),
            alpha = 0,
            color = "red",
            lty = 2)+
    geom_text(aes(label = 
                  ifelse(tickets_div_age < ((max(tickets_div_age)-min(tickets_div_age))/2+min(tickets_div_age))&
                        percent_claimed_500 < ((max(percent_claimed_500)-min(percent_claimed_500))/2+min(percent_claimed_500)),Game,"")),
            vjust = 1,
            hjust = 0,
            fontface = "bold",
            show.legend = FALSE)+
    theme_bw()+
    labs(x = "# Tickets / Age of Game",
       y = "% of Winning Tickets Claimed")+
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 12))
   
  tickets_age_per_claimed_plot[[i]] <- x
}

#plot of tickets in games vs odds for $500 winner for all game amounts
odds_500_tickets_plot <- list()
for(i in 1:nrow(amount_list)){
x<-game_df_list[[i]] %>% 
  arrange(desc(Date)) %>% 
  filter(Date == Date[1]) %>% 
  ggplot(aes(x = no_tickets, y = odds_500, color = Game))+
  geom_point()+
  geom_rect(aes(xmin = min(no_tickets), 
                ymin = min(odds_500),
                xmax = ((max(no_tickets)-min(no_tickets))/2+min(no_tickets)),
                ymax =((max(odds_500)-min(odds_500))/2+min(odds_500))),
            alpha = 0,
            color = "red",
            lty = 2)+
  geom_text(aes(label = 
                  ifelse(no_tickets < ((max(no_tickets)-min(no_tickets))/2+min(no_tickets))&
                           odds_500 < ((max(odds_500)-min(odds_500))/2+min(odds_500)),Game,"")),
            vjust = 1,
            hjust = 0,
            fontface = "bold",
            show.legend = FALSE)+
  scale_x_continuous(labels = scales::label_scientific())+
  theme_bw()+
  labs(x = "# Tickets in Game",
       y = "Odds of >$500 Winning Ticket")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12))

odds_500_tickets_plot[[i]] <- x
}
