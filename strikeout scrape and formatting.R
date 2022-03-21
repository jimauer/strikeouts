library(baseballr)
library(tidyverse)
library(gt)

#function to get statcast pitch level data
scrape_statcast <- function(season) {
  
  # create weeks of dates for season from mar - nov
  # includes spring training + postseason
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = 'week')
  
  date_grid <- tibble(start_date = dates, 
                      end_date = dates + 6)
  
  # create 'safe' version of scrape_statcast_savant in case week doesn't process
  safe_savant <- safely(scrape_statcast_savant)
  
  # loop over each row of date_grid, and collect each week in a df
  payload <- map(.x = seq_along(date_grid$start_date), 
                 ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                   
                   payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                          end_date = date_grid$end_date[.x], type = 'pitcher')
                   
                   return(payload)
                 })
  
  payload_df <- map(payload, 'result')
  
  # eliminate results with an empty dataframe
  number_rows <- map_df(.x = seq_along(payload_df), 
                        ~{number_rows <- tibble(week = .x, 
                                                number_rows = length(payload_df[[.x]]$game_date))}) %>%
    filter(number_rows > 0) %>%
    pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  combined <- payload_df_reduced %>%
    bind_rows()
  
  return(combined)
}

df <- scrape_statcast(2021)

write_csv(df,'/Users/jimauer/R/strikeouts/statcast.csv')

#formatting data
format_statcast<-function(data){
  
  
df1<- data %>%
  mutate(pitch_type = ifelse(pitch_type == "", "UN", pitch_type),
         # expand game_type codes for clarity
         game_type = case_when(
           game_type == "E" ~ "Exhibition",
           game_type == "S" ~ "Spring Training",
           game_type == "R" ~ "Regular Season",
           game_type == "F" ~ "Wild Card",
           game_type == "D" ~ "Divisional Series",
           game_type == "L" ~ "League Championship Series",
           game_type == "W" ~ "World Series"),
         # create binary handedness indicators
         is_lhb = ifelse(stand == "L", "1", "0"),
         is_lhp = ifelse(p_throws == "L", "1", "0"),
         # create fielderid to accompany hit_location, giving the
         fielderid = case_when(
           hit_location == "1" ~ as.character(pitcher),
           hit_location == "2" ~ as.character(fielder_2),
           hit_location == "3" ~ as.character(fielder_3),
           hit_location == "4" ~ as.character(fielder_4),
           hit_location == "5" ~ as.character(fielder_5),
           hit_location == "6" ~ as.character(fielder_6),
           hit_location == "7" ~ as.character(fielder_7),
           hit_location == "8" ~ as.character(fielder_8),
           hit_location == "9" ~ as.character(fielder_9)),
         fielderid = as.numeric(fielderid),
         # binary inning half indicator
         is_bottom = ifelse(inning_topbot == "Bot", "1", "0"),
         # add spray angle 
         spray_angle = round(atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75, 1),
         # standardize team abbreviations (some deprecated)
         home_team = case_when(
           home_team == "FLA" ~ "MIA",
           home_team == "KC" ~ "KCR",
           home_team == "SD" ~ "SDP",
           home_team == "SF" ~ "SFG",
           home_team == "TB" ~ "TBR",
           TRUE ~ home_team),
         away_team = case_when(
           away_team == "FLA" ~ "MIA",
           away_team == "KC" ~ "KCR",
           away_team == "SD" ~ "SDP",
           away_team == "SF" ~ "SFG",
           away_team == "TB" ~ "TBR",
           TRUE ~ away_team),
         # runner status
         run_on_1b = ifelse(on_1b == "0", NA, on_1b),
         run_on_2b = ifelse(on_2b == "0", NA, on_2b),
         run_on_3b = ifelse(on_3b == "0", NA, on_3b),
         # pitch information
         is_bip = ifelse(type == "X", 1, 0),
         is_stk = ifelse(type == "S", 1, 0),
         # baseout state before PA event
         basecode_before = case_when(
           is.na(run_on_1b) & is.na(run_on_2b) & is.na(run_on_3b) ~ "000",
           is.na(run_on_1b) & is.na(run_on_2b) ~ "001",
           is.na(run_on_1b) & is.na(run_on_3b) ~ "010",
           is.na(run_on_2b) & is.na(run_on_3b) ~ "100",
           is.na(run_on_3b) ~ "110",
           is.na(run_on_2b) ~ "101",
           is.na(run_on_1b) ~ "011",
           TRUE ~ "111")) %>%
  rename(vis_team = away_team,
         batterid = batter,
         pitcherid = pitcher,
         event_type = events,
         event_description = des,
         pitch_description = description,
         outs_before = outs_when_up,
         hit_distance = hit_distance_sc,
         pa_number = at_bat_number,
         bat_score_before = bat_score,
         gameid = game_pk,
         field_score = fld_score,
         release_spin = release_spin_rate) %>%
  arrange(game_date, gameid, pa_number, pitch_number)

return(df1)
}

plays <- format_statcast(df)

#identifying all second out strikeouts
secondout <- plays %>% filter(game_type=='Regular Season',event_type=='strikeout',outs_before==1) %>% 
  select(event_type, game_type, outs_before, inning, inning_topbot, 
                 gameid, pitch_number, batterid) %>% 
  arrange(desc(pitch_number)) %>% 
  group_by(gameid,inning,inning_topbot,batterid) %>% 
  summarise(fistpitch=min(pitch_number))

#identifying all first out strikeouts
firstout <- plays %>% filter(game_type=='Regular Season',event_type=='strikeout',outs_before==0) %>% 
  select(event_type, game_type, outs_before, inning, inning_topbot, 
         gameid, pitch_number, batterid) %>% 
  arrange(desc(pitch_number)) %>% 
  group_by(gameid,inning,inning_topbot,batterid) %>% 
  summarise(fistpitch=min(pitch_number))

#identifying all instances of first and second outs as strikouts
firstsec <- semi_join(firstout,secondout, by=c("gameid","inning","inning_topbot")) %>% 
  mutate(ident=paste0(gameid,inning,inning_topbot,2))

remove(firstout,secondout)

#selecting columns and creating a column for runs to end of inning
plays1<-left_join(plays %>% filter(game_type=="Regular Season",inning<9,event_type!="NA",
                 home_team!="COL") %>% 
  mutate() %>%
  select(gameid,inning,inning_topbot,outs_before,basecode_before,event_type,bat_score_before),
  
  plays %>% select(gameid,inning,inning_topbot,pa_number,post_bat_score) %>% 
    group_by(gameid,inning,inning_topbot) %>% 
    summarise(end_inn_score=max(post_bat_score)),
  by=c('gameid','inning','inning_topbot')) %>% 
  mutate(runs=end_inn_score-bat_score_before)

#lumping some categories together
plays1$event_type <- gsub(".*caught_stealing.*","caught_stealing",plays1$event_type)
plays1$event_type <- gsub(".*pickoff.*","pickoff",plays1$event_type)
plays1$event_type <- gsub(".*fielders_choice.*","Out (on batted ball)",plays1$event_type)
plays1$event_type <- gsub(".*field_out.*","Out (on batted ball)",plays1$event_type)
plays1$event_type <- gsub(".*force_out.*","Out (on batted ball)",plays1$event_type)
plays1$event_type <- gsub(".*double_play.*","Out (on batted ball)",plays1$event_type)
plays1$event_type <- gsub(".*other_out.*","Out (on batted ball)",plays1$event_type)


#run value table for the 2021 season
runvalue<-plays1 %>% mutate() %>% group_by(event_type) %>% 
  summarise(n=n(),tot_runs=sum(runs),mean_runs=mean(runs)) %>% 
  arrange(desc(mean_runs))

#run value toble omitting anything happening after a second strikout in an inning
runvaluetwostrikout <- plays1 %>% mutate(ident=paste0(gameid,inning,inning_topbot,outs_before)) %>% 
  filter(!ident %in% firstsec$ident) %>% 
  group_by(event_type) %>% 
  summarise(n=n(),tot_runs=sum(runs),mean_runs=mean(runs)) %>% 
  arrange(desc(mean_runs))

full_join(runvalue%>% select(event_type,mean_runs),
          runvaluetwostrikout %>% select(event_type,mean_runs) %>% 
            rename(mean_runs_end_2K=mean_runs),
          by="event_type") %>% 
  filter(mean_runs!=0) %>% 
  gt() %>% 
  tab_header(
    title="2021 Run Values",
    subtitle="Run Values vs Run Values if Inning Ended at Two Stirkouts") %>% 
  cols_label(event_type="Event",
             mean_runs="2021 Run Values",
             mean_runs_end_2K="Run Value 2 K's") %>%
  fmt_number(
    columns = c("mean_runs", "mean_runs_end_2K"),
    decimals = 2,
  ) %>% 
  tab_source_note(
    source_note = "Data from www.baseballsavant.mlb.com"
  )
