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

df<-read_csv('/Users/jimauer/R/strikeouts/statcast.csv')

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

remove(df)

##### Building Run Expectancy table for 2021 ######

#making a table of the max score to end each inning
maxinningscore <- plays %>% group_by(gameid,inning,inning_topbot) %>% 
  summarise(inning_max_score=max(post_bat_score))

#creating a run expectancy table
retable<-left_join(plays %>% filter(game_type=="Regular Season",inning<9,event_type!="NA",
                 home_team!="COL"), 
  maxinningscore,
  by=c("gameid","inning","inning_topbot")) %>% 
  group_by(outs_before,basecode_before) %>% 
  summarise(re=mean(inning_max_score-bat_score_before)) %>% 
  ungroup()

retablegt<-retable %>% gt() %>%  
  tab_header(
    title="2021 Run Expectancy for Base-Out State",
    subtitle="") %>% 
  cols_label(outs_before="Outs Before",
             basecode_before="Base State",
             re="Run Expectancy") %>% 
  fmt_number(
    columns = c("re"),
    decimals = 2,
  ) %>% 
  cols_align(
    align="right",
    columns= basecode_before
  ) %>% 
  tab_source_note(
    source_note = "Data from www.baseballsavant.mlb.com"
  ) %>% gtsave('retable.png','/Users/jimauer/R/strikeouts')

#calculating the average starting run expectancy
starting_re <- left_join(plays %>% filter(game_type=="Regular Season",inning<9,event_type!="NA",
                           home_team!="COL") %>% 
            select(event_type,outs_before,basecode_before), 
         retable,
          by=c("outs_before","basecode_before")) %>% 
  group_by(event_type) %>% 
  summarise(starting_re=mean(re)) %>% 
  ungroup()



#recreating the run expextancy table 1 from The Book
RE_T1<-left_join(plays %>% filter(game_type=="Regular Season",inning<9,event_type!="NA",
                           home_team!="COL"), 
          maxinningscore,
          by=c("gameid","inning","inning_topbot")) %>% 
  group_by(event_type) %>% 
  summarise(n=n(),runs=sum(inning_max_score-bat_score_before),ave_runs=runs/n) %>% 
  arrange(desc(ave_runs)) %>% 
  ungroup()


#recreating the run expextancy table 2 from The Book
RE_T2 <- left_join(RE_T1,
          starting_re,
          by=c("event_type"))


#recreating the run expextancy table 3 from The Book
RE_T2 %>% mutate(run_value=(ave_runs-starting_re)) %>% 
  arrange(desc(run_value)) %>% 
  gt() %>%  
  tab_header(
    title="2021 Run Values",
    subtitle="") %>% 
  cols_label(event_type="Event",
             runs="Runs",
             ave_runs="Average Runs",
             starting_re="Starting RE",
             run_value="Run Value") %>% 
  fmt_number(
    columns = c("ave_runs", "starting_re","run_value"),
    decimals = 2,
  ) %>% 
  tab_source_note(
    source_note = "Data from www.baseballsavant.mlb.com"
  ) %>% 
  gtsave('expected runs.png','/Users/jimauer/R/strikeouts')
  



 
#### Building play data set that omits plays after the second strikeout#####  

#identifying all second out strikeouts
secondout <- plays %>% filter(game_type=='Regular Season',event_type=='strikeout',outs_before==1) %>% 
  select(event_type, game_type, outs_before, inning, inning_topbot, 
         gameid, pitch_number, batterid,pitcherid) %>% 
  arrange(desc(pitch_number)) %>% 
  group_by(gameid,inning,inning_topbot,batterid,pitcherid) %>% 
  summarise(fistpitch=min(pitch_number))

#identifying all first out strikeouts
firstout <- plays %>% filter(game_type=='Regular Season',event_type=='strikeout',outs_before==0) %>% 
  select(event_type, game_type, outs_before, inning, inning_topbot, 
         gameid, pitch_number, batterid,pitcherid) %>% 
  arrange(desc(pitch_number)) %>% 
  group_by(gameid,inning,inning_topbot,batterid,pitcherid) %>% 
  summarise(fistpitch=min(pitch_number))

#identifying all instances of first and second outs as strikouts
firstsec <- semi_join(firstout,secondout, by=c("gameid","inning","inning_topbot")) %>% 
  mutate(ident=paste0(gameid,inning,inning_topbot,2))

remove(firstout,secondout)

plays1 <- plays %>% mutate(ident=paste0(gameid,inning,inning_topbot,outs_before)) %>% 
  filter(!ident %in% firstsec$ident)

#making a table of the max score to end each inning
maxinningscore1 <- plays1 %>% group_by(gameid,inning,inning_topbot) %>% 
  summarise(inning_max_score=max(post_bat_score))

#creating a run expectancy table
retable1<-left_join(plays1 %>% filter(game_type=="Regular Season",inning<9,event_type!="NA",
                                    home_team!="COL"), 
                   maxinningscore1,
                   by=c("gameid","inning","inning_topbot")) %>% 
  group_by(outs_before,basecode_before) %>% 
  summarise(re=mean(inning_max_score-bat_score_before)) %>% 
  ungroup()

#calculating the average starting run expectancy
starting_re1 <- left_join(plays1 %>% filter(game_type=="Regular Season",inning<9,event_type!="NA",
                                          home_team!="COL") %>% 
                           select(event_type,outs_before,basecode_before), 
                         retable1,
                         by=c("outs_before","basecode_before")) %>% 
  group_by(event_type) %>% 
  summarise(starting_re=mean(re)) %>% 
  ungroup()


#recreating the run expextancy table 1 from The Book
RE_T11<-left_join(plays1 %>% filter(game_type=="Regular Season",inning<9,event_type!="NA",
                                  home_team!="COL"), 
                 maxinningscore1,
                 by=c("gameid","inning","inning_topbot")) %>% 
  group_by(event_type) %>% 
  summarise(n=n(),runs=sum(inning_max_score-bat_score_before),ave_runs=runs/n) %>% 
  arrange(desc(ave_runs)) %>% 
  ungroup()


#recreating the run expextancy table 2 from The Book
RE_T21 <- left_join(RE_T11,
                   starting_re1,
                   by=c("event_type"))


#recreating the run expextancy table 3 from The Book
RE_T21 %>% mutate(run_value=(ave_runs-starting_re)) %>% 
  arrange(desc(run_value)) %>% 
  gt() %>%  
  tab_header(
    title="2021 Run Values If 2 K's Ended an Inning",
    subtitle="") %>% 
  cols_label(event_type="Event",
             runs="Runs",
             ave_runs="Average Runs",
             starting_re="Starting RE",
             run_value="Run Value") %>% 
  fmt_number(
    columns = c("ave_runs", "starting_re","run_value"),
    decimals = 2,
  ) %>% 
  tab_source_note(
    source_note = "Data from www.baseballsavant.mlb.com"
  )


####Combining the two tables#####

full_join(RE_T2 %>% mutate(run_value=(ave_runs-starting_re)) %>% 
  arrange(desc(run_value)) %>% 
    select(event_type,run_value),
  RE_T21 %>% mutate(run_value_2K=(ave_runs-starting_re)) %>% 
    select(event_type,run_value_2K),
  by="event_type") %>% 
  gt() %>%  
  tab_header(
    title="2021 Run Values",
    subtitle="Standard vs If Inning Ended at 2K's") %>% 
  cols_label(event_type="Event",
             run_value="Run Value",
             run_value_2K="Run Value End @ 2K") %>% 
  fmt_number(
    columns = c("run_value","run_value_2K"),
    decimals = 2,
  ) %>% 
  tab_source_note(
    source_note = "Data from www.baseballsavant.mlb.com"
  )%>% 
  gtsave('expected runs two ks.png','/Users/jimauer/R/strikeouts')

#leaders in getting ks as first outs of inning
left_join(firstsec %>% group_by(pitcherid) %>% summarise(n=n()) %>% 
            arrange(desc(n)),
          chadwick %>% select(key_mlbam,name_first,name_last),
          by=c("pitcherid"="key_mlbam")) %>% 
  head(n=10) %>% 
  mutate(Name=paste(name_first,name_last)) %>% 
  select(-pitcherid,-name_first,-name_last) %>% 
  gt()%>%  
  tab_header(
    title="2021 Leaders in Getting Ks for 1st 2 Outs",
    subtitle="") %>% 
  cols_label(n="Two Ks to Start"
  ) %>% 
  tab_source_note(
    source_note = "Data from www.baseballsavant.mlb.com"
  ) %>% 
  gtsave('Leaders in ks.png','/Users/jimauer/R/strikeouts')


chadwick<-get_chadwick_lu()


######Some Options for Combining Categories of Events#######

#lumping some categories together
plays1$event_type <- gsub(".*caught_stealing.*","caught_stealing",plays1$event_type)
plays1$event_type <- gsub(".*pickoff.*","pickoff",plays1$event_type)
plays1$event_type <- gsub(".*fielders_choice.*","Out (on batted ball)",plays1$event_type)
plays1$event_type <- gsub(".*field_out.*","Out (on batted ball)",plays1$event_type)
plays1$event_type <- gsub(".*force_out.*","Out (on batted ball)",plays1$event_type)
plays1$event_type <- gsub(".*double_play.*","Out (on batted ball)",plays1$event_type)
plays1$event_type <- gsub(".*other_out.*","Out (on batted ball)",plays1$event_type)


#####Sample code for a GT table####
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


plays %>% filter(game_type=="Regular Season",inning<9,event_type!="NA",
                 home_team!="COL") %>% 
  group_by(gameid,inning,inning_topbot) %>% 
  summarise(n=n())
