library(dplyr)
library(gt)
library(gtExtras)
library(ggflags)
library(paletteer)
library(systemfonts)
#if you dont have gtExtras-or ggflags uncomment below to install
#remotes::install_github("jthomasmock/gtExtras")
#devtools::install_github("rensa/ggflags")
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

#convert character to date
df$match_date<-as.Date(df$match_date,'%b %d, %Y')

#get info for team 1
t1<-df%>%select(team1, score_team1, wickets_team, team2, winner, margin, series, match_date, match_id)%>%
rename(team = team1, score=score_team1, wickets=wickets_team, opponent=team2)

#get info for team 2
t2<-df%>%select(team2, score_team2, wickets_team2, team1, winner, margin, series, match_date, match_id)%>%
  rename(team = team2, score=score_team2, wickets=wickets_team2, opponent=team1)

#combine team 1 and team 2 data, create vertical format for data
df_new<-rbind(t1,t2)
#create win indicator, if team name is the same as the winner name, then 1 (won), or 0 (lost)
df_new$win<-ifelse(df_new$team==df$winner,1,0)

#get data just for World Cup 1996
df_1996<-df_new%>%filter(match_date>=as.Date("1996-02-16") & match_date<=as.Date("1996-03-17"))%>%
  group_by(team)%>%
  summarise(score=sum(score),
            wickets=sum(wickets),
            games=n(),
            avg_score=round(sum(score)/n(),0),
            wins=sum(win),
            losses=n()-sum(win),
            win_perc=round(sum(win)/n(),2))%>%
  #arrange data set based on wins
  arrange(desc(wins, win_perc))%>%
  #add flag images - images taken from https://www.flaticon.com/packs/countrys-flags
  mutate(
    flag = case_when(
      team == "Sri Lanka" ~ 'https://cdn-icons-png.flaticon.com/512/197/197398.png',
      team == "South Africa" ~ 'https://cdn-icons-png.flaticon.com/512/197/197562.png',
      team == "Pakistan" ~ 'https://cdn-icons-png.flaticon.com/512/197/197606.png',
      team == "India" ~ 'https://cdn-icons-png.flaticon.com/512/197/197419.png',
      team == "Australia" ~ 'https://cdn-icons-png.flaticon.com/512/197/197507.png',
      team == "Netherlands" ~'https://cdn-icons-png.flaticon.com/512/197/197441.png',
      team == "United Arab Emirates" ~'https://cdn-icons-png.flaticon.com/512/197/197569.png',
      team == "Kenya" ~'https://cdn-icons-png.flaticon.com/512/197/197608.png',
      team == "Zimbabwe" ~'https://cdn-icons-png.flaticon.com/512/197/197394.png',
      team == "England" ~'https://cdn-icons-png.flaticon.com/512/197/197485.png',
      team == "New Zealand" ~'https://cdn-icons-png.flaticon.com/512/197/197589.png',
      team == "West Indies" ~'https://s.ndtvimg.com/images/entities/300/west-indies-2119.png'
    ))%>%
  select(flag, everything())

#create list of scores
scores<-df_new%>%filter(match_date>=as.Date("1996-02-16") & match_date<=as.Date("1996-03-17"))%>%
  arrange(match_date)%>%group_by(team)%>%summarise(
  scores_data = list(score), .groups = "drop")
#create list of wins and losses
wins_losses<-df_new %>% 
  filter(match_date>=as.Date("1996-02-16") & match_date<=as.Date("1996-03-17"))%>%
  arrange(match_date)%>%
  group_by(team) %>% 
  summarise(outcomes = list(win), .groups = "drop")%>%
  select(team,outcomes)

#merge data sets
df_1996<-left_join(df_1996,scores,by=c("team"="team"))
df_1996<-left_join(df_1996,wins_losses,by=c("team"="team"))

#create table
table<-df_1996%>%
  select(flag, team, score, wickets, games, wins, losses, avg_score, scores_data, outcomes, win_perc)%>%
  gt()%>%
  gt_img_rows(flag)%>%
  fmt_percent(columns = win_perc, decimals = 0)%>%
  gt_color_rows(win_perc, palette = "ggsci::blue_material")%>%
  gt_sparkline(scores_data,  line_color = "#505050")%>%
  gt_plt_winloss(outcomes, max_wins = 16)%>%
  gt_theme_espn()%>%
  cols_align(
    align = "center",
    columns = c(scores_data, flag, outcomes)
  )%>%
  tab_footnote("Outcomes represents wins (blue) and losses (red) for all games played",
               locations = cells_column_labels(columns = outcomes))%>%
  cols_label(
    flag = "",
    win_perc = "WIN %",
    score = "TOTAL PTS",
    scores_data = "PTS PER GAME",
    avg_score = "AVG PTS"
  )%>%
  tab_options(heading.title.font.size = 20)%>%
  tab_header(title = "CRICKET WORLD CUP 1996",
             subtitle= "Outcomes summary by team from Willis World Cup")%>%
  tab_source_note("Data from ESPN | Table Graphic by @tanya_shapiro")

table
