library(tidyverse)
library(ggimage)
library(sysfonts)
library(ggtext)
library(showtext)

sysfonts::font_add_google("Roboto", "Roboto")
#download font awesome locally first! https://fontawesome.com/download
font_add('fs', 'fonts/Font Awesome 6 Free-Solid-900.otf')
sysfonts::font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')


showtext_auto()
#adjust dpi in showtext -- fix issues with saving (showtext + ggtext problem)
showtext::showtext_opts(dpi = 300)

#dowbload data
wcmatches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

#subset to specific year - 2014
matches<-wcmatches|>filter(year==2014)

#reshape data part 1
by_game<-matches|>
  select(-year, -country, -city, -month, -dayofweek)|>
  filter(grepl("Group",stage))|>
  pivot_longer(cols=c("home_team","away_team"), names_to="home_away", values_to="team")|>
  mutate(gf = case_when(home_away=="home_team"~home_score,
                        home_away=="away_team"~away_score),
         ga = case_when(home_away=="away_team"~home_score,
                        home_away=="home_team"~away_score),
         w = case_when(team==winning_team ~ 1, TRUE ~ 0),
         l = case_when(team==losing_team ~ 1, TRUE ~ 0),
         d = case_when(gf==ga ~ 1, TRUE ~ 0)
         )|>
  rename(group=stage)


#reshape the data part 2 - get aggregates for metrics
group_summary<-by_game|>
  select(group, team, gf, ga, w, l, d)|>
  group_by(group, team)|>
  summarise_all(sum)|>
  mutate(gd = gf - ga, 
         pts = w*3 + d*1)|>
  arrange(group, -pts, -gd)|>
  group_by(group)|>
  mutate(rank = row_number())|>
  select(rank, group, team, w, d, l, gf, ga, gd, pts)|>
  ungroup()

#reshape the data part 3 - get list of outcomes
group_outcomes<-by_game|>
  select(group, team, date, w, l, d)|>
  mutate(outcome = case_when(w==1 ~ "W", d==1 ~ "D", l==1 ~ "L"),
         outcome_icon = case_when(w==1 ~ "<span style='font-family:fs;color:#29C458;'>&#xf058;</span>", 
                                  d==1 ~ "<span style='font-family:fs;color:#6F6F6F;'>&#xf056;</span>", 
                                  l==1 ~ "<span style='font-family:fs;color:#D31818;'>&#xf057;</span>"))|>
  arrange(group, team, date)|>
  group_by(team)|>
  mutate(seq = row_number(),
         seq = case_when(seq==1~ 12.2, seq==2~12.9, seq==3~13.6))|>
  select(group, team, outcome, outcome_icon, seq)|>
  left_join(group_summary|>distinct(team, rank), by=c("team"))
 



df_plot<-group_summary|>
  mutate(icon = paste0("flag-icons/",str_replace_all(tolower(team)," ","-"),".png"),
         team=str_replace(team," and "," & "))


spacing= seq(from=6, to=11, length.out=7)

df_plot_long<-df_plot|>
  pivot_longer(cols=c(w,d,l,gf,ga,gd,pts))|>
  mutate(xpos = case_when(name=="w" ~ spacing[1],
                          name=="d" ~ spacing[2], 
                          name=="l" ~ spacing[3],
                          name=="gf" ~spacing[4], 
                          name=="ga" ~spacing[5], 
                          name=="gd" ~spacing[6],
                          name=="pts" ~spacing[7]))

#df for table header
headers = data.frame(
  text = c("Team","W","D","L","GF","GA","GD","Pts","Matches"),
  xpos = c(0.7, spacing, 12.9)
)


#html text for title & caption - use with ggtext::element_textbox_simple in theme
title = paste0(
  "<span style='font-family:Roboto;font-size:17pt;'>**2014 FIFA World Cup | Group Stage** </span><br>",
  "<span style='font-family:Roboto;font-size:10pt;color:#5D5D5D;'>Data from Kaggle. Graphic created in the style of Google's FIFA summary.</span>"
)

caption = paste0(
  "<span style='font-family:Roboto;color:#5D5D5D;font-size:8pt;'>Matches</span><br>",
  "<span style='font-family:fs;color:#29C458;'>&#xf058;</span>",
  "<span style='font-family:Roboto;color:white;font-size:7pt;'>..</span>",
  "<span style='font-family:Roboto;color:#5D5D5D;font-size:7pt;'> Win</span>",
  "<span style='font-family:Roboto;color:white;font-size:7pt;'>...</span>",
  "<span style='font-family:fs;color:#6F6F6F;'>&#xf056;</span>", 
  "<span style='font-family:Roboto;color:white;font-size:9pt;'>..</span>",
  "<span style='font-family:Roboto;color:#5D5D5D;font-size:7pt;'> Draw</span>",
  "<span style='font-family:Roboto;color:white;font-size:7pt;'>...</span>",
  "<span style='font-family:fs;color:#D31818;'>&#xf057;</span>", 
  "<span style='font-family:Roboto;color:white;font-size:9pt;'>..</span>",
  "<span style='font-family:Roboto;color:#5D5D5D;font-size:7pt;'> Loss</span><br><br>",
  "<span style='font-family:Roboto;color:#5D5D5D;font-size:7pt;'>Flag icons from Freepik on flaticon.com</span><br>",
  "<span style='font-family:fb;color:#5D5D5D;font-size:7pt;'>&#xf4f6;</span>",
  "<span style='font-family:Roboto;font-size:7pt;color:#5D5D5D;'> fosstodon.org/tanya_shapiro</span>",
  "<span style = 'color:white;;font-size:7pt;'>...</span>",
  "<span style='font-family:fb;color:#5D5D5D;;font-size:7pt;'>&#xf099;</span>",
  "<span style='font-family:Roboto;color:#5D5D5D;;font-size:7pt;'> tanya_shapiro</span>",
  "<span style = 'color:white;font-size:7pt;'>...</span>",
  "<span style='font-family:fb;color:#5D5D5D;font-size:7pt;'  >&#xf09b;</span>",
  "<span style='font-family:Roboto;color:#5D5D5D;font-size:7pt;'> tashapiro </span>"
)


text_size=2.4

#PLOT
ggplot(data=df_plot)+
  #headers
  geom_text(data=headers, mapping=aes(x=xpos, y=0.2, label=text), size=2.2, color="grey40", family="Roboto")+
  #team
  geom_text(mapping=aes(x=1.5, y=rank, label=team), hjust=0, size=text_size, family="Roboto")+
  #team flag
  geom_image(mapping=aes(x=1, y=rank, image=icon), asp=14/4.5, size=0.04)+
  #rank
  geom_text(mapping=aes(x=0.25, y=rank, label=rank), hjust=0, size=text_size, family="Roboto")+
  #table lines
  geom_segment(mapping=aes(x=0, xend=14, y=rank-0.5, yend=rank-0.5), linewidth=0.2, color="#DADADA")+
  #points data 
  geom_text(data=df_plot_long, mapping=aes(x=xpos, y=rank, label=value), size=text_size, family="Roboto")+
  #outcomes
  geom_textbox(data=group_outcomes, mapping=aes(y=rank, x=seq, label=outcome_icon),  
               fill = NA, box.size=NA, size= 3, halign=0.5, show.legend=FALSE, size=2)+
  #facet/pivot by group
  facet_wrap(~group, ncol=2)+
  #adjust scales for axis and coordinates
  scale_y_reverse(limits=c(4.5,0), expand=c(0,0))+
  scale_x_continuous(limits=c(0,14), expand=c(0,0))+
  #title
  labs(title=title, caption=caption)+
  theme_minimal()+
  theme(
    axis.text=element_blank(),
    axis.title=element_blank(),
    panel.grid = element_blank(),
    panel.spacing.x = unit(1.5,"lines"),
    plot.title=element_textbox_simple(margin=ggplot2::margin(b=5)),
    plot.caption = element_textbox_simple(margin=margin(t=5)),
    plot.margin = margin(t=15, b=5),
    strip.text=element_text(face="bold", hjust=0)
  )+
  coord_equal()


#save plot
ggsave("fifa_2014_group.png", bg="white", height=7, width=7.5, units='in')
