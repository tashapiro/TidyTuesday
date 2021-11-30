library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

#Import Data
wwc <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

#Create new columns for wins, losses, and ties

#Wins
wwc$W[wwc$win_status=="Won"]=1
wwc$W[wwc$win_status!="Won"]=0

#Losses
wwc$L[wwc$win_status=="Lost"]=1
wwc$L[wwc$win_status!="Lost"]=0

#Ties
wwc$Tie[wwc$win_status=="Tie"]=1
wwc$Tie[wwc$win_status!="Tie"]=0

#Standard count - makes it easy to summarize with dplyr later
wwc$count<-1

#Get list of all years, teams, rounds - use to complete the dataframes in dplyr 
years<-unique(wwc$year)
teams<-unique(wwc$team)
rounds<-unique(wwc$round)


#Data Frame 1 - Summarize # of games by Year & Team 
df<-wwc%>%group_by(year)%>%
  complete(team= teams)%>%
  group_by(year,team)%>%
  summarise(total_games=sum(count),
            group_games=sum(count[round=='Group']),
            round16_games=sum(count[round=='Round of 16']),
            qfinal_games=sum(count[round=='Quarter Final']),
            sfinal_games=sum(count[round=='Semi Final']),
            final_games=sum(count[round=='Final']),
            wfinal_games=sum(count[round=='Final'& W==1])
            )

#What was the furthest level reached by each team during the WC?
df$top_level[is.na(df$total_games)]="DNQ"
df$top_level[df$group_games>=1]="Group"
df$top_level[df$round16_games==1]="R16"
df$top_level[df$qfinal_games==1]="Quarter Final"
df$top_level[df$sfinal_games==1]="Semi Final"
df$top_level[df$wfinal_games!=1 & df$final==1]="Final"
df$top_level[df$wfinal_games==1]="Winner"

#create a factor to organize top levels
df$top_level<-factor(df$top_level, levels=c("DNQ","Group","R16","Quarter Final","Semi Final","Final","Winner"))



t<-wwc%>%group_by(year)%>%
  complete(team= teams, round=rounds)%>%
  group_by(year,team, round)%>%
  summarise(goals=sum(score),
            games=sum(count),
            wins=sum(W),
            losses=sum(L),
            ties=sum(Tie))

#Data Frame to see how many World Cups each team competed in
y<-wwc%>%group_by(team)%>%summarise(yrs=n_distinct(year))

t<-merge(t,df,by=c("team"="team","year"="year"))
t<-merge(t,y,by=c("team"="team"))
t<-merge(t,codes,by=c("team"="team"))

#Create Status based on how each team performed during each round/year
t$status[is.na(t$total_games)]="DNQ"
t$status[t$total_games>0 & t$round=="Group" & t$top_level=="Group"]="Lost"
t$status[t$total_games>0 & t$round=="Group" & t$top_level!="Group"]="Won"
t$status[t$total_games>0 & is.na(t$games)]="Prev Eliminated"
t$status[t$round!="Group" & t$wins==1]="Won"
t$status[t$round!="Group" & t$losses==1]="Lost"
t$status[t$round!="Group" & t$ties==1]="Tied"
t$status[t$round=="Round of 16" & t$year<2015]="NA"

#Remove Third Place Playoff
t<-t%>%filter(round != "Third Place Playoff")

#Create factor for rounds to organize chronologically instead of by alphabetical order
t$round<-factor(t$round, levels=c("Group","Round of 16","Quarter Final","Semi Final","Final"))


#Create factor for status
t$status<-factor(t$status, levels=c("Won","Lost","Prev Eliminated","NA","DNQ"))


#Color Palette inspired by FIFA 2019
blue<-'#00B5ED'
red<-'#D6000A'
yellow<-'#FDDB00'
lgrey<-'#E8E8E8'
dgrey<-'#404040'

palette<-c(blue,red,yellow,dgrey,lgrey)

#Creating Plot
heat<-ggplot(t, aes(x=round, y=reorder(country,yrs), fill=status))+
  geom_tile(colour="white", , 
            width=.9, height=.9) + 
  facet_grid(~year) + 
  theme_minimal() +
  scale_fill_manual(values=palette)+
  coord_fixed()+
  theme(
    plot.title=element_text(face="bold",size=12),
    title=element_text(face="bold"),
    plot.subtitle=element_text(face="italic"),
    legend.text=element_text(colour="grey20",size=10),
    axis.ticks.y=element_blank(),
    axis.text.x=element_text(angle = 90),
    panel.grid=element_blank(),
    strip.text.x = element_text(face="bold")
  )+
  labs(
    title="FIFA Women's World Cup",
    subtitle="Advancement of teams by year and round",
    x="YEAR | ROUND",
    y="TEAM",
    fill="Round Status"
  )

heat

#Add Annotations bottom of the plot
p<-add_sub(heat, "NA - Round of 16 not introduced until 2015", fontface="italic", size=11, x = 0, hjust = 0)
ggdraw(p)

#Render image
ggsave("FIFA Women.png", plot = p, width = 50, height = 30, units = "cm")





