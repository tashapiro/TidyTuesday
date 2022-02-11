library(ggplot2)
library(dplyr)
library(maps)

#import data
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

#clean up country names
olympics<-olympics%>%
  mutate(team2 = case_when(grepl("\\d", team) ~ gsub('[[:digit:]]+', '', team), TRUE ~team))%>%
  mutate(team = case_when(grepl("\\d", team) ~ substr(team2,1,nchar(team2)-1), TRUE ~team))%>%
  mutate(team = case_when(
                  team == 'United States' ~ "USA", 
                  team %in% c('Great Britain','England') ~ "UK",
                  TRUE ~team))


#get medals per team for winter olympics
by_country<-olympics%>%
  filter(season=="Winter" & year>=2000)%>%
  group_by(team)%>%
  summarise(gold_medals = length(id[medal=='Gold']),
            silver_medals = length(id[medal=='Silver']),
            bronze_medals = length(id[medal=='Bronze']),
            )%>%
  mutate(total_medals = gold_medals + silver_medals + bronze_medals)%>%
  arrange(-total_medals)

#get top sport per country based on total medals
by_country_sport<-olympics%>%
  filter(season=="Winter")%>%
  group_by(team, sport)%>%
  summarise(gold_medals = length(id[medal=='Gold']),
            silver_medals = length(id[medal=='Silver']),
            bronze_medals = length(id[medal=='Bronze']),
  )%>%
  mutate(total_medals = gold_medals + silver_medals + bronze_medals)%>%
  slice(which.max(total_medals))%>%
  select("team","sport","total_medals")%>%
  rename("top_sport_medals"="total_medals","top_sport"="sport")%>%
  arrange(-top_sport_medals)

#combine datasets together
data<-merge(by_country, by_country_sport, by="team")

#combine map data with olympics data. use dplyr left join to avoid omitting countries not included in olympics data!
map_data <- left_join(map, data, by=c("region"="team"))%>%
  #remove Antarctica
  filter(region!="Antarctica")

#custom color palette
pal_custom <-c  ("#666a86","#788aa3","#92b6b1","#b2c9ab","#e8ddb5","#f1bf98","#d3a588","#ee9480","#85414C")

ggplot(map_data, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=top_sport), color="white",size=0.2)+
  scale_fill_manual(na.value="grey", values = pal_custom, 
                       guide=guide_legend(title.position="top", 
                                          title.hjust=0.5))+
  #adjust the aspect ratio of x to y
  coord_map(xlim=c(-180,180))+
  #remove plot background, labels, etec
  labs(title="WINTER OLYPMICS: TOP SPORT PER COUNTRY",
       subtitle="Top sport determined by most medals received from 1924 to 2014",
       caption= "Data from www.sports-reference.com | Graph by @tanya_shapiro",
       fill="")+
  theme_void()+
  theme(
    legend.position="top",
    text = element_text(family="Gill Sans"),
    plot.title= element_text(hjust=0.5, size = 15, margin=margin(b=10)),
    plot.subtitle=element_text(hjust=0.5, margin=margin(b=15)),
    plot.margin= margin(t=25,b=10),
    legend.key.size = unit(0.5, 'cm'),
    legend.box.margin = margin(b=20)
  )

ggsave("winter_olympics.jpeg", height=6, width=9)
