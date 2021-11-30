library(ggplot2)
library(dplyr)
library(tidyr)
library(DescTools)
library(reshape2)
library(extrafont)
library(data.table)

df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv")

#Multiple Roles listed and separated by ';' - max found were 5 roles in one episode
df<-df%>%separate(role, c('Role1','Role2','Role3','Role4','Role5'), ";")

#melt data to see records on role and episode level
df<-melt(df, id=c("season","number","production_code","episode_title","guest_star"))

#remove NAs
df <- na.omit(df)

#rename column
colnames(df)[colnames(df)=="value"] <- "role"

#create new dataset groubed by guest star
df2<-df%>%group_by(guest_star)%>%
  summarise(appearences=n_distinct(production_code),
            seasons=n_distinct(season),
            roles=n_distinct(role),
            self_portrayal=n_distinct(production_code[role %like% '%self%' | role %like% '%selves%'])
            )

#only look at top guest stars (by # of appearences) - take out Marcia, skews data 
data<-df2%>%filter(guest_star != 'Marcia Wallace')%>%top_n(20,appearences)

roles<-df%>%group_by(guest_star,role)%>%summarise(total=n_distinct(production_code))

top_role<-setDT(roles)[, .SD[which.max(total)], guest_star]

data2<-merge(data,top_role,by=c('guest_star'='guest_star'))

data2$category[data2$total==data2$self_portrayal]='Self Portrayal'
data2$category[data2$total>data2$self_portrayal]='Other Character'

#Commented this out - code to download/import new fonts, need Simpsonfont
#font_import()
#loadfonts(device="win")

#Simposns Color Palette
blue<-'#009DDC'
yellow<-'#FED41D'
flame<-'#F14E28'
white<-'#FFFFFF'

p<-ggplot()+
  geom_bar(data=data, 
           mapping=aes(x=reorder(guest_star,appearences),y=appearences), 
           fill=yellow,
           stat='identity')+
  geom_point(data=data2%>%filter(total!=self_portrayal & self_portrayal>0), 
             mapping=aes(x=reorder(guest_star,appearences),y=self_portrayal),
             color=flame,
             shape=8,
             size=3)+
  geom_point(data=data.frame(x=8, y=22), 
             mapping=aes(x=x,y=y),
             color=flame,
             shape=8,
             size=3)+
  geom_point(data=data2%>%filter(total>1), 
             mapping=aes(x=reorder(guest_star,appearences),y=total, color=category), 
             size=3)+
  geom_text(data=data2%>%filter(total>1), 
             mapping=aes(x=reorder(guest_star,appearences),y=total+.6, label=role), 
             color='black',
             position=position_dodge(0.9),
             family='Karla',
             hjust=0,
             size=3.5)+
  geom_text(data=data.frame(x=23,y=21.5), mapping=aes(x = x, y = y),
            fontface="italic",
            position=position_dodge(0.9),
            hjust=0,
            family='Karla', 
            size=3.5,
            label ='Most dynamic guest star. Played 32 different roles!')+
  scale_colour_manual(values=c(blue,flame))+
  geom_text(data=data.frame(x=8,y=23.5), mapping=aes(x = x, y = y),
            fontface="italic",
            position=position_dodge(0.9),
            hjust=0,
            family='Karla', 
            label ='Number of appearences as themselves. \n Less than most popular character.')+
  coord_flip()+
  theme_minimal()+
  theme(
    plot.title=element_text(family='Simpsonfont', size=rel(2)),
    plot.subtitle = element_text(face='italic'),
    title=element_text(face='bold',color='black', size=rel(1)),
    axis.title=element_text(family='Akbar'),
    axis.text = element_text(size=rel(1)),
    text=element_text(family="Karla",color='black'),
    #title=element_text(face="bold"),
    panel.background = element_rect(fill=white, color=white),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.y=element_blank(),
    plot.background = element_rect(fill=white, color=white)
  )+
  labs(
       title = 'The Simpsons guest stars',
       subtitle=' Most popular guest stars, their most notable roles and self portrayals.\n Excl. Marcia Wallace, aka Edna Krabapple (appeared in 156 episodes). Data from Wikipedia. ',
       y='# of Appearences (Episodes)',
       color='Most Popular Character',
       x='Guest Star')

p


ggsave("Simpsons Guests.png", plot = p, width = 30, height = 20, units = "cm")
