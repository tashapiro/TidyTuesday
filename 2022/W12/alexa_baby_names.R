library(tidyverse)
library(sysfonts)

#import fonts and set colors
font_add_google("Chivo", "chivo")
showtext_auto()

#data ----
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

data<-df%>%
  group_by(year)%>%
  summarise(count=sum(n[name=='Alexa']),
            total=sum(n))%>%
  mutate(perc_total = count/total)

note<-data.frame(x=c(2018, 2003),
                 y=c(0.00175, 0.0005),
                 label= c("Peak Year \n 2015", "2014 Amazon Alexa \n released in the U.S.")
)

points<-data.frame(x=2015, y=0.001643945)

#colors
col_back<-'#1E2B3C'
col_or <- "#FF9A00"
col_bl <- "#3EA4D3"

# plot ----
ggplot(data, aes(x=year, y=perc_total))+
  geom_line(color=col_or, size=0.8)+
  geom_point(data=points, mapping=aes(x=x,y=y), color=col_bl)+
  geom_vline(xintercept=2014, linetype='dashed', color="white")+
  geom_text(data=note, mapping=aes(x=x,y=y,label=label), size=3.5, color="white")+
  geom_curve(curvature=0.15, color=col_bl, 
             arrow = arrow(length = unit(0.07, "inch")), size=0.2,
             mapping=aes(x=2004, xend = 2013, y=0.00035, yend=0.0002))+
  labs(title="ALEXA, HOW POPULAR IS YOUR NAME?", 
       subtitle="Popularity of Alexa baby name in the United States from 1960 to 2017",
       caption="Data from babynames R package | Chart by @tanya_shapiro",
       y="% of Total", x="Year")+
  scale_x_continuous(limits=c(1960,2020))+
  scale_y_continuous(limits=c(0,0.0018), labels=scales::label_percent(accuracy=0.01))+
  theme_minimal()+
  theme(text=element_text(family = "chivo", color="white"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="black", size=0.1),
        axis.title=element_text(face="bold"),
        axis.text =element_text(color="white"),
        axis.title.y=element_text(margin=margin(r=10)),
        plot.title=element_text(face="bold"),
        plot.background = element_rect(fill=col_back),
        plot.margin  = margin(t=20, b=20, r=10, l=10))

ggsave("alexa_names.png", height=5, width=7.5)
