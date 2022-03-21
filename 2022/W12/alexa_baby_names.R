library(tidyverse)
library(sysfonts)

#import fonts and set colors
font_add_google("Chivo", "chivo")
showtext_auto()

#data ----
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

data<-df%>%filter(name=='Alexa')%>%group_by(year)%>%summarise(n=sum(n))

note<-data.frame(x=c(2005, 2003),
                 y=c(2000, 6400),
                 label= c("2014 Amazon Alexa \n released in the U.S.",
                          "Peak Alexa Year - 2006")
)

points<-data.frame(x=2006, y=6118)

#colors
col_back<-'#1E2B3C'
col_or <- "#FF9A00"
col_bl <- "#007eb9"

# plot ----
ggplot(data, aes(x=year, y=n))+
  geom_line(color=col_or)+
  geom_point(data=points, mapping=aes(x=x,y=y), color=col_bl)+
  geom_vline(xintercept=2014, linetype='dashed', color="white")+
  geom_text(data=note, mapping=aes(x=x,y=y,label=label), size=3, color="white")+
  geom_curve(curvature=0.15, color=col_bl, 
             arrow = arrow(length = unit(0.07, "inch")), size=0.2,
             mapping=aes(x=2005, xend = 2013, y=1550, yend=1150))+
  labs(title="ALEXA, HOW POPULAR IS YOUR NAME?", 
       subtitle="History of baby names in the United States, specifically for Alexa",
       caption="Data from babynames R package | Chart by @tanya_shapiro",
       y="Count", x="Year")+
  scale_x_continuous(limits=c(1960,2017))+
  scale_y_continuous(limits=c(0,6500))+
  theme_minimal()+
  theme(text=element_text(family = "chivo", color="white"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="black", size=0.1),
        axis.title=element_text(face="bold"),
        axis.text =element_text(color="white"),
        plot.title=element_text(face="bold"),
        plot.background = element_rect(fill=col_back),
        plot.margin  = margin(t=10, b=10, r=10, l=10))

ggsave("alexa_names."