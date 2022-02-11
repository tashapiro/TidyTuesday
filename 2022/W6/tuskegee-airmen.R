library(tidyverse)
library(ggimage)
library(ggtext)

airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')


df<-airmen%>%
  filter(graduation_date<as.Date('1945-09-02'))%>%
  group_by(class,graduated_from, graduation_date)%>%
  summarise(graduates = n())

df$graduation_date<-as.Date(df$graduation_date)
df$icon<-"plane_icon.png"


#create calendar of dates
month <- seq(as.Date('1942-01-01'),as.Date('1945-09-01'),by="month")
year <- format(month, '%Y')
month_abbr <-format(month, '%b')
month_num <- format(month, '%m')
cal<-data.frame(month, year, month_abbr, month_num)

#aesthetics
col_line = 'black'
col_back = '#DEB51E'
col_point = '#12192D'

#comments
text<-c("Captain Benjamin Oliver Davis Jr. graduates with Class SE-42-C. Davis was the commander of the 99th Fighter Squandron and the first black general in the U.S. Army.",
        "Class SE-44-E had the biggest class size with 30 graduates.")
x <- c(as.Date('1942-03-06'), as.Date('1944-05-23'))
yend <- c(10,27)
ystart = c(5,30)
comments<-data.frame(text,x,y)

ggplot(df)+
  #timelnine
  geom_hline(yintercept=0, color=col_line)+
  #airplane points
  geom_image(mapping=aes(x=graduation_date, y=graduates, image=icon), size=0.05, asp=1.5)+
  #timeline text and tick marks
  geom_label(data = cal%>%filter(month_abbr=="Jan"), mapping= aes(x =month, y=-0.8, label=year), color=col_back, fill="black", family="Gill Sans Bold")+
  geom_segment(data = cal%>%filter(month_abbr=="Jan"), mapping=aes(x=month, xend=month, y=-0.025, yend=0.025), color=col_line)+
  geom_text(data = cal%>%filter(month_abbr %in% c("Apr","Jul","Oct")), mapping= aes(x =month, y=-.8, label=month_abbr), family="Gill Sans", size=4)+
  geom_segment(data = cal%>%filter(month_abbr %in% c("Apr","Jul","Oct")), mapping=aes(x=month, xend=month, y=-0.015, yend=0.015), color=col_line)+
  #line for comments
  geom_segment(data = comments, y=yend, yend=ystart, x=x, xend=x, linetype=2)+
  geom_textbox(data=comments, aes(x=x,y=yend,label=text), size=3,  col="black",
               fill=col_back,
               width = unit(0.15, "npc"),
               box.color="black")+
  theme_void()+
  scale_y_continuous(limits=c(-1,32), breaks = c(10,20,30))+
  #scale_size_continuous(range=c(3,8))+
  labs(title="RED TAILS IN THE SKY",
       subtitle = "Tuskegee Airmen were the first African-American Aviators \n Graphic commemorates the 57 graduating classes during World War II",
       size = "Graduate Class Size",
       y="CLASS SIZE",
       x= "GRADUATION DATE",
       caption= "Data from the Commemorative Air Force | Graphic by @tanya_shapiro")+
  theme(legend.position = "top",
        plot.title=element_text(family="Market Deco", size=30, hjust=0.5, margin=margin(b=5)),
        plot.subtitle=element_text(hjust=0.5, size=14),
        plot.caption=element_text(size=10, margin=margin(t=10)),
        axis.ticks.y = element_line(),
        axis.text.y=element_text(family="Gill Sans", size=12),
        axis.title.y=element_text(family="Market Deco", angle=90, size=15, hjust=0.5, vjust=2),
        axis.title.x = element_text(family="Market Deco", size = 15),
        text= element_text(family="Gill Sans"),
        plot.margin = margin(t=30, r=30, l=30, b=15),
        plot.background = element_rect(fill=col_back))

ggsave("tuskegee_airmen.png", width=12, height=9)
