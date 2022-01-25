library(tidyverse)
library(ggimage)

details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

#get top 3 for all games between 1950 and 2009
df_top_3<-details%>%
  filter(yearpublished>1950 & yearpublished<2010)%>%
  select(id, primary, yearpublished, owned)%>%
  mutate(decade = yearpublished - yearpublished %% 10)%>%
  arrange(decade, -owned)%>%
  group_by(decade)%>%
  slice(1:3)

#get top 2 games for 1940s and 2010s 
df_top_2<-details%>%
  select(id, primary, yearpublished, owned)%>%
  mutate(decade = yearpublished - yearpublished %% 10)%>%
  filter(decade %in% c(1940,2010))%>%
  arrange(decade, -owned)%>%
  group_by(decade)%>%
  slice(1:2)

#combine datasets 
df<-rbind(df_top_3, df_top_2)%>%arrange(decade, -owned)

#border
xstart = c(0,0,2,11,0,0,0,13)
xend = c(13,13,2,11,13,13,0,13)
ystart = c(2,11,0,0,0,13,0,0)
yend = c(2,11,13,13,0,13,13,13)
border <- data.frame(xstart,xend,ystart, yend)

#board tile lines
xstart = c(3:10,3:10, rep(0,8), rep(11,8))
xend = c(3:10,3:10, rep(2,8), rep(13,8))
ystart = c(rep(0,8), rep(11,8), 3:10, 3:10)
yend = c(rep(2,8), rep(13,8), 3:10, 3:10)
lines<-data.frame(xstart,xend,ystart,yend)

#header fill dataset
decade = c("1940s","1940s","1950s","1950s","1950s","1980s","1980s","1980s","1990s","1990s","1990s",
         "1960s","1960s","1960s","1970s","1970s","1970s","2000s","2000s","2000s","2010s","2010s")
xmin = c(8,10,2,3,5,2,4,5,7,8,10,rep(1.6,6),rep(11,5))
xmax = c(9,11,3,4,6,3,5,6,8,9,11,rep(2,6),rep(11.4,5))
ymin = c(rep(1.6,5), rep(11,6),2,4,5,7,9,10,10,9,7,4,2)
ymax = c(rep(2,5), rep(11.4,6),3,5,6,8,10,11,11,10,8,5,3)
header_fill<-data.frame(xmin,xmax,ymin,ymax,decade)

#plot text
type = c("title","subtitle")
text = c("BOARD GAME FAVORITES", "Popular board games throughout the decades based on ownership. \n Data from Kaggle & BoardGameGeek.")
x = c(6.5,6.5)
y = c(9,8)
board_text <-data.frame(type,text,x,y)


#setting up data points for board game texts
x = c(2.5,3.5,5.5,8.5,10.5,1.2,1.2,1.2,1.2,1.2,1.2, 2.5, 4.5,5.5, 7.5, 8.5, 10.5, 11.8, 11.8, 11.8, 11.8, 11.8)
y = c(1.2,1.2,1.2,1.2,1.2,2.5,4.5,5.5,7.5,9.5,10.5, 11.8, 11.8,11.8, 11.8, 11.8, 11.8, 10.5, 9.5,7.5,4.5,2.5)
decade <- c(rep(1950,3),rep(1940,2),rep(1960,3), rep(1970,3), rep(1980,3), rep(1990,3), rep(2000,3), rep(2010,2))
pos <- c(rep("bottom",5), rep("left",6), rep("top",6), rep("right",5))
text_angle = c(rep(0,5),rep(-90,6), rep(180, 6), rep(90, 5))
text_params<-data.frame(x,y,text_angle, decade, pos)

#create board game text df
game_text<-text_params%>%
  arrange(decade)%>%
  select(-decade)%>%
  cbind(df)%>%
  mutate(x_pos = case_when(pos %in% c('bottom','top')~x,
                           pos == 'left'~ x-0.5,
                           pos == 'right' ~x+0.5),
         y_pos = case_when(pos %in% c('left','right')~y + 0.5,
                           pos == 'top'~ y+1,
                           pos == 'bottom' ~y-0.2),
         primary = case_when(primary=='The Game of Life'~'The Game \n of Life',
                          primary=='Scotland Yard'~'Scotland \n Yard',
                          grepl("Sherlock",primary)~"Sherlock \n Holmes",
                          grepl("Escape from Atlantis",primary)~"Escape from \n Atlantis",
                          TRUE~primary)
         )

#bonus squares
x = c(12,12,1, 1)
y = c(1,12, 12, 1)
angle = c(45,135, -135, -45)
text = c("GO","GO TO \n JAIL", "FREE \n PARKING", "JAIL")
bonus_text = data.frame(x,y,text, angle)

#images
x = c(6.5, 6.5, 1, 12, 4.5, 3.5, 12,9.5,1,12)
y = c(1, 12, 6.5, 6.5, 1, 12, 5.5,1,8.5,8.5)
image = c('images/ggplot.png','images/ggplot_t.png','images/ggplot_l.png','images/ggplot_r.png',
          'images/rstudio.png','images/rstudio_t.png','images/rstudio_l.png',
          'images/dplyr.png', 'images/dplyr_l.png', 'images/dplyr_r.png')
image_df = data.frame(x,y,image)

#fill
xmin = 0
xmax = 13
ymin = 0
ymax= 13
df_fill = data.frame(xmin,xmax,ymin,ymax)

ggplot()+
  geom_rect(data = df_fill, mapping=aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), fill='#FBF9F9')+
  #border lines
  geom_segment(data = border, mapping=aes(x=xstart, xend=xend, y=ystart, yend=yend))+
  #tile lines
  geom_segment(data = lines, mapping=aes(x=xstart, xend=xend, y=ystart, yend=yend))+
  #tile color headers
  geom_rect(data=header_fill, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=decade), color="black")+
  #title
  geom_text(data = board_text%>%filter(type=='title'), mapping=aes(x=x, y=y, label=text), family="Gill Sans Bold", size=6)+
  geom_text(data = board_text%>%filter(type=='subtitle'), mapping=aes(x=x, y=y, label=text), family="Gill Sans", size=4)+
  #games
  geom_text(data=game_text, mapping=aes(x=x,y=y, label=primary, angle=text_angle),family="Gill Sans", size=2.9)+
  geom_point(data=game_text, mapping=aes(x=x_pos,y=y_pos-0.5,size=owned/1000),shape=21, color="black")+
  #bonus square text
  geom_text(data=bonus_text, mapping=aes(x=x,y=y, label=text, angle=angle),family="Gill Sans Bold", size=5)+
  #image squares 
  geom_image(data = image_df, mapping=aes(x=x,y=y, image=image), angle=45)+
  #customize legend
  scale_fill_viridis_d(guide=guide_legend(title.position="top", label.position="bottom",title.hjust=0.5, nrow=2, override.aes=list(color=NULL)))+
  scale_size(breaks=c(25,50,75, 100, 125, 150), range=c(1,8),
             guide=guide_legend(nrow=1, title.position="top",title.hjust=0.5, label.position = "bottom"))+
  scale_x_continuous(limits=c(0,13))+
  scale_y_continuous(limits=c(0,13))+
  coord_fixed()+
  labs(fill="Decade Published",
       size="Owned Games (thousands)")+
  theme_void()+
  theme(
    legend.position = c(0.5, 0.4), 
    legend.box.jus = c("center"),
    legend.margin = margin(c(5,0,5,0)),
    text = element_text(family="Gill Sans")
  )

ggsave("board_games.png", height=9, width=9)

