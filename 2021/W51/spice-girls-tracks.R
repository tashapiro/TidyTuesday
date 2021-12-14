library(tidyr)
library(tidyverse)
library(reshape2)
library(dplyr)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load('2021-12-14')
tuesdata <- tidytuesdayR::tt_load(2021, week = 51)

studio_album_tracks <- tuesdata$studio_album_tracks

# Or read in the data manually
studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')

#get variables for plotting
tracks<-studio_album_tracks%>%
  select(track_id, track_name, mode_name, key_mode, loudness, danceability, energy, speechiness, acousticness, instrumentalness,
         valence, liveness)
#reshape data
tracks<-melt(tracks, id=c("track_id","track_name","mode_name","key_mode","loudness"))
#set min and max for segments
tracks$min_val<-0
tracks$max_val<-1

#pick songs to subset
song_sample<-c("Say You'll Be There","Who Do You Think You Are","Wannabe",
               "Stop","Spice Up Your Life","2 Become 1",
               "Move Over","Mama","Viva Forever")


#create abbreviations for song features
tracks<-tracks%>%
  mutate(
    abbrev= case_when(
      variable=="energy"~"ENERGY",
      variable=="valence"~"VAL",
      variable=="danceability"~"DANCE",
      variable=="liveness"~"LIVE",
      variable=="instrumentalness"~"INSTR",
      variable=="speechiness"~"SPCH",
      variable=="acousticness"~"ACOUSTIC",
      
    )
  )

#create as factor (optional)
tracks$abbrev<-factor(tracks$abbrev, levels=c("INSTR", "DANCE", "VAL", "ENERGY", "ACOUSTIC", "SPCH","LIVE"))
#create subset of songs
df<-tracks%>%filter(track_name %in% song_sample)

df$x_int = 1
y = points<-c(0.25,0.5,0.75,1)
x = c(1,1,1,1)
text = c("0.25","0.50","0.75","1.00")
grouped = df%>%group_by(track_name,loudness,mode_name)%>%summarise(t = n())
text_df<-merge(data.frame(x,y,text),grouped,all=TRUE)

circle_lines = "grey85"
text_col = "white"
line_size = 0.2

#create plot
ggplot(df, aes(abbrev, y=value, group=1, fill=loudness))+
  geom_hline(yintercept = 1, color=circle_lines, size=line_size)+
  geom_hline(yintercept = 0.75, color=circle_lines, size=line_size)+
  geom_hline(yintercept = 0.5, color=circle_lines, size=line_size)+
  geom_hline(yintercept = 0.25, color=circle_lines, size=line_size)+
  geom_hline(yintercept = 0, color=circle_lines, size=line_size)+
  geom_col()+
  coord_polar(start = -0.45) +
  facet_wrap(~track_name)+
  scale_y_continuous(
    limits=c(-0.5,1),
    expand = c(0, 0),
    breaks = c(0, 0.25, 0.50, 0.75,1.00)
  ) + 
  geom_segment(aes(x = x_int, xend=x_int, y=0, yend=1), color = "grey70", size = 0.3) +
  geom_text(data=text_df, mapping=aes(x=x,y=y,label=text), vjust=1.5,hjust=1.2, size=2.1, family="Gill Sans", color='white')+
  labs(
    title="ANATOMY OF SPICE GIRLS HITS",
    subtitle= "Spotify Audio Features of Spice Girls songs. Scale from 0 to 1. \n 
    DANCE = Danceability  •  VAL = Valence  •  INSTR = Instrumentalness  •  SPCH = Speechiness  •  ACOUSTIC = Acousticness  •  LIVE = Liveness\n",
    caption = "Data from Spotify | Chart by @tanya_shapiro"
  )+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5, title="Loudness (dB)", title.position="top", title.hjust=0.5))+
  scale_fill_fermenter(n.breaks = 5, palette = "PiYG", direction=1)+
  theme_minimal()+
  theme(legend.position="top",
        text=element_text(family="Gill Sans", color=text_col),
        plot.title=element_text(hjust=0.5,size=16, family="Gill Sans Bold"),
        plot.caption=element_text(size=10),
        plot.subtitle=element_text(hjust=0.5),
        axis.text.y=element_blank(),
        strip.text = element_text(family="Gill Sans", color=text_col,size=10),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(color=circle_lines, size=line_size),
        plot.background = element_rect(fill="#202030", color="#202030"),
        plot.margin = unit(c(0.8, 4, 0.4, 4), "cm"),
        panel.spacing.x = unit(5, "lines"),
        axis.text.x=element_text(size=7.5, color=text_col, family="Gill Sans Bold"),
        axis.title=element_blank())

ggsave("spice-girls-tracks.png", width=12, height=10)

