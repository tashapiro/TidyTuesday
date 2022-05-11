library(tidyverse)
library(gganimate)
library(extrafont)
library(showtext)

font_import()
font_add("Georgia", "/Library/Fonts/Georgia Regular.otf")

nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

top_titles<-nyt_full%>%
  mutate(decade=year-year%%10, counter=1, title=str_to_title(title))%>%
  group_by(author, decade, title_id, title)%>%
  summarise(weeks=sum(counter))%>%
  group_by(author,decade)%>%
  slice_max(order_by=-weeks, n=1)%>%
  arrange(author, decade, title, -weeks)%>%
  summarise(titles = paste(title, collapse=", "))

decade<-nyt_full%>%
  mutate(decade= year- year %%10, counter=1)%>%
  filter(!grepl(" and ",author))%>%
  group_by(author)%>%
  mutate(debut_decade = min(decade))%>%
  group_by(author, decade, debut_decade)%>%
  summarise(weeks=sum(counter), books=n_distinct(title_id))%>%
  arrange(decade, weeks, books)%>%
  group_by(decade)%>%
  mutate(rank = rank(-weeks, ties.method = "first"))%>%
  left_join(top_titles, by=c("author"="author","decade"="decade"))
 # slice_max(order_by=weeks, n=10, with_ties = FALSE)%>%
  select(rank, author, debut_decade, decade, weeks, books, titles)

decade<-decade%>%
  mutate(prev_decade=decade-10)%>%
  filter(!grepl(" and ",author))%>%
  left_join(decade%>%select(author, decade, rank), by = c("author"="author","prev_decade"="decade"))%>%
  rename(rank=rank.x, prev_rank = rank.y)%>%
  mutate(pos = case_when(decade == debut_decade ~ "Debut", 
                         rank<prev_rank~"Up",
                         rank>prev_rank~"Down",
                         rank==prev_rank~"Same"),
         pos = factor(pos, levels=c("Up","Down","Same","Debut")))%>%
  filter(decade>=1970)%>%
  group_by(decade)%>%
  slice_max(order_by=weeks, n=10, with_ties=FALSE)

ggplot(decade, aes(y=rank))+
  geom_segment(data=data.frame(x1=1.5, x2=4.5, y1=-0.25, y2=-0.25), color='#333333', mapping=aes(x=x1, xend=x2, y=y1, yend=y2))+
  geom_text(aes(label=toupper(author), x=2, y=rank-0.1), color="black", hjust=0, size=3.5, fontface="bold")+
  geom_text(aes(label=paste0(weeks, " WEEKS ON THE LIST"), x=2, y=rank-0.4), hjust=0,  size=2.2, color="#333333")+
  geom_text(aes(label=rank, x=1.9, y=rank-0.35), hjust=1, size=4.7, color="#999999", family="Georgia")+
  geom_text(aes(label=str_to_title(titles), x=2, y=rank+0.2), color = "#333333", hjust=0, size=2.35, family="Georgia")+
  geom_point(aes(x=1.8, y=rank+0.12, shape=pos), size=1.5, color="#999999")+
  scale_shape_manual(values=c(24,25,21,8), guide=guide_legend(override.aes=list(size=2)))+
  facet_wrap(~paste0(decade,"s"))+
  scale_y_reverse(limits=c(10.5,-0.25))+
  scale_x_continuous(limits=c(0,6))+
  labs(title="New York Times Best Selling Authors", subtitle="Top 10 authors per decade. Based on number of weeks featured per book. Best performing book listed beneath each author.",
       color="", shape="Decade Rank Shift", caption = "Data from The New York Times | Graphic @tanya_shapiro")+
  theme_void()+
  theme(text = element_text(color="black"),
        legend.position="top",
        legend.title = element_text(size=9),
        strip.text = element_text(face="bold", size=12, family="Georgia"),
        plot.title=element_text(hjust=0.5, face="bold", family="Georgia", margin=margin(b=8)),
        plot.subtitle=element_text(hjust=0.5, size=10, margin=margin(b=8)),
        plot.caption = element_text(hjust=0.9, family="Georgia"),
        plot.margin=margin(t=20,b=15))