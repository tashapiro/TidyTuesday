
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggforce)
library(cowplot)
library(stringr)


df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

#convert release date to date format
df$release_date<-as.Date(df$release_date, format='%b %d,%Y')
#extract year from release date
df$release_year<-format(df$release_date,'%Y')
#add 1 to make dplyr sums easy
df$count<-1
#create dataset of games with metascores - filter out NA metsacores
df_scores<-df%>%filter(!is.na(metascore))

#Who are the top publishers by # of games?
top_publishers<-df_scores%>%group_by(publisher)%>%summarise(games=sum(count))%>%top_n(10,games)

#reduce dataset to only look at games by top 10 publshers
df_scores<-df_scores%>%filter(publisher %in% top_publishers$publisher)

#get the average metascore overall 
total_avg<-mean(df_scores$metascore)

#get average per top publisher
averages<-df_scores%>%group_by(publisher)%>%summarise(total_games=sum(count),average_score=mean(metascore))

#merge averages per publisher with rest of dataset
df_scores<-merge(df_scores,averages, by=c("publisher"="publisher"))

#format publisher name to all caps
df_scores$publisher<-toupper(df_scores$publisher)


#Plot Image w/ Annotations
plot<-ggplot(df_scores, aes(x=reorder(publisher, average_score), y=metascore))+
  geom_jitter(aes(color=publisher), size=5, alpha = 0.25, width = 0.15)+
  geom_mark_circle(x=10,y=94, color='grey50', label.fill = NA, expand = unit(4, "mm"))+
  scale_y_continuous(limits=c(20,100))+
  geom_segment(aes(x = publisher, xend = publisher,y = total_avg, yend = average_score), size = 0.5, color='gray30')+
  geom_point(mapping=aes(x=publisher, y=average_score, fill=publisher), color="gray30", shape=21, size=7, stroke=1)+
  geom_hline(aes(yintercept = total_avg), color = "gray30", size = 0.5) +
  annotate("text", x = 6.6, y = 86, fontface="bold", label ='Average Overall')+
  annotate("text", x = 6.3, y = 86, label = glue::glue('{round(total_avg, 1)} Metascore'))+
  annotate("text", x = 2.5, y = 55, fontface="italic", label = 'Average per publisher')+
  annotate("text", x = 9.7, y = 45, fontface="bold", label = 'Worst Game Overall')+
  annotate("text", x = 9.4, y = 45, label = "Rogue Warrior")+
  annotate("text", x = 9.6, y = 88, fontface="bold", label = 'Best Games Overall')+
  annotate("text", x = 9.3, y = 88, label = "Elder Scroll Series")+
  coord_flip()+
  theme_minimal()+
  theme(
    title=element_text(face="bold"),
    plot.subtitle=element_text(face="italic"),
    axis.title=element_text(face="bold"),
    axis.text.y=element_text(size=10)
  )+
  guides(color = FALSE, fill = FALSE)+
  labs(
    title="PC Game Ratings by Publisher",
    subtitle="Ratings based on Metascore out of 100",
    x="",
    y='METASCORE',
    color="# of Owners"
  )


#create arrows for annotations
arrows <- tibble(
  x1 = c(6.2, #Avg Overall
         2.6, #Avg Per Publisher
         2.6, #Avg Per Publisher
         9.4 , #Worst Game
         9.7 #Best Game
         ),
  x2 = c(5.5,  #Avg Overall
         4.1,
         3.1,
         9.9,
         9.9
         ),
  y1 = c(86,  #Avg Overall
         55, #Avg Per Publisher
         55, #Avg Per Publisher
         40,
         88
         ),
  y2 = c(total_avg,  #Avg Overall
         70.2, #Avg Per Publisher
         68.2, #Avg Per Publisher
         30,
         92
         ) 
)

#add arrows
p<-plot + geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
                    arrow = arrow(length = unit(0.07, "inch")), size = 0.6,
                    color = "gray20", curvature = -0.25)

#Save as image
ggsave("PC Games.png", plot = p, width = 40, height =18, units = "cm")
