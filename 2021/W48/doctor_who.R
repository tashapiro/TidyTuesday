library(tidytuesdayR)
library(png)
library(ggimage)
library(ggplot2)
library(dplyr)

#load tidy tuesday data
tuesdata <- tidytuesdayR::tt_load('2021-11-23')

#break out data sets
directors <- tuesdata$directors
writers <- tuesdata$writers
episodes<-tuesdata$episodes
imdb<-tuesdata$imdb

#list of doctors from seasons
doctor<-c("Christopher Eccleston","David Tennant","David Tennant","David Tennant",
           "Matt Smith","Matt Smith","Matt Smith","Peter Capaldi","Peter Capaldi",
           "Peter Capaldi","Jodie Whittaker","Jodie Whittaker","Jodie Whittaker")
season<-1:13
doctors<-data.frame(season,doctor)

#create main data frame
df<- left_join(episodes,directors, by=c("story_number"="story_number"))
df<- left_join(df,doctors, by=c("season_number"="season"))
df<- df%>%filter(type=="episode")

#summarize data by doctor
doc_summary<-df%>%group_by(doctor)%>%
  summarise(avg_doc_rating=mean(rating, na.rm=TRUE),
            avg_doc_viewers=mean(uk_viewers, na.rm=TRUE))

#join averages by doctor to main df 
df <-left_join(df, doc_summary, by=c("doctor"="doctor"))
df$avg_rating<-mean(df$rating, na.rm=TRUE)

#palettes
col_back<-"#00060b"
col_text<-"white"
col_line<-"white"
pal<-c(
'#D9695F', #red
'#658C5A', #green
'#F2BE5C', #orange
'#5C4673', #puprle
'#72A5B8' #lightblue
)

#append images from local
df$image<-ifelse(df$doctor=="Peter Capaldi","peter_c.png",
                 ifelse(df$doctor=="David Tennant","david_t.png",
                        ifelse(df$doctor=="Jodie Whittaker","jodie_w.png",
                               ifelse(df$doctor=="Matt Smith","matt_s.png",
                               "chris_e.png"))))
                        
                 





p<-ggplot(df, aes(x=reorder(doctor, avg_doc_rating), y=rating))+
  geom_jitter(aes(color=doctor), size=5, alpha = 0.6, width = 0.15)+
  geom_hline(aes(yintercept = avg_rating), color = col_line, size = 0.5) +
  geom_segment(aes(x = doctor, xend = doctor,y = avg_rating, yend = avg_doc_rating), size = 0.5, color=col_line)+
  geom_point(aes(x=doctor, y=avg_doc_rating), color="white",size=18)+
  geom_image(aes(x=doctor, y=avg_doc_rating,image=image),asp=1.5)+
  geom_text(aes(x=doctor, y=avg_doc_rating, label=round(avg_doc_rating,2)), family="Gill Sans",size=3.5, vjust=5, color=col_text)+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal)+
   coord_flip()+
  labs(
    title='DOCTOR WHO...WAS THE BEST?',
    subtitle = "Ratings Per Doctor Who Episode, Scaled 1-100",
    x="DOCTOR",
    y="RATINGS",
    caption="Data from datardis | Chart @tanya_shapiro"
  )+
  annotate("text", x = 1.7, y = 87, family="Gill Sans", color=col_text, label ='Overall Average: 84.17')+
  annotate("text", x = 2.5, y = 79, family="Gill Sans", color=col_text, label ='Average Per Doctor')+
  annotate("text", x = 5, y = 81.3, family="Gill Sans", color=col_text, label ='Episode Rating')+
  theme_void()+
  theme(text=element_text(family="Gill Sans",color=col_text),
        plot.title=element_text(family="Gill Sans Bold",color=col_text,size=18),
        plot.caption =element_text(size=12, vjust=-3,hjust=0.98),
        plot.subtitle=element_text(size=14),
        plot.margin = unit(c(0, 0, 0.8, 0), "cm"),
        legend.position="none",
        legend.text=element_text(color=col_text),
        axis.title.x=element_text(color=col_text),
        axis.text=element_text(color=col_text),
        axis.text.y=element_text(color=col_text,size=14),
        axis.ticks.x=element_line(color="white"),
        plot.background = element_rect(col_back))

#add arrows
arrows <- tibble(
  x1 = c(1.58, #Avg Overall
         2.6, #Avg Per Doc
         2.4, #Avg Per Doc
         4.9  #Ep Rating
  ),
  x2 = c(1.3,  #Avg Overall
         3,
         2,
         4.8
  ),
  y1 = c(86.2,  #Avg Overall
         80, #Avg Per Publisher
         80, #Avg Per Publisher
         81.5
  ),
  y2 = c(84.25,  #Avg Overall
         82.3, #Avg Per Publisher
         81.75, #Avg Per Publisher
         83.3
  ),
  curve = c("up","up","down","down")
)





p + geom_curve(data = arrows%>%filter(curve=="up"), aes(x = x1, y = y1, xend = x2, yend = y2),
                  arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
                  color = "white", curvature =-0.15)+
  geom_curve(data = arrows%>%filter(curve=="down"), aes(x = x1, y = y1, xend = x2, yend = y2),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "white", curvature =0.15)



ggsave("doctor_who.png", width=12, height=8)

