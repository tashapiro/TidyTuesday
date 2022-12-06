library(tidyverse)
library(ggimage)
library(glue)
library(sysfonts)
library(showtext)
library(ggtext)
library(geomtextpath)

#import data
tuesdata <- tidytuesdayR::tt_load('2021-11-23')

#break out data for eps
episodes<-tuesdata$episodes
#base url for images
base_url = "https://raw.githubusercontent.com/tashapiro/TidyTuesday/master/2021/W48/images/"

#data frame for doctors
df_doctor<-data.frame(
  doctor = c("Christopher Eccleston",
             rep("David Tennant",3),
             rep("Matt Smith",3),
             rep("Peter Capaldi",3),
             rep("Jodie Whitaker",3)),
  doctor_num = c("Ninth Doctor",
             rep("Tenth Doctor",3),
             rep("Eleventh Doctor",3),
             rep("Twelfth Doctor",3),
             rep("Thirteenth Doctor",3)),
  season= 1:13
)

df_doctor$image<-paste0(base_url,str_replace(df_doctor$doctor," ","_"),".png")

#data
df_eps<-episodes|>
  left_join(df_doctor,by=c("season_number"="season"))|>
  filter(!is.na(rating) & !is.na(season_number))|>
  group_by(doctor)|>
  mutate(avg_rating = mean(rating))|>
  ungroup()

overall_avg <-mean(df_eps$rating)

df_doc_avg<-df_eps|>distinct(doctor, doctor_num, image, avg_rating)|>
  mutate(label = glue("<span style='font-size:10pt;'>**{doctor}**</span><br><span style='font-size:8pt;'>{doctor_num}</span>"))


#pre-set aesthetics
pal<-c(
  '#D9695F', #red
  '#658C5A', #green
  '#F2BE5C', #orange
  '#5C4673', #puprle
  '#72A5B8' #lightblue
)

pal_line = "white"
pal_text = "white"
pal_bg = "black"

sysfonts::font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Open Sans","Open Sans")
sysfonts::font_add_google("Open Sans","os")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

#titles & labels for {ggtext}
caption<-paste0(
  "<span style='font-family:os;'>Source: {datardis}</span><br>",
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style='font-family:os;'>@tanya_shapiro</span>",
  "<span style='font-family:os;color:black;'>...</span>",
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:os;'> tashapiro </span>")

title<-"<p style='color:#FFD495;'>Doctor Who<span style='color:white;'> was The Best?</span></p>"

subtitle<-paste0("<span style='color:#D1D1D1;'>Ratings by Episode and Doctor for the popular TV series, Doctor Who.</span>")

ggplot()+
  geom_jitter(data=df_eps, 
              mapping = aes(x=rating, y=reorder(doctor,avg_rating), color=doctor),
              show.legend = FALSE, size=3, alpha=0.6, height=0.2
  )+
  geomtextpath::geom_textvline(mapping=aes(xintercept=overall_avg, label=paste0("Overall Avg: ",round(overall_avg,0))), 
             size=3, color=pal_line, hjust=0.86, vjust=-.6, family="Open Sans")+
  geom_segment(data=df_doc_avg, 
               mapping = aes(x=avg_rating, xend=overall_avg, y=doctor, yend=doctor),
               color=pal_line)+
  geom_point(data=df_doc_avg, 
             mapping=aes(x=avg_rating, y=doctor),
             size=14.5, color="white")+
  geom_image(data=df_doc_avg, 
             mapping=aes(x=avg_rating, y=doctor, image=image),
             size = 0.06, asp=1.61)+
  geom_text(data=df_doc_avg, 
             mapping=aes(x=avg_rating, y=doctor, label=round(avg_rating,1)),
             size=2.75, color="white", vjust=4.5, family="Open Sans")+
  geom_textbox(data=df_doc_avg, 
            mapping=aes(x=59.1, y=doctor, label=label),
            family="Open Sans", fill=NA, box.size=NA, box.padding=unit(rep(0,4),"pt"),
            color=pal_text, hjust=0)+
  #arrows
  annotate(geom="text",label="Avg Rating\nper Doctor", x=76, y=2.5,
           size=2.5, color="white", family="Open Sans")+
  geom_curve(mapping=aes(x=77, xend=81.4, y=2.7, yend=3), 
             color="white", curvature=-0.2, linewidth=0.3, arrow=arrow(length=unit(0.08,"in")))+
  geom_curve(mapping=aes(x=77, xend=80.8, y=2.3, yend=2), 
             color="white", curvature=0.2, linewidth=0.3, arrow=arrow(length=unit(0.08,"in")))+
  scale_x_continuous(limits=c(59,95), expand=c(0,0), breaks=c(70, 75,80,85,90,95))+
  scale_color_manual(values=pal)+
  coord_equal(ratio = 50/12)+
  labs(title=title,
       subtitle=subtitle,
       caption = caption,
       x="<span style='font-size:11pt;'>Episode Rating</span><br><span style='font-size:8pt;'>(out of 100) </span>")+
  theme(plot.background = element_rect(fill=pal_bg, color=pal_bg), 
        panel.background = element_blank(), 
        panel.grid=element_blank(),
        plot.margin = margin(l=20, r=40, b=10, t=20),
        plot.caption = element_textbox_simple(size=7, color="grey80"),
        plot.title = element_textbox_simple(size=14, face="bold", margin = margin(b=5)),
        plot.subtitle  = element_textbox_simple(size=9),
        text = element_text(color=pal_text, family="Open Sans"),
        axis.text = element_text(color=pal_text),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_textbox_simple(margin=margin(t=10), halign=0.675, hjust=0.5),
        axis.ticks=element_blank())


ggsave("doctor_who.png", width=8, height=6)
