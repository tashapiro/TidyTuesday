library(tidyverse)
library(ggplot2)
library(extrafont)
library(showtext)
library(ggtext)
library(tools)
library(emojifont)
library(reshape)
library(ggthemes)
#IMPORT DATA
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')
#DATA CLEANING
#select varaibles
gang<-df%>%select(captured_daphnie,captured_shaggy,captured_fred,captured_velma,captured_scooby,
                  caught_daphnie,caught_shaggy,caught_fred,caught_velma,caught_scooby,
                  unmask_daphnie,unmask_shaggy,unmask_fred,unmask_velma,unmask_scooby)
#convert characters to 0s and 1s for True and False
gang[1:15] <- lapply(gang[1:15], as.logical)
gang[1:15] <- lapply(gang[1:15], as.numeric)
#replace all NAs with 0 instead, NAs will cause errors in our sum totals
gang[is.na(gang)] <- 0
gang_sum<-colSums(gang)
gang<-as.data.frame(t(t(gang_sum)))
gang<-rownames_to_column(gang)
#split up rows into two new columns, e.g. capture_fred becomes "captured" for variable and "fred" for member
gang<-gang%>%separate(rowname, c("variable", "member"), "_")%>%
  mutate(member=toTitleCase(member))
#daphne spelled wrong 
gang[gang$member=="Daphnie","member"]="Daphne"
#reshape data using reshape cast function
gang<-cast(gang, member~variable, sum)
#include ratio
gang$ratio<-round(gang$caught/gang$captured,2)
#AESTHETICS
#color palette taken from Scooby Doo group photo
gang<-gang%>%mutate(color=case_when(member=='Fred' ~ '#20A8E4',
                                    member=='Velma' ~ '#EFAE08',
                                    member=='Scooby' ~ '#B77E03',
                                    member=='Shaggy' ~ '#BCCF03',
                                    member=='Daphne' ~ '#794D9C',
                                    ))
col_background<-'grey20'
col_font<-'white'
col_background<-'grey92'
#Icons taken from Jory Raphel's art, asked for their permission directly to use in this chart. 
#Files not included in repo to respectt copyright purposes. But one can subsitute with their own graphics!
gang<-gang%>%mutate(icon=paste('icons/',member,'.png',sep=""))
gang$label<-paste0("<img src='", gang$icon,  "' width='60' /><br>*", gang$member,"*")
#ANNOTATIONS
#header caption info in place of a legend
x<-c(83,160,121.5)
y<-c(5.29,5.29,5.29)
text<-c("CAPTURED","CAUGHT","RATE CAPTURED/CAUGHT")
df_text<-data.frame(x,y,text)
#annotations for observations
label<-c("Scooby & Fred are the heroes of the show, both have caught monsters in 100+ episodes.",
  "Meanwhile, Daphne & Velma more likely to be captured by monster than catch a monster.")
x<-c(50,120)
y<-c(4.5,1.5)
desc<-c("Female","Male")
df_notes<-data.frame(label,x,y,desc)
#Arrows for notes
arrows <- tibble(
  x1 = 86,
  x2 = 81,
  y1 = 3.32,
  y2 = 3.05
)


#BUILDING PLOT
p<-ggplot(data=gang)+
  geom_segment( aes(color=color, x=caught, xend=captured, y=reorder(label,caught), yend=reorder(label,caught)), size=1.5) +
  geom_textbox(data=df_notes,
               aes(x=x,y=y,label=label),size=4,family='Gill Sans',
               width = unit(0.3, "npc"),fill=col_background,
               color="grey10", box.color=col_background
  )+
  geom_point(aes(y=reorder(label,caught),x=caught, color=color),size=10, shape=19, stroke=2)+
  geom_point(aes(y=reorder(label,caught),x=captured, color=color),fill=col_background,size=10, shape=21, stroke=2)+
  geom_text(aes(y=reorder(label,caught),x=captured, label=captured))+
  geom_text(aes(y=reorder(label,caught),x=caught, label=caught), color="white")+
  geom_text(data=df_text,aes(y=y,x=x, label=text), color="grey20", size=3)+
  #remove Shaggy, line too narrow to plot full text at given height
  geom_text(data= gang%>%filter(member!='Shaggy'),
    aes(y=reorder(label,caught),x=(captured+caught)/2, label=paste(ratio,"x",sep="")), color="grey20",vjust=-1)+
  #create specific height for Shaggy rate text
  geom_text(data= gang%>%filter(member=='Shaggy'),
            aes(y=reorder(label,caught),label=paste(ratio,"x",sep="")), x=90, color="grey20",vjust=-3.2)+
  geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "grey30", curvature =0.15)+
  scale_color_identity(guide = "legend")+
  theme_minimal()+
  theme(plot.title=element_text(family="Lemon Milk Bold",size=16),
        plot.background=element_rect(fill=col_background),
        text = element_text(family="Lemon Milk Light"),
        axis.text.y  = element_markdown(color = "black", size = 8, family="Lemon Milk Bold", halign=0.6), 
        axis.text.x  = element_text(size=12),
        axis.title.x  = element_text(family="Lemon Milk Bold"),
        plot.caption = element_text(family='Gill Sans', color="grey20", margin=margin(10,0,0,2), hjust=0.98, size=10),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major=element_line(color='grey85',size=0.3),
        legend.position="none",
        axis.title.y = element_blank())+
  labs(x='# of Episodes',
       title="Scooby-Doo: Are Female Characters Damsels in Distress?",
       subtitle="Number of episodes characters caught monsters vs. were captured by monsters",
       caption="Data from Kaggle | Icons by Jory Raphel | Chart by @tanya_shapiro")

p

16/9
11/1.77
#save image
ggsave(p,file="scooby-doo.jpeg",width=10.5,height=6.4)

