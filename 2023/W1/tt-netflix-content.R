library(tidyverse)
library(geomtextpath)
library(ggimage)
library(magick)
library(glue)
library(ggtext)

#import data
df_year <- read.csv("./data/yearly_netflix.csv")


#border function to apply with ggimage
border <- function(im) {
  ii <- magick::image_info(im)
  ii_min <- min(ii$width, ii$height)
  
  img <- image_blank(width = ii_min, height = ii_min, color = "none")
  drawing <- image_draw(img)
  symbols(ii_min/2, ii_min/2, circles = ii_min/2, bg = 'white', inches = FALSE, add = TRUE)
  dev.off()
  
  image_composite(image_scale(drawing, "x530"), image_scale(im,500), offset = "+15+15")
}

#df for eras line segment labels beneath years
eras = data.frame(
  start = c(2011.5, 2013, 2017.1, 2020.1),
  end = c(2012.9, 2016.9, 2019.9,2022.2),
  era = c("Streaming", "Original Programming","Global Expansion","Gaming & Present")
)


#df for labels for images
labels = data.frame(
  x = c(2012, 2013, 2021,2015, 2017.8),
  y= c(110, 345,420,170, 250),
  label=c(
    "<span>First TV Series<br><span style='color:#E50914;'>**Lilyhammer**</span></span>",
    "<span>+7 shows including:<br><span style='color:#E50914;'>**House of Cards**</span><br><span style='color:#E50914;'>**OITNB**</span>",
    "<span>Releases<br><span style='color:#E50914;'>**Squid Game**</span><br><span style='color:white;'>most popular<br> series to-date</span>",
    "<span>**Netflix & Chill**<br>becomes a thing</span>",
    "<span>Surge in new content,<br>growth in international<br>markets</span>"
  ),
  hjust=c(rep(0.5,4),0)
)

#df for dotted segment lines 
segments = data.frame(
  y = c(50, 200,635, 150,440), 
  yend = c(2, 8,500, 60,280),
  x= c(2012, 2013,2021, 2015,2018),
  xend = c(2012, 2013,2021, 2015, 2018)
)

#make sure to download these fonts locally!
fb = '"Font Awesome 6 Brands"'
font="Bebas Neue"
f = '"Bebas Neue"'
f2 = '"Roboto Light"'

#create plot title, subtitle, and caption to apply with ggtext::element_textbox_simple
title = glue("<span style='font-family:{f};color:white;font-size:40pt;'>Netflix Content<br><span style='color:#E50914;font-size:45pt;'>E x p a n s i o n</span></span>")
subtitle = glue("<span style='font-family:{f2};color:white;font-size:11pt;'>New Netflix originals by release year. Netflix released its first original TV series in 2012. Today, a decade later, Netflix is distributing more than 700 new original programs a year.</span>")
caption=glue("<span>Source: What's On Netflix</span><br><span style='font-family:{fb};color:#E50914;'>&#xf099;</span><span style='color:black;'>.</span><span>@tanya_shapiro</span><span style='color:black;'>...</span><span style='font-family:{fb};color:#E50914;'>&#xf09b;</span><span style='color:black;'>.</span><span>tashapiro</span>")


#PLOT
ggplot(data = df_year, 
       mapping = aes(x=year, y=total))+
  geom_area(color="#E50914", 
            fill = "#FF7B82",
            alpha=0.25,
            linewidth=1.5)+
  geom_text(mapping=aes(label=year, y=-20, x=year), size=4, color="white",
            fontface="bold", family="Roboto")+
  geom_textbox(inherit.aes=FALSE, 
               mapping=aes(label=title, x=2012, y=650),
               width=unit(3.5,"in"),
               fill="black", box.size=NA,
               halign=0,
               hjust=0)+
  geom_textbox(inherit.aes=FALSE, 
               mapping=aes(label=subtitle, x=2012, y=552),
               width=unit(3.2,"in"),
               fill="black", box.size=NA,
               halign=0,
               hjust=0)+
  geom_richtext(data=labels, 
                mapping=aes(x=x,y=y,label=label, hjust=hjust),
                color="white", 
                label.size=NA,
                size=3,
                fill=NA,
                family="Roboto")+
  geom_textsegment(inherit.aes = FALSE,
                   data = eras,
                   mapping=aes(x=start, xend=end, y=-50, yend=-50, label=era), 
                   family = "Bebas Neue", color="white", size=3.25, 
                   linecolor="#E50914")+
  geom_segment(data=segments,
               mapping=aes(x=x, xend=xend, y=y, yend=yend), color="white", linetype="dotted")+
  geom_image(inherit.aes=FALSE,
             mapping=aes(x=2012, y=50, image="./images/lilyhammer.png"), size=0.08,
             image_fun = border)+
  geom_image(inherit.aes=FALSE,
             mapping=aes(x=2013, y=200, image="./images/oitnb.png"),
             size=0.08, image_fun=border)+
  geom_image(inherit.aes=FALSE,
             mapping=aes(x=2013, y=275, image="./images/hoc.png"),
             size=0.08, image_fun=border)+
  geom_image(inherit.aes=FALSE,
             mapping=aes(x=2013, y=275, image="./images/hoc.png"),
             size=0.08, image_fun=border)+
  geom_image(inherit.aes=FALSE,
             mapping=aes(x=2021, y=500, image="./images/squid.png"),
             size=0.08, image_fun=border)+
  labs(y="",
       caption = caption)+
  scale_y_continuous(expand=c(0,0), limits=c(-70,780), 
                     breaks = c(150,300,450,600,750))+
  scale_x_continuous(limits=c(2011,2023), expand=c(0,0))+
  coord_equal(ratio=12/850)+
  theme(
    plot.background=element_rect(fill="black", color="black"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y=element_line(color="#272727", linewidth=.15),
    text = element_text(color="white"),
    plot.caption = element_textbox_simple(family="Roboto", color="#DCDCDC", margin=margin(t=10), size=8),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    plot.margin = margin(l=5, b=15,t=10),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="white", family="Roboto", size=10)
  )

#save plot
ggsave(filename="netflix.png", width=8, height=8)
