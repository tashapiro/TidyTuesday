library(tidyverse)
library(ggtext)
library(ggchicklet)
library(sysfonts)
library(showtext)
#remotes::install_github("hrbrmstr/ggchicklet")


#download fonts ----
sysfonts::font_add_google("Poppins","poppins")
sysfonts::font_add_google("Poppins","poppinslight", regular.wt=200, bold.wt=500)
showtext::showtext_auto()
sysfonts::font_add_google("Shadows Into Light Two","shadow")
#must download font awesome locally first! https://fontawesome.com/download
font_add('fs', 'fonts/Font Awesome 6 Free-Solid-900.otf')


# data ----
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')

tag_split<-df|>
  mutate(category_tag = substr(category_tags, 2, nchar(category_tags)-1),
         category_tag = str_replace_all(category_tag,"'",""),
         category_tag = strsplit(as.character(category_tag), ","))|>
  unnest(category_tag)|>
  mutate(category_tag = trimws(category_tag))


top_iphone<-tag_split|>
  filter(grepl("IPHONE", category_tags))|>
  #consolidate similar categories, abbreviatate tag napes
  mutate(category_tag = case_when(grepl("GAMES", category_tag) ~ "GAMES",
                                  category_tag %in% c("ARTIFICIAL INTELLIGENCE","AUGMENTED REALITY") ~ "AI",
                                  category_tag == "USER EXPERIENCE" ~ "UX",
                                  category_tag == "TASK MANAGEMENT" ~ "PRODUCTIVITY",
                                  category_tag %in% c("IMESSAGE APPS","MESSAGING") ~ "MESSAGING",
                                  category_tag == "CALENDAR AND SCHEDULING" ~ "CALENDAR",
                                  category_tag %in% c("FINTECH","CRYPTO") ~ "FINANCE",
                                  TRUE ~ category_tag),
         category_tag = str_replace(category_tag,"TOOLS",""),
         category_tag = str_replace(category_tag," AND ", " & "),
         category_tag = trimws(category_tag))|>
  group_by(category_tag)|>
  summarise(count=n())|>
  arrange(-count)|>
  filter(!category_tag %in% c("IPHONE","TECH","ANDROID","APPLE","MAC","IPAD","WEB APP","WINDOWS"))|>
  head(24)|>
  mutate(index = row_number(),
         y = (index-1) %/% 4,
         x = (index-1) %% 4,
         merged = case_when(category_tag %in% c("AI","MESSAGING","PRODUCTIVITY") ~ 2, TRUE ~0),
         count_label = case_when(count<1000 ~ as.character(count), TRUE ~ paste0(round(count/1000,0),"K"))
  )


#map font awesome icon IDs
top_iphone<-top_iphone|>
  mutate(icon=case_when(
    category_tag=="HEALTH & FITNESS" ~"<span style='font-family:fs'>&#xf44b;</span>",
    category_tag=="GAMES" ~ "<span style='font-family:fs'>&#xf11b;</span>",
    category_tag=="UX" ~ "<span style='font-family:fs'>&#xf007;</span>",
    category_tag=="AI" ~ "<span style='font-family:fs'>&#xf544;</span>",
    category_tag=="PHOTOGRAPHY" ~ "<span style='font-family:fs'>&#xf030;</span>",
    category_tag %in% c("MESSAGING","IMESSAGE") ~ "<span style='font-family:fs'>&#xf27a;</span>",
    category_tag=="SOCIAL MEDIA" ~ "<span style='font-family:fs'>&#xf164;</span>",
    category_tag=="MUSIC" ~ "<span style='font-family:fs'>&#xf001;</span>",
    category_tag=="EDUCATION" ~ "<span style='font-family:fs'>&#xf19d;</span>",
    category_tag=="TRAVEL" ~ "<span style='font-family:fs'>&#xf3c5;</span>",
    category_tag=="DEVELOPER" ~ "<span style='font-family:fs'>&#xf7d9;</span>",
    category_tag=="APPLE WATCH" ~ "<span style='font-family:fs'>&#xf017;</span>",
    category_tag=="DESIGN" ~ "<span style='font-family:fs'>&#xf53f;</span>",
    category_tag=="FINANCE" ~ "<span style='font-family:fs'>&#xf81d;</span>",
    category_tag=="MARKETING" ~ "<span style='font-family:fs'>&#xf0a1;</span>",
    category_tag=="VIDEO STREAMING" ~ "<span style='font-family:fs'>&#xf03d;</span>",
    category_tag %in% c("PRODUCTIVITY","TASKING") ~ "<span style='font-family:fs'>&#xf0ae;</span>",
    category_tag=="HOME" ~ "<span style='font-family:fs'>&#xf015;</span>",
    category_tag=="NEWS" ~ "<span style='font-family:fs'>&#xf1ea;</span>",
    category_tag=="FUNNY" ~ "<span style='font-family:fs'>&#xf588;</span>",
    category_tag=="CALENDAR" ~ "<span style='font-family:fs'>&#xf133;</span>",
    category_tag=="DATING" ~ "<span style='font-family:fs'>&#xf004;</span>",
    category_tag=="E-COMMERCE" ~ "<span style='font-family:fs'>&#xf07a;</span>",
    category_tag=="ANALYTICS" ~ "<span style='font-family:fs'>&#xf201;</span>",
    TRUE ~ "")
  )


#pre-plot custuomization ----

#create limits for iphone ends
base_ymin = 8
base_ymax = -1.25 


#custom title to use with ggtext::geom_textbox()
title = "<span style='font-size:18pt;color:#1d1d1f;font-family:poppins;'>**Top iPhone App Tags**</span><br><br>
<span style='font-size:10pt;color:#5D5D5D;font-family:poppinslight;'>Based on data from **The Gamer and the Nihilist Product Hunt**, the internet's largest social network and clearinghouse for apps.
<br><br>Graphic depicts most common product tags associated with iPhone apps based on total count of products released. Apps can contain multiple product tags. <br><br> Some classifications were consolidated to reduce redundancy, e.g. *Messaging* and *iMessage Apps*. Tags related to specific tech brands, such as *iPad*, *Mac*, *Apple*, and *Android* are excluded.</span><span style='font-size:8pt;color:#757575;font-family:poppinslight;'><br><br> Graphic @tanya_shapiro</span>"



#plot ----
ggplot(data=top_iphone, mapping=aes(x=x, y=y))+
  #phone outer edge
  geom_rrect(mapping=aes(xmin=-1, xmax=4, ymin=base_ymin, ymax=base_ymax),
             r = unit(0.1, 'npc'), fill="black")+
  #phone background/wallpaper
  geom_rrect(mapping=aes(xmin=-0.85, xmax=3.85, ymin=base_ymin-0.15, ymax=base_ymax+0.15),
             r = unit(0.1, 'npc'), fill="#0184C1")+
  #phone camera edge
  geom_rrect(mapping=aes(xmin=0.75, xmax=2.25, ymin=base_ymax+0.5, ymax=base_ymax),
             r = unit(0.3, 'npc'), fill="black")+
  #phone camera
  geom_point(inherit.aes=FALSE, mapping=aes(x=1, y=-1), color="#313131")+
  #icons at tom
  geom_text(mapping=aes(label="4:30", x=0, y=-0.85), color="white", size=3)+
  geom_richtext(mapping=aes(label="<span style='font-family:fs'>&#xf012;</span>", x=2.7, y=-0.85), fill = NA, label.color = NA,hjust=0.425, size= 3, color="white")+
  geom_richtext(mapping=aes(label="<span style='font-family:fs'>&#xf1eb;</span>", x=3, y=-0.85), fill = NA, label.color = NA,hjust=0.425, size= 3, color="white")+
  geom_richtext(mapping=aes(label="<span style='font-family:fs'>&#xf241;</span>", x=3.3, y=-0.85), fill = NA, label.color = NA,hjust=0.425, size= 3, color="white")+
  #bottom bar main apps
  geom_rrect(mapping=aes(xmin=-0.6, xmax=3.6, ymin=base_ymin-0.3, ymax=base_ymin-1.4),
             r = unit(0.4, 'npc'), fill="#004D71", alpha=0.8)+
  #bottom search
  geom_rrect(mapping=aes(xmin=1, xmax=2, ymin=base_ymin-1.8, ymax=base_ymin-2.1),
             r = unit(0.4, 'npc'), fill="#004D71", alpha=0.8)+
  geom_richtext(mapping=aes(y=base_ymin-1.95, x=1.5, label="<span style='font-family:fs'>&#xf002;</span style='font-family:poppins;'><span> Search</span>"), color="white", label.color=NA, fill=NA, hjust=0.45, size=2.5)+
  #phone apps
  geom_rrect(mapping=aes(xmin=x-0.275, xmax=x+0.275, ymin=y+0.275, ymax=y-0.275), fill="#1FB35F", color="white", size=0.2)+
  #phone app icons
  geom_richtext(mapping=aes(x=x, y=y+0.02, label=icon), size=4.5, fill = NA, label.color = NA,hjust=0.5)+
  #phone app titles
  geom_text(aes(label=category_tag, x=x, y=y+0.45), size=2, color="white", family="poppins")+
  #notification bubbles & text
  geom_point(mapping=aes(x=x+0.27, y=y-0.27, color=count), size=6)+
  geom_text(data=top_iphone|>filter(count>2000), mapping=aes(x=x+0.27, y=y-0.27, label=count_label), size=2, color="white", family="poppins", fontface="bold")+
  geom_text(data=top_iphone|>filter(count<2000), mapping=aes(x=x+0.27, y=y-0.27, label=count_label), size=2, color="black", family="poppins", fontface="bold")+
  #title text
  geom_textbox(mapping=aes(label=title, x=-4, y=3), 
               fill=NA,
               box.colour = NA,
               width = unit(0.4, "npc"))+
  #call out labels 
  geom_text(mapping=aes(x=-2, y=-0.8, label="Product Tag"), color="#757575", family="shadow", size=4)+
  geom_text(mapping=aes(x=-2, y=6.4, label="Total Product \n Count"), color="#757575", family="shadow", size=4)+
  #call out arrows
  geom_curve(mapping=aes(x=-2, xend=-0.3, y=-0.6, yend=0), arrow = arrow(length = unit(0.07, "inch")), size = 0.3,
             curvature=0.2,
               color="#1E1E1E")+
  geom_curve(curvature=-0.2, color="#1E1E1E",
             arrow = arrow(length = unit(0.07, "inch")), size = 0.3,
             mapping=aes(x=-1.9, xend=0.1, y=6, yend=4.7))+
  #scales 
  scale_color_distiller(palette = "YlOrRd", direction=1, 
                        trans = "log",
                        breaks = c(100, 250, 500, 1000, 2000, 3000), 
                        labels=c("100","250","500","1K","2K","3K"),
                       guide=guide_legend(title="PRODUCT COUNT", 
                                          label.position = "bottom",
                                          title.position = "top", 
                                          title.hjust=0.5, 
                                          direction="horizontal", 
                                          override.aes=c(size=4),
                                          #barwidth = 9, barheight=0.5
                                          ))+
  scale_x_continuous(limits=c(-6,4))+
  scale_y_reverse(limits=c(8.5,-1.75), expand=c(0,0))+
  coord_equal()+
  theme_void()+
  theme(legend.position = c(0.725,0.135),
        text = element_text(family="poppins"),
        panel.background = element_rect(fill="white", color="white"),
        plot.margin = margin(t=10, b=10, r=10, l=10),
        plot.title = element_textbox_simple(halign=0.5, valign=-200),
        legend.text = element_text(color="white", size=7),
        legend.title = element_text(color="white", size=7, face="bold"))


ggsave(filename="iphone_plot.png", height=7, width=7,   bg = "white")
