library(tidyverse)
library(patchwork)
library(ggtext)
library(sysfonts)
library(showtext)




font_add_google("Merriweather", family = "Merriweather")
#must install font awesome brands locally
sysfonts::font_add('Font Awesome 6 Brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto()
showtext::showtext_opts(dpi=300)

font='Merriweather'

emissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-21/emissions.csv')

by_commodity<-emissions|>
  mutate(commodity = case_when(grepl('Coal', commodity) ~ 'Coal', TRUE ~ commodity))|>
  group_by(commodity, year)|>
  summarise(total = sum(total_emissions_MtCO2e))
  
pal<-c("#44af69","#f8333c","#fcab10","#2b9eb3")

df_segment<-data.frame(
  y = c(5000, 10000, 15000),
  x = rep(min(by_commodity$year),3),
  xend = rep(max(by_commodity$year),3)
)

plot<-ggplot(by_commodity)+
  geom_segment(
    data=df_segment,
    mapping=aes(x=x, xend=xend, y=y, yend=y),
    color='#EEEEEE'
  )+
  geom_line(
    mapping=aes(x=year, y=total, group=commodity, color=commodity),
    linewidth=0.75
  )+
  geom_text(
    data = by_commodity|>filter(year == max(year)),
    mapping=aes(x=year+3, y=total, label=commodity, color=commodity),
    family=font,
    show.legend = F,
    hjust=0
  )+
  scale_color_manual(values=pal)+
  scale_x_continuous(
    expand=c(0,0),
    limits=c(min(by_commodity$year), max(by_commodity$year)+30),
    breaks = seq(1860, 2020, by=20))+
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())  
    )+
  theme_minimal()+
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(family=font),
    plot.title = element_text(face='bold')
  )


grey<-'#6C6C6C'
#from social caption file
source("social_caption.R")
caption<-social_caption(twitter=NA, bluesky='@tanyashapiro.bsky.social',linkedin='shapirotanya', font_family=font, font_color=grey, icon_color=grey)

plot + 
plot_annotation(
  title = 'Historical Carbon Emissions by Commodity Type',
  subtitle = "Measured in Million tons of Carbon Dioxide equivalent (MtCO2e). Represents emissions produced by 122 of the world’s largest oil, gas, coal, and
cement producers from 1854 to 2022.",
  caption = c(caption,'<span>Source: Carbon Majors</span>')) & 
theme(text = element_text(family=font),
      plot.subtitle = ggtext::element_textbox(width=0.95, color=grey),
      plot.title = element_text(face='bold', size=16),
      plot.caption = ggtext::element_markdown(color=grey, hjust=c(0,-5), vjust=c(0, 0.01), size=8),
      plot.margin = margin(t=10, b=10, l=10,r=10))


ggsave(filename="carbon_emissions.png", bg='white', height=6, width=8)
