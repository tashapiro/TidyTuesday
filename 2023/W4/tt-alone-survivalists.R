library(tidyverse)
library(ggbeeswarm)
library(geomtextpath)
library(glue)
library(sysfonts)
library(showtext)
library(ggrepel)

#import fonts

sysfonts::font_add_google("Roboto Slab","rs")
showtext::showtext_auto()
showtext_opts(dpi=300)
sysfonts::font_add('Font Awesome 6 Brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')

#import code for social caption
source("social-caption.R")

#import data
survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')

#create new column for status
survivalists = survivalists|>mutate(status = case_when(reason_category=="Family / personal"~ "Out - Personal",
                                          reason_category=="Medical / health" ~ "Out - Medical",
                                          result==1 ~ "Winner",
                                          TRUE ~ "Out - Loss of Inventory")
)

#create aggregates by gender
summary = survivalists|>
  group_by(gender)|>
  summarise(survivalists=n(),
            avg_days_lasted = mean(days_lasted),
            median_days_lasted = median(days_lasted))


#create plot subtitle + caption
subtitle = "Analysis of survivalists competing on the US reality TV series, **Alone**, across all seasons (1-9). Comparison of days lasted by gender. Although the show has yet to crown a female winner, on average as a group, female survivalists last 13 days longer than male competitors."
#use custom social caption function to generate html for plot caption (used with ggtext)
caption = paste0("Source: {alone}<br>",
                 social_caption(font_family="rs", font_color="black", icon_color="#2B4162",linkedin="shapirotanya", mastodon="fosstodon/tanya_shapiro"))

ggplot(data=survivalists)+
  geom_point(mapping=aes(y=days_lasted, x=gender, fill=status), 
             color="white", shape=21,
             position = position_beeswarm(cex=4.5),
             size=6)+
  geomtextpath::geom_texthline(yintercept = mean(survivalists$days_lasted), color="grey20", linewidth=0.4, linetype="dashed",
                               label = glue("Avg: {round(mean(survivalists$days_lasted),0)} Days"), 
                                hjust = 0.05, vjust=-0.5, family='rs',
                               size=3)+
  geom_point(data=summary, mapping=aes(x=gender, y=avg_days_lasted), size=4.5, shape=23, fill="black", color="white")+
  scale_fill_manual(values=rev(c("#FBB13C","#9AADBF","#C3423F","#2B4162")))+
  scale_x_discrete(labels=c("**Female**<br>(n=20)","**Male**<br>(n=74)"))+
  coord_flip()+
  annotate(geom="text", x="Male", y=100, vjust=2, label="Roland\nWelker", family="rs", color="grey50", size=2)+
  annotate(geom="text", x="Female", y=89, vjust=-1.5, label="Callie\nRussell", family="rs", color="grey50", size=2)+
  annotate(geom="text", x=2.3, y=31, label="Male Avg\n36.2 Days", family="rs", size=2)+
  annotate(geom="text", x=1.28, y=49.5, label="Female Avg\n49.5 Days", family="rs", size=2)+
  geom_segment(mapping=aes(y=49.5, yend=49.5, x=1.2, xend=1), linewidth=0.15)+
  geom_segment(mapping=aes(y=31, yend=36.2, x=2.22, xend=2), linewidth=0.15)+
  labs(
    title = "On average, female survivalists outlast their male counterparts",
    subtitle = subtitle,
    caption = caption, 
       x="",
       y="Days Lasted",
       fill="Survivalist Status")+
  theme(text = element_text(family="rs"),
        axis.ticks = element_blank(),
        legend.position="top",
        legend.justification = c(0, 0),
        legend.title = element_text(face="bold"),
        axis.text.y=ggtext::element_markdown(hjust=0),
        axis.line.x = element_line(linewidth=0.5, color="black"),
        panel.background = element_blank(),
        plot.title = element_textbox_simple(face="bold", size=15, margin = margin(b=10, t=10), width = grid::unit(6.75, "in"), halign=0, hjust=0.15),
        plot.subtitle = element_textbox_simple( width = grid::unit(6.75, "in"), halign=0, hjust=0.15),
        plot.caption = element_textbox_simple(color="black"),
        panel.grid = element_blank(), 
        panel.grid.major.x = element_line(linewidth=0.2, color='grey90'),
        legend.key = element_blank())


ggsave("alone-survivalists.png", bg="white", height=6, width=8)
