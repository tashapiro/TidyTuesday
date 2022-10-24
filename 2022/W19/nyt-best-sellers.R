library(tidyverse)
library(sysfonts)
library(showtext)
library(ggimage)
library(ggtext)

df <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')


#must download font awesome locally first! https://fontawesome.com/download
sysfonts::font_add('fs', 'fonts/Font Awesome 6 Free-Solid-900.otf')
sysfonts::font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
#must download Chomsky locally https://www.dafont.com/chomsky.font
sysfonts::font_add("Chomsky","fonts/Chomsky.otf")

#fonts from Google font Library
sysfonts::font_add_google("Open Sans","opensans")

showtext::showtext_auto()
#set dpi for showtext to avoid issues with ggsave export
showtext::showtext_opts(dpi = 300)

#Data Manipulation ----
#dataset with top 5 authors by decade
top_five<-df|>
  mutate(decade = year - (year %% 10))|>
  group_by(author, decade)|>
  summarise(appearences = n(),
            books = n_distinct(title_id))|>
  arrange(decade, -appearences, - books)|>
  group_by(decade)|>
  slice_head(n=5)|>
  mutate(rank = row_number(),
         prev_decade = decade-10)|>
  ungroup(decade)

#dataset with total weeks by year for each author and decade 
yearly<-df|>
  filter(author %in% top_five$author & year>=1960 & year<=2019)|>
  select(year, author, title)|>
  group_by(author,year)|>
  summarise(weeks = n())|>
  group_by(author)|>
  complete(year = 1960:2019)|>
  ungroup(author)|>
  mutate(weeks = replace_na(weeks,0), 
         decade = year - (year %% 10))|>
  group_by(author, decade)|>
  summarise(total_weeks = sum(weeks),
            years = list(weeks), .groups = "drop")|>
  filter(total_weeks>0)

#data for top book per author 
top_book<-df|>
  mutate(decade = year - (year %% 10))|>
  group_by(author, title, decade)|>
  summarise(appearences = n())|>
  arrange(decade, -appearences)|>
  group_by(decade, author)|>
  slice_head(n=1)|>
  ungroup(decade)|>
  rename(book_weeks = appearences)


subset<-top_five|>
  left_join(top_book, by=c("author"="author","decade"="decade"))|>
  left_join(yearly|>select(-total_weeks), by=c("author"="author","decade"="decade"))|>
  mutate(image = paste0("https://raw.githubusercontent.com/tashapiro/TidyTuesday/master/2022/W19/authors/",str_replace(str_replace_all(tolower(author)," ","-"),"\\.",""),".png"),
         decade_label = paste0(decade,"s"),
         author_label = case_when(author=="Robert James Waller"~"Robert Waller",
                            author=="James A. Michener"~"James Michener",
                            TRUE ~ author))|>
  filter(decade>=1960 & decade<=2010)





#create html text to use for title & caption with ggtext::element_textbox_simple()
title<-paste0(
  "<span style='font-family:Chomsky;font-size:25pt;color:white;'> The New York Times</span>",
  "<span style='font-family:opensans;font-size:18pt;color:white;'> **Best Selling Authors**</span>",
  "<br><span style='font-family:opensans;font-size:9pt;color:#D6D6D6'>Top authors by decade. Ranking based on number of weeks author appeared on list. Sparkline depicts total weeks by year. Top performing book included beneath each author's name. Data from Post45 Data Collective.</span><br>"
)

caption<-paste0(
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style='font-family:opensans;'> @tanya_shapiro </span>",
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:opensans;'> tashapiro </span>")


#preset aesthetics
pal_bg<-"black"
pal_text<-"white"
pal_text2<-"#D6D6D6"
pal_line<-"#606060"
font_sans<-"opensans"

#dataset to create sparklines witin ggplot -- note, plot uses reverse y axis - apply calc to adjust y values (scale them too)
sparkline<-yearly|>
  left_join(subset|>select(author, decade, rank), by=c("author"="author","decade"="decade"))|>
  filter(!is.na(rank))|>
  unnest(years)|>
  mutate(adjust = -(years * .005) + rank+0.2,
         decade_label=paste0(decade,"s"))

sparkline$x<-rep(seq(3.4, by=.05, length.out=10),30)

#plot
ggplot(subset)+
  geom_text(mapping=aes(y=rank, x=1, label=author_label), hjust=0, size=3.5, fontface="bold", family=font_sans, color=pal_text)+
  geom_text(mapping=aes(y=rank-0.2, x=0.65, label=rank), hjust=0, size=4, family=font_sans, color=pal_text2)+
  geom_text(mapping=aes(y=rank-0.3, x=1, label=paste0(appearences," WEEKS ON THE LIST")), hjust=0, size=2, color=pal_text2, family=font_sans)+
  geom_point(mapping=aes(y=rank-0.3, x=2.65, color=appearences), size=1)+
  geom_text(mapping=aes(y=rank+0.3, x=1, label=str_to_title(title)), hjust=0, size=2.15, family=font_sans, color=pal_text2)+
  geom_segment(mapping=aes(y=rank+0.5, yend=rank+0.5, x=0.5, xend=5), size=0.1, color=pal_line)+
  geom_segment(mapping=aes(y=0.5, yend=0.5, x=0.5, xend=5), size=0.1, color=pal_line)+
  geom_line(data = sparkline, mapping=aes(x=x, y=adjust, group=author), color="white", size=0.2)+
  geom_text(data = sparkline|>filter(x==min(sparkline$x)), mapping=aes(x=x-.07, y=adjust, label=years), hjust=1, color="white", size=1.5)+
  geom_text(data = sparkline|>filter(x==max(sparkline$x)), mapping=aes(x=x+.07, y=adjust, label=years), hjust=0, color="white", size=1.5)+
  geom_image(data=subset, mapping=aes(y=rank, x=4.6, image=image), size=0.115, asp=1.2, color=pal_text2)+
  geom_image(data=subset, mapping=aes(y=rank, x=4.6, image=image), size=0.11, asp=1.2)+
  facet_wrap(~decade_label)+
  scale_y_reverse(limits=c(5.6,0.25), expand=c(0,0))+
  scale_x_continuous(limits=c(0.5,5))+
  scale_color_distiller(palette = "Spectral")+
  labs(title=title, caption = caption)+
  theme_minimal()+
  theme(plot.background = element_rect(fill=pal_bg, color=pal_bg),
        plot.title = element_textbox_simple(halign=0.5, maxwidth = unit(6.75, "in")),
        strip.text.x = element_text(color=pal_text),
        legend.position = "none",
        plot.caption = element_textbox_simple(halign=0.97, hjust=1, color="white", size=7, margin=margin(t=8)),
        panel.grid = element_blank(),
        plot.margin = margin(t=20, b=8),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(family=font_sans, hjust=0.5, lineheight=0.1))



ggsave("nyt_authors2.png", width=9, height=7)
