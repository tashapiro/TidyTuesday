library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)


bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

bc<-bird_counts%>%separate(species_latin, c("Genus", "Species"))
bc$GenusSpecies<-paste(bc$Genus,bc$Species)
#1950 onwards, not a lot of hrs before...
bc<-bc%>%filter(year>=1950)

#format data
df<-bc%>%group_by(year)%>%
  summarise(total_hrs=mean(total_hours),
            birds_counted=sum(how_many_counted),
            hrly_count=round(sum(how_many_counted)/mean(total_hours),0),
            species_count=n_distinct(GenusSpecies[how_many_counted>=1]),
            genus_count=n_distinct(Genus[how_many_counted>=1]),
            max_for_species=max(how_many_counted))
#get subset of other data with needed details
sub<-data.frame(bc$year,bc$species, bc$Genus, bc$Species, bc$GenusSpecies, bc$how_many_counted)
#merge to get max species details
df<-merge(df,sub, by.x=c("year","max_for_species"), by.y=c("bc.year","bc.how_many_counted"))

pi<-'#f06292'
pur<-'#9c27b0'
or<-'#ff8a65'
am<-'#ffca28'
rd<-'#d32f2f'
gr<-'#4caf50'
li<-'#aeea00'
gy<-'#607d8b'
tl<-'#4db6ac'

colors<-c(rd,pur,tl,or,gy,am,li,pi)

p<-ggplot(df)+
  geom_col(aes(x=year,y=species_count, fill=total_hrs))+
  scale_y_continuous(breaks=c(20,40,60,80,100,120))+
  scale_x_continuous(breaks=c(1950,1960,1970,1980,1990,2000,2010,2017))+
  scale_fill_distiller(name="# Hrs Observed", palette = "Blues", direction=1)+
  geom_point(aes(x=year,y=species_count+5, color=bc.species),size=5)+
  scale_color_manual(name="Most Spotted Bird",values=colors)+
  labs(
    title="Canadian Christmas Birds - Number of Species Observed Per Year",
    subtitle="Number of Species vs. Hours spent observing birds. Most sighted bird based on total # spotted.",
    x="Year",
    y="# of Species"
  )+
  theme_minimal()+
  theme(legend.direction = "vertical")+
  guides(colour = guide_legend(override.aes = list(size=5)))
  
#add annotations - brackets are a workaround
p<-p+
  annotate("segment", x = 1951, xend = 1962, y = 102, yend = 102)+
  annotate("segment", x = 1951, xend = 1951, y = 100, yend = 102)+
  annotate("segment", x = 1962, xend = 1962, y = 100, yend = 102)+
  annotate("text", x = 1957, y = 105, label = "Herring Gull Most Sighted until '63")+
  annotate("segment", x = 1967, xend = 1987, y = 108, yend = 108)+
  annotate("segment", x = 1967, xend = 1967, y = 106, yend = 108)+
  annotate("segment", x = 1987, xend = 1987, y = 106, yend = 108)+
  annotate("text", x = 1977, y = 111, label = "European Starling Overtakes Herring Gull, 20Yr Streak")+
  annotate("segment", x = 2000, xend = 2017, y = 113, yend = 113)+
  annotate("segment", x = 2000, xend = 2000, y = 111, yend = 113)+
  annotate("segment", x = 2017, xend = 2017, y = 111, yend = 113)+
  annotate("text", x = 2009, y = 116, label = "Other Birds Make Top - Greater Scaup, Long-Tailed Duck, & Canada Goose")
  #theme(legend.box = "vertical") 

p


