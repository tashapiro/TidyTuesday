library(tidytuesdayR)
library(tidyverse)
library(ggradar)

#import dataset
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')


#create data for plot
data<-breed_traits%>%
  cbind(breed_rank_all%>%select(`2020 Rank`))%>%
  arrange(`2020 Rank`)%>%
  #get top 20 based on 2020 rank
  head(20)%>%
  #select personality features (traits)
  select('Breed',`Energy Level`,`Openness To Strangers`, `Barking Level`,
         `Watchdog/Protective Nature`,`Playfulness Level`,`Trainability Level`,
         `Adaptability Level`)%>%
  #relabel to make it friendly for axis!
  rename(Energy = `Energy Level`,
         Open = `Openness To Strangers`,
         Play = `Playfulness Level`,
         Protect = `Watchdog/Protective Nature`,
         Bark = `Barking Level`,
         Train = `Trainability Level`,
         Adapt = `Adaptability Level`)

#create factor to help order ggplot facet (will default to alphabetical)
data$Breed<-factor(data$Breed, levels=data$Breed)

#get names of columns
names(t)[data]<-"group"

#radar plot
data%>%
  ggradar(
    grid.min = 0, grid.mid = 2.5, grid.max = 5,
    values.radar = c("0", "2.5","5"),
    group.point.size = 2 ,
    group.line.width = 1 ,
    grid.label.size=4,
    axis.label.size=4,
    gridline.mid.colour = "grey",
    group.colours = "#56638A"
  )+
  facet_wrap(vars(group), ncol=5)+
  labs(title="PERSONALITIES OF PAWPULAR DOG BREEDS",
       subtitle="Based on traits scaled from 1 (low) to 5 (high). Popularity rank based on American Kennel Club registration statistics from 2020.",
       caption="Data from the American Kennel Club | Chart by @tanya_shapiro")+
  theme_void()+
  theme(legend.position="none",
        strip.text=element_text(size=11, family="Roboto", face='bold'),
        text = element_text(family="Roboto"),
        plot.subtitle  = element_text(hjust=0.5, margin=margin(0,0,30,0)),
        plot.caption=element_text(size=10),
        plot.margin=margin(20,0,10,0),
        plot.title=element_text(hjust=0.5, face="bold", size=18, margin=margin(0,0,10,0)))


