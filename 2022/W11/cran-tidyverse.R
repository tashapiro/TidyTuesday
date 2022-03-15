library(tidyverse)
library(ggtext)

cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

#list of core tidyverse packages
core_tidy <- c('ggplot2','dplyr','tidyr','readr','purrr','tibble','stringr','forcats')


tidy_cran<-cran%>%
  #filter cran dataframe to only include core tidy packages
  filter(package %in% core_tidy)%>%
  #cast dates. messy data has more than one formatting use case_when (e.g. Sat Jun 9 10:58:29 2007 and 2009-12-01-01 12:10:15 UTC)
  mutate(date = case_when(grepl("^[[:digit:]]",(substr(date,4,nchar(date)))) ~ as.Date(date), 
                          TRUE ~ as.Date(substr(date,5,nchar(date)), '%b %d %H:%M:%S %Y'))
  )%>%
  #create columns with date partitioned, e.g. year, month
  mutate(
         first_release = min(date),
         year = as.numeric(format(date,'%Y')),
         month_abbr = format(date,'%b'),
         month_num = format(date,'%m'),
         quarter = ceiling(as.numeric(format(date,'%m'))/3))%>%
  arrange(package,date)

tidy_cran$counter<-1

data<-tidy_cran%>%
  #use "complete" to make sure we have data points for all years and packages
  complete(package = core_tidy, year = min(year):max(year))%>%
  mutate(
    icon = case_when(package!="tibble" ~ paste0('<img src = https://', package,'.tidyverse.org/logo.png </img>'),
                     TRUE ~paste0('<img src = https://', package,'.tidyverse.org/logo.svg </img>'))
    #icon = paste0('icons/',package,'.png')
  )%>%
  #summarize data by year and package, include first_relase date to allow us to reorder plot
  group_by(package,year,icon, first_release)%>%
  summarise(releases = sum(counter))
  #create path for png file for each package, icons downloaded from tidyverse page and stored in "icons" directory
  
  
#build plot with ggplot and geom_tile
ggplot(data%>%filter(package!="tibble"), aes(y=icon, x=year, fill=releases))+
#add border to geom tile
geom_tile(color="white", height=0.95, width=0.95)+
labs(y="", x="Year", fill="Number of Releases")+
scale_x_continuous(breaks=2007:2021)+
coord_equal()+
theme_minimal()+
theme(panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      axis.text.y=element_markdown(color='black', size=0.01),
      legend.position = "top")

