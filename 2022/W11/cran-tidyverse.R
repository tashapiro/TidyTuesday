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
  #create columns with date partitioned by year
  mutate(year = as.numeric(format(date,'%Y')))%>%
  arrange(package,date)

#cheat to summarise data later
tidy_cran$counter<-1

data<-tidy_cran%>%
  #use "complete" to make sure we have data points for all years and packages
  complete(package = core_tidy, year = min(year):max(year))%>%
  #create path for png file for each package, icons downloaded from tidyverse page and stored in "icons" directory
  mutate(
   icon = paste0("<img src = icons/", package, ".png width='45' </>")
  )%>%
  #summarize data by year and package, include first_relase date to allow us to reorder plot
  group_by(package,year,icon)%>%
  summarise(releases = sum(counter))%>%
  #get first release date per package
  merge(tidy_cran%>%group_by(package)%>%summarise(first = min(date))%>%arrange(first), by="package")


#aesthetics 
col_back<-"#0D1116"
col_na <- "#1A232D"

#CREATE GGPLOT
ggplot(data, aes(y=reorder(icon,desc(first)), x=year, fill=releases))+
#add border to geom tile
geom_tile(color=col_back, height=0.92, width=0.92)+
labs(title="TIDYVERSE TIMELINE", 
     subtitle="CRAN package releases for core tidyverse packages by year",
     y="", x="", fill="Number of Releases",
     caption="Data from CRAN archives & Robert Flight | Chart by @tanya_shapiro")+
scale_x_continuous(breaks=2007:2021)+
scale_fill_steps(high='#9BFFC6', low='#008439', na.value=col_na,
                 guide=guide_colorsteps(title.vjust=0.8, direction="horizontal"))+
coord_equal()+
theme_minimal()+
theme(panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background = element_rect(fill=col_back),
      text=element_text(color="white", family="Roboto"),
      plot.title= element_text(face="bold", size = 16, hjust=0.05),
      plot.subtitle=element_text(size = 12, hjust=0.06),
      plot.caption=element_text(size=10, hjust=0.93),
      axis.text.x=element_text(color="white"),
      axis.text.y=element_markdown(color='black', size=0.01),
      plot.margin = margin(l=40,t=20,b=20),
      legend.position = c(0.86,1.03)
)

ggsave("tidyverse-timeline.jpeg", width=15, height=8.5)
