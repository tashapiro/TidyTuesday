library(ggplot2)
library(dplyr)



df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

df$count<-1


df$dynasty<-factor(df$dynasty, 
                   levels=c('Julio-Claudian','Flavian','Nerva-Antonine','Severan',
                            'Gordian','Constantinian','Valentinian','Theodosian'))

#format years
df$reign_start_yr<-as.numeric(format(df$reign_start,'%Y'))
df$reign_end_yr<-as.numeric(format(df$reign_end,'%Y'))

#Change start year for Augustus (BC not AD)
df$reign_start_yr[df$index == "1"] <- -27


rulers<-df%>%group_by(dynasty)%>%summarise(rulers=sum(count))
eras<-df%>%group_by(dynasty)%>%summarise(era_start=format(min(reign_start),'%Y'),era_end=format(max(reign_end),'%Y'))
eras$period<-paste(eras$era_start,'-',eras$era_end)
eras$span<-as.numeric(eras$era_end)-as.numeric(eras$era_start)
causes<-df%>%group_by(cause)%>%summarise(cause_count=sum(count))

causes

df<-merge(df,rulers,by=c("dynasty"="dynasty"))
df<-merge(df,eras,by=c("dynasty"="dynasty"))
df<-merge(df,causes,by=c("cause"="cause"))

df$ruler_count<-paste("Rulers:",df$rulers)
df$era_span<-paste("Span:",df$span,"Yrs")

assassination<-'#FE4A49'
natural<-'#FED766'
battle<-'#C8E087'
captivity<-'#D0CEBA'
exec<- '#2AB7CA'
suicide<-'#646165'
unknown<-'grey'

palette<-c(assassination, natural, exec, battle, suicide,unknown, captivity)


main_data<-df%>%filter(!dynasty %in% c('Valentinian','Theodosian'))

mcolor<-"#323031"

p<-ggplot() + 
  geom_segment(data=main_data, aes(x = reign_start_yr, xend = reign_end_yr, y = reorder(name, -index), yend=name, color = reorder(cause,-cause_count)), size=3)+
  scale_x_continuous(limits=c(-27,400),breaks = c(-27,1,100,200,300,400), labels = c("27 BC","1 AD", "100 AD", "200 AD", "300 AD", "400 AD")) +
  scale_color_manual(values=palette)+
  theme_minimal()+
  theme(
    plot.title=element_text(face="bold",size=12),
    title=element_text(face="bold",color="#373F51"),
    plot.subtitle=element_text(face="italic"),
    legend.text=element_text(colour="grey20",size=10),
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.justification = 'left',
    strip.text.y = element_text(angle =270, color="white"),
    strip.background = element_rect(color=mcolor,fill=mcolor),
    panel.spacing = unit(1, "lines"),
    axis.text.x=element_text(face='bold'),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_line(color="#888888",size=1)
  )+
  facet_grid(dynasty+era_span+ruler_count~., scales = "free_y", space = "free_y", switch='x')+
  labs(
      title='Roman Emperors - Reign Spans & Ultimate Downfalls',
       subtitle="Data from Wikipedia. Spans from Julio-Claudian to Constantian Dynasty.",
       x="TIMELINE",
       y="DYSTASTY | EMPEROR",
       color="Cause of Death")+
    guides(color = guide_legend(title.position = "top"))

p



dat_text <- data.frame(
  label = c('Characterized by \n power struggles and \n short ruling periods \n (avg 2.3 yrs per emperor).',
            'Calm and stable? \n Most rulers die of \n natural causes.',
            'Majority assassinated (80%). \n Two emperors killed by their wives.'
  ),
  dynasty   = c('Gordian','Nerva-Antonine','Julio-Claudian'),
  era_span=c('Span: 50 Yrs','Span: 96 Yrs','Span: 54 Yrs'),
  ruler_count=c('Rulers: 22','Rulers: 7','Rulers: 5'),
  x     =  c(150,300,200),
  y     = c(13,4,3)
)

plot<-p + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label),
  fontface='italic',
  color='#505050',
  size=4
)


ggsave("Roman Emperors2.png", plot = plot, width = 20, height = 40, units = "cm")



