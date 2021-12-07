library(dplyr)
library(tidyr)
library(ggplot2) # needs to be version ≥ 2.1.0
library(scales)
library(GGally)
library(geomnet)
library(ggnetwork)
library(igraph)
library(network)
library(ggnet)

#import data
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

#summary of count by family, species, and genus
family<-df%>%group_by(family)%>%summarise(genus_count=n_distinct(genus),spider_count=n())
genus<-df%>%group_by(family,genus)%>%summarise(spider_count=n())
species<-df%>%group_by(family,genus,species)%>%summarise(spider_count=n())


#get a sample of spider families
sp <-c('Ammoxenidae','Archoleptonetidae','Austrochilidae','Ischnothelidae')
family_sample<-genus%>%filter(family %in% sp)
genus_species_sample<-species%>%filter(family %in% sp)
family_sample<-family_sample%>%select(family,genus)%>%rename("from_id"=family,"to_id"=genus)%>%mutate(family=from_id, level="family")
genus_sample<-genus_species_sample%>%rename("from_id"=genus,"to_id"=species)%>%select(from_id,to_id)%>%mutate(level="genus")
species_sample<-genus_species_sample%>%rename("from_id"=species)%>%select(from_id)%>%mutate(to_id=NA,level="species")
sample<-rbind(family_sample,genus_sample)

#get size of species for each node
sample$size<-ifelse(sample$level=='family',family[match(sample$from_id,family$family),]$spider_count,
ifelse(sample$level=='genus',genus[match(sample$from_id,genus$genus),]$spider_count, 1))

#create network out of dataframe
net_spiders <- as.network(x = sample, # the network object
                          directed = TRUE, # specify whether the network is directed
                          loops = FALSE, # do we allow self ties (should not allow them)
                          matrix.type = "edgelist" # the type of input
)

#create nodecolor based on classification 
net_spiders %v% "nodecolor" = ifelse(grepl(paste0(family$family, collapse="|"),
                                           network.vertex.names(net_spiders)),"FAMILY",
                              ifelse(substr(network.vertex.names(net_spiders),1,1)==toupper(substr(network.vertex.names(net_spiders),1,1)),
                                     "SPECIES","GENUS"))
#create nodesize
net_spiders %v% "size" = ifelse(substr(network.vertex.names(net_spiders),1,1)==toupper(substr(network.vertex.names(net_spiders),1,1)),
                                sample[match(network.vertex.names(net_spiders),sample$from_id),]$size,1)

#palette
col = c("FAMILY" = "#F39237", "SPECIES" = "#BF1363", "GENUS" = "#0E79B2")
col_font = "white"
col_label = "white"
col_background = '#191923'

ggnet2(net_spiders, color="nodecolor", size="size",color.legend = "Classification", 
       size.legend="Number of Species", mode='kamadakawai')+
  scale_color_manual(values=col, guide = guide_legend(override.aes = list(size = 6), title="TAXONOMIC RANK"))+
  scale_size_discrete(range=c(4,10), guide="none")+
  geom_text(aes(label = label), color = col_label, family = "Gill Sans")+
  labs(title="A Tangled Web of Spider Taxonomy",
       subtitle="Taxonomic rank of selected families within the Araneae order",
       caption= "Data from World Spiders Database| Chart by @tanya_shapiro",
       size="Size")+
  theme(plot.background = element_rect(fill=col_background),
        text= element_text(family="Gill Sans", color=col_font),
        plot.title=element_text(family="Gill Sans Bold", size=16, hjust=0.5),
        plot.subtitle=element_text(size=12, hjust=0.5),
        legend.position='top',
        legend.background = element_rect(fill=col_background),
        plot.margin = unit(c(0.5, 0.9, 0.5, 0.9), "cm")
        )

#save image
ggsave("spider_taxonomy.png", height=6, width=9)


