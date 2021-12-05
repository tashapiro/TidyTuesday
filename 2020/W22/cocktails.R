library(ggplot2)
library(tidyverse)
library(ggimage)
#import datasets
drinks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
b_drinks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')
#focus on cocktails only

my_cocktails<-c(
  'French 75',
  'Kir Royale',
  'Margarita',
  'Pink Lady',
  'Sex on the Beach',
  'Screwdriver',
  'Sidecar',
  'Tom Collins',
  'Whiskey Sour',
  'White Russian',
  'Aviation',
  'Manhattan',
  'Cosmopolitan',
  'Martini',
  'Old Fashioned',
  'Mojito')

cocktails<-drinks%>%filter(drink %in% my_cocktails)
#standardize ingredients with difference cases, convert to upper case
cocktails$ingredient<-toupper(cocktails$ingredient)
#function to convert mixed decimals - useful for measure amts
converter <- function(x) {
  if(grepl('-',x)){eval(parse(text=sub("-", "+",x)))/2} 
  if(x==""){0}
  else {eval(parse(text=sub(" ", "+",x)))}
}

#transform dataset
cocktails<-cocktails%>%
  filter(ingredient!='ICE')%>%
  select(drink,ingredient,measure, glass, drink_thumb)%>%
  mutate(
    measure_type = case_when(
      grepl('splash',measure) ~ 'splash',
      grepl('oz',measure) ~ 'ounce',
      grepl('shot|Shot',measure) ~ 'shot',
      grepl('cup',measure) ~ 'cup',
      grepl('tblsp|tbsp',measure) ~ 'tblsp',
      grepl('tsp',measure) ~ 'tsp',
      grepl('jigger',measure) ~ 'shot',
      grepl('cl|cL',measure) ~ 'cl',
      grepl('can',measure) ~ 'can',
      grepl('dash|Dash',measure) ~ 'dash',
      grepl('drop',measure) ~ 'drop',
      grepl('part|Part|measure',measure) ~ 'part',
      grepl('ml',measure) ~ 'ml',
      grepl('Fill|Top',measure) ~ 'remainder',
      TRUE ~ 'garnish'
    ),
    ingredient_type = case_when(
      grepl('JUICE|TROPICANA|SYRUP|GRENADINE|SUGAR',ingredient) ~ 'JUICE/SWEETENER',
      grepl('BITTERS',ingredient) ~ 'BITTERS',
      grepl('VERMOUTH|CHAMPAGNE',ingredient) ~ 'OTHER ALCOHOL',
      grepl('VODKA|ABSOLUT',ingredient) ~ 'VODKA',
      grepl('TEQUILA',ingredient) ~ 'TEQUILA',
      grepl('COINTREAU|TRIPLE SEC|LIQUEUR|CREME|SCHNAPPS',ingredient) ~ 'LIQUEUR',
      grepl('RUM',ingredient) ~ 'RUM',
      grepl('GIN| ST.GERMAIN',ingredient) ~ 'GIN',
      grepl('BOURBON|WHISKEY',ingredient) ~ 'WHISKEY',
      grepl('ORANGE PEEL|MARASCHINO CHERRY|OLIVE',ingredient) ~ 'GARNISH',
      TRUE ~ 'OTHER MIXER'),
    measure_cleaned = case_when(
      measure == '70ml/2fl oz' ~ "2",
      grepl('1-2',measure) ~ "1.5",
      grepl('2-3',measure) ~ "2.5",
      grepl('2-4',measure) ~ "3",
      grepl('/',measure) ~ str_trim(gsub("[^0-9./]", " ", measure),side="both"),
      TRUE ~ gsub("[^0-9.-]", "", measure)
    ))
#convert character amts to numeric
measure_amt_list<-apply(X = cocktails[,8], MARGIN = 1, FUN = converter)
measure_amt_df<-data.frame(measure_amt_list)
names(measure_amt_df)[1] <- 'measure_amt'
cocktails<-cbind(cocktails,measure_amt_df)



#excluse garnish from amounts
cocktails<-cocktails%>%filter(measure_type!='garnish')
cocktails<-cocktails%>%
  mutate(
    measure_oz=case_when(
      measure_type=='ounce'~measure_amt*1,
      measure_type=='shot'~measure_amt*1.5,
      measure_type=='cup'~measure_amt*8,
      measure_type=='cl'~measure_amt*0.33814,
      measure_type=='ml'~measure_amt*0.033814,
      measure_type=='tsp'~measure_amt*0.166667,
      measure_type=='tblsp'~measure_amt*0.5,
      measure_type=='drop'~measure_amt*0.0016907,
      measure_type=='part'~measure_amt*1,
      measure_type=='dash'~measure_amt*0.021,
      measure_type=='can'~measure_amt*11,
      TRUE ~ measure_amt
    )
  )
#manually fix screwdriver, appears as 100% vodka, missing OJ!
screwdriver<-data.frame('Screwdriver',
                        'ORANGE JUICE',
                        '4 oz', 
                        'Highball glass',
                        'thumb',
                        'ounce',
                        'JUICE/SWEETENER',
                        '2',
                        4,
                        4)
names(screwdriver)<-names(cocktails)
cocktails<-rbind(cocktails,screwdriver)


#add measure amt back to origina data frame
cocktail_total<-cocktails%>%group_by(drink)%>%summarise(total_oz=sum(measure_oz))
cocktails<-left_join(cocktails,cocktail_total, by=c("drink"="drink"))
cocktails$measure_oz_perc<-cocktails$measure_oz/cocktails$total_oz

#create factor out of cocktail ingredient type to order legend
cocktails$ingredient_type<-factor(cocktails$ingredient_type,levels=
          c('GIN','RUM','TEQUILA','VODKA','WHISKEY',
            'LIQUEUR','OTHER ALCOHOL','JUICE/SWEETENER','BITTERS','OTHER MIXER'))

#remove ingredients with 0 measure amt 
plot_df<-cocktails%>%filter(measure_amt>0)
plot_df$ymax = ave(plot_df$measure_oz_perc, plot_df$drink, FUN=cumsum)
plot_df<-plot_df %>%
  group_by(drink) %>%
  mutate(ymin = dplyr::lag(ymax, n = 1, default = 0))
plot_df$labelPosition <- (plot_df$ymax + plot_df$ymin) / 2
# Compute a good label
plot_df$label <-plot_df$ingredient
plot_df$glass_icon<-paste('icons/',toupper(plot_df$glass),".png",sep='')


col_font='white'
ggplot(plot_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=ingredient_type)) +
  geom_rect() +
  geom_text( x=3.55, aes(y=labelPosition, label=stringr::str_wrap(label, 6)),  color=col_font, size=2) + # x here controls label position (inner / outer)
  geom_text(data=plot_df%>%filter(ingredient_type %in% c('GIN','RUM','TEQUILA')),
                               x=3.55, aes(y=labelPosition, label=stringr::str_wrap(label, 6)),  
                               color='black', size=2) +
  geom_image(x=1, y=0.5, aes(image=glass_icon), alpha=0.8,size=0.3)+
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  ylim(0,1)+
  facet_wrap(~toupper(drink))+
  scale_fill_viridis_d(direction =-1)+
  scale_color_viridis_d(direction =-1)+
  guides(fill = guide_legend(title = "Ingredient Type", title.position = "top",
                             title.hjust = 0.5))+
  theme_void() +
  theme(
    plot.title = element_text(hjust=0.5, family="Gill Sans Bold", size=16, color=col_font,vjust=8),
    plot.subtitle = element_text(hjust=0.5, size=12, vjust=10),
    plot.margin = unit(c(1.5,1,1,1), "cm"),
    text = element_text(family="Gill Sans", color=col_font),
    legend.position="top",
    plot.background = element_rect('#191919'),
    legend.spacing.x = unit(0.6, 'cm'),
    legend.box.margin=margin(0,0,25,0)
    #legend.text = element_text(margin = margin(r = 30, unit = "pt"))
  )+
  labs(
    title="MIXOLOGY 101",
    subtitle="Cocktail Ingredient Ratio for Popular Drinks",
    caption = "Data from Kaggle | Chart @tanya_shapiro",
    fill = "Ingredient Type"
  )


ggsave('cocktails.png',height=12.5,width=10)
