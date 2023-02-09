library(tidyverse)
library(reactablefmtr)
library(htmltools)
library(htmlwidgets)
library(lubridate)
library(glue)

#import data
df_sp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
df_comp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

#change IBM company name to short form
df_comp <- df_comp|>mutate(company = case_when(stock_symbol=="IBM" ~ "IBM Corporation", TRUE ~ company))

#look at the most recent date of stock values per company
sp_last_date = df_sp|>
  group_by(stock_symbol)|>
  summarise(last_date = max(date))

#minimum value of last date among all companies
last_date = min(sp_last_date$last_date)

#get dates for 1m, 6m ago
date_1m = last_date %m-% months(1) 
date_6m = last_date %m-% months(6) 
date_1y = last_date %m-% months(12) 

#get last close value of stock per company
df_last_sp <- df_sp|>
  filter(date == last_date)|>
  select(stock_symbol, close)|>
  rename(last=close)

#aggregate by company list of stock prices past month
df_trend_1m <- df_sp|>
  filter(date <= last_date & date >= date_1m)|>
  arrange(stock_symbol, date)|>
  group_by(stock_symbol)|>
  summarise(trend_1m = list(close))

#aggregate by company list of stock prices past 6 months
df_trend_6m <- df_sp|>
  filter(date <= last_date & date >= date_6m)|>
  group_by(stock_symbol)|>
  summarise(trend_6m = list(close))

#aggregate by company list of stock prices past year
df_trend_1y <- df_sp|>
  filter(date <= last_date & date >= date_1y)|>
  group_by(stock_symbol)|>
  summarise(trend_1y = list(close))

#get the stock prices per company 1 month ago, 6 months ago, and 1 year ago
df_values <- df_sp|>
  filter(date %in% c(date_1m, date_6m, date_1y))|>
  select(stock_symbol, date, close)|>
  mutate(close = round(close,2))|>
  pivot_wider(id_cols=stock_symbol, names_from = date, values_from = close)|>
  rename(value_1y = 2, value_6m = 3, value_1m = 4)


df_table<- df_last_sp|>
  #combine and merge all data sets - values & trends
  left_join(df_values, by="stock_symbol")|>
  left_join(df_trend_1m, by="stock_symbol")|>
  left_join(df_trend_6m, by="stock_symbol")|>
  left_join(df_trend_1y, by="stock_symbol")|>
  #add a color comp variable to change table fonts based on positive/negative values
  mutate(comp_1m = case_when(value_1m>last ~ "#F73131", TRUE ~ "#0BC157"),
         comp_6m = case_when(value_6m>last ~ "#F73131", TRUE ~ "#0BC157"),
         comp_1y = case_when(value_1y>last ~ "#F73131", TRUE ~ "#0BC157"),
         #create delta values
         delta_1m = (last - value_1m)/value_1m, 
         delta_6m = (last - value_6m)/value_6m,
         delta_1y = (last - value_1y)/value_1y
         )|>
  #rearrange order of variables before passing it into table
  select(stock_symbol, last, delta_1m, comp_1m, trend_1m, delta_6m, comp_6m, trend_6m, delta_1y, comp_1y, trend_1y)


#base url for where logo images are hosted
base_url = "https://raw.githubusercontent.com/tashapiro/TidyTuesday/master/2023/W6/logos/"

font="Archivo"

  
#create table
reactable(data=df_table,
          fullWidth = FALSE,
          defaultPageSize=14,
          defaultColDef = colDef(vAlign="center", align="center", width=100),
          theme = reactableTheme(
            style=list(fontFamily=font)
          ),
          #create grouped columns (equivalent of table spanners in gt)
          columnGroups = list(
            colGroup(name="1M", columns = c("delta_1m", "trend_1m")),
            colGroup(name="6M", columns = c("delta_6m", "trend_6m")),
            colGroup(name="1Y", columns = c("delta_1y", "trend_1y"))
            ),
          #customize individual columns
          columns = list(
            #hid columns with color reference values
            comp_1y = colDef(show=FALSE),
            comp_1m = colDef(show=FALSE),
            comp_6m = colDef(show=FALSE),
            stock_symbol = colDef(name="COMPANY", 
                                  align="left",
                                  width = 280,
                                  #plug in html for stock symbols - add image, company name, and symbol value underneath
                                  cell = function(value){
                                    #create image variable
                                    img_url <- paste0(base_url,value,".png")
                                    company_name = df_comp$company[df_comp$stock_symbol==value]
                                    #use htmltools to create divs
                                    htmltools::tagList(
                                      div(style = "display: inline-block;vertical-align:middle;width:50px",
                                          img(src=img_url, style = "height: 33px;")
                                          ),
                                      div(style="display: inline-block;vertical-align:middle;",
                                          div(style = "vertical-align:middle;font-weight:bold;", company_name),
                                          div(style = "vertical-align:middle;font-size:8pt;color:#8C8C8C;padding-top:5px;", paste0("SYMBL: ",value))
                                    )
                                    )
                                  }),
            last = colDef(name = "VALUE",
                          width=120,
                          #format last value with html to align $ symbol on left and USD value on right
                           cell = function(value){
                             price = format(value, nsmall = 2)
                             tagList(
                               span("$", style="text-align:left;display:inline-block;width:4px;font-weight:bold;"),
                               span(price, style="text-align:right;display:inline-block;width:70px;padding-left:5px;font-weight:bold;")
                             )
                           }),
            delta_1m = colDef(name="DELTA",
                              #use cell_style to add left sided border
                              style = cell_style(
                                border_color = "#f2f2f2",
                                border_style = "solid",
                                border_width = "1px 0px 0px 2px",
                                vertical_align = "center"
                              ),
                              #use custom function to create html for deltas (arrow direction, colors, etc)
                              cell = function(value, index){
                                color = df_table$comp_1m[index]
                                perc = paste0(abs(round(value*100,1)),"%")
                                bg = ifelse(value<0,"#FCE8E4","#DBFFEA")
                                icon = ifelse(value<0,"arrow-down","arrow-up")
                                tagList(
                                  span(shiny::icon(icon), style=glue("text-align:left;display:inline-block;width:4px;color:{color}")),
                                  span(perc, style=glue("text-align:right;display:inline-block;width:60px;color:black;"))
                                )
                              }),
            delta_6m = colDef(name="DELTA",
                              style = cell_style(
                                border_color = "#f2f2f2",
                                border_style = "solid",
                                border_width = "1px 0px 0px 2px",
                                vertical_align = "center"
                              ),
                              cell = function(value, index){
                                color = df_table$comp_6m[index]
                                perc = paste0(abs(round(value*100,1)),"%")
                                bg = ifelse(value<0,"#FCE8E4","#DBFFEA")
                                icon = ifelse(value<0,"arrow-down","arrow-up")
                                tagList(
                                  span(shiny::icon(icon), style=glue("text-align:left;display:inline-block;width:4px;color:{color}")),
                                  span(perc, style=glue("text-align:right;display:inline-block;width:60px;color:black;"))
                                )
                              }),
            delta_1y = colDef(name="DELTA",
                              style = cell_style(
                                border_color = "#f2f2f2",
                                border_style = "solid",
                                border_width = "1px 0px 0px 2px",
                                vertical_align = "center"
                              ),
                              cell = function(value, index){
                                color = df_table$comp_1y[index]
                                perc = paste0(abs(round(value*100,1)),"%")
                                bg = ifelse(value<0,"#FCE8E4","#DBFFEA")
                                icon = ifelse(value<0,"arrow-down","arrow-up")
                                tagList(
                                  span(shiny::icon(icon), style=glue("text-align:left;display:inline-block;width:4px;color:{color}")),
                                  span(perc, style=glue("text-align:right;display:inline-block;width:60px;color:black;"))
                                )
                              }),
            trend_1m = colDef(name = "TREND", 
                              cell = react_sparkline(data = df_table,
                                                     line_curve = "linear",
                                                     show_area = TRUE,
                                                     line_color_ref = "comp_1m",
                                                     area_color_ref = "comp_1m")),
            trend_6m = colDef(name = "TREND", 
                               cell = react_sparkline(data = df_table,
                                                      line_curve = "linear",
                                                      show_area = TRUE,
                                                      line_color_ref = "comp_6m",
                                                      area_color_ref = "comp_6m")),
            trend_1y = colDef(name = "TREND", 
                              cell = react_sparkline(data = df_table,
                                                     line_curve = "linear",
                                                     show_area = TRUE,
                                                     line_color_ref = "comp_1y",
                                                     area_color_ref = "comp_1y"))
          ))%>%
  #use htmlwidgets to add title and subtitle with prependContent
  htmlwidgets::prependContent(
    tagList(
      h1("THE RISE & FALL OF BIG TECH 2022", style=glue('font-family:{font};margin-bottom:4px;font-size:35px;')),
      div("Stock prices for big tech companies as of December 29th, 2022. All companies show negative one year trends except IBM.", style=glue('font-family:{font};padding-bottom:5px;color:#A2A2A2;font-size:17px;'))
    )
  )%>%
  #use htmlwidgets to add table footer with info on Source + personal social icons
  htmlwidgets::appendContent(
    tagList(
      div(
      div("Source: Kaggle | W6 2023 #TidyTuesday", style=glue('display:inline-block;align:right;text-align:left;vertical-align:middle;font-family:{font};font-size:15px;margin-top:10px;padding-bottom:3px;color:#787878;width:500px;')),
      div(span(shiny::icon("twitter", style="color:#0BC157;")),
          span("@tanya_shapiro",style=glue('font-family:{font};font-size:15px;')),
          span(shiny::icon("github",style="color:#0BC157;")),
          span("tashapiro", style=glue('font-family:{font};font-size:15px;')),
          span(shiny::icon("linkedin",style="color:#0BC157;")),
          span("shapirotanya", style=glue('font-family:{font};font-size:15px;')),
          style=glue('display:inline-block;align:right;text-align:right;vertical-align:middle;font-family:{font};font-size:15px;margin-top:10px;padding-bottom:3px;color:#787878;width:485px;'))
    , style='width:1000px;border-top-style:solid;border-color:#f2f2f2;')
    )
  )

