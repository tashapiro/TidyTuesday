library(tidyverse)
library(reactablefmtr)
library(htmltools)
library(htmlwidgets)
library(lubridate)
library(glue)

df_sp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
df_comp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

df_comp <- df_comp|>mutate(company = case_when(stock_symbol=="IBM" ~ "IBM Corporation", TRUE ~ company))

sp_last_date = df_sp|>
  group_by(stock_symbol)|>
  summarise(last_date = max(date))

#minimum value of last date among all companies
last_date = min(sp_last_date$last_date)

#get dates for 1m, 6m ago
date_1m = last_date %m-% months(1) 
date_6m = last_date %m-% months(6) 
date_1y = last_date %m-% months(12) 


df_last_sp <- df_sp|>
  filter(date == last_date)|>
  select(stock_symbol, close)|>
  rename(last=close)

df_trend_1m <- df_sp|>
  filter(date <= last_date & date >= date_1m)|>
  group_by(stock_symbol)|>
  summarise(trend_1m = list(close))

df_trend_6m <- df_sp|>
  filter(date <= last_date & date >= date_6m)|>
  group_by(stock_symbol)|>
  summarise(trend_6m = list(close))

df_trend_1y <- df_sp|>
  filter(date <= last_date & date >= date_1y)|>
  group_by(stock_symbol)|>
  summarise(trend_1y = list(close))


df_values <- df_sp|>
  filter(date %in% c(date_1m, date_6m, date_1y))|>
  select(stock_symbol, date, close)|>
  mutate(close = round(close,2))|>
  pivot_wider(id_cols=stock_symbol, names_from = date, values_from = close)|>
  rename(value_1m = 2, value_6m = 3, value_1y = 4)

df_table<- df_last_sp|>
  left_join(df_values, by="stock_symbol")|>
  left_join(df_trend_1m, by="stock_symbol")|>
  left_join(df_trend_6m, by="stock_symbol")|>
  left_join(df_trend_1y, by="stock_symbol")|>
  mutate(comp_1m = case_when(value_1m>last ~ "#F73131", TRUE ~ "#0BC157"),
         comp_6m = case_when(value_6m>last ~ "#F73131", TRUE ~ "#0BC157"),
         comp_1y = case_when(value_1y>last ~ "#F73131", TRUE ~ "#0BC157"),
         delta_1m = (last - value_1m)/value_1m, 
         delta_6m = (last - value_6m)/value_6m,
         delta_1y = (last - value_1y)/value_1y
         )|>
  select(stock_symbol, last, delta_1m, comp_1m, trend_1m, delta_6m, comp_6m, trend_6m, delta_1y, comp_1y, trend_1y)


#base url for where logo images are hosted
base_url = "https://raw.githubusercontent.com/tashapiro/TidyTuesday/master/2023/W6/logos/"


#function modified from https://glin.github.io/reactable/articles/cookbook/cookbook.html
status_badge <- function(color = "#aaa", width = "0.55rem", height = width) {
  span(style = list(
    display = "inline-block",
    marginRight = "0.5rem",
    width = width,
    height = height,
    backgroundColor = color,
    borderRadius = "50%"
  ))
}


table = reactable(data=df_table,
          fullWidth = FALSE,
          defaultPageSize=14,
          defaultColDef = colDef(vAlign="center", align="center", width=100),
          theme = reactableTheme(
            style=list(fontFamily="Inter")
          ),
          columnGroups = list(
            colGroup(name="1M", columns = c("delta_1m", "trend_1m")),
            colGroup(name="6M", columns = c("delta_6m", "trend_6m")),
            colGroup(name="1Y", columns = c("delta_1y", "trend_1y"))
            ),
          columns = list(
            comp_1y = colDef(show=FALSE),
            comp_1m = colDef(show=FALSE),
            comp_6m = colDef(show=FALSE),
            stock_symbol = colDef(name="COMPANY", 
                                  align="left",
                                  width = 280,
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
                           cell = function(value){
                             price = format(value, nsmall = 2)
                             tagList(
                               span("$", style="text-align:left;display:inline-block;width:4px;"),
                               span(price, style="text-align:right;display:inline-block;width:70px;padding-left:5px;")
                             )
                           }),
            delta_1m = colDef(name="DELTA",
                              cell = function(value, index){
                                color = df_table$comp_1m[index]
                                perc = paste0(abs(round(value*100,1)),"%")
                                bg = ifelse(value<0,"#FCE8E4","#DBFFEA")
                                icon = ifelse(value<0,"arrow-down","arrow-up")
                                tagList(
                                  span(shiny::icon(icon), style=glue("text-align:left;display:inline-block;width:4px;color:{color}")),
                                  span(perc, style=glue("text-align:right;display:inline-block;width:70px;padding-left:5px;color:black;"))
                                )
                              }),
            delta_6m = colDef(name="DELTA",
                              cell = function(value, index){
                                color = df_table$comp_6m[index]
                                perc = paste0(abs(round(value*100,1)),"%")
                                bg = ifelse(value<0,"#FCE8E4","#DBFFEA")
                                icon = ifelse(value<0,"arrow-down","arrow-up")
                                tagList(
                                  span(shiny::icon(icon), style=glue("text-align:left;display:inline-block;width:4px;color:{color}")),
                                  span(perc, style=glue("text-align:right;display:inline-block;width:70px;padding-left:5px;color:black;"))
                                )
                              }),
            delta_1y = colDef(name="DELTA",
                              cell = function(value, index){
                                color = df_table$comp_1y[index]
                                perc = paste0(abs(round(value*100,1)),"%")
                                bg = ifelse(value<0,"#FCE8E4","#DBFFEA")
                                icon = ifelse(value<0,"arrow-down","arrow-up")
                                tagList(
                                  span(shiny::icon(icon), style=glue("text-align:left;display:inline-block;width:4px;color:{color}")),
                                  span(perc, style=glue("text-align:right;display:inline-block;width:70px;padding-left:5px;color:black;"))
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
  htmlwidgets::prependContent(
    tagList(
      h1("BIG TECH STOCKS YE 2022", style='font-family:Inter;margin-bottom:2px;'),
      div("Stock prices for big tech companies as of December 29th, 2022", style='font-family:Inter;padding-bottom:-0.75px;color:#787878;font-size:15px;')
    )
  )%>%
  htmlwidgets::appendContent(
    tagList(
      div("Source: Kaggle, W6 2023 #TidyTuesday", style='font-family:Inter;padding-top:8px;padding-bottom:3px;color:black;margin-top:5px;font-size:14px;border-top-style:solid;border-color:#DADADA;width:1000px;'),
      div(span(shiny::icon("twitter")),
          span("@tanya_shapiro",style='font-family:Inter;font-size:15px;')
          span(shiny::icon("github")),
          span("tashapiro", style='font-family:Inter;font-size:15px;'),
          span(shiny::icon("linkedin")),
          span("shapirotanya", style='font-family:Inter;font-size:15px;'),
          style='display:inline-block;vertical-align:middle;"font-family:Inter;font-size:15px;padding-top:3px;padding-bottom:3px;color:#787878')
      )
    )

table
