library(tidyverse)
library(lubridate)
library(gt)
library(gtExtras)
library(pelotonR)
#link to install pelotonR in the comment below
#devtools::install_github("lgellis/pelotonR")

#different class types
class_types <- get_metadata_mapping_df('class_types')

#set up credentials to get personal data, enter username and password as string, replace with your own username & password
auth_response <-authenticate("username",'password')

#get list of workouts
workouts <-get_workouts_df() 
#get list of instructors
instructors<-get_instructors_df()

#append list of instructors by id to workiuts
my_workouts<-left_join(workouts,instructors, by=c("peloton.ride.instructor_id"="id"))
#convert epoch time to standard time
my_workouts$start_time<-as.POSIXct(my_workouts$created_at, origin="1970-01-01")
my_workouts$start_year<-format(my_workouts$start_time, '%Y')
my_workouts$end_time<-as.POSIXct(my_workouts$end_time, origin="1970-01-01")

#calculate duration of workouts, remove any NA times
my_workouts$duration<-(my_workouts$end_time - my_workouts$start_time)/60
my_workouts<-my_workouts%>%drop_na(duration)
my_workouts$start_year<-format(my_workouts$start_time, '%Y')

my_workouts %>%
  filter(start_year==2021)%>%
  mutate(month2 = format(start_time,"%Y-%m")) %>%
  group_by(month2)%>%summarise(workouts=n())

by_month<-my_workouts %>%
  filter(start_year==2021)%>%
  mutate(month_num = format(start_time,"%m"), month_name=format(start_time,'%b')) %>%
  group_by(month_num,month_name)%>%summarise(workouts=n(), minutes=sum(duration))

month_nums<-unique(by_month$month_num)

#get trend of workouts per month
monthly_workouts<-my_workouts %>%
  filter(start_year==2021)%>%
  mutate(month_num = format(start_time,"%m")) %>%
  group_by(instructor,month_num)%>%
  summarise(workouts=n(), minutes=sum(duration))%>%
  complete(month_num=month_nums)%>%
  mutate(workouts = ifelse(is.na(workouts), 0, workouts))%>%
  group_by(instructor)%>%
  summarise(workout_data = list(workouts), .groups = "drop")

#get top class type/discipline by instructor
top_discipline<-my_workouts%>%
  filter(start_year==2021)%>%
  group_by(instructor,fitness_discipline)%>%
  summarise(workout_time=as.numeric(round(sum(duration)/60,1)))%>%
  slice(which.max(workout_time))%>%
  rename("top_discipline"="fitness_discipline","top_time"="workout_time")

#creating the data 
data<-my_workouts%>%
  filter(start_year==2021)%>%
  group_by(instructor)%>%
  summarise(workouts=n(),
            workout_time=round(sum(duration)/60,1),
            avg_difficulty = round(mean(peloton.ride.difficulty_rating_avg),2)
            )%>%
  left_join(top_discipline, by=c("instructor"="instructor"))%>%
  left_join(monthly_workouts, by=c("instructor"="instructor"))%>%
  mutate(image=paste0("https://github.com/tashapiro/peloton-stats/blob/main/images/instructors/",
                      gsub(' ','%20',instructor),".jpg?raw=true",sep=""),
         top_discipline=str_to_title(gsub('_',' ',top_discipline)))%>%
  arrange(-workouts)%>%
  head(10)%>%
  select(image,instructor,top_discipline,avg_difficulty,workout_time,workouts, workout_data )

#creating the table
data%>%
  gt()%>%
  gt_img_rows(image)%>%
  gt_color_rows(avg_difficulty, 
                palette = "ggsci::amber_material", 
                direction=1,
                type="continuous")%>% 
  gt_sparkline(workout_data,  
               type="sparkline",
               range_colors=c("#FE6847","#FBB13C"),
               line_color = "#BFC2C5")%>%
  cols_label(
    image = "",
    instructor = "Instructor",
    workouts =  "Workouts",
    avg_difficulty = "Avg Difficulty",
    workout_time = "Total Hrs",
    top_discipline = "Top Class Type",
    workout_data = "Monthly Trend"
  )%>%
  tab_header(title = md("MY PELOTON INSTRUCTORS 2021"),
             subtitle= md("Personal Workout Summary by Instructor"))%>%
  tab_source_note("Data from Peloton API | Table by @tanya_shapiro")%>%
  tab_footnote("Number of workouts per month",
               locations = cells_column_labels(columns = workout_data))%>%
  gt_theme_538()
