library(data.table)
library(dplyr)
library(tidyverse)
library(sf)
library(tidyverse)         # for reading in data, graphing, and cleaning
library(tidymodels)        # for modeling ... tidily
library(glmnet)            # for regularized regression, including LASSO
library(naniar)            # for examining missing values (NAs)
library(lubridate)         # for date manipulation
library(moderndive)        # for King County housing data
library(vip)               # for variable importance plots
library(rmarkdown)         # for paged tables
theme_set(theme_minimal()) # my favorite ggplot2 theme :)



library(themis)            # for step functions for unbalanced data
library(stacks)            # for stacking models
library(lubridate)         # for date manipulation
library(moderndive)        # for King County housing data
library(vip)               # for variable importance plots
library(DALEX)             # for model interpretation  
library(DALEXtra)          # for extension of DALEX
library(patchwork)         # for combining plots nicely
theme_set(theme_minimal()) # Lisa's favorite theme


library(scales)
library(plotly)
library(gridExtra)
library(tidytext)
library(modelr)
library(caret)
library(ROSE)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)


# read_csv("/Users/francosalinas/Desktop/ADV_DATA/final_project/small_accidents.csv")
traffic_final <- read_csv("traffic_final.csv")
traffic_mod <- readRDS("traffic_final_stacked.rds")



##ADD PREPROCESSING IF NECESSARY

Cities <- 
  traffic_mod$train  %>% 
  select(City) %>% 
  distinct(City) %>% 
  arrange(City) %>% 
  pull(City)

# Counties <- 
#   traffic_mod$train  %>% 
#   select(County) %>% 
#   distinct(County) %>% 
#   arrange(County) %>% 
#   pull(County)

Weather <- 
  traffic_mod$train  %>% 
  select(Weather_Condition) %>% 
  distinct(Weather_Condition) %>% 
  arrange(Weather_Condition) %>% 
  pull(Weather_Condition)


# Find min's, max's, and median's for quantitative vars:

stats_num <-
  traffic_mod$train  %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(),
               names_to = "variable", 
               values_to = "value") %>% 
  group_by(variable) %>% 
  summarize(min_val = min(value),
            max_val = max(value),
            med_val = median(value))


### Add model
## git reset HEAD~1 (if we commit something and can't push it)

library(shiny)

ui <- fluidPage(
  # selectInput(inputId = "TMC",
  #                           label = "TMC code",
  #                           choices = list(201,202,203,206,222,229,236,241,244,245,246,247,248,336,339,341,343,406)),
                # sliderInput(inputId = "Year",
                #             label = "Year of Accident",
                #             min = stats_num %>% 
                #               filter(variable =="Year") %>% 
                #               pull(min_val),
                #             max = stats_num %>% 
                #               filter(variable =="Year") %>% 
                #               pull(max_val),
                #             value = stats_num %>% 
                #               filter(variable =="Year") %>% 
                #               pull(med_val), 
                #             step = 1, 
                #             round = TRUE),
                # sliderInput(inputId = "Day",
                #             label = "Day of Accident",
                #             min = stats_num %>% 
                #               filter(variable =="Day") %>% 
                #               pull(min_val),
                #             max = stats_num %>% 
                #               filter(variable =="Day") %>% 
                #               pull(max_val),
                #             value = stats_num %>% 
                #               filter(variable =="Day") %>% 
                #               pull(med_val), 
                #             step = 1, 
                #             round = TRUE),
                sliderInput(inputId = "Hour",
                            label = "Hour of Accident",
                            min = stats_num %>% 
                              filter(variable =="Hour") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Hour") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Hour") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Month",
                            label = "Month of Accident",
                            min = stats_num %>% 
                              filter(variable =="Month") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Month") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Month") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Wday",
                            label = "Week day of Accident",
                            min = stats_num %>% 
                              filter(variable =="Wday") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Wday") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Wday") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Duration",
                            label = "Duration of Accident in seconds",
                            min = stats_num %>% 
                              filter(variable =="Duration") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Duration") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Duration") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Start_Lat",
                            label = "Starting latitude of the Accident",
                            min = stats_num %>% 
                              filter(variable =="Start_Lat") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Start_Lat") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Start_Lat") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Start_Lng",
                            label = "Starting longitude of the Accident",
                            min = stats_num %>% 
                              filter(variable =="Start_Lng") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Start_Lng") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Start_Lng") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Distance",
                            label = "Distance of the Accident",
                            min = stats_num %>% 
                              filter(variable =="Distance") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Distance") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Distance") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                selectInput(inputId = "Side",
                            label = "Side of the street where the accident happened",
                            choices = list(Right = "R",
                                           Left = "L")),
                sliderInput(inputId = "Temperature",
                            label = "Temperature when accident happened",
                            min = stats_num %>% 
                              filter(variable =="Temperature") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Temperature") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Temperature") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Wind_Chill",
                            label = "Wind chill in degrees Farenheit when accident happened",
                            min = stats_num %>%
                              filter(variable =="Wind_Chill") %>%
                              pull(min_val),
                            max = stats_num %>%
                              filter(variable =="Wind_Chill") %>%
                              pull(max_val),
                            value = stats_num %>%
                              filter(variable =="Wind_Chill") %>%
                              pull(med_val),
                            step = 1,
                            round = TRUE),
                sliderInput(inputId = "Humidity",
                            label = "Humidity when accident happened",
                            min = stats_num %>% 
                              filter(variable =="Humidity") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Humidity") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Humidity") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Pressure",
                            label = "Pressure when accident happened",
                            min = stats_num %>% 
                              filter(variable =="Pressure") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Pressure") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Pressure") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Visibility",
                            label = "Visibility when accident happened",
                            min = stats_num %>% 
                              filter(variable =="Visibility") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Visibility") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Visibility") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Wind_Speed",
                            label = "Wind speed when accident happened",
                            min = stats_num %>% 
                              filter(variable =="Wind_Speed") %>% 
                              pull(min_val),
                            max = stats_num %>% 
                              filter(variable =="Wind_Speed") %>% 
                              pull(max_val),
                            value = stats_num %>% 
                              filter(variable =="Wind_Speed") %>% 
                              pull(med_val), 
                            step = 1, 
                            round = TRUE),
                sliderInput(inputId = "Precipitation",
                            label = "Precipitation when accident happened in inches",
                            min = stats_num %>%
                              filter(variable =="Precipitation") %>%
                              pull(min_val),
                            max = stats_num %>%
                              filter(variable =="Precipitation") %>%
                              pull(max_val),
                            value = stats_num %>%
                              filter(variable =="Precipitation") %>%
                              pull(med_val),
                            step = 1,
                            round = TRUE),
                selectInput(inputId = "Crossing",
                            label = "Is there a crossing where the accident happened?",
                            choices = list(Yes = "TRUE",
                                           No = "FALSE")),
                selectInput(inputId = "Junction",
                            label = "Is there a junction where the accident happened?",
                            choices = list(Yes = "TRUE",
                                           No = "FALSE")),
                selectInput(inputId = "Traffic_Signal",
                            label = "Is there a traffic signal where the accident happened?",
                            choices = list(Yes = "TRUE",
                                           No = "FALSE")),
                selectInput(inputId = "Sunrise_Sunset",
                            label = "Is it night or day?",
                            choices = list(Yes = "Night",
                                           No = "Day")),
                selectInput(inputId = "Civil_Twilight",
                            label = "Is there enough natural light to be day?",
                            choices = list(Yes = "Day",
                                           No = "Night")),
                selectInput(inputId = "Nautical_Twilight",
                            label = "Is it nautical day or night?",
                            choices = list("Day","Night")),
                selectInput(inputId = "Astronomical_Twilight",
                            label = "Was the ski illuminated by the sun?",
                            choices = list(Yes = "Day",
                                           No = "Night")),
                selectInput(inputId = "Weather_Condition",
                            label = "Weather condition when accident happened",
                            choices = Weather),
                selectInput(inputId = "City",
                            label = "City where the accident happened",
                            choices = Cities),
                # selectInput(inputId = "County",
                #             label = "County where the accident happened",
                #             choices = Counties),
                ###Check how to use the model to not have to type all the names out.
                                mainPanel(verbatimTextOutput("Pred"))
)



server = function (input,output) {
  output$Pred <- renderPrint({
  data <- tibble(  TMC=201,
                   Month=10,
                   # County = "Rockwall",
                   Hour= 12,
                   Wday=4,
                   Duration=100,
                   Start_Lat= 38.58390,
                   Start_Lng=-121.4604,
                   Distance=0.01,
                   Side="R",
                   Temperature= 70,
                   Humidity=82,
                   Pressure=28.91,
                   Visibility=10,
                   Wind_Speed=0,
                   Wind_Chill= 50,
                   Precipitation=10.02,
                   Crossing= "TRUE",
                   Junction="FALSE",
                   Traffic_Signal="FALSE",
                   Sunrise_Sunset="Night",
                   Civil_Twilight="Day",
                   Nautical_Twilight="Day",
                   Astronomical_Twilight="Day",
                   Weather_Condition="Cloudy",
                   City="Dallas"
                   )
  
                # TMC=input$TMC,
               # Month=input$Month,
               # Hour=input$Hour,
               # Wday=input$Wday,
               # Duration=input$Duration,
               # Start_Lat=input$Start_Lat,
               # Start_Lng=input$Start_Lng,
               # Distance=input$Distance,
               # Side=input$Side,
               # Temperature=input$Temperature,
               # Wind_Chill=input$Wind_Chill,
               # Humidity=input$Humidity,
               # Pressure=input$Pressure,
               # Visibility=input$Visibility,
               # Wind_Speed=input$Wind_Speed,
               # Precipitation=input$Precipitation,
               # Crossing=input$Crossing,
               # Junction=input$Junction,
               # Traffic_Signal=input$Traffic_Signal,
               # Sunrise_Sunset=input$Sunrise_Sunset,
               # Civil_Twilight=input$Civil_Twilight,
               # Nautical_Twilight=input$Nautical_Twilight,
               # Astronomical_Twilight=input$Astronomical_Twilight,
               # Weather_Condition=input$Weather_Condition,
               # City=input$City
              
  
  pred <- 
    predict(traffic_mod,data)
  
  pred}
  )
}
  

shinyApp(ui = ui, server = server)



##Classification or probability:
##Probability that it is classified as severe. 
##Variable importance: having one or two of those local graphs. 
