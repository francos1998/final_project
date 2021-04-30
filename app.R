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
traffic_mod <- readRDS("traffic_final_stack.rds")

##ADD PREPROCESSING IF NECESSARY
City <- 
  traffic_mod$train  %>% 
  select(City) %>% 
  distinct(City) %>% 
  arrange(City) %>% 
  pull(City)

# Fix employment length

Weather_Condition <- 
  traffic_mod$train %>% 
  select(Weather_Condition) %>% 
  distinct() %>% 
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
  
library(shiny)

ui <- fluidPage(selectInput(inputId = "TMC",
                            label = "TMC code",
                            choices = list(201,202,203,206,222,229,236,241,244,245,246,247,248,336,339,341,343,406)),
                sliderInput(inputId = "Year",
                            label = "Year of Accident",
                            min = min(traffic_final$Year),
                            max = max(traffic_final$Year),
                            value = c(min(traffic_final$Year)),
                            sep = ""),
                sliderInput(inputId = "Month",
                            label = "Month of Accident",
                            min = min(traffic_final$Month),
                            max = max(traffic_final$Month),
                            value = c(min(traffic_final$Month)),
                            sep = ""),
                sliderInput(inputId = "Day",
                            label = "Day of Accident",
                            min = min(traffic_final$Day),
                            max = max(traffic_final$Day),
                            value = c(min(traffic_final$Day)),
                            sep = ""),
                sliderInput(inputId = "Hour",
                            label = "Hour of Accident",
                            min = min(traffic_final$Hour),
                            max = max(traffic_final$Hour),
                            value = c(min(traffic_final$Hour)),
                            sep = ""),
                sliderInput(inputId = "Wday",
                            label = "Week day of Accident",
                            min = min(traffic_final$Wday),
                            max = max(traffic_final$Wday),
                            value = c(min(traffic_final$Wday)),
                            sep = ""),
                sliderInput(inputId = "Duration",
                            label = "Duration of Accident",
                            min = min(traffic_final$Duration),
                            max = max(traffic_final$Duration),
                            value = c(min(traffic_final$Duration)),
                            sep = ""),
                sliderInput(inputId = "Start_Lat",
                            label = "Starting latitude of the Accident",
                            min = min(traffic_final$Start_Lat),
                            max = max(traffic_final$Start_Lat),
                            value = c(min(traffic_final$Start_Lat)),
                            sep = ""),
                sliderInput(inputId = "Start_Lng",
                            label = "Starting longitude of the Accident",
                            min = min(traffic_final$Start_Lng),
                            max = max(traffic_final$Start_Lng),
                            value = c(min(traffic_final$Start_Lng)),
                            sep = ""),
                sliderInput(inputId = "Distance",
                            label = "Distance of the Accident",
                            min = min(traffic_final$Distance),
                            max = max(traffic_final$Distance),
                            value = c(min(traffic_final$Distance)),
                            sep = ""),
                selectInput(inputId = "Side",
                            label = "Side of the street where the accident happened",
                            choices = list(Right = "R",
                                           Left = "L")),
                sliderInput(inputId = "Temperature",
                            label = "Temperature when accident happened",
                            min = min(traffic_final$Temperature),
                            max = max(traffic_final$Temperature),
                            value = c(min(traffic_final$Temperature)),
                            sep = ""),
                sliderInput(inputId = "Wind_Chill(F)",
                            label = "Wind chill in degrees Farenheit when accident happened",
                            min = min(traffic_final$`Wind_Chill(F)`),
                            max = max(traffic_final$`Wind_Chill(F)`),
                            value = c(min(traffic_final$`Wind_Chill(F)`)),
                            sep = ""),
                sliderInput(inputId = "Humidity",
                            label = "Humidity when accident happened",
                            min = min(traffic_final$Humidity),
                            max = max(traffic_final$Humidity),
                            value = c(min(traffic_final$Humidity)),
                            sep = ""),
                sliderInput(inputId = "Pressure",
                            label = "Pressure when accident happened",
                            min = min(traffic_final$Pressure),
                            max = max(traffic_final$Pressure),
                            value = c(min(traffic_final$Pressure)),
                            sep = ""),
                sliderInput(inputId = "Visibility",
                            label = "Visibility when accident happened",
                            min = min(traffic_final$Visibility),
                            max = max(traffic_final$Visibility),
                            value = c(min(traffic_final$Visibility)),
                            sep = ""),
                sliderInput(inputId = "Wind_Speed",
                            label = "Wind speed when accident happened",
                            min = min(traffic_final$Wind_Speed),
                            max = max(traffic_final$Wind_Speed),
                            value = c(min(traffic_final$Wind_Speed)),
                            sep = ""),
                sliderInput(inputId = "Precipitation(in)",
                            label = "Precipitation when accident happened in inches",
                            min = min(traffic_final$`Precipitation(in)`),
                            max = max(traffic_final$`Precipitation(in)`),
                            value = c(min(traffic_final$`Precipitation(in)`)),
                            sep = ""),
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
                sliderInput(inputId = "Weather_Condition",
                            label = "Weather condition when accident happened",
                            min = min(traffic_final$`Precipitation(in)`),
                            max = max(traffic_final$`Precipitation(in)`),
                            value = c(min(traffic_final$`Precipitation(in)`)),
                            sep = ""),
                selectInput(inputId = "City",
                            label = "City where the accident happened",
                            choices = City),
###Check how to use the model to not have to type all the names out. 
                mainPanel(textOutput("Pred")))



server = function (input,output) {
  data <- reactive({
    data.frame(TMC=input$TMC,
               Year=input$Year,
               Month=input$Month,
               Day=input$Day,
               Hour=input$Hour,
               Wday=input$Wday,
               Duration=input$Duration,
               Start_Lat=input$Start_Lat,
               Start_Lng=input$Start_Lng,
               Distance=input$Distance,
               Side=input$Side,
               Temperature=input$Temperature,
               `Wind_Chill(F)`=input$`Wind_Chill(F)`,
               Humidity=input$Humidity,
               Pressure=input$Pressure,
               Visibility=input$Visibility,
               Wind_Speed=input$Wind_Speed,
               `Precipitation(in)`=input$`Precipitation(in)`,
               Crossing=input$Crossing,
               Junction=input$Junction,
               Traffic_Signal=input$Traffic_Signal,
               Sunrise_Sunset=input$Sunrise_Sunset,
               Civil_Twilight=input$Civil_Twilight,
               Nautical_Twilight=input$Nautical_Twilight,
               Astronomical_Twilight=input$Astronomical_Twilight,
               Weather_Condition=input$Weather_Condition,
               City=input$City
               )
  })
  
  pred <- reactive({
    predict(traffic_mod,data())
  })
  
  output$Pred <- renderPrint(pred())
}
  
shinyApp(ui = ui, server = server)



##Classification or probability:
  ##Probability that it is classified as severe. 
  ##Variable importance: having one or two of those local graphs. 
