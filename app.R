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
read_csv("/Users/francosalinas/Desktop/ADV_DATA/final_project/traffic_final.csv")


##ADD PREPROCESSING IF NECESSARY

### Add model
  
library(shiny)

ui <- fluidPage(selectInput(inputId = "TMC",
                            label = "TMC code",
                            choices = list(201,202,203,206,222,229,236,241,244,245,246,247,248,336,339,341,343,406)),
                selectInput(inputId = "Severity",
                            label = "Accident's Severity",
                            choices = list(1,2,3,4)),
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
                selectInput(inputId = "City",
                            label = "City where the accident happened",
                            choices = City)
                

)


server <- function(input, output) {}
shinyApp(ui = ui, server = server)





