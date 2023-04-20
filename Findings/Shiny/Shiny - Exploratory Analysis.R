library(shiny)
library(tidyverse)
library(ggplot2)
ui <- fluidPage(
  selectInput("attribute", "Attribute:",
              c("age",
                "age_group",
                "gender",
                "bmi_group",
                "smoker"
              )),plotOutput("plot")
)
# CalcNPS function - calculate a net promoter score
# ltr - a vector of likelihood-to-recommend values
# neutral - the range, below which someone
# is considered a detractor and above which
# some is consider a promoter (default is 7 to 8)

#generate a barchart, with fill color representing the number of surveys
#and the user defining the attribute to be charted
server <- function(input, output) {
  health_data2 <- readHealthData()
  output$plot <- renderPlot(
    {
    if(input$attribute == "age"){
      ggplot(data=health_data2, mapping=aes(x=age,fill= factor(expensive)))+
      geom_bar(stat="count",width=0.5)+
      ylab("total count")+
      labs(fill = "expensive")
    }
    else if(input$attribute == "age_group"){
      ggplot(data=health_data2, mapping=aes(x=age_group,fill= factor(expensive)))+
        geom_bar(stat="count",width=0.5)+
        ylab("total count")+
        labs(fill = "expensive")
    }
      else if(input$attribute == "bmi"){
        ggplot(data=health_data2, mapping=aes(x=bmi,fill= factor(expensive)))+
          geom_bar(stat="count",width=0.5)+
          ylab("total count")+
          labs(fill = "expensive")
      }
      else if(input$attribute == "bmi_group"){
        ggplot(data=health_data2, mapping=aes(x=bmi_group,fill= factor(expensive)))+
          geom_bar(stat="count",width=0.5)+
          ylab("total count")+
          labs(fill = "expensive")
      }
      else if(input$attribute == "gender"){
        ggplot(data=health_data2, mapping=aes(x=gender,fill= factor(expensive)))+
          geom_bar(stat="count",width=0.5)+
          ylab("total count")+
          labs(fill = "expensive")
      }
      else if(input$attribute == "smoker"){
        ggplot(data=health_data2, mapping=aes(x=smoker,fill= factor(expensive)))+
          geom_bar(stat="count",width=0.5)+
          ylab("total count")+
          labs(fill = "expensive")
      }
      }
  )}
#read in the json survey, similar to other chapters
readHealthData <- function() {
  health_data <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv")
  health_data <- as.data.frame(health_data)
  #Remove rows with'Likelihood.to.recommend' == NA
  # create a new column named expensive, if cost > 5779.8, the valuse is 1, otherwise 0
  # deal with the missing values
  library(imputeTS)
  health_data$bmi <- na_interpolation(health_data$bmi)
  
  # use sample to fill NA values in hypertension column, in case generate decimal
  set.seed(1)
  health_data$hypertension <- ifelse(is.na(health_data$hypertension),
                                     sample(c(0,1), size = nrow(health_data), replace = TRUE),
                                     health_data$hypertension)
  health_data$expensive <- ifelse(health_data$cost > 5779.8, 1, 0)
  # group the age by 10 years
  health_data$age_group <- cut(health_data$age, breaks = seq(10, 70, 10))
  # group the bmi by 5
  health_data$bmi_group <- cut(health_data$bmi, breaks = seq(15, 55, 5))
  # create a new dataframe and delete the age and bmi columns
  # health_data2 <- health_data[, -c(1,2,3,14)]
  health_data2 <- health_data
  return(health_data2)
}
# Run the application
shinyApp(ui = ui, server = server)
