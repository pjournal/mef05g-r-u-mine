#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(eeptools)
library(lubridate)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(RColorBrewer)



tbn <- read_rds("https://raw.githubusercontent.com/pjournal/mef05g-r-u-mine/gh-pages/files/tourist_by_nationalities_melt.rds")


tbn2 <- tbn %>% 
          group_by(year = lubridate::year(Tarih), variable) %>% 
          summarise(total = sum(value)) 

tbn2$variable <- as.character(tbn2$variable)

tbn2 <- arrange(tbn2, variable)

tbn2$variable <- as.factor(tbn2$variable)


tbn4 <- tbn %>% 
  group_by(year = lubridate::year(Tarih), month = lubridate::month(Tarih, label=TRUE), variable) %>% 
  summarise(total = sum(value))

tbn4$variable <- as.character(tbn4$variable)

tbn4 <- arrange(tbn4, variable)

tbn4$variable <- as.factor(tbn4$variable)



addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  
    

    # Application title
    titlePanel("Tourism Statistics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          
            pickerInput("country", "Select Countries (Max 3 countries)", choices = unique(as.character(tbn2$variable)), multiple = TRUE,options = list(`max-options` = 3), selected = c("ALMANYA", "RUSYA")),
            
            sliderInput("year", "Select year", min = min(tbn2$year), max(tbn2$year), value = 2010)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          tabsetPanel(
            tabPanel("Yearly Plot", plotOutput("Plot")),
            tabPanel("Monthly Plot", plotOutput("Plot2"))
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  data1 <- reactive({
    tbn2 %>% filter(variable %in% input$country)
  })
  
  
  data2 <- reactive({
    tbn4 %>% filter((variable %in% input$country & year == input$year))
  })
  
  
  output$Plot <- renderPlot({
    
    tbn3 <- data1()
    
    ggplot(tbn3, aes(year, total, fill=tbn3$variable)) +
      geom_col(position = position_dodge(width = 0.4),alpha = 0.8) +
      scale_fill_brewer(palette = "Dark2") +
      labs(x= "Year",
           y = "Number of tourist",
           title = "Tourists by Nationalities",
           fill = "countries") +
      scale_x_discrete(limits = unique(tbn2$year)) +
      scale_y_continuous(labels = addUnits)
    
    
  }
  )
  
  
  output$Plot2 <- renderPlot({
    
    tbn5 <- data2()
    
    ggplot(tbn5, aes(month, total, fill=tbn5$variable)) +
      geom_col(position = position_dodge(width = 0.4),alpha = 0.8) +
      scale_fill_brewer(palette = "Dark2") +
      labs(x= "Month",
           y = "Number of tourist",
           title = "Tourists by Nationalities",
           fill = "countries") +
      scale_x_discrete(limits = unique(tbn4$month)) +
      scale_y_continuous(labels = addUnits)
    
    
  }
  )
  
  
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
