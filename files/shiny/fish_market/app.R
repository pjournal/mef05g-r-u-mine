#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(shinythemes)
library(tidyr)
library(ggplot2)



raw_df <- read.csv("balik_hal_fiyatlari.csv",stringsAsFactors = FALSE, header = TRUE, sep = ";", encoding="UTF-8")

fish_market <- raw_df %>%
  select(date = "TARIH", product_type = "MAL_TURU", product_name = "MAL_ADI", units = "BIRIM", min_price = "ASGARI_UCRET", max_price = "AZAMI_UCRET") %>%
  mutate(month = lubridate::month(date, label = TRUE))



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
  
  h1("Izmir Fish Market Dashboard"),              
                
                
  titlePanel("Product Data"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("pt", "Choose a product type:",
                  choices = fish_market$product_type, selected = "BALIK"),
      
      selectInput("pn", "Choose a product:",
                  choices = fish_market$product_name)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
        DT::dataTableOutput("Table")
      )
    ),
    
    
    
    titlePanel("Monthly Price Difference of Selected Product Type"),
    
    sidebarLayout(
      
      sidebarPanel(
        selectInput("pt2", "Choose a product type:",
                    choices = fish_market$product_type, selected = "BALIK")
      ),
      
      # Main panel for displaying outputs
      mainPanel(
        
        plotOutput("Plot")
      )
    )
    
  )


server <- function(session,input, output) {
  
  output$Table <- DT::renderDataTable({
    
    
    subset(fish_market,fish_market$product_type == input$pt & fish_market$product_name == input$pn)
    
    
    data_fish <- fish_market %>% 
      filter(product_type == input$pt & product_name == input$pn) %>% 
      select(-month)
    
    data_fish
  })
  
  observeEvent(input$pt,
               {updateSelectInput(session,
                                  inputId="pn",
                                  choices=unique(fish_market[fish_market$product_type == input$pt, "product_name"]))})
  

  
  output$Plot <- renderPlot({
    
    avg_max_min_price <- fish_market %>% select(date, product_type, product_name, units, min_price, max_price) %>%
      group_by(month = lubridate::month(date, label = TRUE), product_type) %>%
      summarize(avg_min_price = mean(min_price), avg_max_price = mean(max_price))
    

    avg_prices <- avg_max_min_price %>% pivot_longer(c(avg_min_price, avg_max_price), names_to = "average_prices", values_to="values") %>% 
      filter(product_type == input$pt2) %>% 
      group_by(month, product_type) %>% 
      select(month, product_type, average_prices, values)
  
    
    
    ggplot(avg_prices, aes(month, values, fill=average_prices)) +
      geom_col(position = position_dodge(width = 0.4), alpha = 0.8) +
      scale_fill_brewer(palette = "Dark2") +
      labs(x= "Month",
           y = "Average Min/Max Prices",
           title = "Monthly Price Difference of Selected Product Type")
    
    
  }
  )

}
# Create Shiny app ----
shinyApp(ui, server)