library(shiny)
library(tidyverse)
library(ggplot2)
library(stringi)


# ----- DATA PRE-PROCESSING ----- #

# Load cleaned disposal data
load("disposalData.RData")

# Drop down options
provinceOptions <- disposalData %>%
  select(`Province`) %>% 
  distinct(`Province`) %>%
  pull()


# ----- APP UI ----- #
ui <- fluidPage(

    # Application title
    titlePanel("Yearly Asbestos Waste by Province and Sector"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "province",
                        label = "Province",
                        choices = provinceOptions
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("lineGraph")
        )
    )
)


# ----- SERVER LOGIC ----- #
server <- function(input, output) {
  
  output$lineGraph <- renderPlot({
    
    # Filter waste data by user's province selection
    disposalData_line <- disposalData %>% 
      filter(`Province` == input$province) %>% 
      group_by(`Year`, `Sector Name`) %>% 
      summarize("Quantity of Asbestos Waste (Kg) per Capita" = sum(`Quantity (Kg)`)/sum(`Population`))
    
    # Plot line graph showing waste quantity by year and sector for the selected province 
    disposalData_line %>% 
      ggplot(aes(x=`Year`, y=`Quantity of Asbestos Waste (Kg) per Capita`, color=`Sector Name`)) +
      geom_line() +
      scale_x_continuous(breaks = round(seq(min(disposalData_line$`Year`),
                                            max(disposalData_line$`Year`), by = 4),1))
  })
  
}


# ----- RUN APP ----- #
shinyApp(ui = ui, server = server)
