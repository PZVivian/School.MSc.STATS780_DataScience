library(shiny)
library(tidyverse)
library(ggplot2)

## DATA PRE-PROCESSING ##
disposalDataRaw <- read_csv(file="NPRI-INRP_DisposalsEliminations_1993-present.csv", locale=locale(encoding="latin1"))

# Select a subset of the data that only includes plastic and rubber waste that ends up at the landfill.
disposalData <- disposalDataRaw %>%
  filter(`CAS_Number / No_CAS` == "1332-21-4") %>% 
  mutate("Quantity (Tonnes)" = if_else(`Units / Unités` == "kg", `Quantity / Quantité`/1000,`Quantity / Quantité`)) %>%
  group_by(`Reporting_Year / Année`,
           `CAS_Number / No_CAS`,
           `Substance Name (English) / Nom de substance (Anglais)`,
           `NAICS / Code_SCIAN`,
           `NAICS Title / Titre Code_SCIAN`,
           `PROVINCE`) %>%
  summarize("Quantity (Tonnes)" = sum(`Quantity (Tonnes)`)) %>%
  rename("Year" = `Reporting_Year / Année`,
         "CAS_Number" = `CAS_Number / No_CAS`,
         "Substance" = `Substance Name (English) / Nom de substance (Anglais)`,
         "NAICS Code" = `NAICS / Code_SCIAN`,
         "NAICS Category" = `NAICS Title / Titre Code_SCIAN`,
         "Province" = `PROVINCE`)

provinceOptions <- disposalData %>%
  ungroup() %>% 
  distinct(`Province`) %>%
  select(`Province`) %>% 
  c()


## APP UI ##
ui <- fluidPage(

    # Application title
    titlePanel("Yearly Asbestos Waste Quantities by Industry"),

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


## SERVER LOGIC ##
server <- function(input, output) {
  
  output$lineGraph <- renderPlot({
    # Waste quantity by year and industry with a toggle on province
    disposalData %>% 
      filter(`Province`==input$province) %>% 
      group_by(`Year`, `NAICS Category`) %>% 
      summarize("Quantity (Tonnes)" = sum(`Quantity (Tonnes)`)) %>% 
      arrange(desc(`Quantity (Tonnes)`), by_group = TRUE) %>% 
      top_n(6) %>% 
      ggplot(aes(x=`Year`, y=`Quantity (Tonnes)`, color=`NAICS Category`)) +
      geom_line()
  })
  
}


## RUN APP ##
shinyApp(ui = ui, server = server)
