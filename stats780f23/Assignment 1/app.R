library(shiny)
library(tidyverse)
library(ggplot2)

## DATA PRE-PROCESSING ##
disposalDataRaw <- read_csv(file="NPRI-INRP_DisposalsEliminations_1993-present.csv", locale=locale(encoding="latin1"))
naicsCodes <- read_csv(file="naics-scian-2017-structure-v3-eng.csv")

# Only keep asbestos waste data
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
  ungroup() %>% 
  mutate("NAICS Code (3-digit)" = substr(`NAICS / Code_SCIAN`, 1, 3)) %>% 
  left_join(naicsCodes, by = c("NAICS Code (3-digit)" = "Code")) %>% 
  select(`CAS_Number / No_CAS`,
         `Substance Name (English) / Nom de substance (Anglais)`,
         `Reporting_Year / Année`,
         `PROVINCE`,
         `NAICS / Code_SCIAN`,
         `NAICS Title / Titre Code_SCIAN`,
         `NAICS Code (3-digit)`,
         `Class title`,
         `Quantity (Tonnes)`) %>% 
  rename("CAS_Number" = `CAS_Number / No_CAS`,
         "Substance" = `Substance Name (English) / Nom de substance (Anglais)`,
         "Year" = `Reporting_Year / Année`,
         "Province" = `PROVINCE`,
         "NAICS Code (6-digit)" = `NAICS / Code_SCIAN`,
         "NAICS Category (6-digit)" = `NAICS Title / Titre Code_SCIAN`,
         "NAICS Category (3-digit)" = `Class title`)

# Dropdown options
provinceOptions <- disposalData %>%
  select(`Province`) %>% 
  distinct(`Province`) %>%
  pull()


## APP UI ##
ui <- fluidPage(

    # Application title
    titlePanel("Yearly Asbestos Waste Quantities by Province and Sector"),

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
      filter(`Province` == input$province) %>% 
      group_by(`Year`, `NAICS Category (3-digit)`) %>% 
      summarize("Quantity (Tonnes)" = sum(`Quantity (Tonnes)`)) %>% 
      arrange(desc(`Quantity (Tonnes)`), by_group = TRUE) %>% 
      top_n(6) %>% 
      ggplot(aes(x=`Year`, y=`Quantity (Tonnes)`, color=`NAICS Category (3-digit)`)) +
      geom_line()
  })
  
}


## RUN APP ##
shinyApp(ui = ui, server = server)
