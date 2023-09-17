library(shiny)
library(tidyverse)

## DATA PRE-PROCESSING ##
disposalDataRaw <- read_csv(file="NPRI-INRP_DisposalsEliminations_1993-present.csv", locale=locale(encoding="latin1"))

# Select a subset of the data that only includes plastic and rubber waste that ends up at the landfill.
disposalData <- disposalDataRaw %>%
  filter(grepl("^3261", `NAICS / Code_SCIAN`),
         `Category (English) / Catégorie (Anglais)` == "Landfill") %>%
  mutate("Quantity (Tonnes)" = if_else(`Units / Unités` == "kg", `Quantity / Quantité`/1000,`Quantity / Quantité`)) %>%
  group_by(`Reporting_Year / Année`,
           `NAICS / Code_SCIAN`,
           `NAICS Title / Titre Code_SCIAN`,
           `PROVINCE`) %>%
  summarize("Quantity (Tonnes)" = sum(`Quantity (Tonnes)`)) %>%
  rename(Year = `Reporting_Year / Année`,
         "NAICS Code" = `NAICS / Code_SCIAN`,
         "NAICS Category" = `NAICS Title / Titre Code_SCIAN`,
         "Province" = `PROVINCE`)

provinceToggle <- disposalData %>%
  ungroup() %>% 
  distinct(`Province`) %>%
  select(`Province`) %>% 
  c()

"All Other Plastic Product Manufacturing" 

## APP UI ##
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
            selectInput("province",
                        "Province:",
                        
              
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


## SERVER LOGIC ##
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
        
        # Waste quantity by type of plastic/rubber and year with a toggle on province
        disposalData %>% 
          ggplot(aes(x=`Year`, y=`Quantity (Tonnes)`, color=`NAICS Category`)) +
          geom_line()
    })
}


## RUN APP ##
shinyApp(ui = ui, server = server)
