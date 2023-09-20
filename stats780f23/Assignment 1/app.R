library(shiny)
library(tidyverse)
library(ggplot2)

### DATA PRE-PROCESSING ###
disposalDataRaw <- read_csv(file="NPRI-INRP_DisposalsEliminations_1993-present.csv", locale=locale(encoding="latin1"))
naicsCodes <- read_csv(file="naics-scian-2017-structure-v3-eng.csv")

## STEP 1: CLEAN NAICS CODE TO CREATE 2-DIGIT NAICS LOOKUP DATA ##
# Get 2-digit NAICS codes and their descriptions. If code ranges exist, then keep it is as is (ex. "31-33" for Manufacturing).
naicsCodes <- data.frame(x=naicsCodesRaw) %>% 
  filter(grepl("<th id=", x)) %>% 
  mutate("Sector Code (2-digit NAICS Code)" = str_match(x, "CPV=\\s*(.*?)\\s*&")[,2],
         "Sector Name" = str_match(x, '"wb-inv">\\s*(.*?)\\s*</span>')[,2]) %>% 
  select(`Sector Code (2-digit NAICS Code)`, `Sector Name`)
naicsCodes$`Sector Name` <- stri_replace_all_regex(naicsCodes$`Sector Name`, pattern = c("&#40;", "&#41;", "&#44;"), replacement = c("(", ")", ","), vectorize = F)

# Break down code ranges into their own rows
codeRangesOnly <- naicsCodes %>% 
  filter(str_length(`Sector Code (2-digit NAICS Code)`)>2) %>% 
  mutate(repStart = as.integer(str_match(`Sector Code (2-digit NAICS Code)`, "([0-9]{2})[-]([0-9]{2})")[,2]),
         repEnd = as.integer(str_match(`Sector Code (2-digit NAICS Code)`, "([0-9]{2})[-]([0-9]{2})")[,3])
  ) %>% 
  group_by(`Sector Name`) %>%
  group_modify(~ tibble("Sector Code (2-digit NAICS Code)" = seq(.$repStart, .$repEnd))) %>%
  ungroup()

# Replace any rows with code ranges with associated rows
naicsCodes <- rbind(naicsCodes, codeRangesOnly) %>% filter(str_length(`Sector Code (2-digit NAICS Code)`)==2)


## STEP 2: FILTER FOR ASBESTOS AND JOIN 2-DIGIT NAICS LOOKUP DATA ##
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
  mutate("Sector Code (2-digit NAICS Code)" = substr(`NAICS / Code_SCIAN`, 1, 2)) %>% 
  left_join(naicsCodes, by = c("Sector Code (2-digit NAICS Code)" = "Sector Code (2-digit NAICS Code)")) %>% 
  select(`CAS_Number / No_CAS`,
         `Substance Name (English) / Nom de substance (Anglais)`,
         `Reporting_Year / Année`,
         `PROVINCE`,
         `NAICS / Code_SCIAN`,
         `NAICS Title / Titre Code_SCIAN`,
         `Sector Code (2-digit NAICS Code)`,
         `Sector Name`,
         `Quantity (Tonnes)`) %>% 
  rename("CAS Number" = `CAS_Number / No_CAS`,
         "Substance" = `Substance Name (English) / Nom de substance (Anglais)`,
         "Year" = `Reporting_Year / Année`,
         "Province" = `PROVINCE`,
         "Industry Code (6-digit NAICS Code)" = `NAICS / Code_SCIAN`,
         "Industry Name" = `NAICS Title / Titre Code_SCIAN`)

# Dropdown options
provinceOptions <- disposalData %>%
  select(`Province`) %>% 
  distinct(`Province`) %>%
  pull()


### APP UI ###
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


### SERVER LOGIC ###
server <- function(input, output) {
  
  output$lineGraph <- renderPlot({
    # Waste quantity by year and sector with a toggle on province
    disposalData %>% 
      filter(`Province` == input$province) %>% 
      group_by(`Year`, `Sector Name`) %>% 
      summarize("Quantity of Asbestos (Tonnes)" = sum(`Quantity (Tonnes)`)) %>% 
      ggplot(aes(x=`Year`, y=`Quantity of Asbestos (Tonnes)`, color=`Sector Name`)) +
      geom_line()
  })
  
}


### RUN APP ###
shinyApp(ui = ui, server = server)
