library(shiny)
library(bslib)
library(here)
library(gapminder)

continents <- forcats::fct_c(unique(gapminder$continent), factor("(All)"))

ui <- fluidPage(
  selectInput("continent", "Continent", choices = continents),
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)

server <- function(input, output, session) {
  continent_filt <- reactive({
    if (input$continent == "(All)") {
      gapminder
    } else {
      dplyr::filter(gapminder, continent == input$continent)
    }
  })

  observeEvent(
    continent_filt(),
    {
      freezeReactiveValue(input, "country")
      choices <- unique(continent_filt()$country)
      updateSelectInput(inputId = "country", choices = choices)
    }
  )

  output$data <- renderTable({
    dplyr::filter(continent_filt(), country == input$country)
  })
}

shinyApp(ui, server)
