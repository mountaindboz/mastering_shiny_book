# ER Injuries Shiny app from Chapter 4

library(shiny)
library(tidyverse)
library(here)

injuries <- vroom::vroom(here("data/injuries.tsv.gz"))
products <- vroom::vroom(here("data/products.tsv"))
population <- vroom::vroom(here("data/population.tsv"))

count_top <- function(df, var, n) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarize(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  fluidRow(
    column(
      6,
      selectInput(
        "code",
        "Product",
        choices = setNames(products$prod_code, products$title),
        width = "100%"
      )
    ),
    column(2, numericInput("rows", "# of Rows", value = 6, min = 2)),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("prev_story", "Previous story")),
    column(2, actionButton("next_story", "Next story")),
    column(8, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))

  # Find the maximum possible of rows.
  max_no_rows <- reactive(
    max(length(unique(selected()$diag)),
        length(unique(selected()$body_part)),
        length(unique(selected()$location)))
  )

  # Update the maximum value for the numericInput based on max_no_rows().
  observeEvent(input$code, {
    updateNumericInput(session, "rows", max = max_no_rows())
  })

  n_rows <- reactive(input$rows - 1)

  output$diag <- renderTable(
    count_top(selected(), diag, n_rows()),
    width = "100%"
  )
  output$body_part <- renderTable(
    count_top(selected(), body_part, n_rows()),
    width = "100%"
  )
  output$location <- renderTable(
    count_top(selected(), location, n_rows()),
    width = "100%"
  )

  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

  output$age_sex <- renderPlot(
    {
      if (input$y == "count") {
        summary() %>%
          ggplot(aes(age, n, color = sex)) +
          geom_line() +
          labs(y = "Estimated number of injuries")
      } else {
        summary() %>%
          ggplot(aes(age, rate, color = sex)) +
          geom_line(na.rm = TRUE) +
          labs(y = "Injuries per 10,000 people")
      }
    },
    res = 96
  )

  # Store the maximum possible number of stories
  max_no_stories <- reactive(length(selected()$narrative))

  # Reactive used to save the current position in the narrative list
  story <- reactiveVal(1)

  # Reset the story counter if the user changes the product code
  observeEvent(input$code, story(1))

  # When the user clicks "Next story", increase the current position in the
  # narrative but never go beyond the interval [1, length of the narrative]
  # Note that the mod function (%%) is keeping `current`` within this interval
  observeEvent(input$next_story, {
    story((story() %% max_no_stories()) + 1)
  })

  # When the user clicks "Previous story" decrease the current position in the
  # narrative
  # Note that we also take advantage of the mod function.
  observeEvent(input$prev_story, {
    story(((story() - 2) %% max_no_stories()) + 1)
  })

  output$narrative <- renderText(selected()$narrative[story()])
}

shinyApp(ui, server)
