# R script: mooncake_try.R

library(shiny)
library(DiagrammeR)
library(tidyverse)

# Load components from your existing game — no subdir!
source("game_text.R")
source("game_fns.R")
source("node_classes.R")

# UI
ui <- fluidPage(
  titlePanel("Press a Button: Analytical Lifecycle Simulator"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("reset_prob",
                  "Probability of Stakeholder Reset:",
                  min = 0, max = 1, value = 0.2, step = 0.05),
      actionButton("start_game", "Start Game")
    ),
    mainPanel(
      verbatimTextOutput("game_output"),
      DiagrammeROutput("graph_out")
    )
  )
)

# SERVER
server <- function(input, output, session) {

  game_log_data <- reactiveVal(data.frame(from = character(), to = character(), why = character()))

  observeEvent(input$start_game, {

    # initial state
    game_path <- data.frame(from = "start", to = "start", why = "start")
    last_edge <- tail(game_path, 1)

    all_text <- c()
    append_text <- function(txt) {
      all_text <<- c(all_text, txt)
    }

    while (
      (last_edge$to == "start") ||
      !(last_edge$from == "answer" && last_edge$to == "question" && last_edge$why == "planned")
    ) {
      here_now <- buttonNode$new(
        node_label = last_edge$to,
        button_nodes = button_nodes,
        reset_prob = input$reset_prob  # inject from slider
      )

      here_state <- here_now$activateNode()

      game_path <- game_path |> add_row(
        from = here_state$from,
        to = here_state$to,
        why = here_state$why
      )

      last_edge <- tail(game_path, 1)
    }

    game_log_data(game_path)

    output$game_output <- renderPrint({
      print(game_log_data())
    })

    output$graph_out <- renderDiagrammeR({
      # Optional: show a vis of final state (not strictly required)
      graph <- create_graph()  # Placeholder if you want visualisation
      grViz(graph)
    })
  })
}

shinyApp(ui = ui, server = server)
