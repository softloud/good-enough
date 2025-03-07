suppressMessages(library(tidyverse))

source("pressabuttonnow/node_classes.R")

button_game <- function() {

  # define edge relations of player's traversal through graph
  game_path <- data.frame(
    from = "start",
    to = "start",
    why = "start"
  )

  # initial edge is so code doesn't fail on first pass
  last_edge <- tail(game_path, 1)

  # display start game text
  text_block(txt_start)

  # iterate through the nodes
  while (
    # don't get caught in a loop at the start
    (last_edge$to == "start") ||
    # doesn't meet end-game conditions
    !(last_edge$from == "answer" &&
      last_edge$to == "question" &&
      last_edge$why == "planned")
  ) {

    # set state
    here_now <- buttonNode$new(
      node_label = last_edge$to,
      button_nodes = button_nodes
    )

    # activate node
    # displays text & menus
    # calculates next edge relation based on player input
    # returns next edge relation
    here_state <- here_now$activateNode()

    # update edges traversed
    game_path <- game_path  %>%
      add_row(
        from = here_state$from,
        to = here_state$to,
        why = here_state$why

      )

    # activate edge relation and move to next node
    last_edge <- tail(game_path, 1)

    # report progress
    game_log(game_path)

  }

  win_screen()
}

button_game()