source("pressabuttonnow/game_text.R")
source("pressabuttonnow/game_fns.R")

buttonNode <- setRefClass("button_node",
  fields = list(
    node_label = "character",
    button_nodes = "data.frame"
  ),
  methods = list(
    activateNode = function() {
      button_title(node_label)

      # text & menus contextual to node
      if (node_label == "start") {
        button_menu(
          text_var = "To experience life as a data scientist,",
          choice_var = "Start game."
        )
      } else if (node_label == "welcome") {
          text_block(txt_welcome)

          button_menu(
            text_var = "To proceed to onboarding",
            choice_var = "Proceed to onboarding.")
      } else if (node_label == "onboarding") {
          text_block(txt_onboarding[[1]])

          button_menu(
            text_var = "To get financial data,", 
            choice_var = "Get financial data.")

          text_block("You get the data.")

          button_menu(
            text_var = "To analyse financial data,", 
            choice_var = "Analyse financial data.")

          text_block(txt_onboarding[[2]])

        button_menu(
          text_var = "To start your career as a data scientist,", 
          choice_var = "Meeting with stakeholder.")

      } else if (node_label == "question") {
        
        text_block(txt_question)
        button_menu(
          text_var = "To proceed to sourcing the data for your analysis,", 
          choice_var = "Get the data.")

      }
        else {
        text_block("todo")
      }

 

      next_edge <- determine_next_edge()

      return(next_edge)
    },
    determine_next_edge = function( ) {

    # determine next edge if nothing goes wrong
      next_edge <- button_nodes  %>% 
        filter(node_label == !!node_label)  %>% 
        select(
          from = node_label,
          to = expected_edge_relation,
          why
        )
    ## stuff that goes wrong

      # stakeholder tends to randomly reset the question
      reset_to_question <- sample(
        c(TRUE, FALSE),
        size = 1,
        prob = c(0.4, 0.6)
        )

      reset_to_question_conditional <- isTRUE(reset_to_question) && 
        !(node_label %in% c("start", "welcome", "onboarding"))
      
      next_edge <- next_edge %>%
        mutate(
          to = if_else(
            isTRUE(reset_to_question_conditional),
            "question",
            to
          ),
          why = if_else(
            isTRUE(reset_to_question_conditional),
            "stakeholder reset question",
            why
          )
        )
    # Debug message to check updated next_edge
      return(next_edge)
      
    }
  )
)

# set nodes & expected edges
button_nodes <- tribble(
  ~node_label, ~node_txt, ~expected_edge_relation,
  "start", txt_start, "welcome",
  "welcome", txt_welcome, "onboarding",
  "onboarding", txt_onboarding, "question",
  "question", NA, "get data",
  "get data", NA, "transform",
  "transform", NA, "analyse",
  "analyse", NA, "answer",
  "answer", NA, "question"
)  %>% 
  mutate(
    why = "planned"
  )

unexpected_edges <- tribble(
  ~from, ~to, 
  "get data", "data platform",
  "get data", "download static",
  "data platform", "analyse",
  "download static", "analyse"
)  %>% 
  mutate(
    why = "unexpected"
  )
