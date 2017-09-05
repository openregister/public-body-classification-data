# Do a shiny thing with a drop-down to select from the top fuzzy matches.

# Instead of a drop-down, use radio buttons

# 1. Load the lists auto-joined (which includes non-joined on both sides)
# 1. Use the given match
# 1. Find other matches using the fuzzyjoin package https://github.com/dgrtwo/fuzzyjoin
# 1. Choose the given match by default, otherwise choose no match
# 1. List the alternatives in the drop-down by ascending distance
# 1. When the match is confirmed, update that row in the file.
# 1. Forward/back buttons.

# The file lists/manually-joined.csv is first created by copying the file
# lists/auto-joined.csv

# Load packages
library(shiny)
library(dplyr)
library(readr)
library(stringdist)
library(here)

# Load data
join_path <- file.path(here(), "lists", "manually-joined.csv")
join_file <-
  join_path %>%
  read_csv() %>%
  filter(!is.na(name))
candidates <- read_csv(file.path(here(), "lists", "organisation.csv"))
target <- ""

# Define UI
# Server code used for all examples
server <- function(input, output, session) {
  observe({
    n = input$n_candidates
    record <- slice(join_file, input$record)
    if(!is.na(record$name_1))  n = n + 1
    top_candidate_ids <-
      tibble(candidate = candidates$name,
             distance = stringdist(record$name,
                                   candidates$name,
                                   method = input$method)) %>%
      mutate(id = row_number()) %>%
      arrange(distance) %>%
      slice(seq_len(n)) %>%
      pull(id)
    top_candidates <-
      candidates %>%
      slice(top_candidate_ids) %>%
      pull(name)
    if(!is.na(record$name_1)) {
      top_candidates <- c(unique(c(record$name_1, top_candidates)),
                          "None selected" = "")
      updateRadioButtons(session,
                         "match",
                         choices = top_candidates,
                         selected = record$name_1)
    } else {
      top_candidates <- c(top_candidates, "None selected" = "")
      updateRadioButtons(session,
                         "match",
                         choices = top_candidates,
                         selected = "")
    }
    target <<- record$name
    updateRadioButtons(session,
                       "target",
                       choices = record$name,
                       selected = record$name)
  })
  save_state <- function() {
    if (isolate(input$match) != "") {
      join_file[join_file$name == target, "name_1"] <<- isolate(input$match)
      write_csv(join_file, join_path)
    }
  }
  observe({
    input$nextButton
    save_state()
    # TODO: don't respond to change from NULL to 1 when the app first starts
    if (isolate(input$record) < nrow(join_file)) {
      updateNumericInput(session, "record", value = isolate(input$record) + 1)
    }
  })
  observe({
    input$previousButton
    save_state()
    # TODO: don't respond to change from NULL to 1 when the app first starts
    if (isolate(input$record) > 1) {
      updateNumericInput(session, "record", value = isolate(input$record) - 1)
    }
  })
  observe({
    input$saveButton
    save_state()
  })
}
ui <- pageWithSidebar(
  headerPanel("Registers Data Matcher"),
  sidebarPanel(
    selectInput("method",
              "Matching algorithm",
              "jw",
              c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine",
                "jaccard", "jw", "soundex")),
    sliderInput("n_candidates",
                 "Number of candidates to choose from",
                 5,
                 min = 1,
                 max = 35),
    numericInput("record",
                 "Record to match",
                 1,
                 min = 0,
                 max = nrow(join_file))
  ),
  mainPanel(
    radioButtons("target",
                 label = NULL,
                 choices = "",
                 width = "100%"),
    div(),
    radioButtons("match",
                 label = NULL,
                 choices = c(c("None selected" = ""), "Please wait ..."),
                 selected = "",
                 width = "100%"),
    actionButton("previousButton", "Previous"),
    actionButton("nextButton", "Next"),
    actionButton("saveButton", "Save")
  )
)
shinyApp(ui, server)
