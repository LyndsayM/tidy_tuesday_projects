#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# APOD Title Word Explorer - Shiny App
# Fully deployable with cached dataset

library(shiny)
library(dplyr)
library(tidytext)
library(stringr)
library(readr)

# --------------------------
# 1. Load preprocessed data
# --------------------------
apod <- readRDS("apod.rds")

apod_words <- apod %>%
  filter(media_type == "image") %>%
  mutate(title_lower = str_to_lower(title)) %>%
  unnest_tokens(word, title_lower) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$"))

word_freq <- apod_words %>%
  count(word, sort = TRUE)

common_words <- word_freq %>%
  filter(n >= 20)

# --------------------------
# 2. UI
# --------------------------
ui <- fluidPage(
  titlePanel("Astronomy Picture of the Day: Title Word Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "search_word",
        label = "Enter a word from APOD titles",
        value = "moon"
      ),
      helpText("Example: moon, galaxy, nebula, mars"),
      uiOutput("image_selector"),
      helpText("Choose an image whose title contains the selected word")
    ),
    
    mainPanel(
      h4(textOutput("match_count")),
      uiOutput("apod_image"),
      br(),
      uiOutput("image_metadata")  # <- metadata placeholder
    )
  ),
  
  tags$footer(
    style = "
      position: fixed;
      bottom: 0;
      width: 100%;
      text-align: center;
      font-size: 11px;
      color: #777;
      background-color: #f8f8f8;
      padding: 4px;
    ",
    HTML(
      "App & analysis: <strong>Lyndsay Miles</strong> &nbsp;|&nbsp; 
      Images & data: NASA Astronomy Picture of the Day (APOD), courtesy of Tidy Tuesday"
    )
  )
)

# --------------------------
# 3. Server
# --------------------------
server <- function(input, output, session) {
  
  # Reactive dataset: images matching the search word
  matches <- reactive({
    req(input$search_word)
    
    apod %>%
      filter(
        media_type == "image",
        str_detect(
          str_to_lower(title),
          fixed(str_to_lower(input$search_word))
        )
      ) %>%
      arrange(desc(date))
  })
  
  # Count of matching images
  output$match_count <- renderText({
    paste("Number of matching images:", nrow(matches()))
  })
  
  # Store currently selected image URL
  selected_url <- reactiveVal(NULL)
  
  # Pick a random image whenever the search word changes
  observeEvent(matches(), {
    if (nrow(matches()) > 0) {
      selected_url(
        slice_sample(matches(), n = 1)$url
      )
    } else {
      selected_url(NULL)
    }
  }, ignoreInit = TRUE)
  
  # Dropdown selector for alternative images
  output$image_selector <- renderUI({
    if (nrow(matches()) == 0) {
      return(tags$em("No matching images found."))
    }
    
    selectInput(
      inputId = "selected_title",
      label = "Choose a different image",
      choices = setNames(matches()$url, matches()$title),
      selected = selected_url()
    )
  })
  
  # Override random selection when user chooses from dropdown
  observeEvent(input$selected_title, {
    selected_url(input$selected_title)
  })
  
  # Render selected image
  output$apod_image <- renderUI({
    req(selected_url())
    
    tags$img(
      src = selected_url(),
      style = "
        max-width: 100%;
        max-height: 60vh;
        object-fit: contain;
        display: block;
        margin-left: auto;
        margin-right: auto;
      "
    )
  })
  
  # Metadata for selected image
  selected_apod <- reactive({
    req(selected_url())
    
    matches() %>%
      filter(url == selected_url()) %>%
      slice(1)
  })
  
  # Inline metadata caption
  output$image_metadata <- renderUI({
    req(selected_apod())
    
    tags$div(
      style = "font-size: 13px; color: #444; text-align: center;",
      tags$strong(selected_apod()$title),
      " | ",
      selected_apod()$date,
      " | ",
      selected_apod()$authors
    )
  })
  
}

# --------------------------
# 4. Launch App
# --------------------------
shinyApp(ui = ui, server = server)




