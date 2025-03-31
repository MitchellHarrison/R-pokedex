library(shiny)

# Sample dex data (replace with your actual data)
dex <- data.frame(
  hp = sample(1:100, 10, replace = TRUE),
  speed = sample(1:100, 10, replace = TRUE),
  attack = sample(1:100, 10, replace = TRUE),
  defense = sample(1:100, 10, replace = TRUE),
  special_attack = sample(1:100, 10, replace = TRUE),
  special_defense = sample(1:100, 10, replace = TRUE)
)

# UI
ui <- fluidPage(
  titlePanel("Dynamic Sliders Example"),
  
  # Dynamic UI for Sliders
  uiOutput("sliders_ui")
)

# Server
server <- function(input, output, session) {
  
  # Vector of columns for which we want sliders
  columns <- c("hp", "speed", "attack", "defense", "special_attack", "special_defense")
  
  # Render sliders dynamically
  output$sliders_ui <- renderUI({
    # Create a list of sliderInput elements dynamically
    slider_list <- lapply(columns, function(col) {
      sliderInput(
        inputId = paste0("filter_", col),  # Unique ID for each slider
        label = paste0(col, ":"),
        min = min(dex[[col]], na.rm = TRUE),
        max = max(dex[[col]], na.rm = TRUE),
        value = c(min(dex[[col]], na.rm = TRUE), max(dex[[col]], na.rm = TRUE)),
        step = 5
      )
    })
    
    # Return the list of sliders as UI elements
    do.call(tagList, slider_list)
  })
}

# Run the app
shinyApp(ui = ui, server = server)