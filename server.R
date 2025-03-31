library(plotly)
library(shiny)
library(tidyverse)

dex <- readr::read_csv("../data/pokemon.csv") |>
  distinct(species_id, .keep_all = TRUE) |>
  mutate(
    label_name = paste(species_id, "-", str_to_title(pokemon)),
    type_1 = str_to_title(type_1),
    type_2 = str_to_title(type_2),
    type_2 = coalesce(type_2, "None"),
    pokemon = str_to_title(pokemon)
  )

# function for drawing lines on Plotly graphs
vline <- function(x = 0, color = "green") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color)
  )
}

# custom function to generate Plotly graphs
plt_histogram <- function(data, column, sel_entry) {
  data |>
    plot_ly(
      x = as.formula(paste0("~", column)),
      type = "histogram",
      marker = list(color = sel_entry()$color_1)
    ) |>
    layout(
      dragmode = FALSE,
      margin = list(l = 0, r = 0, t = 0, b = 0),
      shapes = list(
        vline(
          pull(sel_entry(), !!sym(column)),
          color = "black"
        )
      ),
      xaxis = list(title = ""),
      yaxis = list(showticklabels = FALSE),
      hovermode = "x unified"
    ) |>
    config(displayModeBar = FALSE) |>
    style(
      hovertemplate = "<b>Count</b>: %{y}<extra></extra>"
    )
}

####### START OF SERVER #########

server <- function(input, output, session) {
  
  # filter data using sidebar inputs
  sel_data <- reactive({
    shiny::req(input$filter_type1)
    shiny::req(input$filter_type2)
    
    type1 <- input$filter_type1
    type2 <- input$filter_type2
    gen_min <- input$filter_gen[1]
    gen_max <- input$filter_gen[2]
    hp_min <- input$filter_hp[1]
    hp_max <- input$filter_hp[2]
    speed_min <- input$filter_speed[1]
    speed_max <- input$filter_speed[2]
    attack_min <- input$filter_attack[1]
    attack_max <- input$filter_attack[2]
    defense_min <- input$filter_defense[1]
    defense_max <- input$filter_defense[2]
    spattack_min <- input$filter_special_attack[1]
    spattack_max <- input$filter_special_attack[2]
    spdefense_min <- input$filter_special_defense[1]
    spdefense_max <- input$filter_special_defense[2]
    
    # apply type filters
    sel <- dex |>
      select(-c(url_icon, url_image))
    
    if (type1 != "Any") {
      sel <- sel |>
        filter(type_1 == type1)
    }
    
    if (type2 != "Any") {
      sel <- sel |>
        filter(type_2 == type2)
    }
    
    # apply non-type filters
    sel |>
      filter(
        generation_id >= gen_min & generation_id <= gen_max,
        hp >= hp_min & hp <= hp_max,
        speed >= speed_min & speed <= speed_max,
        attack >= attack_min & attack <= attack_max,
        defense >= defense_min & defense <= defense_max,
        special_attack >= spattack_min & special_attack <= spattack_max,
        special_defense >= spdefense_min & special_defense <= spdefense_max,
      )
  })
  
  # use selected data to update filters
  shiny::observe({
    choices <- sel_data() |>
      arrange(species_id) |>
      pull(label_name)
    
    selected <- sel_data() |>
      filter(label_name %in% choices) |>
      slice(1) |>
      pull(label_name)
    
    updateSelectInput(
      session = session, 
      inputId = "filter_label", 
      choices = choices,
      selected = selected
    )
  })
  
  shiny::observe({
    types <- sort(unique(dex$type_1))
    types <- c("Any", str_to_title(types))
    
    updateSelectInput(
      session = session, 
      inputId = "filter_type1", 
      choices = types,
      selected = "Any"
    )
  })
  
  shiny::observe({
    types <- sort(unique(dex$type_2))
    types <- c("Any", str_to_title(types))
    
    updateSelectInput(
      session = session, 
      inputId = "filter_type2", 
      choices = types,
      selected = "Any"
    )
  })
  
  # render Pokemon image
  output$entry_image <- renderUI({
    pic <- dex |>
      filter(label_name == input$filter_label) |>
      slice(1) |>
      pull(url_image)
    
    img(src = pic, width = "90%")
  })
  
  # get selection info
  sel_entry <- reactive({
    shiny::req(input$filter_label)
    sel_data() |>
      filter(label_name == input$filter_label)
  })
  
  ######## INTERESTING TRICK ALERT #########
  # This code prevents me from manually assigning each Pokemon
  # stat by hand into the output dictionary. I select the relevant columns,
  # walk through their values, and add them to the output list with the
  # extremely helpful "walk" function from purrr.
  colrange <- which(colnames(dex) == "type_1"):which(colnames(dex) == "speed")
  cols <- c(colnames(dex)[colrange], "pokemon", "generation_id")
  
  walk(cols, ~{
    output[[paste0("entry_", .)]] <- renderText({
      sel_entry() |> 
        pull(.)
    })
  })
    
  ######## SAME TRICK AGAIN #########
  # This time, we use the same trick as above for the Plotly graphs by walking
  # through the relevant stats and building a Plotly graph using a custom
  # function.
  
  cols_stats <- cols[3:length(cols) - 1] # removes type from column list
  
  walk(cols_stats, ~{
    output[[paste0("plot_", .)]] <- renderPlotly({
      plt_histogram(dex, ., sel_entry)
    })
  })
  
  # full PokeDex tab
  output$full_data <- renderDT({
    dex |>
      select(-c(url_image, url_icon, label_name, species_id)) |>
      rename_with(~ str_replace_all(., "_", " ")) |>
      rename_with(~ str_to_title(.)) |>
      datatable(options = list(pageLength = 25))
  })
}