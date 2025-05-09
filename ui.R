library(bslib)
library(DT)
library(plotly)
library(shiny)
library(tidyverse)

APP_TITLE <- "Mitch's Interactive Pokedex!"
REPO_URL <- "https://github.com/MitchellHarrison/R-pokedex"
SHOUTOUT_TEXT <- c(paste(
  "Hello, everyone! This is a PokeDex built entirely with R Shiny and Plotly,",
  "and it's my humble entry for this week's TidyTuesday. If you want to see",
  "its code, click the 🤍 at the top-right corner, which will take you to this",
  "project's Github repo. If you have any specific questions, feel free",
  "to connect with me on"
  ), # Linkedin
  "or", # Discord
  "Talk soon!"
)

# read_data and set starting color
dex <- read_csv("data/pokemon.csv") |>
  distinct(species_id, .keep_all = TRUE)
starting_color <- dex |>
  slice(1) |>
  pull(color_1)

######## START OF UI ########

ui <- page_navbar(
  title = APP_TITLE,
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly"
  ),
  sidebar = sidebar(
    width = "18%",
    
    selectInput(
      "filter_label",
      "PokeDex ID:",
      choices = NULL,
      selected = NULL
    ),
    
    selectInput(
      "filter_type1",
      "Type 1:",
      choices = sort(unique(dex$type_1)),
      selected = "Any"
    ),
    
    selectInput(
      "filter_type2",
      "Type 2:",
      choices = sort(unique(dex$type_2)),
      selected = "Any"
    ),
    
    sliderInput(
      "filter_gen",
      "Generation:",
      min = 1,
      max = max(dex$generation_id, na.rm = TRUE),
      value = c(1, max(dex$generation_id, na.rm = TRUE)),
      step = 1
    ),
    
    sliderInput(
      "filter_hp",
      "HP:",
      min = 1,
      max = max(dex$hp, na.rm = TRUE),
      value = c(1, max(dex$hp, na.rm = TRUE)),
      step = 5
    ),
    
    sliderInput(
      "filter_speed",
      "Speed:",
      min = 1,
      max = max(dex$speed, na.rm = TRUE),
      value = c(1, max(dex$speed, na.rm = TRUE)),
      step = 5
    ),
    
    sliderInput(
      "filter_attack",
      "Attack:",
      min = 1,
      max = max(dex$attack, na.rm = TRUE),
      value = c(1, max(dex$attack, na.rm = TRUE)),
      step = 5
    ),
    
    sliderInput(
      "filter_defense",
      "Defense:",
      min = 1,
      max = max(dex$defense, na.rm = TRUE),
      value = c(1, max(dex$defense, na.rm = TRUE)),
      step = 5
    ),
    
    sliderInput(
      "filter_special_attack",
      "Special Attack:",
      min = 1,
      max = max(dex$special_attack, na.rm = TRUE),
      value = c(1, max(dex$special_attack, na.rm = TRUE)),
      step = 5
    ),
    
    sliderInput(
      "filter_special_defense",
      "Special Defense:",
      min = 1,
      max = max(dex$special_defense, na.rm = TRUE),
      value = c(1, max(dex$special_defense, na.rm = TRUE)),
      step = 5
    )
  ),
  
  ######### Main PokeDex Entry Page ########
  nav_panel(
    title = "Pokemon Entry",
    fluidRow(
      column(
        width = 3,
        uiOutput("entry_image"),
        br(),
        br(),
        br(),
        card(
          p(
            "Hello everyone! This is a PokeDex built entirely with R Shiny",
            "and Plotly. It's my humble entry into this week's TidyTuesday!",
            "If you want to see the code that built this PokeDex, click",
            a("here", href = "https://github.com/MitchellHarrison/R-pokedex"),
            "or on the 🤍 at the top-right corner, which will take you to the",
            "Github repo. If you have any questions, feel free to connect with",
            "me on",
            a("LinkedIn", href = "https://linkedin.com/in/harrisonme"),
            "or on my",
            a("Discord", href = "https://discord.gg/vF6W2bdKFH"),
            "server. Talk soon!"
          )
        )
      ),
      
      column(
        width = 6,
        h1(textOutput("entry_pokemon")),
        br(),
        
        fluidRow(
          column(
            width = 4,
            h5("Type 1:"),
            textOutput("entry_type_1")
          ),
          column(
            width = 4,
            h5("Type 2:"),
            textOutput("entry_type_2")
          ),
          column(
            width = 4,
            h5("Generation:"),
            textOutput("entry_generation_id")
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 6,
            h5("HP:"),
            textOutput("entry_hp"),
            plotlyOutput(outputId = "plot_hp", height = "150px")
          ),
          column(
            width = 6,
            h5("Speed:"),
            textOutput("entry_speed"),
            plotlyOutput(outputId = "plot_speed", height = "150px")
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 6,
            h5("Attack:"),
            textOutput("entry_attack"),
            plotlyOutput(outputId = "plot_attack", height = "150px")
          ),
          column(
            width = 6,
            h5("Defense:"),
            textOutput("entry_defense"),
            plotlyOutput(outputId = "plot_defense", height = "150px")
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 6,
            h5("Special Attack:"),
            textOutput("entry_special_attack"),
            plotlyOutput(outputId = "plot_special_attack", height = "150px")
          ),
          column(
            width = 6,
            h5("Special Defense:"),
            textOutput("entry_special_defense"),
            plotlyOutput(outputId = "plot_special_defense", height = "150px")
          )
        )
      ),
      column(3)
    )
  ),
  
  ######## Data Page ########
  nav_panel(
    title = "All Data",
    DTOutput("full_data")
  ),
  
  nav_spacer(),
  nav_item(a(href = REPO_URL, "Made with 🤍 and Shiny."))
)

ui