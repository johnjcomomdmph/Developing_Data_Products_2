## The Palmer Penguins dataset contains data on three
## penguin species collected from islands in the Palmer 
## Archipelago in Antarctica. The three species are 
## Adelie, Gentoo, and Chinstrap. The dataset includes
## bill length, bill depth, flipper length, and 
## body mass. We will be focusing on the latter.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(shinyWidgets)

# Prepare data
df <- na.omit(penguins)

# Define UI
species_colors <- c(
  "Adelie" = "#66c2a5",     # green
  "Chinstrap" = "#fc8d62",  # orange
  "Gentoo" = "#8da0cb"      # blue
)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  titlePanel("Penguin Body Mass Histogram"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "species",
        label = "Select Species:",
        choices = names(species_colors),
        selected = names(species_colors),
        multiple = TRUE,
        choicesOpt = list(
          choicesOpt = list(
            content = lapply(names(species_colors), function(sp) {
              HTML(sprintf(
                '<span style="color:white; padding:4px 8px; border-radius:4px; background-color:%s">%s</span>',
                species_colors[sp], sp
              ))
            })
          )
        )
      ),
      
      sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 30)
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$distPlot <- renderPlot({
    filtered_df <- df %>% filter(species %in% input$species)
    
    ggplot(filtered_df, aes(x = body_mass_g, fill = species)) +
      geom_histogram(bins = input$bins, position = "identity", alpha = 0.7, color = "black") +
      scale_fill_manual(values = species_colors) +
      labs(
        x = "Body Mass (g)",
        y = "Count",
        fill = "Species"
      ) +
      theme_minimal(base_size = 14)
  })
}  
  
  # Run the App
  shinyApp(ui = ui, server = server)