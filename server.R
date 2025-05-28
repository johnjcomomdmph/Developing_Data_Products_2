library(shiny)
library(ggplot2)
library(dplyr)
library(palmerpenguins)

# Use the same color map for consistency
species_colors <- c(
  "Adelie" = "#66c2a5",
  "Chinstrap" = "#fc8d62",
  "Gentoo" = "#8da0cb"
)

df <- na.omit(penguins)

server <- function(input, output, session) {
  # Print for debug confirmation
  print("SERVER LOADED")
  
  # Filter the data based on selected species
  filtered_data <- reactive({
    df %>%
      filter(species %in% input$species) %>%
      mutate(species = factor(species, levels = names(species_colors)))
  })
  
  # Render the histogram
  output$distPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = body_mass_g, fill = species)) +
      geom_histogram(bins = input$bins, color = "white", alpha = 0.7, position = "identity") +
      scale_fill_manual(values = species_colors) +
      labs(
        title = "Histogram of Penguin Body Mass",
        x = "Body Mass (g)",
        y = "Count"
      ) +
      theme_minimal(base_size = 12)
  })
}
