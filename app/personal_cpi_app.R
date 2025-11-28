library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(gt)

# Ensure an output directory exists for saving artifacts
output_dir <- "output"
if (!dir.exists(output_dir)) {
  if (dir.exists("../output")) {
    output_dir <- "../output"
  } else {
    dir.create(output_dir, recursive = TRUE)
  }
}

# Source the components script to get the persona engine function
# We assume the script is in ../scripts/ relative to the app, or we are running from root.
# Shiny apps are often run from the app directory.
# We'll try to locate the script.

if (file.exists("scripts/cpi_components.R")) {
  source("scripts/cpi_components.R")
} else if (file.exists("../scripts/cpi_components.R")) {
  source("../scripts/cpi_components.R")
} else {
  stop("Could not find scripts/cpi_components.R")
}

# Load Data
data_path <- if (file.exists("output/cpi_data.rds")) {
  "output/cpi_data.rds"
} else if (file.exists("../output/cpi_data.rds")) {
  "../output/cpi_data.rds"
} else {
  stop("Please run scripts/cpi_components.R to generate output/cpi_data.rds before launching the app.")
}

cpi_data <- readRDS(data_path)
cpi_data$components$date <- as.Date(cpi_data$components$date)
components <- cpi_data$components
weights <- cpi_data$weights
canonical_cpi <- cpi_data$canonical_cpi

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  titlePanel("Personal Inflation Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Household Persona"),
      helpText("Adjust the toggles to reflect your spending habits."),
      
      checkboxInput("vegetarian", "Vegetarian", FALSE),
      checkboxInput("vegan", "Vegan", FALSE),
      
      radioButtons("transport", "Transport",
                   choices = c("Neutral" = "neutral", 
                               "Car Owner" = "car_owner", 
                               "Car Free" = "car_free"),
                   selected = "neutral"),
      
      radioButtons("housing", "Housing",
                   choices = c("Neutral" = "neutral", 
                               "Renter" = "renter", 
                               "Homeowner" = "homeowner"),
                   selected = "neutral"),
      
      checkboxInput("traveller", "Frequent Traveller", FALSE),
      checkboxInput("family", "Family with Children", FALSE),
      checkboxInput("smoker", "Smoker", FALSE),
      
      radioButtons("energy", "Energy Usage",
                   choices = c("Neutral" = "neutral", 
                               "High Energy" = "high", 
                               "Low Energy" = "low"),
                   selected = "neutral"),
      
      hr(),
      actionButton("update", "Update Calculation", class = "btn-primary")
    ),
    
    mainPanel(
      plotOutput("inflation_plot", height = "500px"),
      br(),
      gt_output("inflation_table")
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output, session) {

  combined_data <- eventReactive(input$update, {
    toggles <- list(
      vegetarian = input$vegetarian,
      vegan = input$vegan,
      transport = input$transport,
      housing = input$housing,
      traveller = input$traveller,
      family = input$family,
      smoker = input$smoker,
      energy = input$energy
    )

    personal_weights <- calculate_persona_weights(weights, toggles)

    personal_index <- components %>%
      left_join(personal_weights, by = "component") %>%
      group_by(date) %>%
      summarise(
        value = sum(value * weight, na.rm = TRUE),
        measure = "Personalised CPI"
      ) %>%
      ungroup()

    both <- bind_rows(canonical_cpi %>% mutate(measure = "Headline CPI"), personal_index)

    both %>%
      group_by(measure) %>%
      mutate(
        lag_val = lag(value, 4),
        rate = (value / lag_val) - 1
      ) %>%
      filter(!is.na(rate)) %>%
      filter(date >= (max(date) - years(5)))
  }, ignoreNULL = FALSE)
  
  output$inflation_plot <- renderPlot({
    df <- combined_data()

    plot_obj <- ggplot(df, aes(x = date, y = rate, color = measure)) +
      # RBA Band
      annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.02, ymax = 0.03,
               fill = "lightgrey", alpha = 0.3) +
      geom_hline(yintercept = 0.025, colour = "black") +

      geom_line(size = 1.2) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      scale_color_manual(values = c("Headline CPI" = "black", "Personalised CPI" = "blue")) +
      labs(title = "Annual Inflation (Year-ended)",
           y = "Inflation Rate", x = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")

    target_file <- file.path(output_dir, "personal_vs_headline_inflation.png")
    try(ggsave(target_file, plot_obj, width = 10, height = 6, dpi = 300), silent = TRUE)

    plot_obj
  })

  output$inflation_table <- render_gt({
    df <- combined_data()
    
    # Get latest values
    latest <- df %>%
      group_by(measure) %>%
      filter(date == max(date)) %>%
      select(measure, rate) %>%
      ungroup()

    # Create table
    table_obj <- latest %>%
      gt() %>%
      fmt_percent(columns = rate, decimals = 2) %>%
      cols_label(rate = "Current Rate") %>%
      data_color(
        columns = rate,
        fn = function(x) {
          # Heatmap logic
          val <- pmax(0.01, pmin(0.04, x))
          scales::col_numeric(
            palette = c("blue", "white", "red"),
            domain = c(0.02, 0.03)
          )(val)
        }
      )

    table_file <- file.path(output_dir, "latest_inflation_rates_table.png")
    try(gtsave(table_obj, table_file, expand = 10), silent = TRUE)

    table_obj
  })
}

# Run
shinyApp(ui, server)
