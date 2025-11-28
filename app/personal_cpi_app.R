library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(gt)

# Ensure an output directory exists for saving artifacts
output_dir <- if (dir.exists("output")) {
  message("Using local output directory: ./output")
  "output"
} else if (dir.exists("../output")) {
  message("Using parent output directory: ../output")
  "../output"
} else {
  dir.create("output", recursive = TRUE)
  message("Created missing output directory at ./output")
  "output"
}

log_save_intent <- function(path, label) {
  dir_path <- dirname(path)
  can_write <- file.access(dir_path, 2) == 0
  message(
    "[", label, "] Preparing to save file: ", path,
    " | dir exists: ", dir.exists(dir_path),
    " | writable: ", can_write,
    " | absolute dir: ", normalizePath(dir_path, winslash = "/", mustWork = FALSE)
  )
}

log_save_result <- function(path, label, success) {
  if (!success) {
    message("[", label, "] Save failed; file not written. Checked path: ", path)
    return(invisible(FALSE))
  }

  if (file.exists(path)) {
    info <- file.info(path)
    message(
      "[", label, "] Save succeeded | size bytes: ", info$size,
      " | last modified: ", info$mtime,
      " | absolute path: ", normalizePath(path, winslash = "/", mustWork = FALSE)
    )
  } else {
    message(
      "[", label, "] Save reported success but file is missing at ", path,
      ". Check directory permissions or relative paths."
    )
  }
}

save_plot_with_logging <- function(plot_obj, filename, ...) {
  log_save_intent(filename, "plot")

  success <- tryCatch({
    ggsave(filename, plot_obj, ...)
    TRUE
  }, error = function(e) {
    message("[plot] Error while saving ", filename, ": ", e$message)
    FALSE
  })

  log_save_result(filename, "plot", success)
}

save_table_with_logging <- function(gt_obj, filename, ...) {
  log_save_intent(filename, "table")

  success <- tryCatch({
    gtsave(gt_obj, filename, ...)
    TRUE
  }, error = function(e) {
    message("[table] Error while saving ", filename, ": ", e$message)
    FALSE
  })

  log_save_result(filename, "table", success)
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
# We expect output/cpi_data.rds to exist.
data_path <- "output/cpi_data.rds"
if (!file.exists(data_path)) {
  if (file.exists("../output/cpi_data.rds")) {
    data_path <- "../output/cpi_data.rds"
  } else {
    # If data doesn't exist, we might need to download it (first run)
    # But for the app, we'll just warn or try to run the download function if loaded.
    message("Data not found. Attempting to download...")
    if (exists("download_cpi_components")) {
      # We need to run the main block logic manually
      components_df <- download_cpi_components()
      weights_df <- download_cpi_weights()
      # ... (simplified processing for fallback)
      # Ideally we just stop and say "Run the data prep script first"
      stop("Please run scripts/cpi_components.R to generate data first.")
    }
  }
}

cpi_data <- readRDS(data_path)
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
  
  # Reactive: Calculate Personal Weights
  personal_weights <- reactive({
    # Depend on Update button
    input$update
    
    # Isolate inputs to wait for button
    isolate({
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
      
      calculate_persona_weights(weights, toggles)
    })
  })
  
  # Reactive: Calculate Personal CPI Index
  personal_index <- reactive({
    w <- personal_weights()
    
    # Sum(w * I)
    components %>%
      left_join(w, by = "component") %>%
      group_by(date) %>%
      summarise(
        value = sum(value * weight, na.rm = TRUE),
        measure = "Personalised CPI"
      ) %>%
      ungroup()
  })
  
  # Reactive: Combine with Headline and Calculate Inflation
  combined_data <- reactive({
    p_idx <- personal_index()
    h_idx <- canonical_cpi %>% mutate(measure = "Headline CPI")
    
    # Combine
    both <- bind_rows(h_idx, p_idx)
    
    # Calculate Annualised Inflation (12-month for the main chart usually, or 3-month?)
    # Prompt: "Plot headline CPI annualised inflation... Plot personalised CPI annualised inflation"
    # Usually "Inflation" implies Annual (12-month) change, but the prompt asks for annualised rates.
    # Let's show 12-month Annualised (which is just Year-ended percentage change).
    # Or maybe 3-month annualised?
    # "Plot headline CPI annualised inflation... Plot personalised CPI annualised inflation on the same time axis."
    # Given the volatility of 1-month/3-month, 12-month is standard for "Inflation".
    # But Section 3 asks for 1m, 3m, 6m, 12m.
    # Let's plot the 12-month rate as the primary "Inflation" metric, or maybe allow user to select?
    # I'll stick to 12-month for the main chart as it's the standard definition of "Inflation".
    
    # Calculate 12-month change
    # (P_t / P_{t-12}) - 1
    # Note: Data is Quarterly. So lag is 4 quarters (12 months).
    # If data is monthly, lag is 12.
    # The components data (Table 7) is Quarterly.
    
    both %>%
      group_by(measure) %>%
      mutate(
        lag_val = lag(value, 4), # Assuming Quarterly
        rate = (value / lag_val) - 1 # Simple % change over 1 year
      ) %>%
      filter(!is.na(rate)) %>%
      filter(date >= (max(date) - years(5))) # Last 5 years
  })
  
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
    message("Attempting to save headline vs personal inflation plot to ", target_file)
    save_plot_with_logging(
      plot_obj,
      filename = target_file,
      width = 10,
      height = 6,
      dpi = 300
    )

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
    message("Attempting to save latest inflation rates table to ", table_file)
    save_table_with_logging(
      table_obj,
      filename = table_file,
      expand = 10
    )

    table_obj
  })
}

# Run
shinyApp(ui, server)
