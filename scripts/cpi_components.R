library(readabs)
library(readxl)
library(tidyverse)
library(lubridate)
library(scales)

# Ensure output directory exists
if (!dir.exists("output")) {
  dir.create("output")
}

# ==============================================================================
# 1. Functions for Data Retrieval and Processing
# ==============================================================================

download_cpi_components <- function() {
  message("Downloading CPI Components (Table 7)...")
  # Table 7: CPI: Group, Sub-group and Expenditure Class, Index Numbers by Capital City
  # We usually want the Weighted Average of Eight Capital Cities.
  
  raw <- read_abs("6401.0", tables = 7, show_progress_bars = FALSE)
  
  # Filter for Weighted Average of Eight Capital Cities
  # And we want the index numbers.
  # The series names usually look like "Bread ;  Weighted Average of Eight Capital Cities ;"
  
  components <- raw %>%
    filter(grepl("Weighted Average of Eight Capital Cities", series)) %>%
    filter(unit == "Index Numbers") %>%
    select(date, value, series, series_id) %>%
    # Clean series names to get component name
    mutate(component = gsub(";.*", "", series)) %>%
    mutate(component = trimws(component)) %>%
    arrange(date)
  
  return(components)
}

download_cpi_weights <- function() {
  message("Loading CPI Weights from local workbook (Table 1)...")
  # The repository includes "Consumer Price Index - 2025 Weighting Pattern.xlsx".
  # Table 1 holds the percentage contribution for each expenditure class.
  # Columns follow the pattern:
  #   A = Group, B = Sub-group, C = Expenditure class,
  #   D = Group weight, E = Sub-group weight, F = Expenditure class weight
  # Downstream logic expects a tidy two-column data frame: component + weight.

  weights_raw <- read_excel(
    "Consumer Price Index - 2025 Weighting Pattern.xlsx",
    sheet = "Table 1",
    skip = 6,
    col_names = FALSE
  )

  names(weights_raw)[1:6] <- c(
    "group",
    "sub_group",
    "expenditure_class",
    "weight_group",
    "weight_sub_group",
    "weight_class"
  )

  weights <- weights_raw %>%
    # Drop header row and empty lines
    filter(!(group == "Group, sub-group and expenditure class")) %>%
    filter(if_any(c(group, sub_group, expenditure_class, weight_group, weight_sub_group, weight_class), ~ !is.na(.))) %>%
    mutate(
      component = coalesce(expenditure_class, sub_group, group),
      raw_weight = coalesce(weight_class, weight_sub_group, weight_group)
    ) %>%
    select(component, weight = raw_weight) %>%
    mutate(weight = as.numeric(weight) / 100) %>%
    filter(!is.na(weight))

  return(weights)
}

# ==============================================================================
# 2. Persona Engine
# ==============================================================================

# Helper to normalize weights
normalize_weights <- function(w) {
  w / sum(w, na.rm = TRUE)
}

# The Engine
calculate_persona_weights <- function(base_weights_df, toggles) {
  # base_weights_df: data frame with 'component' and 'weight' columns
  # toggles: list or vector of named booleans/strings
  
  # Copy weights
  w <- base_weights_df
  
  # 1. VEGETARIAN
  if (isTRUE(toggles$vegetarian) || isTRUE(toggles$vegan)) { # Vegan implies Vegetarian logic first
    # Zero out Meat and seafood
    # Components: "Beef and veal", "Pork", "Lamb and goat", "Poultry", "Other meats", "Fish and other seafood"
    # We need to be careful with matching names.
    meat_keywords <- c("Beef", "Pork", "Lamb", "Poultry", "Other meats", "Fish", "Seafood", "Meat")
    
    is_meat <- grepl(paste(meat_keywords, collapse = "|"), w$component, ignore.case = TRUE) & 
               !grepl("Vegetables", w$component, ignore.case = TRUE) # Avoid "Meat pies" if they exist? No, "Meat and seafoods" group.
    
    # Identify target for redistribution: Fruit, Veg, Bread, Cereals
    target_keywords <- c("Fruit", "Vegetables", "Bread", "Cereal", "Grains")
    is_target <- grepl(paste(target_keywords, collapse = "|"), w$component, ignore.case = TRUE)
    
    # Reallocate
    removed_weight <- sum(w$weight[is_meat], na.rm = TRUE)
    w$weight[is_meat] <- 0
    
    # Distribute proportionally to targets
    target_sum <- sum(w$weight[is_target], na.rm = TRUE)
    if (target_sum > 0) {
      w$weight[is_target] <- w$weight[is_target] + (w$weight[is_target] / target_sum) * removed_weight
    }
  }
  
  # 2. VEGAN
  if (isTRUE(toggles$vegan)) {
    # Zero out Dairy
    # Components: "Milk", "Cheese", "Yoghurt", "Dairy", "Eggs" (usually included in vegan exclusion)
    dairy_keywords <- c("Milk", "Cheese", "Yoghurt", "Dairy", "Eggs", "Butter")
    
    is_dairy <- grepl(paste(dairy_keywords, collapse = "|"), w$component, ignore.case = TRUE)
    
    # Targets: Fruit, Veg, Cereals
    target_keywords <- c("Fruit", "Vegetables", "Bread", "Cereal", "Grains")
    is_target <- grepl(paste(target_keywords, collapse = "|"), w$component, ignore.case = TRUE)
    
    removed_weight <- sum(w$weight[is_dairy], na.rm = TRUE)
    w$weight[is_dairy] <- 0
    
    target_sum <- sum(w$weight[is_target], na.rm = TRUE)
    if (target_sum > 0) {
      w$weight[is_target] <- w$weight[is_target] + (w$weight[is_target] / target_sum) * removed_weight
    }
  }
  
  # 3. CAR OWNER vs CAR FREE
  if (identical(toggles$transport, "car_owner")) {
    # Increase Motor vehicles, fuel, etc.
    # Reduce Public Transport.
    car_keywords <- c("Motor vehicles", "Automotive fuel", "Maintenance and repair of motor vehicles", "Spares and accessories")
    pt_keywords <- c("Urban transport fares", "Public transport")
    
    is_car <- grepl(paste(car_keywords, collapse = "|"), w$component, ignore.case = TRUE)
    is_pt <- grepl(paste(pt_keywords, collapse = "|"), w$component, ignore.case = TRUE)
    
    # Increase car weights by e.g. 50% (arbitrary rule for "Car Owner" vs average)
    # The prompt says "Increase weights... Reduce weight for public transport".
    # We'll boost car by factor X and reduce PT by factor Y.
    # Or just shift weight from PT to Car.
    # Let's double the car weight and zero the PT weight? No, "Reduce weight for public transport proportionally".
    
    # Implementation: Boost Car by 20%, reduce PT by 50%. Then normalize.
    w$weight[is_car] <- w$weight[is_car] * 1.5
    w$weight[is_pt] <- w$weight[is_pt] * 0.5
    
  } else if (identical(toggles$transport, "car_free")) {
    # Zero out car
    car_keywords <- c("Motor vehicles", "Automotive fuel", "Maintenance and repair of motor vehicles", "Spares and accessories", "Registration", "Insurance")
    pt_keywords <- c("Urban transport fares", "Public transport")
    
    is_car <- grepl(paste(car_keywords, collapse = "|"), w$component, ignore.case = TRUE)
    is_pt <- grepl(paste(pt_keywords, collapse = "|"), w$component, ignore.case = TRUE)
    
    removed_weight <- sum(w$weight[is_car], na.rm = TRUE)
    w$weight[is_car] <- 0
    
    # Redistribute to PT and General (everything else)
    # Prompt: "Redistribute to public transport + possibly general consumption"
    # Let's put 50% of savings into PT, 50% into General.
    
    pt_share <- 0.5 * removed_weight
    gen_share <- 0.5 * removed_weight
    
    pt_sum <- sum(w$weight[is_pt], na.rm = TRUE)
    if (pt_sum > 0) {
      w$weight[is_pt] <- w$weight[is_pt] + (w$weight[is_pt] / pt_sum) * pt_share
    }
    
    # General redistribution (all non-zero items)
    w_sum <- sum(w$weight, na.rm = TRUE)
    if (w_sum > 0) {
      w$weight <- w$weight + (w$weight / w_sum) * gen_share
    }
  }
  
  # 4. RENTER vs HOMEOWNER
  if (identical(toggles$housing, "renter")) {
    # Increase Rents, Reduce Owner-occupier
    rents_key <- "Rents"
    own_key <- c("New dwelling purchase", "Owner-occupier")
    
    is_rents <- grepl(rents_key, w$component, ignore.case = TRUE)
    is_own <- grepl(paste(own_key, collapse = "|"), w$component, ignore.case = TRUE)
    
    # Zero out owner costs
    removed <- sum(w$weight[is_own], na.rm = TRUE)
    w$weight[is_own] <- 0
    
    # Add to rents
    w$weight[is_rents] <- w$weight[is_rents] + removed
    
  } else if (identical(toggles$housing, "homeowner")) {
    # Zero Rents
    rents_key <- "Rents"
    own_key <- c("New dwelling purchase", "Owner-occupier", "Property rates", "Insurance")
    
    is_rents <- grepl(rents_key, w$component, ignore.case = TRUE)
    is_own <- grepl(paste(own_key, collapse = "|"), w$component, ignore.case = TRUE)
    
    removed <- sum(w$weight[is_rents], na.rm = TRUE)
    w$weight[is_rents] <- 0
    
    # Add to owner costs
    own_sum <- sum(w$weight[is_own], na.rm = TRUE)
    if (own_sum > 0) {
      w$weight[is_own] <- w$weight[is_own] + (w$weight[is_own] / own_sum) * removed
    }
  }
  
  # 5. FREQUENT TRAVELLER
  if (isTRUE(toggles$traveller)) {
    # Increase Holiday travel
    travel_key <- c("Holiday travel", "Accommodation")
    is_travel <- grepl(paste(travel_key, collapse = "|"), w$component, ignore.case = TRUE)
    
    # Increase by factor 2
    w$weight[is_travel] <- w$weight[is_travel] * 2
    # Normalization at the end will handle the offset (reducing others proportionally)
  }
  
  # 6. FAMILY WITH CHILDREN
  if (isTRUE(toggles$family)) {
    # Increase Childcare, Education, Groceries
    fam_key <- c("Child care", "Education", "Food", "Non-alcoholic beverages")
    # Reduce Restaurants, Alcohol
    reduce_key <- c("Restaurant", "Take away", "Alcohol", "Tobacco")
    
    is_fam <- grepl(paste(fam_key, collapse = "|"), w$component, ignore.case = TRUE)
    is_reduce <- grepl(paste(reduce_key, collapse = "|"), w$component, ignore.case = TRUE)
    
    w$weight[is_fam] <- w$weight[is_fam] * 1.2
    w$weight[is_reduce] <- w$weight[is_reduce] * 0.8
  }
  
  # 7. SMOKER
  if (isTRUE(toggles$smoker)) {
    # Increase Tobacco, Alcohol
    smoke_key <- c("Tobacco", "Alcohol")
    is_smoke <- grepl(paste(smoke_key, collapse = "|"), w$component, ignore.case = TRUE)
    
    w$weight[is_smoke] <- w$weight[is_smoke] * 2.0
  }
  
  # 8. ENERGY USER
  if (identical(toggles$energy, "high")) {
    # Increase Electricity, Gas
    energy_key <- c("Electricity", "Gas", "Other household fuels")
    is_energy <- grepl(paste(energy_key, collapse = "|"), w$component, ignore.case = TRUE)
    
    w$weight[is_energy] <- w$weight[is_energy] * 1.5
    
  } else if (identical(toggles$energy, "low")) {
    # Decrease Electricity, Gas
    energy_key <- c("Electricity", "Gas", "Other household fuels")
    is_energy <- grepl(paste(energy_key, collapse = "|"), w$component, ignore.case = TRUE)
    
    w$weight[is_energy] <- w$weight[is_energy] * 0.5
  }
  
  # Final Normalization
  w$weight <- normalize_weights(w$weight)
  
  return(w)
}

# ==============================================================================
# 3. Main Execution Block
# ==============================================================================

# Only run if this script is executed directly (not sourced)
if (sys.nframe() == 0) {
  
  message("Starting CPI Components processing...")
  
  # 1. Download Data
  components_df <- download_cpi_components()
  weights_df <- download_cpi_weights()
  
  # 2. Align Data
  # We need to ensure we have weights for all components.
  # Join weights to components.
  # Note: Names might not match perfectly. This is a common issue with ABS data.
  # We will do a fuzzy match or just inner join and warn.
  
  # Simplify component names for matching
  # (In a real production system, we'd have a mapping table)
  
  # Let's check overlap
  common_names <- intersect(unique(components_df$component), unique(weights_df$component))
  message(paste("Matched", length(common_names), "components."))
  
  # Filter to common components
  components_clean <- components_df %>%
    filter(component %in% common_names)
  
  weights_clean <- weights_df %>%
    filter(component %in% common_names) %>%
    # Ensure unique weights (take latest if duplicates)
    group_by(component) %>%
    slice(1) %>%
    ungroup()
  
  # Normalize weights to sum to 1 (since we might have dropped some)
  weights_clean$weight <- normalize_weights(weights_clean$weight)
  
  # 3. Build Canonical Headline CPI
  # CPI_t = sum(w_i * I_{i,t})
  # But indices have different base years.
  # We assume the index numbers in Table 7 are re-referenced to the same base (usually 2011-12=100).
  # So we can just sum weighted indices.
  
  canonical_cpi <- components_clean %>%
    left_join(weights_clean, by = "component") %>%
    group_by(date) %>%
    summarise(
      value = sum(value * weight, na.rm = TRUE),
      measure = "Canonical Headline CPI"
    ) %>%
    ungroup()
  
  # Compare with Official Headline (if we had it loaded, but we'll just save it)
  
  # 4. Save Data for App
  # We save:
  # - components_clean (Long format: date, component, value)
  # - weights_clean (component, weight)
  # - canonical_cpi
  
  saveRDS(list(
    components = components_clean,
    weights = weights_clean,
    canonical_cpi = canonical_cpi
  ), "output/cpi_data.rds")
  
  message("Saved cpi_data.rds to output/")
  
  # 5. Generate Persona Series (Example for verification)
  # We can generate a few standard personas and save them as CSVs as requested.
  
  # Define standard personas
  personas <- list(
    "Vegetarian" = list(vegetarian = TRUE),
    "Car_Owner" = list(transport = "car_owner"),
    "Renter" = list(housing = "renter")
  )
  
  persona_results <- list()
  
  for (p_name in names(personas)) {
    p_toggles <- personas[[p_name]]
    p_weights <- calculate_persona_weights(weights_clean, p_toggles)
    
    p_index <- components_clean %>%
      left_join(p_weights, by = "component") %>%
      group_by(date) %>%
      summarise(
        value = sum(value * weight, na.rm = TRUE),
        measure = paste("Persona:", p_name)
      ) %>%
      ungroup()
    
    persona_results[[p_name]] <- p_index
  }
  
  all_personas <- bind_rows(persona_results)
  write_csv(all_personas, "output/persona_cpi_indices.csv")
  
  message("Done.")
}
