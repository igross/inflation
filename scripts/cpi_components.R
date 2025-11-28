library(readabs)
library(readxl)
library(tidyverse)
library(lubridate)
library(scales)

# Ensure output directory exists
if (!dir.exists("output")) {
  dir.create("output")
}

# Persona weight calculator kept as a single function for reuse by the Shiny app
calculate_persona_weights <- function(base_weights_df, toggles) {
  w <- base_weights_df

  # Vegetarian or vegan
  if (isTRUE(toggles$vegetarian) || isTRUE(toggles$vegan)) {
    meat_keywords <- c("Beef", "Pork", "Lamb", "Poultry", "Other meats", "Fish", "Seafood", "Meat")
    is_meat <- grepl(paste(meat_keywords, collapse = "|"), w$component, ignore.case = TRUE) &
      !grepl("Vegetables", w$component, ignore.case = TRUE)

    target_keywords <- c("Fruit", "Vegetables", "Bread", "Cereal", "Grains")
    is_target <- grepl(paste(target_keywords, collapse = "|"), w$component, ignore.case = TRUE)

    removed_weight <- sum(w$weight[is_meat], na.rm = TRUE)
    w$weight[is_meat] <- 0

    target_sum <- sum(w$weight[is_target], na.rm = TRUE)
    if (target_sum > 0) {
      w$weight[is_target] <- w$weight[is_target] + (w$weight[is_target] / target_sum) * removed_weight
    }
  }

  # Vegan-specific reallocation
  if (isTRUE(toggles$vegan)) {
    dairy_keywords <- c("Milk", "Cheese", "Yoghurt", "Dairy", "Eggs", "Butter")
    is_dairy <- grepl(paste(dairy_keywords, collapse = "|"), w$component, ignore.case = TRUE)

    target_keywords <- c("Fruit", "Vegetables", "Bread", "Cereal", "Grains")
    is_target <- grepl(paste(target_keywords, collapse = "|"), w$component, ignore.case = TRUE)

    removed_weight <- sum(w$weight[is_dairy], na.rm = TRUE)
    w$weight[is_dairy] <- 0

    target_sum <- sum(w$weight[is_target], na.rm = TRUE)
    if (target_sum > 0) {
      w$weight[is_target] <- w$weight[is_target] + (w$weight[is_target] / target_sum) * removed_weight
    }
  }

  # Transport
  if (identical(toggles$transport, "car_owner")) {
    car_keywords <- c("Motor vehicles", "Automotive fuel", "Maintenance and repair of motor vehicles", "Spares and accessories")
    pt_keywords <- c("Urban transport fares", "Public transport")

    is_car <- grepl(paste(car_keywords, collapse = "|"), w$component, ignore.case = TRUE)
    is_pt <- grepl(paste(pt_keywords, collapse = "|"), w$component, ignore.case = TRUE)

    w$weight[is_car] <- w$weight[is_car] * 1.5
    w$weight[is_pt] <- w$weight[is_pt] * 0.5
  } else if (identical(toggles$transport, "car_free")) {
    car_keywords <- c("Motor vehicles", "Automotive fuel", "Maintenance and repair of motor vehicles", "Spares and accessories", "Registration", "Insurance")
    pt_keywords <- c("Urban transport fares", "Public transport")

    is_car <- grepl(paste(car_keywords, collapse = "|"), w$component, ignore.case = TRUE)
    is_pt <- grepl(paste(pt_keywords, collapse = "|"), w$component, ignore.case = TRUE)

    removed_weight <- sum(w$weight[is_car], na.rm = TRUE)
    w$weight[is_car] <- 0

    pt_share <- 0.5 * removed_weight
    gen_share <- 0.5 * removed_weight

    pt_sum <- sum(w$weight[is_pt], na.rm = TRUE)
    if (pt_sum > 0) {
      w$weight[is_pt] <- w$weight[is_pt] + (w$weight[is_pt] / pt_sum) * pt_share
    }

    w_sum <- sum(w$weight, na.rm = TRUE)
    if (w_sum > 0) {
      w$weight <- w$weight + (w$weight / w_sum) * gen_share
    }
  }

  # Housing
  if (identical(toggles$housing, "renter")) {
    is_rents <- grepl("Rents", w$component, ignore.case = TRUE)
    own_key <- c("New dwelling purchase", "Owner-occupier")
    is_own <- grepl(paste(own_key, collapse = "|"), w$component, ignore.case = TRUE)

    removed <- sum(w$weight[is_own], na.rm = TRUE)
    w$weight[is_own] <- 0
    w$weight[is_rents] <- w$weight[is_rents] + removed
  } else if (identical(toggles$housing, "homeowner")) {
    is_rents <- grepl("Rents", w$component, ignore.case = TRUE)
    own_key <- c("New dwelling purchase", "Owner-occupier", "Property rates", "Insurance")
    is_own <- grepl(paste(own_key, collapse = "|"), w$component, ignore.case = TRUE)

    removed <- sum(w$weight[is_rents], na.rm = TRUE)
    w$weight[is_rents] <- 0

    own_sum <- sum(w$weight[is_own], na.rm = TRUE)
    if (own_sum > 0) {
      w$weight[is_own] <- w$weight[is_own] + (w$weight[is_own] / own_sum) * removed
    }
  }

  # Traveller
  if (isTRUE(toggles$traveller)) {
    travel_key <- c("Holiday travel", "Accommodation")
    is_travel <- grepl(paste(travel_key, collapse = "|"), w$component, ignore.case = TRUE)
    w$weight[is_travel] <- w$weight[is_travel] * 2
  }

  # Family
  if (isTRUE(toggles$family)) {
    fam_key <- c("Child care", "Education", "Food", "Non-alcoholic beverages")
    reduce_key <- c("Restaurant", "Take away", "Alcohol", "Tobacco")

    is_fam <- grepl(paste(fam_key, collapse = "|"), w$component, ignore.case = TRUE)
    is_reduce <- grepl(paste(reduce_key, collapse = "|"), w$component, ignore.case = TRUE)

    w$weight[is_fam] <- w$weight[is_fam] * 1.2
    w$weight[is_reduce] <- w$weight[is_reduce] * 0.8
  }

  # Smoker
  if (isTRUE(toggles$smoker)) {
    smoke_key <- c("Tobacco", "Alcohol")
    is_smoke <- grepl(paste(smoke_key, collapse = "|"), w$component, ignore.case = TRUE)
    w$weight[is_smoke] <- w$weight[is_smoke] * 2.0
  }

  # Energy
  if (identical(toggles$energy, "high")) {
    energy_key <- c("Electricity", "Gas", "Other household fuels")
    is_energy <- grepl(paste(energy_key, collapse = "|"), w$component, ignore.case = TRUE)
    w$weight[is_energy] <- w$weight[is_energy] * 1.5
  } else if (identical(toggles$energy, "low")) {
    energy_key <- c("Electricity", "Gas", "Other household fuels")
    is_energy <- grepl(paste(energy_key, collapse = "|"), w$component, ignore.case = TRUE)
    w$weight[is_energy] <- w$weight[is_energy] * 0.5
  }

  w$weight <- w$weight / sum(w$weight, na.rm = TRUE)

  w
}

# Only run the heavy download and processing steps when executed directly
if (sys.nframe() == 0) {
  message("Starting CPI Components processing...")

  # Download CPI components (Table 7, Weighted Average of Eight Capital Cities)
  message("Downloading CPI Components (Table 7)...")
  components_df <- read_abs("6401.0", tables = 7, show_progress_bars = FALSE)
  components_df <- components_df %>%
    filter(grepl("Weighted Average of Eight Capital Cities", series)) %>%
    filter(unit == "Index Numbers") %>%
    select(date, value, series, series_id) %>%
    mutate(component = trimws(gsub(";.*", "", series))) %>%
    arrange(date)

  # Load CPI weights from the bundled workbook
  message("Loading CPI Weights from local workbook (Table 1)...")
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
    mutate(across(starts_with("weight_"), ~ readr::parse_number(as.character(.)))) %>%
    filter(!(group == "Group, sub-group and expenditure class")) %>%
    filter(if_any(c(group, sub_group, expenditure_class, weight_group, weight_sub_group, weight_class), ~ !is.na(.))) %>%
    mutate(
      component = coalesce(expenditure_class, sub_group, group),
      raw_weight = coalesce(weight_class, weight_sub_group, weight_group)
    ) %>%
    select(component, weight = raw_weight) %>%
    mutate(weight = as.numeric(weight) / 100) %>%
    filter(!is.na(weight))

  # Align components and weights
  common_names <- intersect(unique(components_df$component), unique(weights$component))
  message(paste("Matched", length(common_names), "components."))

  components_clean <- components_df %>%
    filter(component %in% common_names)

  weights_clean <- weights %>%
    filter(component %in% common_names) %>%
    group_by(component) %>%
    slice(1) %>%
    ungroup()

  weights_clean$weight <- weights_clean$weight / sum(weights_clean$weight, na.rm = TRUE)

  canonical_cpi <- components_clean %>%
    left_join(weights_clean, by = "component") %>%
    group_by(date) %>%
    summarise(
      value = sum(value * weight, na.rm = TRUE),
      measure = "Canonical Headline CPI",
      .groups = "drop"
    )

  saveRDS(list(
    components = components_clean,
    weights = weights_clean,
    canonical_cpi = canonical_cpi
  ), "output/cpi_data.rds")

  message("Saved cpi_data.rds to output/")

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
        measure = paste("Persona:", p_name),
        .groups = "drop"
      )

    persona_results[[p_name]] <- p_index
  }

  all_personas <- bind_rows(persona_results)
  write_csv(all_personas, "output/persona_cpi_indices.csv")

  message("Done.")
}
