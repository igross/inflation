env <- new.env()
# Sourcing (not running) the script defines calculate_persona_weights() without
# triggering the download block, which is guarded by sys.nframe() == 0.
suppressPackageStartupMessages(sys.source(file.path(repo_root(), "scripts", "cpi_components.R"), envir = env))
calculate_persona_weights <- env$calculate_persona_weights

base_weights <- function() {
  w <- synthetic_weights()
  tibble::tibble(component = w$expenditure_class, weight = w$weight_class / sum(w$weight_class))
}

wt <- function(w, comp) w$weight[w$component == comp]

test_that("neutral persona returns normalised base weights", {
  b <- base_weights()
  w <- calculate_persona_weights(b, list())
  expect_equal(w$weight, b$weight)
  expect_equal(sum(w$weight), 1)
})

test_that("every toggle yields valid normalised weights", {
  toggles <- list(
    list(vegetarian = TRUE), list(vegan = TRUE),
    list(transport = "car_owner"), list(transport = "car_free"),
    list(housing = "renter"), list(housing = "homeowner"),
    list(traveller = TRUE), list(family = TRUE), list(smoker = TRUE),
    list(energy = "high"), list(energy = "low"),
    list(vegan = TRUE, transport = "car_free", housing = "renter", traveller = TRUE,
         family = TRUE, smoker = TRUE, energy = "high")
  )
  for (tg in toggles) {
    w <- calculate_persona_weights(base_weights(), tg)
    expect_equal(nrow(w), nrow(base_weights()))
    expect_false(anyNA(w$weight))
    expect_true(all(w$weight >= 0))
    expect_equal(sum(w$weight), 1, tolerance = 1e-12)
  }
})

test_that("vegetarian removes meat and boosts plant foods", {
  b <- base_weights()
  w <- calculate_persona_weights(b, list(vegetarian = TRUE))
  for (m in c("Beef and veal", "Pork", "Lamb and goat", "Poultry", "Other meats", "Fish and other seafood")) {
    expect_equal(wt(w, m), 0, info = m)
  }
  expect_gt(wt(w, "Vegetables"), wt(b, "Vegetables"))
  expect_gt(wt(w, "Milk"), 0)
})

test_that("vegan additionally removes dairy and eggs", {
  w <- calculate_persona_weights(base_weights(), list(vegan = TRUE))
  for (d in c("Milk", "Cheese", "Eggs", "Ice cream and other dairy products")) expect_equal(wt(w, d), 0, info = d)
})

test_that("housing toggles move weight between rents and ownership", {
  b <- base_weights()
  r <- calculate_persona_weights(b, list(housing = "renter"))
  expect_equal(wt(r, "New dwelling purchase by owner-occupiers"), 0)
  expect_gt(wt(r, "Rents"), wt(b, "Rents"))
  h <- calculate_persona_weights(b, list(housing = "homeowner"))
  expect_equal(wt(h, "Rents"), 0)
})

test_that("car-free removes private motoring", {
  w <- calculate_persona_weights(base_weights(), list(transport = "car_free"))
  for (c in c("Motor vehicles", "Automotive fuel", "Maintenance and repair of motor vehicles", "Insurance")) {
    expect_equal(wt(w, c), 0, info = c)
  }
  expect_gt(wt(w, "Urban transport fares"), wt(base_weights(), "Urban transport fares"))
})
