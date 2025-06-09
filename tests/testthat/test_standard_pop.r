test_errors <- tibble(
  id = 1:6,
  age_band = c(-1, 0, 5, 22, 90, 95),
  values = rep(c(1234, 5678), 3)
)

test_returns <- tibble(
  id = 1:4,
  age_band = c(0, 5, 20, 90),
  values = rep(c(1234, 5678), 2)
)

test_that("Values are assigned correctly according to the age band", {
  result <- add_esp(test_returns, age_band)

  expect_equal(result$esp2013[1], 5000)
  expect_equal(result$esp2013[2], 5500)
  expect_equal(result$esp2013[3], 6000)
  expect_equal(result$esp2013[4], 1000)
})

test_that("Other values are unchanged", {
  result <- add_esp(test_returns, age_band)

  expect_equal(result$values[1], 1234)
  expect_equal(result$values[2], 5678)
})

test_that("Function errors when ages are not in age bands", {
  expect_error(
    add_esp(test_errors, age_band),
    regexp = "Invalid age band values found"
  )
})

test_that("Function errors when column does not exist", {
  expect_error(
    add_esp(test_df, trick),
    regexp = "object 'trick' not found"
  )
})
