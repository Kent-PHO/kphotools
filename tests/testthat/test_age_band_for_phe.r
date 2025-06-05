test_df <- data.frame(id = 1:7, age = c(0, -2, 4, 5, 12, 91, 121))

test_that("Age band classification works correctly", {
  result <- age_band_for_phe_le(test_df, age)

 expect_equal(as.character(result$age_band[1]), "0")
 expect_true(is.na(result$age_band[2])) # Expect NA for negative age
 expect_equal(as.character(result$age_band[3]), "1")
 expect_equal(as.character(result$age_band[4]), "5")
 expect_equal(as.character(result$age_band[5]), "10")
 expect_equal(as.character(result$age_band[6]), "90")
 expect_true(is.na(result$age_band[7])) # Expect NA for age outside bounds


test_that("Function errors when column does not exist", {
 expect_error(
 age_band_for_phe_le(test_df, trick),
 regexp = "object 'trick' not found"
 )
})


test_that("Function errors when column is not numeric", {
 df_non_numeric <- data.frame(id = 1:3, age = c("young", "middle", "old"))
 expect_error(
 age_band_for_phe_le(df_non_numeric, age),
 regexp = "'x' must be numeric"
 )
})

})
