test_df <- data.frame(id = 1:6, age = c(0, 3, 4, 5, 12, 91))

test_that("Age band classification works correctly", {
  result <- age_band_for_phe(test_df, age)

  
 expect_equal(as.character(result$age_band[1]), "0")
 expect_equal(as.character(result$age_band[2]), "1")
 expect_equal(as.character(result$age_band[3]), "1")
 expect_equal(as.character(result$age_band[4]), "5")
 expect_equal(as.character(result$age_band[5]), "10")
 expect_equal(as.character(result$age_band[6]), "90")

})
