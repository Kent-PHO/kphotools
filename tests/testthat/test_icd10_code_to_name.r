test_df <- tibble(
  id = 1:7,
  icd10 = c("C34", "I21", "X81", "F03", "Z99", "C50", "G40")
)

test_that("Detailed classification works correctly", {
  result <- icd10_name(test_df, icd10, detailed = TRUE, excel_decomp = FALSE)

  expect_equal(result$icd10_name_detailed[1], "Lung cancer")
  expect_equal(result$icd10_name_detailed[2], "Ischaemic heart diseases")
  expect_equal(result$icd10_name_detailed[3], "Suicide and injury of undetermined intent")
  expect_equal(result$icd10_name_detailed[4], "Dementia and Alzheimer disease")
  expect_equal(result$icd10_name_detailed[5], "Other")
  expect_equal(result$icd10_name_detailed[6], "Breast cancer")
  expect_equal(result$icd10_name_detailed[7], "Epilepsy")
})

test_that("Broad classification works correctly", {
  result <- icd10_name(test_df, icd10, detailed = FALSE)

  expect_equal(result$icd10_name_broad[1], "Cancer")
  expect_equal(result$icd10_name_broad[2], "Circulatory")
  expect_equal(result$icd10_name_broad[3], "External causes")
  expect_equal(result$icd10_name_broad[4], "Mental and behavioural")
  expect_equal(result$icd10_name_broad[5], "Other")
  expect_equal(result$icd10_name_broad[6], "Cancer")
  expect_equal(result$icd10_name_broad[7], "Other")
})

test_that("Detailed classification works correctly when excel_decomp is true", {
  result <- icd10_name(test_df, icd10, detailed = TRUE, excel_decomp = TRUE)

  expect_equal(result$icd10_name_detailed[1], "Lung cancer")
  expect_equal(result$icd10_name_detailed[2], "Ischaemic heart diseases")
  expect_equal(result$icd10_name_detailed[3], "Suicide and injury of undetermined intent")
  expect_equal(result$icd10_name_detailed[4], "Dementia and Alzheimer disease")
  expect_equal(result$icd10_name_detailed[5], "Other")
  expect_equal(result$icd10_name_detailed[6], "Breast cancer")
  expect_equal(result$icd10_name_detailed[7], "Other")
})
