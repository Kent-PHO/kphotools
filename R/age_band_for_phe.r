#' @title age_banding
#'
#' @description This function creates a new column of 5 year age bands based on a 
#' column of single year ages. The age bands are labelled with the lower bound of 
#' each band to match the PHEindicators package input requirements.
#' 
#' @param df Dataframe containing a column with single year ages.
#' @param age_column A column found in 'data' containing ages.
#' @param min_age (numeric) The minimum age to include in the age bands. Default is -1, which includes all ages up to 120. 
#' @param zero_band (logical) If TRUE, includes a band for age 0 and one for 1-4. If FALSE, the first age band is 0-4. Default is TRUE.
#' 
#' @return (dataframe) A dataframe with a new column containing the age bands.
#' 
#' @examples 
#' test_df <- data.frame(id = 1:6, age = c(0, 3, 4, 5, 12, 91))
#' 
#' # to return age bands with a zero band, 1-4 and then in 5 year bands 
#' df <- age_banding(df = test_df,
#'                        age_column = age)
#' 
#' # to return age bands with a 0-4 and then in 5 year bands 
#' df <- age_banding(df = test_df,
#'                        age_column = age,
#'                        zero_band = FALSE)
#' 
#' 
#' # to return 5 year age bands from ages 15+ 
#' df <- age_banding(df = test_df,
#'                        age_column = age,
#'                        min_age = 15)
#' 
#' 
#' @export
age_banding <- function(df, age_column, min_age = -1, zero_band = TRUE) {
  breaks <- if (zero_band) {
    c(-1, 0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 120)
  } else {
    c(-1, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 120)
  }
  labels <- if (zero_band) {
    c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
  } else {
    c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
  }
  df %>%
    dplyr::mutate(age_band = cut({{age_column}}, breaks = breaks, labels = labels)) %>%
    dplyr::filter({{age_column}} >= min_age)
}

