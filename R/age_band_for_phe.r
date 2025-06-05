#' @title age_band_for_phe
#'
#' @description This function creates a new column of 5 year age bands based on a 
#' column of single year ages. The age bands are labelled with the lower bound of 
#' each band to match the PHEindicators package input requirements.
#' 
#' @param data Dataframe containing a column with single year ages.
#' @param age_column A column found in 'data' containing ages.
#' 
#' @return (dataframe) A dataframe with a new column containing the age bands.
#' 
#' @examples 
#' test_df <- data.frame(id = 1:6, age = c(0, 3, 4, 5, 12, 91))
#' 
#' df <- age_band_for_phe(data = test_df,
#'                             age_column = age)
#' 
#' 
#' @export
age_band_for_phe_le <- function(df, age_column) {
  df <- df %>%
  mutate(age_band = cut({{age_column}}, 
                     breaks = c(-1, 0, 4, 9, 14, 19, 24, 29, 34, 39, 
                                44, 49, 54, 59, 64, 69, 74, 79, 
                                84, 89, 120), 
                     labels = c(0, 1, 5, 10, 15, 20, 25, 
                                30, 35, 40, 45, 50, 
                                55, 60, 65, 70, 
                                75, 80, 85, 90)))
}