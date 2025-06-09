#' @title add_esp
#'
#' @description This function creates a new column of the European standard population 
#' in 5 year age bands to match 5 year age bands in your dataframe. Your dataframe should
#' already have a column called 'age_band' with 5 year age bands starting from 0 and the
#' age bands should be labelled in the following format: 0, 5, 10, 15, 20, 25, 30, 35, 40, 45,
#' 50, 55, 60, 65, 70, 75, 80, 85, 90.
#' 
#' @param df Dataframe containing a column with 5 year age bands.
#' @param age_band_col A column found in df containing age bands.

#' @return (dataframe) A dataframe with a new column containing the european standard 
#' population values for each age band.
#' 
#' @examples 
#' test_df <- data.frame(id = 1:6, age_band = c(0, 5, 10, 15, 35, 50))
#' 
#' # to return age bands with a zero band, 1-4 and then in 5 year bands 
#' df <- add_esp(df = test_df,
#'               age_column = age_band)
#' 
#' @export
add_esp <- function(df, age_band_col) {
# Define valid age bands
 valid_age_bands <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
 
 # Extract the column as a vector
 age_values <- dplyr::pull(df, {{age_band_col}})
 
 # Check for invalid values
 invalid_values <- setdiff(unique(age_values), valid_age_bands)
 
 if (length(invalid_values) > 0) {
    stop(paste("Invalid age band values found:", paste(invalid_values, collapse = ", ")))
 }

    df <- df %>%
        dplyr::mutate(esp2013 = case_when(
            {{age_band_col}} == 0 ~ 5000,
            {{age_band_col}} == 5 ~ 5500,
            {{age_band_col}} == 10 ~ 5500,
            {{age_band_col}} == 15 ~ 5500,
            {{age_band_col}} == 20 ~ 6000,
            {{age_band_col}} == 25 ~ 6000,
            {{age_band_col}} == 30 ~ 6500,
            {{age_band_col}} == 35 ~ 7000,
            {{age_band_col}} == 40 ~ 7000,
            {{age_band_col}} == 45 ~ 7000,
            {{age_band_col}} == 50 ~ 7000,
            {{age_band_col}} == 55 ~ 6500,
            {{age_band_col}} == 60 ~ 6000,
            {{age_band_col}} == 65 ~ 5500,
            {{age_band_col}} == 70 ~ 5000,
            {{age_band_col}} == 75 ~ 4000,
            {{age_band_col}} == 80 ~ 2500,
            {{age_band_col}} == 85 ~ 1500,
            {{age_band_col}} == 90 ~ 1000
        ))
}