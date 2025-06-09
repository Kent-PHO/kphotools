#' @title icd10_name
#'
#' @description This function creates a new column of death categories based on a column of ICD 10 codes
#' 
#' @param df Dataframe containing a column with ICD 10 codes.
#' @param code_column A column found in 'data' containing ICD 10 codes.
#' @param detailed Specifies whether to return detailed or broad categories. Defaults to TRUE for detailed categories. Set to FALSE for broad categories.
#' 
#' @return (dataframe) A dataframe with a new column containing the ICD 10 code names.
#' 
#' @examples 
#' df_death <- icd10_name(df = death_data,
#'                             code_column = underlying_cause_of_death,
#'                             detailed = TRUE)
#' 
#' to get broad categories 
#' df_death <- icd10_name(df = death_data,
#'                             code_column = underlying_cause_of_death,
#'                             detailed = FALSE)
#' 
#' @export
icd10_name <- function(df, code_column, detailed = TRUE) {
  code_column <- enquo(code_column)
  if (detailed) {
    df <- df %>% mutate(icd10_name_detailed = case_when(
      grepl("^U071|^U072|^U099|^U109", !!code_column) ~ "COVID-19",
      grepl("^C18|^C19|^C20|^C21", !!code_column) ~ "Colorectal cancer",
      grepl("^C33|^C34", !!code_column) ~ "Lung cancer",
      grepl("^C71", !!code_column) ~ "Brain cancer",
      grepl("^C41", !!code_column) ~ "Bone cancer",
      grepl("^C43|^44", !!code_column) ~ "Skin cancer",
      grepl("^C61", !!code_column) ~ "Prostate cancer",
      grepl("^C50", !!code_column) ~ "Breast cancer",
      grepl("^C8|^C90|^C91|^C92|^C93|^C94|^C95|^C96", !!code_column) ~ "Leukaemia and lymphoma",
      grepl("^F01|^F03|^G30", !!code_column) ~ "Dementia and Alzheimer disease",
      grepl("^F", !!code_column) ~ "Other mental and behavioural disorders",
      grepl("^G40", !!code_column) ~ "Epilepsy",
      grepl("^I20|^I21|^I22|^I23|^I24|^I25", !!code_column) ~ "Ischaemic heart diseases",
      grepl("^I6", !!code_column) ~ "Cerebrovascular diseases (stroke)",
      grepl("^I3|^I4|^I5", !!code_column) ~ "Other forms of heart disease",
      grepl("^I0|^I1|^I7|^I8|^I9", !!code_column) ~ "Other circulatory diseases",
      grepl("^J09|^J1", !!code_column) ~ "Influenza and pneumonia",
      grepl("^J4", !!code_column) ~ "Chronic lower respiratory diseases",
      grepl("^J", !!code_column) ~ "Other respiratory diseases",
      grepl("^K70|^K71|^K72|^K73|^K74|^K75|^K76", !!code_column) ~ "Cirrhosis and other diseases of liver",
      grepl("^K", !!code_column) ~ "Other digestive diseases",
      grepl("^P", !!code_column) ~ "Perinatal conditions",
      grepl("^Q", !!code_column) ~ "Congenital and chromosomal conditions",
      grepl("^R95", !!code_column) ~ "Sudden infant death syndrome",
      grepl("^V", !!code_column) ~ "Transport accidents",
      grepl("^X4", !!code_column) ~ "Accidental poisoning",
      grepl("^X6|^X7|^X80|^X81|^X82|^X83|^X84|^Y1|^Y2|^Y30|^Y31|^Y32|^Y33|^Y34", !!code_column) ~ "Suicide and injury of undetermined intent",
      grepl("^X|^Y", !!code_column) ~ "Other external causes",
      TRUE ~ "Other"))
  }
    else if (detailed == FALSE) {
       df <- df %>% mutate(icd10_name_broad = case_when(
        grepl("^C", !!code_column) ~ "Cancer",
        grepl("^F|^G30", !!code_column) ~ "Mental and behavioural",
        grepl("^I", !!code_column) ~ "Circulatory", 
        grepl("^J", !!code_column) ~ "Respiratory",
        grepl("^K", !!code_column) ~ "Digestive", 
        grepl("^V|^X|^Y", !!code_column) ~ "External causes", 
        TRUE ~ "Other"))
    }
  }
