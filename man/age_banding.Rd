% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_band_for_phe.r
\name{age_banding}
\alias{age_banding}
\title{age_banding}
\usage{
age_banding(df, age_column, min_age = -1, zero_band = TRUE)
}
\arguments{
\item{df}{Dataframe containing a column with single year ages.}

\item{age_column}{A column found in 'data' containing ages.}

\item{min_age}{(numeric) The minimum age to include in the age bands. Default is -1, which includes all ages up to 120.}

\item{zero_band}{(logical) If TRUE, includes a band for age 0 and one for 1-4. If FALSE, the first age band is 0-4. Default is TRUE.}
}
\value{
(dataframe) A dataframe with a new column containing the age bands.
}
\description{
This function creates a new column of 5 year age bands based on a
column of single year ages. The age bands are labelled with the lower bound of
each band to match the PHEindicators package input requirements.
}
\examples{
test_df <- data.frame(id = 1:6, age = c(0, 3, 4, 5, 12, 91))

# to return age bands with a zero band, 1-4 and then in 5 year bands 
df <- age_banding(df = test_df,
                       age_column = age)

# to return age bands with a 0-4 and then in 5 year bands 
df <- age_banding(df = test_df,
                       age_column = age,
                       zero_band = FALSE)


# to return 5 year age bands from ages 15+ 
df <- age_banding(df = test_df,
                       age_column = age,
                       min_age = 15)


}
