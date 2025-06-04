## ---------------------------
## Script name: underlying cause of death from pcmd data
## Purpose of script: to become quarto report on deaths in Kent
## Project name: Cause of death report
## Project reference number: 

## Author: HoptoH01
## Date created: 2025-02-05
## Date last updated: 
## Copyright (c) Kent Public Health Observatory, Kent County Council, 2024
## Email: kpho@kent.gov.uk

## ---------------------------
## Notes:
##   info on the phe_life_expectancy package can be found here:
##   https://github.com/ukhsa-collaboration/PHEindicatormethods/blob/master/R/LifeExpectancy.R
## ---------------------------


## ---- Settings ---- ##
setwd("C:/Users/HoptoH01/OneDrive - KENT COUNTY COUNCIL/KPHO/Cause of death report/analysis")

# set start date and end date for aggregating years
start_year <- 2022
end_year <- 2024

# delete as appropriate
area_type <- "county"/ "district" / "hcp"
area_name <- 

## ---- Packages ---- ##
library(tidyr)
library(dplyr)
library(janitor)
library(openxlsx)
library(ggplot2)
library(stringr)
library(writexl)
library(forcats)
library(ggrepel)
library(PHEindicatormethods)
library(patchwork)

## ---- Get data ---- ##

# load data and lookups
pcmd <- readRDS("//kcc-app898/PublicHealth/Vital statistics/Deaths/R/pcmd.Rds")
pcmd <- pcmd$data
pcmd <- clean_names(pcmd)

# get premade population file which has single years of sex and sex by lsoa
pop <- readRDS("C:/Users/HoptoH01/OneDrive - KENT COUNTY COUNCIL/KPHO/Cause of death report/analysis/population_syoa.rds")
pop <- clean_names(pop)

# up to date postcode file just needs to have postcode to lsoa lookup
postcode_file <- read.csv("G:/ST Strategic Commissioning/Analytics (R)/Public Health Observatory (R)/data/lookups/postcode file/postcode file 2024.csv")
postcode_file <- clean_names(postcode_file)

# get coastal lookup file - should be updated at some point
coastal_lookup <- read.xlsx("G:/ST Strategic Commissioning/Analytics (R)/Public Health Observatory (R)/data/lookups/BUA - Coastal, Rural, Urban/Coastal lookup - In Development.xlsx")
coastal_lookup <- coastal_lookup %>% clean_names() %>% 
                mutate(coastal = ifelse(coastal_non_coastal == "Coastal", "Coastal", "Non-coastal"))

# file with 2021 lsoas and 2019 imd deciles and quintiles
imd <- read.xlsx("G:/ST Strategic Commissioning/Analytics (R)/Public Health Observatory (R)/data/lookups/lsoa/2021-lsoa-imd-lookup.xlsx", sheet = 5)
imd <- clean_names(imd)

## ---- Processing ---- ##

# join lookups to pcmd data and filter to Kent and selected years
df_deaths <- pcmd %>% 
                filter(county_of_residence_code == "E10000016" & 
                        !is.na(underlying_cause_of_death) & 
                        underlying_cause_of_death != "" &
                        year_of_registration >= start_year &
                        year_of_registration <= end_year) %>%
                left_join(postcode_file %>% select(postcode_one_gap, lsoa_2021_code), 
                        by = c("dec_usual_address_postcode" = "postcode_one_gap")) %>%
                left_join(coastal_lookup %>% select(lsoa_2021_best_fit, coastal, bua11nm) %>% distinct(),
                        by = c("lsoa_2021_code" = "lsoa_2021_best_fit")) %>%
                left_join(imd %>% select(lsoa21cd, imd2019_deciles_lsoa21_within_combined_utla23, imd2019_quintiles_lsoa21_within_combined_utla23), 
                        by = c("lsoa_2021_code" = "lsoa21cd")) %>%
                mutate(sex = ifelse(sex == "1", "Male", ifelse(sex == "2", "Female", "Unknown")))


df_deaths$age_band_5y <- cut(df_deaths$age_years, 
    breaks = c(-1, 0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 
                54, 59, 64, 69, 74, 79, 84, 89, 120), 
    labels = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 
                50, 55, 60, 65, 70, 75, 80, 85, 90))

df_deaths$age_band_wide <- cut(df_deaths$age_years, 
    breaks = c(-1, 0, 19, 34, 49, 64, 79, 120), 
    labels = c("Infant", "1-19", "20-34", "35-49", "50-64", "65-79", "80+"))

df_deaths$age_band_4cat <- cut(df_deaths$age_years, 
    breaks = c(-1, 49, 69, 84, 120), 
    labels = c("Age under 50", "50-69", "70-84", "85+"))

# assign death categories 
assign_detailed_deathcat <- function(icd_code) { 
  if (grepl("^U071|^U072|^U099|^U109", icd_code)) {
 return("COVID-19")
 } else if (grepl("^C18|^C19|^C20|^C21", icd_code)) {
 return("Colorectal cancer")
 } else if (grepl("^C33|^C34", icd_code)) {
 return("Lung cancer")
 } else if (grepl("^C71", icd_code)) {
 return("Brain cancer")
 } else if (grepl("^C41", icd_code)) {
 return("Bone cancer")
 } else if (grepl("^C43|^44", icd_code)) {
 return("Skin cancer")
 } else if (grepl("^C61", icd_code)) {
 return("Prostate cancer")
 } else if (grepl("^C50", icd_code)) {
 return("Breast cancer")
 } else if (grepl("^C8|^C90|^C91|^C92|^C93|^C94|^C95|^C96", icd_code)) {
 return("Leukaemia and lymphoma")
 } else if (grepl("^C", icd_code)) {
 return("Other cancers")
 } else if (grepl("^F01|^F03|^G30", icd_code)) {
 return("Dementia and Alzheimer disease")
 } else if (grepl("^F", icd_code)) {
 return("Other mental and behavioural disorders")
 } else if (grepl("^G40", icd_code)) {
 return("Epilepsy")
 } else if (grepl("^I20|^I21|^I22|^I23|^I24|^I25", icd_code)) {
 return("Ischaemic heart diseases")
 } else if (grepl("^I6", icd_code)) {
 return("Cerebrovascular diseases (stroke)")
 } else if (grepl("^I3|^I4|^I5", icd_code)) {
 return("Other forms of heart disease")
 } else if (grepl("^I0|^I1|^I7|^I8|^I9", icd_code)) {
 return("Other circulatory diseases")
 } else if (grepl("^J09|^J1", icd_code)) {
 return("Influenza and pneumonia")
 } else if (grepl("^J4", icd_code)) {
 return("Chronic lower respiratory diseases")
 } else if (grepl("^J", icd_code)) {
 return("Other respiratory diseases")
 } else if (grepl("^K70|^K71|^K72|^K73|^K74|^K75|^K76", icd_code)) {
 return("Cirrhosis and other diseases of liver")
 } else if (grepl("^K", icd_code)) {
 return("Other digestive diseases")
 } else if (grepl("^P", icd_code)) {
 return("Perinatal conditions")
 } else if (grepl("^Q", icd_code)) {
 return("Congenital and chromosomal conditions")
 } else if (grepl("^R95", icd_code)) {
 return("Sudden infant death syndrome")
 } else if (grepl("^V", icd_code)) {
 return("Transport accidents")
  } else if (grepl("^X4", icd_code)) {
 return("Accidental poisoning")
 } else if (grepl("^X6|^X7|^X80|^X81|^X82|^X83|^X84|^Y1|^Y2|^Y30|^Y31|^Y32|^Y33|^Y34", icd_code)) {
  return("Suicide and injury of undetermined intent")
} else if (grepl("^X|^Y", icd_code)) {
 return("Other external causes")
 } else return("Other")}

df_deaths <- df_deaths %>%
    mutate(cause_of_death_category = sapply(underlying_cause_of_death, assign_detailed_deathcat))

as_hist <- ggplot(df_deaths, aes(x = age_years, fill = sex)) +
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c(Male = "#2D7C82", Female = "#B5E2E5")) +
    labs(title = "Age Distribution of Deaths in Kent (2022-2024)",
         x = "Age",
         y = "Number of Deaths", 
         legend = "Sex") +
  theme(theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        legend.position = "right", 
        legend.key.size = unit(0.6, "cm")))

df_deaths_grouped <- df_deaths %>%
    group_by(age_band_wide) %>%
    mutate(band_pop = n()) %>%
    group_by(age_band_wide, band_pop, cause_of_death_category) %>%
    summarise(deaths = n(), .groups = "drop") %>%
    mutate(pct = deaths / band_pop * 100) 

df_deaths_grouped_top5 <- df_deaths_grouped %>%
    filter(cause_of_death_category != "Other" & 
            cause_of_death_category != "Other cancers" &
            cause_of_death_category != "Other circulatory diseases") %>%
    group_by(age_band_wide) %>%
    arrange(desc(pct)) %>%
    slice_head(n = 5)

df_deaths_grouped_top5 <- df_deaths_grouped_top5 %>%
    group_by(age_band_wide) %>%
    arrange(desc(pct)) %>%
    mutate(rank = row_number())


# trim underlying cause of death code to 3 characterrs in new column 
df_deaths$underlying_cause_of_death_3 <- substr(df_deaths$underlying_cause_of_death, 1, 3)


df_deaths %>%
    filter(age_band_wide == "1-19" & cause_of_death_category == "Other") %>%
    group_by(underlying_cause_of_death_3) %>%
    summarise(deaths = n()) %>%
    arrange(desc(deaths)) %>%
    print(n = 100)


df_deaths_grouped_top5 %>%
filter(age_band_wide != "Infant") %>%
ggplot(aes(x = fct_rev(age_band_wide), y = pct, fill = fct_rev(as.factor(rank)))) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("1" = "#2D7C82", "2" = "#FF2C5F", "3" = "#7D3F64", "4" = "#228B22", "5" = "#FFD700")) +
    theme_minimal() +
    labs(title = "Top 5 Causes of Death in Kent (2022-2024)",
         x = "Age Group",
         y = "Percentage of Deaths") +
    theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
          legend.position = "none")
   
ggsave("top_5_causes_of_death.png", width = 8, height = 8)

######### death rates by age from 2014 to 2024 #########

df_deaths_10y <- pcmd %>% 
                filter(county_of_residence_code == "E10000016" & 
                        !is.na(underlying_cause_of_death) & 
                        underlying_cause_of_death != "" &
                        year_of_registration %in% c("2014", "2019", "2024")) %>%
                mutate(sex = ifelse(sex == "1", "Male", ifelse(sex == "2", "Female", "Unknown")))

df_deaths_10y$age_band_4cat <- cut(df_deaths_10y$age_years, 
    breaks = c(-1, 49, 69, 84, 120), 
    labels = c("Age under 50", "50-69", "70-84", "85+"))

df_deaths_10y_grouped <- df_deaths_10y %>%
    group_by(year_of_registration, age_band_4cat, sex) %>%
    summarise(deaths = n())

# process population data for mortality rate 10y comparison chart
View(head(pop, 100))

df_pop <- pop %>% 
    mutate(year = year + 2) %>% # the population file is 2 years behind
    filter(year %in% c("2014", "2019", "2024") &
            district != "Medway" &
            gender != "Total")

df_pop$age <- as.numeric(df_pop$age)

df_pop$age_band_4cat <- cut(df_pop$age, 
    breaks = c(-1, 49, 69, 84, 120), 
    labels = c("Age under 50", "50-69", "70-84", "85+"))

df_pop_grouped <- df_pop %>%
    group_by(year, age_band_4cat, gender) %>%
    summarise(population = sum(population))


df_10y <- df_deaths_10y_grouped %>%
    left_join(df_pop_grouped, by = c("year_of_registration" = "year", "age_band_4cat", "sex" = "gender"))

df_10y <- df_10y %>%
    group_by(year_of_registration, age_band_4cat, sex) %>%
    phe_rate(deaths, population)

mortality_rate$age_band_4cat <- factor(mortality_rate$age_band_4cat, 
    levels = c("Age under 50", "50-69", "70-84", "85+"))




plot_age_group <- function(data, title) {
 ggplot(data, aes(x = year_of_registration, y = value, color = sex)) +
 geom_line(size = 1) +
 labs(title = title, x = NULL, y = "Mortality Rate per 100,000") +
 scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
 theme_minimal()
}

# Create individual plots
p1 <- plot_age_group(filter(df_10y, age_band_4cat == "Age under 50"), "Age under 50")
p2 <- plot_age_group(filter(df_10y, age_band_4cat == "50-69"), "Age 50-69")
p3 <- plot_age_group(filter(df_10y, age_band_4cat == "70-84"), "Age 70-84")
p4 <- plot_age_group(filter(df_10y, age_band_4cat == "85+"), "Age 85+")

# Combine plots
(p1 | p2 | p3 | p4) + plot_layout(guides = "collect")

# european standard population for 2013 from the PHEindicatormethods package
# to use as reference for age standardisation
esp2013 <- as.vector(esp2013)
esp2013df <- data.frame(esp2013)

# add age bands to the esp2013df
stdpop <- esp2013df %>%
    mutate(row = row_number()) %>%
    mutate(group = case_when(
        row <= 10 ~ "Age under 50",
        row %in% c(11, 12, 13, 14)  ~ "50-69",
        row %in% c(15, 16, 17) ~ "70-84",
        row >= 18 ~ "85+"
    )) %>%
    group_by(group) %>%
    summarise(stdpop = sum(esp2013))

# join to my age banded data 
df_10y <- df_10y %>%
    left_join(stdpop %>% select(group, stdpop), by = c("age_band_4cat" = "group")) 

# calculate the direct standardised rate
mortality_rate <- df_10y %>%
    group_by(year_of_registration, age_band_4cat, sex) %>%
    calculate_dsr(deaths, population, stdpop)



## ---- Outputs ---- ##

# plot and save the mortality rates
ggplot(mortality_rate, aes(x = year_of_registration, y = value, color = sex)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = lowercl, ymax = uppercl), width = 0.2, color = "black") +
    theme_classic() + 
    scale_color_manual(values = c("Female" = "turquoise", "Male" = "pink")) +
    facet_wrap(~ age_band_4cat, scales = "free")

ggsave("mortality_rate_10y.png", width = 12, height = 8)


