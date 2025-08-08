## Data manipulation by year Sex-Age

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# Load your Excel file
df <- read_excel("incidence.xlsx")

# Function to reshape data per year for a specific cancer
reshape_by_year <- function(df, cancer_type) {
  df %>%
    filter(cause == cancer_type) %>%
    group_by(year, sex, age) %>%
    summarise(val = sum(val, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = c(sex, age),
      values_from = val,
      values_fill = 0
    ) %>%
    arrange(year)
}

# Apply function for each cancer type
pancreatic_by_year <- reshape_by_year(df, "Pancreatic cancer")
gallbladder_by_year <- reshape_by_year(df, "Gallbladder and biliary tract cancer")

# Export to excel
write_xlsx(pancreatic_by_year, "pancreatic_inci_by_sex_age.xlsx")
write_xlsx(gallbladder_by_year, "gallbladder_inci_by_sex_age.xlsx")

# Export to CSV (optional)
write.csv(pancreatic_by_year, "pancreatic_by_sex_age.csv", row.names = FALSE)
write.csv(gallbladder_by_year, "gallbladder_by_sex_age.csv", row.names = FALSE)

######################################################################################

## Data manipulation by Sex

library(readxl)
library(dplyr)
library(tidyr)

# Load your Excel file
df <- read_excel("mortality.xlsx")

# Function to reshape data per year for a specific cancer
reshape_by_year <- function(df, cancer_type) {
  df %>%
    filter(cause == cancer_type) %>%
    group_by(year, sex) %>%
    summarise(val = sum(val, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = c(sex),
      values_from = val,
      values_fill = 0
    ) %>%
    arrange(year)
}

# Apply function for each cancer type
pancreatic_by_year <- reshape_by_year(df, "Pancreatic cancer")
gallbladder_by_year <- reshape_by_year(df, "Gallbladder and biliary tract cancer")

# Export to excel
write_xlsx(pancreatic_by_year, "pancreatic_inci_by_sex.xlsx")
write_xlsx(gallbladder_by_year, "gallbladder_inci_by_sex.xlsx")

# Export to CSV (optional)
write.csv(pancreatic_by_year, "pancreatic_by_sex.csv", row.names = FALSE)
write.csv(gallbladder_by_year, "gallbladder_by_sex.csv", row.names = FALSE)

#############################################################################################
## Data manipulation by age

library(readxl)
library(dplyr)
library(tidyr)

# Load your Excel file
df <- read_excel("mortality.xlsx")

# Function to reshape data per year for a specific cancer
reshape_by_year <- function(df, cancer_type) {
  df %>%
    filter(cause == cancer_type) %>%
    group_by(year, age) %>%
    summarise(val = sum(val, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = c(age),
      values_from = val,
      values_fill = 0
    ) %>%
    arrange(year)
}

# Apply function for each cancer type
pancreatic_by_year <- reshape_by_year(df, "Pancreatic cancer")
gallbladder_by_year <- reshape_by_year(df, "Gallbladder and biliary tract cancer")

# Export to excel
write_xlsx(pancreatic_by_year, "pancreatic_inci_by_age.xlsx")
write_xlsx(gallbladder_by_year, "gallbladder_inci_by_age.xlsx")

# Export to CSV (optional)
write.csv(pancreatic_by_year, "pancreatic_by_age.csv", row.names = FALSE)
write.csv(gallbladder_by_year, "gallbladder_by_age.csv", row.names = FALSE)



