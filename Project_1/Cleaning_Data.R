# load libraries
library(tidyverse)

# read the file
file <- "FD_03-21-2017 10-22-16-34_timeSeries.csv"
df <- readr::read_csv(file);

# drop spaces and convert headers to lower case
names(df) <- gsub("[ ]", "_", names(df))
gsub(pattern = '[[:upper:]]', replacement = '[[:lower:]]', names(df))
names(df) <- tolower(names(df))

# drop unecessary columns
df <- df[ , !(names(df) %in% c("attribute"))]

years <- df %>% select(-country_name, -country_code, -indicator_name, -indicator_code) %>% names 
n_years <- years %>% length

for (i in 1:n_years) {
  year <- years[i]
  year_data <- df %>%
    select(country_name, country_code, indicator_name, indicator_code, year) %>%
    dplyr::mutate(year = year)
  
  names(year_data) <- c('country_name', 'country_code', 'indicator_name', 'indicator_code', 'values', 'year')
  total <- if(i != 1) rbind(total, year_data)  else  year_data
}

df <- total %>% arrange(country_name, indicator_code, year)

# Change null values in the country_code to 0.
df <- df %>% tidyr::replace_na(list(country_code = 0))

# Remove non-printable characters from all column values.
df <- df %>% dplyr::mutate_all(funs(gsub(pattern="[^ -~]", replacement= "", .)))

# The following write_csv followed immediately by a read_csv, fixes the column types.
write_csv(df, "./tmp.csv")
df <- read_csv("./tmp.csv", col_types = list(
  country_name = col_character(),
  country_code = col_integer(),
  indicator_name = col_character(),
  indicator_code = col_character(),
  values = col_number(),
  year = col_integer()
))

# delete the temporary file
file.remove("./tmp.csv")

# Now save the cleaned data to new.csv
write_csv(df, "./cleaned_data.csv")



