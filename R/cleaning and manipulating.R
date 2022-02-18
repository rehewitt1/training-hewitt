library(rvest)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)

webpage <- read_html("https://www.pwrc.usgs.gov/bbl/manual/speclist.cfm")

tbls <- html_nodes(webpage, "table") %>%
  html_table(fill = TRUE)

species <- tbls[[1]] %>%
  clean_names() %>%
  select(alpha_code, common_name) %>%
  mutate(alpha_code = tolower(alpha_code))

pred <- read_csv("https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A9ffec04c-7e2d-41dd-9e88-b6c2e8c4375e")

nests <- read_csv("https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A982bd2fc-4edf-4da7-96ef-0d11b853102d")

#Define a function to join the species table to an arbirary table with a species column in this dataset. Note that this version has some extra conditionals for the optional challenge.

assign_species_name <- function(df, species){
  if (!("alpha_code" %in% names(species)) |
      !("species" %in% names(df)) |
      !("common_name" %in% names(species))){
    stop("Tables appear to be formatted incorrectly.")
  }

  return_df <- left_join(df, species, by = c("species" = "alpha_code"))

  if (nrow(return_df) > nrow(df)){
    warning("Joined table has more rows than original table. Check species table for duplicated code values.")
  }

  if (length(which(is.na(return_df$common_name))) > 0){
    x <- length(which(is.na(return_df$common_name)))
    warning(paste("Common name has", x, "rows containing NA"))
  }

  return(return_df)

}

# Or a simple version without the extra challenges added:
assign_species_name <- function(df, species){
  return_df <- left_join(df, species, by = c("species" = "alpha_code"))
  return(return_df)
}

#Calculate the number of each species by year. Species counts with no common name are removed after examining the data and determining that the species code in these cases was “none”, as in, no predators were observed that day.

pred_species <- assign_species_name(pred, species) %>%
  group_by(year, common_name) %>%
  summarise(pred_count = sum(count, na.rm = T), .groups = "drop") %>%
  filter(!is.na(common_name))

ggplot(pred_species, aes(x = year, y = pred_count, color = common_name)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of Predators", color = "Species") +
  theme_bw() #or use custom theme from mytools
