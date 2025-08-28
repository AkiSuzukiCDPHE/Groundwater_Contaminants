
### SUMMARY STATISTICS PER CONTAMINANT BY COUNTY


# Section 1: Importing data with counties spatially joined ####

library(readxl)

getwd()

# Import the data with the counties. The columns import without data so you will need to merge with another dataset.
GW_Contaminants_Counties <- read_excel("02_Raw_Data/GW_Merged_Counties.xlsx",
                                       col_types = c(rep("guess", 18), "text", rep("guess", 14), "text", rep("guess", 8)))

# Subet to just incldue counties and unique IDs
GW_Contaminants_Counties2 <- GW_Contaminants_Counties |>  select(Unique_ID, COUNTY)

# Import the dataset without counties. This is the final dataset from the 04_GW_Contmainants_Merged Script
GW_Contaminants_NoCounties <- read_excel("02_Raw_Data/GW_Merged_ForGIS.xlsx",
                                         col_types = c(rep("guess", 17), "text", rep("guess", 14), "text", rep("guess", 2)))


# Section 2: Data cleaning

# Merge the two dataset by unique ID
GW_Contaminants_Counties_Merged <- left_join(GW_Contaminants_NoCounties, GW_Contaminants_Counties2, by = "Unique_ID")


# Subset to remove any observations where the COUNTY is na
GW_Contaminants_Counties_Merged1 <- GW_Contaminants_Counties_Merged |> filter(!is.na(COUNTY))


# Create a data frame with all unique analytes
Analytes <- (as.data.frame(unique(GW_Contaminants_Counties_Merged1$Analyte)))

# Export analytes to determine which analytes are spelled the same
library(writexl)
write_xlsx(Analytes, "02_Raw_Data/Analytes/Analytes_Same_Spelling.xlsx")


# After identifying duplicates with different capitalizations, rname analytes
GW_Contaminants_Counties_Merged2 <- GW_Contaminants_Counties_Merged1 %>%
  mutate(
    Analyte = case_when(
      Analyte == "ZINC" ~ "Zinc",
      Analyte == "VANADIUM" ~ "Vanadium",
      Analyte == "TEPH DIESEL RANGE ORGANICS" ~ "TEPH Diesel Range Organics",
      Analyte == "SULFATE" ~ "Sulfate",
      Analyte == "STRONTIUM" ~ "Strontium",
      Analyte == "SILVER" ~ "Silver",
      Analyte == "SELENIUM" ~ "Selenium",
      Analyte == "ANTIMONY" ~ "Antimony",
      Analyte == "PHENOL" ~ "Phenol",
      Analyte == "NITRITE" ~ "Nitrite",
      Analyte == "NITRATE" ~ "Nitrate",
      Analyte == "NICKEL" ~ "Nickel",
      Analyte == "MANGANESE" ~ "Manganese",
      Analyte == "LEAD" ~ "Lead",
      Analyte == "ISOPHORONE" ~ "Isophorone",
      Analyte == "IRON" ~ "Iron",
      Analyte == "FLUORIDE" ~ "Fluoride",
      Analyte == "DICHLOROMETHANE (methylene chloride)" ~ "Dichloromethane (methylene chloride)",
      Analyte == "COPPER" ~ "Copper",
      Analyte == "COBALT" ~ "Cobalt",
      Analyte == "cis-1,2-dichloroethylene" ~ "cis-1,2-Dichloroethylene",
      Analyte == "CHROMIUM VI" ~ "Chromium VI",
      Analyte == "CHROMIUM" ~ "Chromium",
      Analyte == "CADMIUM" ~ "Cadmium",
      Analyte == "BROMIDE" ~ "Bromide",
      Analyte == "BORON" ~ "Boron",
      Analyte == "BERYLLIUM" ~ "Beryllium",
      Analyte == "BARIUM" ~ "Barium",
      Analyte == "ARSENIC" ~ "Arsenic",
      Analyte == "ALUMINUM" ~ "Aluminum",
      Analyte == "2,4,5-TRICHLOROPHENOL" ~ "2,4,5-Trichlorophenol",
      Analyte == "1,2-DICHLOROETHANE" ~ "1,2-Dichloroethane",
      TRUE ~ Analyte # This line keeps all other values in the 'Analyte' column as they are.
    )
  )


# Idnrtifying if the same contaminant has multiple units
analyte_units_table <- GW_Contaminants_Counties_Merged2 %>%
  group_by(Analyte) %>%
  # Use distinct() to get a new data frame with only the unique
  # combinations of 'Analyte' and 'Units'.
  # This effectively shows all the different units used for each analyte.
  distinct(Analyte, Units) %>% 
# Now, we will add a new column to identify analytes with multiple units.
# We first count the number of unique units for each analyte.
 add_count(Analyte, name = "unit_count") %>%
  # Then, we use mutate to create a new logical column.
  mutate(has_multiple_units = unit_count > 1) 



# Export the analyte units table to identify which units to keep and which to transform
write_xlsx(analyte_units_table, "02_Raw_Data/Analytes/analyte_units_table.xlsx")



# Check the units for each contaminant measurement are the same and if not convert them




# Section 2: Summary statistics per contaminant by county ####

Stats_By_County <- GW_Contaminants_Counties1 %>% 
  group_by(Analyte) %>% 
  summarise(
    min = min(Result, na.rm = TRUE),
    q1 = quantile(Result, 0.25, na.rm = TRUE),
    median = median(Result, na.rm = TRUE),
    mean = mean(Result, na.rm = TRUE),
    q3 = quantile(Result, 0.75, na.rm = TRUE),
    max = max(Result, na.rm = TRUE),
    n = n(),
    .groups = 'drop' # This is a good practice to ungroup at the end
  )
