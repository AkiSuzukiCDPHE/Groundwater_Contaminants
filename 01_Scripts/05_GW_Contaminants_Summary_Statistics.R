

### SUMMARY STATISTICS PER CONTAMINANT BY COUNTY


# Section 1: Importing data with counties spatially joined ####

library(readxl)
library(dplyr)

getwd()

# Import the data with the counties. The columns import without data so you will need to merge with another dataset.
GW_Contaminants_Counties <- read_excel("02_Raw_Data/GW_Merged_Counties.xlsx",
                                       col_types = c(
                                         rep("guess", 18),
                                         "text",
                                         rep("guess", 14),
                                         "text",
                                         rep("guess", 8)
                                       ))

# Subset to just include counties and unique IDs
GW_Contaminants_Counties2 <- GW_Contaminants_Counties |>  select(Unique_ID, COUNTY)

# Import the dataset without counties. This is the final dataset from the 04_GW_Contmainants_Merged Script
GW_Contaminants_NoCounties <- read_excel("02_Raw_Data/GW_Merged_ForGIS.xlsx",
                                         col_types = c(
                                           rep("guess", 17),
                                           "text",
                                           rep("guess", 14),
                                           "text",
                                           rep("guess", 2)
                                         ))


# Section 2: Data cleaning

# Merge the two dataset by unique ID
GW_Contaminants_Counties_Merged <- left_join(GW_Merged9, GW_Contaminants_Counties2, by = "Unique_ID")



GW_Contaminants_Counties_Merged1 <- GW_Contaminants_Counties_Merged %>%
  mutate(Qualifier = case_when(
    !is.na(Qualifier) & !is.na(NonDetect) ~ paste(Qualifier, NonDetect, sep = " "),
    !is.na(Qualifier) & is.na(NonDetect) ~ Qualifier,
    is.na(Qualifier) & !is.na(NonDetect) ~ NonDetect,
    TRUE ~ NA_character_
  ))


Data_Qualifiers <- as.data.frame(unique(GW_Contaminants_Counties_Merged1$Qualifier))


library(writexl)
# Export the analyte units table to identify which units to keep and which to transform
write_xlsx(Data_Qualifiers,
           "02_Raw_Data/Analytes/Data_Qualifiers.xlsx")



Not_Detected <- c("value extrapolated at low endbelow the detection level",
                  "U T8",
                  "U",
                  "u",
                  "see result laboratory commentResult below sample specific critical level.",
                  "see result field comment",
                  "sample-specific MDC (ssMDC) above contractual MDCResult below sample specific critical level.",
                  "Result below sample specific critical level.",
                  "ND",
                  "below the detection levelsee result laboratory comment",
                  "below the detection level",
                  "0 Not Detected",
                  "0 Detected")

Invalid <- c("sample was warm when received",
            "sample was diluted",
            "sample was warm when receivedbelow the reporting level but at or above the detection level",
            "sample was warm when received",
            "sample was dilutedsee result laboratory commentholding time exceeded",
            "sample was dilutedbelow the reporting level but at or above the detection level",
            "lab control sample (LCS) recovery outside of range or criteria",
            "improper preservationvalue extrapolated at low endbelow the reporting level but at or above the detection level",
            "improper preservationvalue extrapolated at low endbelow the detection level",
            "improper preservationsample was dilutedsee result laboratory comment",
            "improper preservationbelow the reporting level but at or above the detection levelsample was diluted",
            "holding time exceededvalue verified by rerun, same methodsee result laboratory comment",
            "holding time exceededvalue verified by rerun, same methodsample was diluted",
            "holding time exceededvalue verified by rerun, same methodbelow the reporting level but at or above the detection level",
            "holding time exceededsee result laboratory commentvalue verified by rerun, same method",
            "holding time exceededsee result laboratory commentsample was diluted",
            "holding time exceededsee result laboratory commentresult may be affected by interference",
            "holding time exceededsee result laboratory commentbelow the reporting level but at or above the detection level",
            "holding time exceededsee result laboratory comment",
            "holding time exceededsample was dilutedsee result laboratory comment",
            "holding time exceededsample was dilutedbelow the reporting level but at or above the detection level",
            "holding time exceededsample was diluted",
            "holding time exceededbelow the reporting level but at or above the detection levelsee result laboratory comment",
            "holding time exceeded",
            "counts outside acceptable range",
            "count greater than or equal to 15 percent (dominant)",
            "below the reporting level but at or above the detection levelholding time exceeded",
            "sample was dilutedholding time exceeded")

Estimated <- c("value extrapolated at low endbelow the reporting level but at or above the detection levelsample was diluted",
               "value extrapolated at low endbelow the reporting level but at or above the detection level",
               "TR",
               "see result laboratory commentsample was dilutedbelow the reporting level but at or above the detection level",
               "J",
               "below the reporting level but at or above the detection levelsee result laboratory comment",
               "below the reporting level but at or above the detection levelsample was dilutedsee result laboratory comment",
               "below the reporting level but at or above the detection levelsample was diluted",
               "below the reporting level but at or above the detection level")

Contaminated_Blanks <- c("blank greater than the sample-specific Critical Levelsee result laboratory comment",
                         "blank greater than the sample-specific Critical Levelsample-specific MDC (ssMDC) above contractual MDCyield outside of contractually acceptable rangeResult below sample specific critical level.",
                         "blank greater than the sample-specific Critical LevelResult below sample specific critical level.",
                         "blank greater than the sample-specific Critical Level")

Biased <- c("negative result may indicate potential negative biasResult below sample specific critical level.","B", "l")


# Subset to remove any observations where the COUNTY is na
GW_Contaminants_Counties_Merged1 <- GW_Contaminants_Counties_Merged1 |> filter(!is.na(COUNTY)) |> 
  mutate(Detection_Status = case_when(Qualifier %in% Biased ~ "Biased",
                                      Qualifier %in% Not_Detected ~ "Not Detected",
                                      Qualifier %in% Contaminated_Blanks ~ "Contaminated Blanks",)


# Create a data frame with all unique analytes
Analytes <- (as.data.frame(unique(
  GW_Contaminants_Counties_Merged1$Analyte
)))

# Export analytes to determine which analytes are spelled the same
library(writexl)
write_xlsx(Analytes,
           "02_Raw_Data/Analytes/Analytes_Same_Spelling.xlsx")


# After identifying duplicates with different capitalizations, rname analytes
GW_Contaminants_Counties_Merged2 <- GW_Contaminants_Counties_Merged1 %>%
  mutate(
    Analyte = case_when(
      Analyte == "ZINC" ~ "Zinc",
      Analyte == "MERCURY" ~ "Mercury",
      Analyte == "LITHIUM" ~ "Lithium",
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




# Convert the units

`Convert_To_mg/L` <- c(
  "Zinc",
  "TOLUENE",
  "THALLIUM",
  "STYRENE",
  "Strontium",
  "Silver",
  "Selenium",
  "PYRENE",
  "Lead",
  "Iron",
  "INDENO(1,2,3-cd)PYRENE",
  "HEXACHLOROCYCLOPENTADIENE",
  "HEXACHLOROBENZENE",
  "Fluoride",
  "FLUORENE",
  "FLUORANTHENE",
  "ETHYLBENZENE",
  "DIMETHYL PHTHALATE"
)

GW_Contaminants_Counties_Merged3 <- GW_Contaminants_Counties_Merged2 |>
  mutate(
    Result = case_when(
      Analyte %in% `Convert_To_mg/L` &
        Units %in% c("ug/L", "ug/l")  ~ Result * .001,
      TRUE ~ Result  # This is the correct way to handle the default case
    ),
    Units = case_when(
      Analyte %in% `Convert_To_mg/L` &
        Units %in% c("ug/L", "ug/l") ~ "mg/L",
      TRUE ~ Units # This is the correct way to handle the default case
    )
  )



`Convert_To_ug/L` <- c(
  "Xylene",
  "DI-n-BUTYL PHTHALATE",
  "VINYL CHLORIDE",
  "Vanadium",
  "Uranium",
  "Lithium",
  "Mercury",
  "TVPH - Gasoline Range Organics",
  "Trichloroethylene",
  "trans-1,2-Dichloroethylene",
  "THORIUM",
  "Tetrachloroethylene",
  "TEPH Diesel Range Organics",
  "SULFIDE",
  "Sulfate",
  "Phenol",
  "PENTACHLOROPHENOL",
  "N-NITROSODIPHENYLAMINE",
  "N-NITROSO-di-n-PROPYLAMINE",
  "N-NITROSODIMETHYLAMINE",
  "NITROBENZENE",
  "Nitrite",
  "Nitrate",
  "Nickel",
  "NAPHTHALENE",
  "MOLYBDENUM",
  "Manganese",
  "Isophorone",
  "Dichloromethane (methylene chloride)",
  "DIBROMOCHLOROMETHANE",
  "Dibenzo(a,h)anthracene",
  "Copper",
  "Cobalt",
  "cis-1,2-Dichloroethylene",
  "CHRYSENE",
  "Chromium VI",
  "Chromium",
  "CHLOROFORM",
  "CHLOROBENZENE",
  "CHLORIDE",
  "CARBON TETRACHLORIDE",
  "Cadmium",
  "BROMOFORM",
  "BROMODICHLOROMETHANE",
  "Bromide",
  "Bromide",
  "Boron",
  "bis(2-CHLOROETHYL)ETHER",
  "bis(2-CHLOROETHOXY)METHANE",
  "Beryllium",
  "BENZO(k)FLUORANTHENE",
  "BENZO(b)FLUORANTHENE",
  "BENZO(a)PYRENE",
  "Benzo(a)anthracene",
  "BENZENE",
  "Barium",
  "Arsenic",
  "Antimony",
  "ANTHRACENE",
  "Aluminum",
  "ACETONE",
  "ACENAPHTHENE",
  "4-NITROPHENOL",
  "2-HEXANONE",
  "2-CHLOROPHENOL",
  "2,4-DINITROTOLUENE",
  "2,4-DINITROPHENOL",
  "2,4-DIMETHYLPHENOL",
  "1,4-DIOXANE",
  "1,4-DICHLOROBENZENE",
  "1,3-DICHLOROBENZENE",
  "1,3,5-TRIMETHYLBENZENE",
  "1,2-DICHLOROPROPANE",
  "1,2-Dichloroethane",
  "1,2-DICHLOROBENZENE",
  "1,2-DIBROMOETHANE",
  "1,2-DIBROMO-3-CHLOROPROPANE",
  "1,2,4-TRIMETHYLBENZENE",
  "1,2,4-TRICHLOROBENZENE",
  "1,2,3-TRICHLOROPROPANE",
  "1,1-Dichlorocthylene",
  "1,1,2-TRICHLOROETHANE",
  "1,1,2,2-TETRACHLOROETHANE",
  "1,1,1-TRICHLOROETHANE"
)

GW_Contaminants_Counties_Merged4 <- GW_Contaminants_Counties_Merged3 |>
  mutate(
    Result = case_when(
      Analyte %in% `Convert_To_ug/L`  &
        Units %in% c("mg/L", "mg/l") ~ Result / .001,
      TRUE ~ Result  # This is the correct way to handle the default case
    ),
    Units = case_when(
      Analyte %in% `Convert_To_ug/L` &
        Units %in% c("mg/L", "mg/l") ~ "ug/L",
      TRUE ~ Units # This is the correct way to handle the default case
    )
  )


`Convert_ng/L_To_ug/L` <- c(
  "Simazine",
  "Prometon",
  "Metribuzin",
  "Dichlorvos",
  "Chlorpyrifos",
  "Atrazine",
  "Acetochlor",
  "Mercury",
  "2,4-D"
)


GW_Contaminants_Counties_Merged5 <- GW_Contaminants_Counties_Merged4 |>
  mutate(
    Result = case_when(
      Analyte %in% `Convert_ng/L_To_ug/L` &
        Units == "ng/l" ~ Result / 1000,
      TRUE ~ Result  # This is the correct way to handle the default case
    ),
    Units = case_when(
      Analyte %in% `Convert_ng/L_To_ug/L` & Units == "ng/l" ~ "ug/L",
      TRUE ~ Units # This is the correct way to handle the default case
    )
  )


`Convert_pCi/L_To_ug/L` <- c("Uranium", "Alpha particle")

GW_Contaminants_Counties_Merged6 <- GW_Contaminants_Counties_Merged5 |>
  mutate(
    Result = case_when(
      Analyte %in% `Convert_pCi/L_To_ug/L` &
        Units == "pCi/L" ~ Result * 1.5,
      TRUE ~ Result  # This is the correct way to handle the default case
    ),
    Units = case_when(
      Analyte %in% `Convert_pCi/L_To_ug/L` & Units == "pCi/L" ~ "ug/L",
      TRUE ~ Units # This is the correct way to handle the default case
    )
  )


# View all units
unique(GW_Contaminants_Counties_Merged7$Units)

# Clean up units so they are spelled the same
GW_Contaminants_Counties_Merged7 <- GW_Contaminants_Counties_Merged6 |>
  mutate(
    Units = case_when(
      Units %in% c("ug/l as Cr", "ug/l") ~ "ug/L",
      Units %in% c("mg/l NO3", "mg/l", "mg/l as N", "mg/L as N") ~ "mg/L",
      Units == "ng/l" ~ "ng/L",
      Units == "MPN/100 ml" ~ "mpn/100ml",
      TRUE ~ Units
    )
  )


# Remove any rows where county is NA
GW_Contaminants_Counties_Merged8 <- GW_Contaminants_Counties_Merged7 |>  filter(!is.na(COUNTY))

# Check the units for each contaminant measurement are the same and if not convert them
# Identifying if the same contaminant has multiple units
analyte_units_table <- GW_Contaminants_Counties_Merged8 %>%
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

analyte_units_table2 <- analyte_units_table |>  filter (unit_count > 1)

# Export the analyte units table to identify which units to keep and which to transform
write_xlsx(analyte_units_table,
           "02_Raw_Data/Analytes/analyte_units_table.xlsx")

library(lubridate)
# Create a new column called "Year" by extracting just the year from the date column
GW_Contaminants_Counties_Merged9 <- GW_Contaminants_Counties_Merged8 |> mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
Year = year(Date))



# Section 2: 

# Summary statistics per contaminant by county using all data ####

Stats_By_County_All <- GW_Contaminants_Counties_Merged9 %>%
  group_by(COUNTY, Analyte, Units) %>%
  summarise(
    min = min(Result, na.rm = TRUE),
    q1 = quantile(Result, 0.25, na.rm = TRUE),
    median = median(Result, na.rm = TRUE),
    mean = mean(Result, na.rm = TRUE),
    q3 = quantile(Result, 0.75, na.rm = TRUE),
    max = max(Result, na.rm = TRUE),
    n = n(),
    # The new line below calculates the date range for each group.
    # It finds the minimum and maximum Date value and combines them into a single text string.
    `Date Range` = paste(min(Year, na.rm = TRUE), "to", max(Year, na.rm = TRUE)),
    .groups = 'drop' # This is a good practice to ungroup at the end
  )

# Turn off scientific notation
options(scipen = 999)


# Summary statistics per contaminant by county for only samples taken in the last 20 years ####

GW_Contaminants_Counties_RecentData <- GW_Contaminants_Counties_Merged9 |>  filter (Year >= 2005)

Stats_By_County_RecentData <- GW_Contaminants_Counties_RecentData %>%
  group_by(COUNTY, Analyte, Units) %>%
  summarise(
    min = min(Result, na.rm = TRUE),
    q1 = quantile(Result, 0.25, na.rm = TRUE),
    median = median(Result, na.rm = TRUE),
    mean = mean(Result, na.rm = TRUE),
    q3 = quantile(Result, 0.75, na.rm = TRUE),
    max = max(Result, na.rm = TRUE),
    n = n(),
    # The new line below calculates the date range for each group.
    # It finds the minimum and maximum Date value and combines them into a single text string.
    `Date Range` = paste(min(Year, na.rm = TRUE), "to", max(Year, na.rm = TRUE)),
    .groups = 'drop' # This is a good practice to ungroup at the end
  )



