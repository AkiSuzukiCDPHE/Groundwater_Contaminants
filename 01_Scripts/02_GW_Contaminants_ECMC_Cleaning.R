#### Section 1 - Importing the data ####

# Importing data from an excel workbook
library(readxl)
library(dplyr)

# Get the original file path
getwd()

# Importing data
ECMC_Results <- read_excel("02_Raw_Data/ECMC_Wells.xlsx", sheet = 1)
ECMC_Samples <- read_excel("02_Raw_Data/ECMC_Wells.xlsx", sheet = 2)
ECMC_Locations <- read_excel("02_Raw_Data/ECMC_Wells.xlsx", sheet = 3)

#### Section 2 - Cleaning the data ####


# Merge the Lat Longs from other tabs
ECMC_Merged1 <- left_join(ECMC_Results, ECMC_Samples, by = "SampleID")
ECMC_Merged2 <- left_join(ECMC_Merged1, ECMC_Locations, by = "FacilityID")


# Create a new variable for the data source
ECMC_Merged3 <- ECMC_Merged2 |> mutate(Source = "ECMC Wells", Media = "Groundwater")


unique(ECMC_Merged3$Qualifier)


# Filtering out various values
ECMC_Merged3 <- ECMC_Merged2 |> filter(
  FractionType != "WW",
  Matrix != c("GAS", "SOIL"),
  `Facility Type` != c(
    "Cistern",
    "Creek",
    "Ditch",
    "Pond",
    "River",
    "Seep",
    "Spring",
    "Surface Water",
    "Tank"
  ),
  # add in qualifier and unit filtering criteria
!grepl( c("<", ">"), Qualifier),
    Units == c(
      "ug/L",
      "ug/Kg",
      "ng/L",
      "mg/L as N",
      "mg/L as CaCO3",
      "mg/L",
      "mg/Kg",
      "g/L",
      "CFU/100mL",
      "CFU/100ml",
      "TU",
      "ppm",
      "pCi/L",
      "mpn/100ml",
      "mg P/L",
      "cfu/ml",
      "CFU"
    ))
    

# Removes values where result is NA
ECMC_Merged4 <- ECMC_Merged3 |> filter(!is.na(ResultValue))

# Remove extraneous variables and rename variables
ECMC_Merged5 <- ECMC_Merged4 |> select(
  -c(
    UtmX83,
    UtmY83,
    Township,
    Section,
    `Sample Reason`,
    ReceiptNumber,
    Range,
    QuarterQuarter,
    MethodCode,
    Meridian,
    Matrix,
    FractionType
  )
) |>
  rename(
    SiteType = `Facility Type`,
    Date = `Sample Date`,
    Analyte = ParamDescription ,
    Result = ResultValue,
    Longitude = Longitude83,
    Latitude = Latitude83,
    Detection_limit=DetectionLimit
  )


# Making sure there are not duplicates
unique_rows_ECMC <- ECMC_Merged5 %>%
  group_by(SampleID, Analyte, Date) %>%
  distinct() %>%
  ungroup() # Ungroup if you don't need the grouping for subsequent operations



# # # Exporting the data to look at it
library(writexl)
write_xlsx(unique_rows_ECMC,"02_Raw_Data/ECMC_Wells_CHECK.xlsx")
