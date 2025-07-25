#### Section 1 Importing the data ####

# Importing data from an excel workbook
library(readxl)
library(dplyr)

# Get the original file path
getwd()

# Importing data
NWQMC_Wells <- read_excel("02_Raw_Data/NWQMC_Wells.xlsx")

# Importing data
NWQMC_LatLongs <- read_excel("02_Raw_Data/NWQMC_LatLongs.xlsx")


#### Cleaning the data ####


# Function to identify columns where every row is NA

identify_all_na_columns <- function(df) {
  
  # Initialize an empty vector to store names of columns that are all NA
  all_na_cols <- character(0)
  
  # Loop through each column in the data frame
  for (col_name in names(df)) {
    current_col <- df[[col_name]] # Get the current column as a vector
    
    # Check if all values in the current column are NA
    # is.na() returns TRUE for NA, FALSE otherwise.
    # all() checks if all elements in the logical vector are TRUE.
    if (all(is.na(current_col))) {
      all_na_cols <- c(all_na_cols, col_name)
    }
  }
  
  return(all_na_cols)
}


# Identify all columns in the dataset where every row is NA
na_columns_found <- identify_all_na_columns(NWQMC_Wells)


unique(na_columns_found)

# Remove these columns from the data frame ---

if (length(na_columns_found) > 0) {
  NWQMC_Wells_1 <- NWQMC_Wells %>%
    dplyr::select(-dplyr::all_of(na_columns_found)) # Use all_of() to select columns by name vector
  

}



# Remove extraneous variables from the lat longs dataset
NWQMC_LatLongs_1 <- NWQMC_LatLongs |> select(c(MonitoringLocationIdentifier, LatitudeMeasure,LongitudeMeasure))


# Merge the Lat Longs from the Site dataset
NWQMC_Wells_Merged <- left_join(NWQMC_Wells_1, NWQMC_LatLongs_1, by = "MonitoringLocationIdentifier")

# Create data source identifier
NWQMC_Wells_Merged <- NWQMC_Wells_Merged |> mutate(Source="NWQMC Wells")

# View all columns in the dataset 
names(NWQMC_Wells_Merged_1)

# Remove extraneous variables for the wells dataset
NWQMC_Wells_Merged_1 <- NWQMC_Wells_Merged|> select(c(OrganizationFormalName, ActivityTypeCode, ActivityMediaSubdivisionName, 
                                                      ActivityStartDate, `ActivityDepthHeightMeasure/MeasureValue`, `ActivityDepthHeightMeasure/MeasureUnitCode`, 
                                                      MonitoringLocationIdentifier, SampleAquifer, ResultIdentifier, ResultDetectionConditionText, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, 
                                                      `ResultMeasure/MeasureUnitCode`, ResultStatusIdentifier, StatisticalBaseCode, ResultValueTypeName, ResultTimeBasisText, 
                                                      `ResultAnalyticalMethod/MethodName`, DetectionQuantitationLimitTypeName, `DetectionQuantitationLimitMeasure/MeasureValue`, 
                                                      `DetectionQuantitationLimitMeasure/MeasureUnitCode`, LatitudeMeasure, LongitudeMeasure, Source))


# Exporting the data to look at it
library(writexl)
write_xlsx(NWQMC_Wells_Merged_1 ,"02_Raw_Data/NWQMC_Wells_Intermediate.xlsx")

# Renaming variables
NWQMC_Wells_Merged_2 <- NWQMC_Wells_Merged_1 |> rename(SiteID =MonitoringLocationIdentifier, Media = ActivityMediaSubdivisionName, Date = ActivityStartDate,
                                                Analyte = CharacteristicName, Result =ResultMeasureValue, Units=`ResultMeasure/MeasureUnitCode`, Longitude=LongitudeMeasure,
                                                Latitude=LatitudeMeasure, Method=`ResultAnalyticalMethod/MethodName`, NonDetect=ResultDetectionConditionText)

