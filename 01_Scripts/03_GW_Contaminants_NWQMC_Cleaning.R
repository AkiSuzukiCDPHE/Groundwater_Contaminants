#### Section 1 Importing the data ####

# Importing data from an excel workbook
library(readxl)
library(dplyr)
library(lubridate)

# Get the original file path
getwd()

# Importing data
NWQMC_Wells<- read_excel("02_Raw_Data/NWQMC_Wells.xlsx",
                          na = c("", "N/A", "NULL"))
# View all variables and types
str(NWQMC_Wells)

# View the c;ass of variables
class(NWQMC_Wells$MonitoringLocationName2)
class(NWQMC_Wells$ActivityEndDate)


# Turn activity end date into a date
NWQMC_Wells$ActivityEndDate <- as.Date(NWQMC_Wells$ActivityEndDate, format = "%m-%d-%Y") 

# Importing data
NWQMC_LatLongs <- read_excel("02_Raw_Data/NWQMC_LatLongs.xlsx")


#### Section 2: Cleaning the data ####


# Remove extraneous variables from the lat longs dataset
NWQMC_LatLongs_1 <- NWQMC_LatLongs |> select(c(MonitoringLocationIdentifier, LatitudeMeasure,LongitudeMeasure))


# Merge the Lat Longs from the Site dataset
NWQMC_Wells_Merged <- left_join(NWQMC_Wells, NWQMC_LatLongs_1, by = "MonitoringLocationIdentifier")

# Create data source identifier
NWQMC_Wells_Merged_1 <- NWQMC_Wells_Merged |> mutate(Source="NWQMC Wells")


# Remove extraneous variables for the wells dataset
NWQMC_Wells_Merged_2 <- NWQMC_Wells_Merged_1|> select(c(OrganizationFormalName, ActivityTypeCode, ActivityMediaSubdivisionName, ActivityEndDate,
                                                        MonitoringLocationIdentifier, MonitoringLocationName, SampleAquifer,HydrologicEvent,
                                                        LatitudeMeasure,LongitudeMeasure, ResultIdentifier,ResultDetectionConditionText, MethodSpeciationName, CharacteristicName, ResultSampleFractionText,
                                                        ResultMeasureValue, ResultStatusIdentifier, ResultValueTypeName,`ResultAnalyticalMethod/MethodDescriptionText`,
                                                        ResultLaboratoryCommentText, DetectionQuantitationLimitTypeName,
                                                        `DetectionQuantitationLimitMeasure/MeasureValue`, `DetectionQuantitationLimitMeasure/MeasureUnitCode`, 
                                                        `ResultMeasure/MeasureUnitCode`, StatisticalBaseCode, ResultTimeBasisText, Source))


# Filter out certain values
NWQMC_Wells_Merged_3 <- NWQMC_Wells_Merged_2 |> 
  filter(
    ActivityTypeCode == "Sample-Routine",
    !ResultSampleFractionText %in% c("Bed Sediment", "Non-filterable", "Settleable"),
    !ResultValueTypeName %in% c("Calculated", "Estimated"),
    !StatisticalBaseCode %in% c("Mean", "Counting Error"),
    !`ResultMeasure/MeasureUnitCode` %in% c(
      "uS/cm @25C",
      "NTU",
      "% saturatn",
      "tons/ac ft",
      "%",
      "tons/day",
      "pct modern",
      "cm3/g STP",
      "ratio",
      "NTRU",
      "cm3/g @STP",
      "None",
      "#/ml",
      "PCU",
      "T.U.",
      "g/mL @ 20C",
      "mm/Hg",
      "years BP",
      "FNU",
      "mg N/l",
      "ft3/s",
      "m3/sec",
      "ft",
      "code"
    ),
    !`DetectionQuantitationLimitMeasure/MeasureUnitCode` %in% c(
      "years BP",
      "tons/ac ft",
      "pct modern",
      "tons/day",
      "NTRU",
      "% saturatn",
      "cm3/g @STP",
      "uS/cm @25C",
      "T.U.",
      "None",
      "m3/sec",
      "ft3/s",
      "%"
    ),
    ResultDetectionConditionText != "Systematic Contamination"|is.na(ResultDetectionConditionText),
    !is.na(ResultMeasureValue)
  )





# for the values below, the value in result column should be the same as the quantification limit

NWQMC_Wells_Merged_4 <- NWQMC_Wells_Merged_3 |>
  mutate(
    ResultMeasureValue = case_when(
      ResultLaboratoryCommentText %in% c(
        "below the detection level",
        "Result below sample specific critical level",
        "see result laboratory commentResult below sample specific critical level"
      ) ~ `DetectionQuantitationLimitMeasure/MeasureValue`,
      TRUE ~ ResultMeasureValue
    )
  )





# Renaming variables
NWQMC_Wells_Merged_5 <- NWQMC_Wells_Merged_4 |> rename(SiteID = MonitoringLocationIdentifier, Media = ActivityMediaSubdivisionName, Date = ActivityEndDate,
                                                Analyte = CharacteristicName, SampleID=ResultIdentifier, Result =ResultMeasureValue, Units=`ResultMeasure/MeasureUnitCode`, Longitude=LongitudeMeasure,
                                                Latitude=LatitudeMeasure, NonDetect=ResultDetectionConditionText)


# if Unit is blank then make unit = detection quantification limit measure
NWQMC_Wells_Merged_6 <- NWQMC_Wells_Merged_5 |>
  mutate(
    Units = case_when(
      is.na(Units) ~ `DetectionQuantitationLimitMeasure/MeasureUnitCode`, # If UnitColumn is NA (blank)
      TRUE ~ Units                                                     # Otherwise, use the value from UnitColumn
    )
  )


# Remove extraneous variables
NWQMC_Wells_Merged_7 <- NWQMC_Wells_Merged_6 |>  select(-c(ResultValueTypeName, MonitoringLocationName,`ResultAnalyticalMethod/MethodDescriptionText`))


# # Making sure there are not duplicates
unique_rows_NWQMC <- NWQMC_Wells_Merged_7 %>%
  group_by(SampleID, Analyte, Date) %>%
  distinct() %>%
  ungroup() # Ungroup if you don't need the grouping for subsequent operations


# Exporting the data 
library(writexl)
write_xlsx(NWQMC_Wells_Merged_7 ,"02_Raw_Data/NWQMC_Wells_Intermediate2.xlsx")

