# Section 1 Importing the data ####

# Importing data from an excel workbook
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)



# Get the original file path
getwd()


NWQMC_Wells <- read_excel("02_Raw_Data/NWQMC_Wells.xlsx",
                          col_types = c(rep("guess", 38), "text", rep("guess", 43)))

# # View all variables and types
# str(NWQMC_Wells)


# Turn activity end date into a date
NWQMC_Wells$ActivityEndDate <- as.Date(NWQMC_Wells$ActivityEndDate, format = "%m-%d-%Y")

# Importing location data
NWQMC_LatLongs <- read_excel("02_Raw_Data/NWQMC_LatLongs.xlsx")


# Section 2: Cleaning the data ####


# Remove extraneous variables from the lat longs dataset
NWQMC_LatLongs_1 <- NWQMC_LatLongs |> select(c(
  MonitoringLocationIdentifier,
  LatitudeMeasure,
  LongitudeMeasure
))


# Merge the Lat Longs from the Site dataset
NWQMC_Wells_Merged <- left_join(NWQMC_Wells, NWQMC_LatLongs_1, by = "MonitoringLocationIdentifier")

# Create data source identifier
NWQMC_Wells_Merged_1 <- NWQMC_Wells_Merged |> mutate(Source = "NWQMC Wells")


# Remove extraneous variables for the wells dataset
NWQMC_Wells_Merged_2 <- NWQMC_Wells_Merged_1 |> select(
  c(
    OrganizationFormalName,
    ActivityTypeCode,
    ActivityMediaSubdivisionName,
    ActivityStartDate,
    MonitoringLocationIdentifier,
    MonitoringLocationName,
    SampleAquifer,
    HydrologicEvent,
    LatitudeMeasure,
    LongitudeMeasure,
    ResultIdentifier,
    ResultDetectionConditionText,
    MethodSpeciationName,
    CharacteristicName,
    ResultSampleFractionText,
    ResultMeasureValue,
    ResultStatusIdentifier,
    ResultValueTypeName,
    `ResultAnalyticalMethod/MethodDescriptionText`,
    ResultLaboratoryCommentText,
    DetectionQuantitationLimitTypeName,
    `DetectionQuantitationLimitMeasure/MeasureValue`,
    `DetectionQuantitationLimitMeasure/MeasureUnitCode`,
    `ResultMeasure/MeasureUnitCode`,
    StatisticalBaseCode,
    ResultTimeBasisText,
    Source
  )
)



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
    )
  )


# Filter out values that are NA for result and filter for result condition text equal to certain values
NWQMC_Wells_Merged_4 <- NWQMC_Wells_Merged_3 |> filter(
  ResultDetectionConditionText %in% c(
    "Not Detected",
    "Present Above Quantification Limit",
    "Detected Not Quantified",
    "*Non-detect"
  ) | is.na(ResultDetectionConditionText),
  !is.na(ResultMeasureValue)
)
# )



# for the values below, the value in result column should be the same as the quantification limit
NWQMC_Wells_Merged_5 <- NWQMC_Wells_Merged_4 |>
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
NWQMC_Wells_Merged_6 <- NWQMC_Wells_Merged_5 |> rename(
  SiteID = MonitoringLocationIdentifier,
  Media = ActivityMediaSubdivisionName,
  Date = ActivityStartDate,
  Analyte = CharacteristicName,
  SampleID = ResultIdentifier,
  Result = ResultMeasureValue,
  Units = `ResultMeasure/MeasureUnitCode`,
  Longitude = LongitudeMeasure,
  Latitude = LatitudeMeasure,
  Detection_Limit_Type = DetectionQuantitationLimitTypeName,
  Detection_Limit = `DetectionQuantitationLimitMeasure/MeasureValue`,
  Detection_Limit_Unit = `DetectionQuantitationLimitMeasure/MeasureUnitCode`,
  Sampling_Length = ResultTimeBasisText,
  NonDetect = ResultLaboratoryCommentText
  
)


# if Unit is blank then make unit = detection quantification limit measure
NWQMC_Wells_Merged_7 <- NWQMC_Wells_Merged_6 |>
  mutate(Units = case_when(
    is.na(Units) ~  Detection_Limit_Unit,
    # If UnitColumn is NA (blank)
    TRUE ~ Units# Otherwise, use the value from UnitColumn
  ))


# Remove extraneous variables
NWQMC_Wells_Merged_8 <- NWQMC_Wells_Merged_7 |>  select(
  -c(
    ResultDetectionConditionText,
    ResultValueTypeName,
    MonitoringLocationName,
    `ResultAnalyticalMethod/MethodDescriptionText`
  )
)


# Making sure there are not duplicates
Cleaned_NWQMC <- NWQMC_Wells_Merged_8 %>%
  group_by(SampleID, Analyte, Date) %>%
  distinct() %>%
  ungroup() # Ungroup if you don't need the grouping for subsequent operations

# Cleaning up vairables before merge
Cleaned_NWQMC$Result <- as.numeric(Cleaned_NWQMC$Result)
Cleaned_NWQMC$Detection_Limit <- as.numeric(Cleaned_NWQMC$Detection_Limit)
Cleaned_NWQMC$Date <- as.Date(Cleaned_NWQMC$Date)



# Exporting the data
library(writexl)
write_xlsx(Cleaned_NWQMC, "02_Raw_Data/Cleaned_NWQMC.xlsx")
