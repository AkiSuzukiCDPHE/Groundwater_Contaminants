#### Section 1: Importing the data ####

# Importing data from an excel workbook
library(readxl)
library(dplyr)

# Get the original file path
getwd()

# Importing data

AWQP_Original <- read_excel("02_Raw_Data/AWQP_Groundwater.xlsx")

AWQP_Original <- read_excel("02_Raw_Data/AWQP_Groundwater.xlsx",
                            col_types = c(rep("guess", 12), "text", rep("guess", 4)))


AWQP_Sites <- read_excel("02_Raw_Data/AWQP_Groundwater.xlsx", sheet = 4)

str(AWQP_Original)




#### Section 2: Cleaning the data ####

# Filter variables
AWQP_Original_1 <- AWQP_Original |> filter(
  !SiteType %in% c("(PZ) Piezometer", "(SD) Spring Discharge"),
  ResultStatus == "Final",
  Units %in% c("mg/L", "ug/L")
)

# Remove extraneous variables
AWQP_Original_2 <- AWQP_Original_1 |> select(-c(ResultNote, ResultStatus))


# Merge the Lat Longs from the Site dataset
AWQP_Merged <- left_join(AWQP_Original_2, AWQP_Sites, by = "SiteID")

# Create data source identifier
AWQP_Merged <- AWQP_Merged |> mutate(Source = "AWQP Groundwater")


# Deleting extraneous variables
AWQP_Merged_1 <- AWQP_Merged |> select(-SiteName.y)

#Renaming variables
AWQP_Merged_2 <- AWQP_Merged_1 |>  rename (
  Media = SiteGroup,
  SiteName = SiteName.x,
  PrimaryUsage = UsagePrim,
  Date = EventDate,
  Analyte = `Analyte Name`,
  AnalyteType = `Analyte Type`,
  NonDetect = BDLIndicator,
  Longitude = Lon,
  Latitude = Lat
)

# Changing the category for detect/nondetect into a character variable
AWQP_Merged_2 <- AWQP_Merged_2 |> mutate(NonDetect = case_when(NonDetect == 1 ~ "Not Detected", NonDetect == 0 ~ "Detected")) |>
  mutate(Qualifier = case_when(Qualifier == 0 ~ NA | Qualifier == 1 ~ U, TRUE ~ Qualifier))


# Making sure there are not duplicates
Cleaned_AWQP <- AWQP_Merged_2 %>%
  group_by(SiteID) %>%
  distinct() %>%
  ungroup() # Ungroup if you don't need the grouping for subsequent operations




# # Exporting to view the dataset
library(writexl)
write_xlsx(Cleaned_AWQP, "02_Raw_Data/Cleaned_AWQP.xlsx")
