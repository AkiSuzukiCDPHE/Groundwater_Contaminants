#### Section 1 Importing the data ####

# Importing data from an excel workbook
library(readxl)
library(dplyr)

# Get the original file path
getwd()

# Importing data
AWQP_Original <- read_excel("02_Raw_Data/AWQP_Groundwater.xlsx")
AWQP_Sites <- read_excel("02_Raw_Data/AWQP_Groundwater.xlsx", sheet=4)

#### Cleaning the data ####

# Create dataframe to view the different well types
AWQP_WellTypes <- as.data.frame(unique(AWQP_Original$SiteType))

# Remove extraneous variables
AWQP_Original_1 <- AWQP_Original |> select(-c(ResultStatus:ResultNote, QualifierType))


# Merge the Lat Longs from the Site dataset
AWQP_Merged <- left_join(AWQP_Original_1, AWQP_Sites, by = "SiteID")

# Create data source identifier
AWQP_Merged <- AWQP_Merged |> mutate(Source="AWQP Groundwater")

names(AWQP_Merged_1)

# Deleting extraneous variables
AWQP_Merged_1 <- AWQP_Merged |> select(-c(SiteName.x, SiteName.y, DetectFrequency))

#Renaming variables
AWQP_Merged_2 <- AWQP_Merged_1 |>  rename (Media=SiteGroup, PrimaryUsage=UsagePrim, Date=EventDate, Analyte=`Analyte Name`, AnalyteType= `Analyte Type`, NonDetect=BDLIndicator,
                                           Longitude=Lon, Latitude=Lat)

# Changing the category for detect/nondetect into a character variable
AWQP_Merged_2 <- AWQP_Merged_2 |> mutate(NonDetect=case_when(NonDetect== 1 ~ "Not Detected",
                                                             NonDetect== 0 ~ "Detected"))
# Exporting to view the dataset
library(writexl)

write_xlsx(AWQP_Merged_2,"02_Raw_Data/AWQP_Groundwater_Intermediate.xlsx")
