#### Section 1 - Importing the data ####

# Importing data from an excel workbook
library(readxl)
library(dplyr)

# Get the original file path
getwd()

# Importing data
ECMC_Results <- read_excel("02_Raw_Data/ECMC_Wells.xlsx", sheet=1)
ECMC_Samples <- read_excel("02_Raw_Data/ECMC_Wells.xlsx", sheet=2)
ECMC_Locations <- read_excel("02_Raw_Data/ECMC_Wells.xlsx", sheet=3)

#### Section 2 - Cleaning the data ####


# Merge the Lat Longs from other tabs
ECMC_Merged1 <- left_join(ECMC_Results, ECMC_Samples, by = "SampleID")
ECMC_Merged2 <- left_join(ECMC_Merged1, ECMC_Locations, by = "FacilityID")


# Create a new variable for the data source
ECMC_Merged2 <- ECMC_Merged2 |> mutate(Source="ECMC Wells", Media= "Groundwater") |> filter(Matrix=="WATER")

       
# Remove extraneous variables
ECMC_Merged3 <- ECMC_Merged2 |> select(-c(QuarterQuarter:ReceiptNumber, Matrix)) |> rename(SiteType=`Facility Type`, Date=`Sample Date`, Analyte=ParamDescription
                                                                                           ,Result=ResultValue, Longitude=Longitude83, Latitude=Latitude83,  Method= MethodCode, UTM_X=UtmX83, UTM_Y=UtmY83)
                                                                                  

# Exporting the data to look at it
library(writexl)
write_xlsx(ECMC_Merged2,"02_Raw_Data/ECMC_Wells_Intermediate.xlsx")






