
#  Section 1 - Merging ####

# Viewing the variables and data types

str(ECMC_Merged3)

str(AWQP_Merged_2)

str(NWQMC_Wells_Merged_2)

# Converting datatypes so they match for the merge
  
AWQP_Merged_2$Qualifier <- as.character(AWQP_Merged_2$Qualifier)
AWQP_Merged_2$Date <- as.Date(AWQP_Merged_2$Date)

NWQMC_Wells_Merged_2$Result <- as.numeric(NWQMC_Wells_Merged_2$Result)
NWQMC_Wells_Merged_2$Date <- as.Date(NWQMC_Wells_Merged_2$Date)

# Merging using bind_rows
GW_Merged <- bind_rows(ECMC_Merged3, AWQP_Merged_2)


GW_Merged2 <- bind_rows (GW_Merged,NWQMC_Wells_Merged_2)

# Reordering columns
GW_Merged3 <- GW_Merged2 |>  select(Source,
                                    Media,
                                    SiteType,
                                    MonitoringRegion,
                                    PrimaryUsage,
                                    Date,
                                    Analyte,
                                    AnalyteType,
                                    Result,
                                    Qualifier,
                                    Units,
                                    NonDetect,
                                    DetectionLimit,
                                    Longitude,
                                    Latitude,
                                    SiteID,
                                    everything())


#  Section 2 - Summarizing the Data #### 

GW_Merged3 <- GW_Merged3 |>  mutate 


DF <- DF |>
  # 1. Group the data by both 'Analyte' and 'County'
  group_by(Analyte, County) |>
  # 2. Calculate the desired summary statistics for each group
  summarise(
    Mean_Value = mean(YourMeasurementColumn, na.rm = TRUE),
    Median_Value = median(YourMeasurementColumn, na.rm = TRUE),
    Min_Value = min(YourMeasurementColumn, na.rm = TRUE),
    Max_Value = max(YourMeasurementColumn, na.rm = TRUE),
    SD_Value = sd(YourMeasurementColumn, na.rm = TRUE),
    N_Observations = n(), # Count the number of observations in each group
    .groups = 'drop' # This removes the grouping structure after summarising
  )