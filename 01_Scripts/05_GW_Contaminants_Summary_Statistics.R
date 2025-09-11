

# Turn off scientific notation
options(scipen = 999)

  
# Summary statistics per contaminant by county using historical data ####

Stats_By_County_Historical <- GW_Contaminants_Historical%>%
  group_by(COUNTY, Analyte, Units) %>%
  summarise(
    min = min(Result, na.rm = TRUE),
    q1 = quantile(Result, 0.25, na.rm = TRUE),
    median = median(Result, na.rm = TRUE),
    mean = mean(Result, na.rm = TRUE),
    q3 = quantile(Result, 0.75, na.rm = TRUE),
    max = max(Result, na.rm = TRUE),
    n = n(),
    # It counts the number of "Non detect" rows, divides by the total number of rows, and multiplies by 100.
    `Percent Non Detect` = sum((Detection_Status) == "Not Detected", na.rm = TRUE) / n() * 100,
    # The line below calculates the date range for each group.
    # It finds the minimum and maximum Date value and combines them into a single text string.
    `Date Range` = paste(min(Year, na.rm = TRUE), "to", max(Year, na.rm = TRUE)),
    .groups = 'drop' # This is a good practice to ungroup at the end
  )


Stats_By_County_Historical_Export <- Stats_By_County_Historical %>%
  mutate(`Percent Non Detect` = paste0(round(`Percent Non Detect`, 1), "%"))



# Summary statistics per contaminant by county for only samples taken in the last 20 years ####
Stats_By_County_Recent <- GW_Contaminants_Recent %>%
  group_by(COUNTY, Analyte, Units) %>%
  summarise(
    min = min(Result, na.rm = TRUE),
    q1 = quantile(Result, 0.25, na.rm = TRUE),
    median = median(Result, na.rm = TRUE),
    mean = mean(Result, na.rm = TRUE),
    q3 = quantile(Result, 0.75, na.rm = TRUE),
    max = max(Result, na.rm = TRUE),
    n = n(),
    # It counts the number of "Non detect" rows, divides by the total number of rows, and multiplies by 100.
    `Percent Non Detect` = sum((Detection_Status) == "Not Detected", na.rm = TRUE) / n() * 100,
    # The new line below calculates the date range for each group.
    # It finds the minimum and maximum Date value and combines them into a single text string.
    `Date Range` = paste(min(Year, na.rm = TRUE), "to", max(Year, na.rm = TRUE)),
    .groups = 'drop' # This is a good practice to ungroup at the end
  )

Stats_By_County_Recent_Export <- Stats_By_County_Recent %>%
  mutate(`Percent Non Detect` = paste0(round(`Percent Non Detect`, 1), "%"))


#### Export the summary stats ####
library(writexl)

write_xlsx(Stats_By_County_Recent_Export,
           "03_Clean_Data/Stats_By_County_Recent_Export.xlsx")


write_xlsx(Stats_By_County_Historical_Export,
           "03_Clean_Data/Stats_By_County_Historical_Export.xlsx")

