

# Summary statistics per contaminant by county using all data ####

Stats_By_County_All <- GW_Counties_Merged9 %>%
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

GW_Counties_RecentData <- GW_Counties_Merged9 |>  filter (Year >= 2005)

Stats_By_County_RecentData <- GW_Counties_RecentData %>%
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



