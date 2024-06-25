

#Main Code 

setwd("C:/Users/User/Downloads")

library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
library(leaflet)
library(readxl)

disaster_df <- read_excel("public_emdat_custom_request_2024-06-20_690755cd-a477-474c-837f-2ec0319ee68d.xlsx")
View(disaster_df)

str(disaster_df)

sum(is.na(disaster_df))

summary(disaster_df)

sapply(disaster_df, function(x) sum(is.na(x)))

# Remove rows with any missing values
cleaned_data <- na.omit(disaster_df)


library(tidyr)

# Handling missing values for numeric and categorical columns
disaster_df <- disaster_df %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%  # Fill numeric with median
  mutate(across(where(is.character), ~replace_na(., "Unknown")))  # Fill character with "Unknown"

# For Date columns, let's assume you have already created StartDate and EndDate.
# If there are NAs in StartDate or EndDate, we might leave them as is or decide based on further context.

# Optionally, if removing rows with any remaining NAs is desired
disaster_df <- na.omit(disaster_df)

# Recheck the dataset structure to confirm changes
str(disaster_df)



#1


disaster_summary <- disaster_df %>%
  count(`Disaster Group`) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(disaster_summary, aes(x = `Disaster Group`, y = Percentage, fill = `Disaster Group`)) +
  geom_bar(stat = "identity", width = 0.9) +  # Adjust width as needed
  scale_fill_manual(values = c("Natural" = "red", "Technological" = "turquoise")) +
  labs(title = "Percentage of Disaster Group",
       x = "Region",
       y = "Percentage") +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5)  # Adding percentage labels

#2

disaster_summary <- disaster_df %>%
  count(`Disaster Subgroup`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Plotting the data
ggplot(disaster_summary, aes(x = `Disaster Subgroup`, y = Percentage, fill = `Disaster Subgroup`)) +
  geom_bar(stat = "identity", width = 0.5) +  # Adjust width as needed
  scale_fill_brewer(palette = "Set3") +  # Color palette for clarity
  labs(title = "Percentage of Disaster Subgroup",
       x = "Subgroup",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x labels for better readability
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5))  # Adding percentage labels inside bars

#3

region_summary <- disaster_df %>%
  count(Region) %>%
  mutate(Percentage = n / sum(n) * 100)

# Plotting the data
ggplot(region_summary, aes(x = Region, y = Percentage, fill = Region)) +
  geom_bar(stat = "identity", width = 0.9) +  # Adjust width as needed
  scale_fill_brewer(palette = "Paired") +  # Color palette for clarity
  labs(title = "Percentage of Disasters by Region",
       x = "Region",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x labels for better readability
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5))  # Adding percentage labels inside bars



#4

library(dplyr)
library(ggplot2)


yearly_disasters <- disaster_df %>%
  group_by(`Start Year`) %>%
  summarise(Number_of_Disasters = n())  

ggplot(yearly_disasters, aes(x = `Start Year`, y = Number_of_Disasters)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
  labs(title = "Disaster Over Time",
       x = "Year",
       y = "Number of Disasters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Analysis for Hypothesis 


library(dplyr)
library(readxl)



# Calculate the number of disasters per country within each continent
disasters_per_country_continent <- disaster_df %>%
  group_by(Country, Region) %>%
  summarise(Disasters = n(), .groups = 'drop')

# Calculate the standard deviation of the number of disasters per continent
continent_disaster_stats <- disasters_per_country_continent %>%
  group_by(Region) %>%
  summarise(
    Total_Countries = n_distinct(Country),
    Total_Disasters = sum(Disasters),
    Mean_Disasters = mean(Disasters),
    Median_Disasters = median(Disasters),
    Std_Disasters = sd(Disasters, na.rm = TRUE),
    IQR_Disasters = IQR(Disasters, na.rm = TRUE),
    Min_Disasters = min(Disasters), 
    Max_Disasters = max(Disasters), 
    .groups = 'drop'
  )

# Print the continent disaster statistics
print(continent_disaster_stats)

cpi_stats_by_continent <- disaster_df %>%
  group_by(Region) %>%
  summarise(
    Total_CPI = sum(CPI, na.rm = TRUE),
    Mean_CPI = mean(CPI, na.rm = TRUE),
    Std_CPI = sd(CPI, na.rm = TRUE),
    Median_CPI = median(CPI, na.rm = TRUE),
    IQR_CPI = IQR(CPI, na.rm = TRUE),
    Min_CPI = min(CPI, na.rm = TRUE),
    Max_CPI = max(CPI, na.rm = TRUE),
    .groups = 'drop'
  )

# Print the results
print(cpi_stats_by_continent)


library(dplyr)
library(ggplot2)
library(readxl)



# Filter the data for Asia and calculate the number of disasters per country within Asia
disasters_in_asia <- disaster_df %>%
  filter(Region == "Asia") %>%
  group_by(Country) %>%
  summarise(Total_Disasters = n(), .groups = 'drop') %>%
  arrange(Total_Disasters) %>%
  mutate(Country = factor(Country, levels = Country))  # Reorder factor levels to match the order of Total_Disasters

# Create a bar plot for the distribution of disasters by country in Asia
ggplot(disasters_in_asia, aes(x = Country, y = Total_Disasters, fill = Country)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Distribution of Disasters by Country in Asia",
       x = "Country",
       y = "Total Number of Disasters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


library(writexl)
asian_countries_data <- disaster_df %>%
  filter(Region == "Asia") %>%
  select(Country, `Disaster Subgroup`) %>%
  arrange(Country)

# Write the data to a new Excel file
write_xlsx(asian_countries_data, path = "AsianCountriesDisasters.xlsx")

# Output the path for confirmation
print("Data exported to AsianCountriesDisasters.xlsx successfully!")


#Main Graph used for proving/rejecting hypothesis 

asian_disasters_summary <- disaster_df %>%
  filter(Region == "Asia") %>%
  group_by(Country, `Disaster Subgroup`) %>%
  summarise(Total_Disasters = n(), .groups = 'drop') %>%
  ungroup() %>%
  arrange(Country, `Disaster Subgroup`)

# Sum disasters across subgroups for ordering
country_order <- asian_disasters_summary %>%
  group_by(Country) %>%
  summarise(Total_Disasters_Sum = sum(Total_Disasters), .groups = 'drop') %>%
  arrange(Total_Disasters_Sum) %>%
  pull(Country)

# Update the country factor levels for plotting in ascending order
asian_disasters_summary$Country <- factor(asian_disasters_summary$Country, levels = country_order)

# Create a bar plot with ggplot2, now with ordered countries
ggplot(asian_disasters_summary, aes(x = Country, y = Total_Disasters, fill = `Disaster Subgroup`)) +
  geom_bar(stat = "identity", position = position_stack(), color = "black") +
  labs(title = "Distribution of Disasters by Country and Subgroup in Asia",
       x = "Country",
       y = "Total Number of Disasters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))




#Another Graph that used to for Hypothesis (CPI)

library(dplyr)
library(ggplot2)
library(readxl)



# Filter the data for Asia and summarize the average CPI by year
cpi_over_time_asia <- disaster_df %>%
  filter(Region == "Asia") %>%
  group_by(`Start Year`) %>%
  summarise(Average_CPI = mean(CPI, na.rm = TRUE))  # Calculate the average CPI per year

# Create a plot of CPI over time for Asia
ggplot(cpi_over_time_asia, aes(x = `Start Year`, y = Average_CPI)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +  # Trend line with linear model
  labs(title = "CPI Over Time in Asia",
       x = "Year",
       y = "Average CPI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



library(dplyr)
library(ggplot2)
library(readxl)

# Assuming the data frame 'disaster_df' is already loaded
# disaster_df <- read_excel("path_to_your_file.xlsx")  # Uncomment this if you need to load the file

# Filter the data for Asia and summarize the total number of disasters by year
disasters_over_time_asia <- disaster_df %>%
  filter(Region == "Asia") %>%
  group_by(`Start Year`) %>%
  summarise(Total_Disasters = n(), .groups = 'drop')  # Calculate the total number of disasters per year

# Create a plot of disasters over time for Asia
ggplot(disasters_over_time_asia, aes(x = `Start Year`, y = Total_Disasters)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +  # Trend line with linear model
  labs(title = "Disasters Over Time in Asia",
       x = "Year",
       y = "Total Number of Disasters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability






library(dplyr)
library(ggplot2)
library(readxl)



# Filter the data to include only 'Natural' and 'Technological' disaster groups
disaster_groups_over_time <- disaster_df %>%
  filter(`Disaster Group` %in% c("Natural", "Technological")) %>%
  group_by(`Start Year`, `Disaster Group`) %>%
  summarise(Total_Disasters = n(), .groups = 'drop')

# Create a line graph for the trend of natural vs technological disasters over time
ggplot(disaster_groups_over_time, aes(x = `Start Year`, y = Total_Disasters, color = `Disaster Group`)) +
  geom_line() +  # Line plot
  geom_point() +  # Add points to the lines
  labs(title = "Trend of Natural vs Technological Disasters Over Time",
       x = "Year",
       y = "Total Number of Disasters") +
  scale_color_manual(values = c("Natural" = "green", "Technological" = "red")) +  # Customize line colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability





library(dplyr)
library(readxl)



# Filter for climatological disasters and group by period
climatological_disasters_counts <- disaster_df %>%
  filter(`Disaster Subgroup` == "Climatological",  # Ensure the column name matches your data structure
         between(`Start Year`, 1980, 2019)) %>%  # Filter years from 1980 to 2019
  mutate(Period = case_when(
    `Start Year` <= 1999 ~ "1980-1999",
    `Start Year` > 1999 ~ "2000-2019"
  )) %>%
  group_by(Period) %>%
  summarise(Total_Climatological_Disasters = n(), .groups = 'drop')

# Print the results
print(climatological_disasters_counts)





#Percentage of Disaster Subgroup by category 
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Assuming disaster_df is already loaded

# Modify the disaster summary calculation to include a category for coloring
disaster_summary <- disaster_df %>%
  mutate(Category = case_when(
    `Disaster Subgroup` %in% c("Geophysical", "Meteorological", "Hydrological", "Climatological", "Biological") ~ "Natural",
    `Disaster Subgroup` %in% c("Industrial accident", "Transport", "Miscellaneous accident") ~ "Technological",
    TRUE ~ "Other"  # Categorize any other types as 'Other'
  )) %>%
  count(`Disaster Subgroup`, Category) %>%
  mutate(Percentage = n / sum(n) * 100)

# Plotting the data with category-based coloring
ggplot(disaster_summary, aes(x = `Disaster Subgroup`, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 0.5) +  # Adjust width as needed
  scale_fill_manual(values = c("Natural" = "green", "Technological" = "red")) +  # Custom colors for categories
  labs(title = "Percentage of Disaster Subgroup by Category",
       x = "Subgroup",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x labels for better readability
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5))  # Adding percentage labels


