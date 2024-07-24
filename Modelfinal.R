setwd("/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis mixed Towers")
#Load Libraries
library(car)
library(MASS)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)
library(data.table)
library(Hmisc)
library(chron)
library(lubridate)
library(rstatix)

#Load data file 
Towermix <- read.csv("/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis mixed Towers/Experiments_mixed_populations.csv", header = T, sep = ",")
file_path <- "/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis mixed Towers/Experiments_mixed_populations.csv"
datamix <- read.csv(file_path, header = TRUE, sep = ",", quote = "\"")
file_path2 <- "/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis mixed Towers/Tower_percenatge.csv"

Percentagemix <-read.csv(file_path2, header = TRUE, sep = ",", quote = "\"")
TowerProportions <- read.csv("/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis mixed Towers/Tower_proportions.csv", header = T, sep = ",")
file_path <- "/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis mixed Towers/Tower_proportions.csv"


# Replace commas with periods and convert to numeric
datamix$Straight_height <- as.numeric(gsub(",", ".", data$Straight_height))
datamix$Pillar_height <- as.numeric(gsub(",", ".", data$Pillar_height))
datamix$Width <- as.numeric(gsub(",", ".", data$Width))

data1mix <- datamix %>%
  mutate(Ratio_Height = Straight_height / Pillar_height)

resultmix <- data1mix %>%
  group_by(Strain, Plate_ID, Day) %>%
  summarise(max_Ratio_Height = max(Ratio_Height), .groups = "drop") %>%
  group_by(Strain) %>%
  summarise(avg_max_Ratio_Height = mean(max_Ratio_Height))

data_theightmix <- datamix

result2mix <- data_theightmix %>%
  group_by(Strain, Plate_ID, Day) %>%
  arrange(desc(Titled_height)) %>%
  slice(1) %>%
  summarise(max_Titled_height = max(Titled_height)) %>%
  group_by(Strain) %>%
  summarise(avg_max_Titled_height = mean(max_Titled_height))

# Create the dataset with the highest Ratio_Height for each Strain
data_highestmix <- data1mix %>%
  group_by(Strain, Plate_ID, Day) %>%
  arrange(desc(Ratio_Height)) %>%
  slice(1) %>%
  ungroup()

data_thighestmix <- data_theightmix %>%
  group_by(Strain, Plate_ID, Day) %>%
  arrange(desc(Titled_height)) %>%
  slice(1) %>%
  ungroup()

# Create a grouped boxplot with median and individual data points
plot9 <- ggplot(data_highestmix, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +  # Boxplot without outliers
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 5, color = "black", position = position_dodge(width = 0.75)) +
  labs(title = "",
       x = "Combination",
       y = "Max normalized height") +
  scale_fill_manual(values = c("A+B" = "#E67E22", "A+C" = "#8E44AD")) +
  theme_minimal() +
  guides(fill = "none")  # Remove the legend for fill

# Show the grouped boxplot with median and individual data points
print(plot9)


Filmed <- merge(data_highestmix, TowerProportions, by=c("Day","Plate_ID","Strain","GFP","RFP"), all.x=TRUE)

FilmedYes <- na.omit(Filmed)

# Create a grouped boxplot with median and individual data points
plot10 <- ggplot(FilmedYes, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +  # Boxplot without outliers
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 5, color = "black", position = position_dodge(width = 0.75)) +
  labs(title = "",
       x = "Combination",
       y = "Max normalized height") +
  scale_fill_manual(values = c("A+B" = "#E67E22", "A+C" = "#8E44AD")) +
  theme_minimal() +
  guides(fill = "none")  # Remove the legend for fill

# Show the grouped boxplot with median and individual data points
print(plot10)


setwd("/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis Tower different strains")
read.csv("/Users/thomas/Library/Mobile Documents/com~apple~CloudDocs/Master Thesis/Thesis/Data")
Tower <- read.csv("/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis Tower different strains/Measurements_Experiment.csv", header = T, sep = ",")
file_path <- "/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis Tower different strains/Measurements_Experiment.csv"
data <- read.csv(file_path, header = TRUE, sep = ",", quote = "\"")
file_path2 <- "/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis Tower different strains/Tower_percentage.csv"
Percentage <-read.csv(file_path2, header = TRUE, sep = ",", quote = "\"")
# Replace commas with periods and convert to numeric
data$Straight_height <- as.numeric(gsub(",", ".", data$Straight_height))
data$Pillar_height <- as.numeric(gsub(",", ".", data$Pillar_height))
data$Width <- as.numeric(gsub(",", ".", data$Width))
data1 <- data %>%
  mutate(Ratio_Height = Straight_height / Pillar_height)

result <- data1 %>%
  group_by(Strain, Plate_ID, Day) %>%
  summarise(max_Ratio_Height = max(Ratio_Height), .groups = "drop") %>%
  group_by(Strain) %>%
  summarise(avg_max_Ratio_Height = mean(max_Ratio_Height))
data_theight <- data

result2 <- data_theight %>%
  group_by(Strain, Plate_ID, Day) %>%
  arrange(desc(Titled_height)) %>%
  slice(1) %>%
  summarise(max_Titled_height = max(Titled_height)) %>%
  group_by(Strain) %>%
  summarise(avg_max_Titled_height = mean(max_Titled_height))

# Create the dataset with the highest Ratio_Height for each Strain
data_highest <- data1 %>%
  group_by(Strain, Plate_ID, Day) %>%
  arrange(desc(Ratio_Height)) %>%
  slice(1) %>%
  ungroup()

data_thighest <- data_theight %>%
  group_by(Strain, Plate_ID, Day) %>%
  arrange(desc(Titled_height)) %>%
  slice(1) %>%
  ungroup()

# Filter data_highest for strains JT11398 and JU775
filtered_strains <- data_highest %>%
  filter(Strain %in% c("JT11398", "JU775", "JU258"))

# Combine the filtered datasets
combined_data <- bind_rows(filtered_strains, FilmedYes)

# Create the box plot separately
plot_combined_all_box <- ggplot(combined_data, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot(width = 0.2, outlier.shape = NA, color = "black") +  # Box plot without outliers
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 5, color = "black", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("JT11398" = "#F8766D", "JU775" = "#619CFF", "A+C" = "#8E44AD", "JU258" = "#00BA38", "A+B" = "#E67E22"), guide = "none") +
  labs(title = "",
       x = NULL,
       y = "Max normalized height") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the box plot
print(plot_combined_all_box)

# Fit a linear regression model including the combined dataset
model <- lm(Ratio_Height ~ Strain, data = combined_data)

# Print the summary of the model
summary(model)

library(ggplot2)
library(patchwork)

# Assuming 'FilmedYes' contains data for A+B and A+C under certain conditions
# 'Filmed' contains data for A+B and A+C under other conditions

# Filter the datasets to include only A+B and A+C strains
FilmedYes_A_B_C <- FilmedYes %>% filter(Strain %in% c("A+B", "A+C"))
Filmed_A_B_C <- Filmed %>% filter(Strain %in% c("A+B", "A+C"))

# Create box plots for A+B and A+C from FilmedYes
plot_filmedYes <- ggplot(FilmedYes_A_B_C, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  labs(title = "FilmedYes",
       x = NULL,
       y = "Max normalized height") +
  scale_fill_manual(values = c("A+B" = "#E67E22", "A+C" = "#8E44AD")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create box plots for A+B and A+C from Filmed
plot_filmed <- ggplot(Filmed_A_B_C, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  labs(title = "Filmed",
       x = NULL,
       y = "Max normalized height") +
  scale_fill_manual(values = c("A+B" = "#E67E22", "A+C" = "#8E44AD")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arrange the plots side by side using patchwork
combined_plots <- plot_filmedYes | plot_filmed

# Display the combined plots
print(combined_plots)

# number 1 having all plots next to each other the 3 controls, all mixed and then only the filmed mixed
# Define y-axis limits
lower_limit <- 0
upper_limit <- 2

# Create box plots for filtered strains
plot_filtered_strains <- ggplot(filtered_strains, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  labs(title = "Control",
       x = NULL,
       y = "Max normalized height") +
  scale_fill_manual(values = c("JT11398" = "#F8766D", "JU258" = "#00BA38", "JU775" = "#619CFF")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +  # Remove legend
  coord_cartesian(ylim = c(lower_limit, upper_limit))  # Set y-axis limits

# Create box plots for A+B and A+C from Filmed
plot_filmed <- ggplot(Filmed_A_B_C, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  labs(title = "All",
       x = NULL,
       y = "") +  # Empty y-axis label
  scale_fill_manual(values = c("A+B" = "#E67E22", "A+C" = "#8E44AD")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +  # Remove legend
  coord_cartesian(ylim = c(lower_limit, upper_limit))  # Set y-axis limits

# Create box plots for A+B and A+C from FilmedYes
plot_filmedYes <- ggplot(FilmedYes_A_B_C, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  labs(title = "Filmed",
       x = NULL,
       y = "") +  # Empty y-axis label
  scale_fill_manual(values = c("A+B" = "#E67E22", "A+C" = "#8E44AD")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +  # Remove legend
  coord_cartesian(ylim = c(lower_limit, upper_limit))  # Set y-axis limits

# Combine the plots in the desired order
combined_plots <- plot_filtered_strains | plot_filmed | plot_filmedYes

# Display the combined plots
print(combined_plots)


# Combine the filtered datasets
combined_data_all <- bind_rows(combined_data, FilmedYes)

# Fit a linear regression model including the combined dataset
model <- lm(Ratio_Height ~ Strain, data = combined_data_all)

# Print the summary of the model
summary(model)

# Convert Strain to factor
combined_data_all$Strain <- factor(combined_data_all$Strain)

# Relevel the Strain factor making 'A+C' the reference level
combined_data_all$Strain <- relevel(combined_data_all$Strain, ref = "A+C")

# Fit a linear regression model with the re-leveled Strain factor
model_custom_intercept <- lm(Ratio_Height ~ Strain, data = combined_data_all)

# Print the summary of the model with the custom intercept
summary(model_custom_intercept)

# number 2 how does the tower looks like mixed or one strain is alone in the tower

# Filter the data for Combination A+C and only the wholeTower component
A_B_wholeTower_filtered <- TowerProportions %>%
  filter(Strain == "A+B" & wholeTower != "") %>%
  select(Plate_ID, wholeTower)

# Define colors for A and Mix
colors <- c("A" = "#F8766D","B" = "#00BA38",  "Mix" = "#E67E22")

# Plot the bar chart
ggplot(A_B_wholeTower_filtered, aes(x = wholeTower, fill = wholeTower)) +
  geom_bar() +
  scale_fill_manual(values = colors) +  # Set colors for A and Mix
  labs(title = "Frequency of wholeTower Component in Combination A+B",
       x = "wholeTower",
       y = "Frequency") +
  theme_minimal()

# Filter the data for Combination A+C and only the wholeTower component
A_C_wholeTower_filtered <- TowerProportions %>%
  filter(Strain == "A+C" & wholeTower != "") %>%
  select(Plate_ID, wholeTower)

# Define colors for A and Mix
colors <- c("A" = "#F8766D", "C" = "#619CFF","Mix" = "#8E44AD")

# Plot the bar chart
ggplot(A_C_wholeTower_filtered, aes(x = wholeTower, fill = wholeTower)) +
  geom_bar() +
  scale_fill_manual(values = colors) +  # Set colors for A and Mix
  labs(title = "Frequency of wholeTower Component in Combination A+C",
       x = "wholeTower",
       y = "Frequency") +
  theme_minimal()

#number3 looking at the performance when one strain took over the whole tower 
library(ggplot2)

# Filter the FilmedYes data for strain A+B
FilmedYes_A_B <- FilmedYes_A_B_C %>% filter(Strain == "A+B")

# Create box plots for strain A+B separated by wholeTower values
plot_filmedYes_A_B <- ggplot(FilmedYes_A_B, aes(x = wholeTower, y = Ratio_Height, fill = wholeTower)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  labs(title = "FilmedYes - Strain A+B",
       x = "Tower",
       y = "Max normalized height") +
  scale_fill_manual(values = c("A" = "#F8766D", "B" = "#00BA38", "Mix" = "#E67E22")) +  # Assign colors for different tower values
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the box plots
print(plot_filmedYes_A_B)

library(ggplot2)

# Filter the FilmedYes data for strain A+C
FilmedYes_A_C <- FilmedYes_A_B_C %>% filter(Strain == "A+C")

# Create box plots for strain A+C separated by wholeTower values
plot_filmedYes_A_C <- ggplot(FilmedYes_A_C, aes(x = wholeTower, y = Ratio_Height, fill = wholeTower)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  labs(title = "FilmedYes - Strain A+C",
       x = "Tower",
       y = "Max normalized height") +
  scale_fill_manual(values = c("A" = "#F8766D", "C" = "#619CFF", "Mix" = "#8E44AD")) +  # Assign colors for different tower values
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the box plots
print(plot_filmedYes_A_C)


# number 4 how does the distribution looks like for base tower and total looks like

# Calculate mean percentages for each column for all combinations
mean_percentages <- TowerProportions %>%
  group_by(Strain) %>%
  summarise(
    Mean_PercentTotA = mean(PercentTotA),
    Mean_PercentTotBC = mean(PercentTotBC),
    Mean_PercentBA = mean(PercentBA),
    Mean_PercentBBC = mean(PercentBBC),
    Mean_PercentTA = mean(PercentTA),
    Mean_PercentTBC = mean(PercentTBC),
    Mean_PercentAinTower = mean(PercentAinTower),
    Mean_PercentBCinTower = mean(PercentBCinTower),
    Mean_PercentAinBase = mean(PercentAinBase),
    Mean_PercentBCinBase = mean(PercentBCinBase)
  )

# Print the result
print(mean_percentages)

# Assuming 'mean_percentages' contains the calculated mean percentages
# Melt the data frame to long format for easier plotting
mean_percentages_long <- tidyr::pivot_longer(mean_percentages, 
                                             cols = starts_with("Mean_Percent"), 
                                             names_to = "Percentage", 
                                             values_to = "Mean_Value")


# Filter the data for Mean_PercentTotA and Mean_PercentTotBC
filtered_percentages <- mean_percentages_long %>%
  filter(Percentage %in% c("Mean_PercentTotA", "Mean_PercentTotBC"))

# Define custom legend labels
legend_labels <- c("Mean_PercentTotA" = "A", "Mean_PercentTotBC" = "B/C")

# Plotting bar plot
ggplot(filtered_percentages, aes(x = Strain, y = Mean_Value, fill = Percentage)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Percentages of Total A and Total BC by Combination",
       x = "Combination",
       y = "Mean Value",
       fill = "Percentage") +
  scale_fill_manual(name = "Legend", values = c("Mean_PercentTotA" = "blue", "Mean_PercentTotBC" = "red"), labels = legend_labels) +  # Set custom legend labels and colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filter the data for Mean_PercentTotA and Mean_PercentTotBC
filtered_percentagesTower <- mean_percentages_long %>%
  filter(Percentage %in% c("Mean_PercentTA", "Mean_PercentTBC"))

# Define custom legend labels
legend_labels <- c("Mean_PercentTA" = "A", "Mean_PercentTBC" = "B/C")
# Plotting bar plot
ggplot(filtered_percentagesTower, aes(x = Strain, y = Mean_Value, fill = Percentage)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Percentages of Total A and Total BC by Combination",
       x = "Combination",
       y = "Mean Value",
       fill = "Percentage") +
  scale_fill_manual(name = "Legend", values = c("Mean_PercentTA" = "blue", "Mean_PercentTBC" = "red"), labels = legend_labels) +  # Set custom legend labels and colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filter the data for Mean_PercentTotA and Mean_PercentTotBC
filtered_percentagesBase <- mean_percentages_long %>%
  filter(Percentage %in% c("Mean_PercentBA", "Mean_PercentBBC"))

# Define custom legend labels
legend_labels <- c("Mean_PercentBA" = "A", "Mean_PercentBBC" = "B/C")
# Plotting bar plot
ggplot(filtered_percentagesBase, aes(x = Strain, y = Mean_Value, fill = Percentage)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Percentages of Total A and Total BC by Combination",
       x = "Combination",
       y = "Mean Value",
       fill = "Percentage") +
  scale_fill_manual(name = "Legend", values = c("Mean_PercentBA" = "blue", "Mean_PercentBBC" = "red"), labels = legend_labels) +  # Set custom legend labels and colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

