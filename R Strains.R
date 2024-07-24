setwd("/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis Tower different strains")
read.csv("/Users/thomas/Library/Mobile Documents/com~apple~CloudDocs/Master Thesis/Thesis/Data")


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
Tower <- read.csv("/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis Tower different strains/Measurements_Experiment.csv", header = T, sep = ",")
file_path <- "/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis Tower different strains/Measurements_Experiment.csv"
data <- read.csv(file_path, header = TRUE, sep = ",", quote = "\"")
file_path2 <- "/Volumes/gr-gnb-share/data/Thomas/Thesis/Analysis/Analysis Tower different strains/Tower_percentage.csv"
Percentage <-read.csv(file_path2, header = TRUE, sep = ",", quote = "\"")

#Preliminary checks 
time(data)
strain(data)
head(data)
summary(data)
Tower1<-as.data.frame(data ,stringsAsFactors=TRUE)

# Replace commas with periods and convert to numeric
data$Straight_height <- as.numeric(gsub(",", ".", data$Straight_height))
data$Pillar_height <- as.numeric(gsub(",", ".", data$Pillar_height))
data$Width <- as.numeric(gsub(",", ".", data$Width))


library(dplyr)

library(dplyr)

data1 <- data %>%
  mutate(Ratio_Height = Straight_height / Pillar_height)

result <- data1 %>%
  group_by(Strain, Plate_ID, Day) %>%
  summarise(max_Ratio_Height = max(Ratio_Height), .groups = "drop") %>%
  group_by(Strain) %>%
  summarise(avg_max_Ratio_Height = mean(max_Ratio_Height))



install.packages("glmnet")

library(dplyr)
library(tidyr)
library(glmnet)

# Assuming 'data' is your dataset and 'Ratio_Height' is the response variable

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

# Prepare the data for modeling
model_data <- data_highest %>%
  select(-Straight_height, -Pillar_height)  # Adjust if needed

# Convert Strain to a factor and set "JT11398" as the reference level
model_data$Strain <- factor(model_data$Strain)
model_data$Strain <- relevel(model_data$Strain, ref = "JT11398")

# Fit a linear regression model
model <- lm(Ratio_Height ~ Strain, data = model_data)

# Print the summary of the model
summary(model)

# Fit linear regression model
model <- lm(Ratio_Height ~ Base_length + Strain, data = data1)

# Check the summary of the model
summary(model)

# Plot scatter plot with regression line
ggplot(data1, aes(x = Base_length, y = Ratio_Height, fill = Strain)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(x = "Base Length", y = "Ratio Height") +  # Add axis labels
  theme_minimal()  # Apply minimal theme
library(dplyr)

# Group by Strain, Plate_ID, and Day, and summarize to get the highest Base_Length and Ratio_Height
data_summary <- data1 %>%
  group_by(Strain, Plate_ID, Day) %>%
  summarize(
    Highest_Base_Length = max(Base_length),
    Highest_Ratio_Height = max(Ratio_Height)
  )

# Plot the highest Base_Length against the highest Ratio_Height
ggplot(data_summary, aes(x = Highest_Base_Length, y = Highest_Ratio_Height, color = Strain)) +
  geom_point() +  # Add points
  labs(x = "Highest Base Length", y = "Highest Ratio Height", color = "Strain") +  # Add axis labels and legend title
  theme_minimal()  # Apply minimal theme

# Plot the highest Base_Length against the highest Ratio_Height with a trend line
ggplot(data_summary, aes(x = Highest_Base_Length, y = Highest_Ratio_Height, color = Strain)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add trend line
  labs(x = "Highest Base Length", y = "Highest Ratio Height", color = "Strain") +  # Add axis labels and legend title
  theme_minimal()  # Apply minimal theme

# Filter the data for the specific strain (e.g., JT11398)
data_summary_strain <- subset(data_summary, Strain == "JT11398")

# Plot the highest Base_Length against the highest Ratio_Height for the specific strain
ggplot(data_summary_strain, aes(x = Highest_Base_Length, y = Highest_Ratio_Height)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add trend line
  labs(x = "Highest Base Length", y = "Highest Ratio Height") +  # Add axis labels
  theme_minimal()  # Apply minimal theme


# Create the dataset with the highest Ratio_Height for each Strain
data_highest <- data1 %>%
  group_by(Strain, Plate_ID, Day) %>%
  arrange(desc(Ratio_Height)) %>%
  slice(1) %>%
  ungroup()

# Create the dataset with the highest Ratio_Height for each Strain
data_highest1 <- data1 %>%
  group_by(Strain, Plate_ID, Day) %>%
  arrange(desc(Straight_height)) %>%
  slice(1) %>%
  ungroup()

# Filter data for the selected strains
selected_strains <- c("JT11398", "MY23", "JU775")
model_data <- data_highest %>%
  filter(Strain %in% selected_strains) %>%
  select(-Straight_height, -Pillar_height)  # Adjust if needed

# Convert Strain to a factor and set "MY23" as the reference level
model_data$Strain <- factor(model_data$Strain)
model_data$Strain <- relevel(model_data$Strain, ref = "MY23")

# Fit a linear regression model
model <- lm(Ratio_Height ~ Strain, data = model_data)

# Print the summary of the model
summary(model)

# Create the dataset with the highest Ratio_Height for each Strain
data_highest3 <- data1 %>%
  group_by(Strain, Plate_ID, Day) %>%
  arrange(desc(Base_length)) %>%
  slice(1) %>%
  ungroup()
    

# Create a grouped violin plot with median and individual data points
plot9 <- ggplot(data_highest, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 5, color = "black", position = position_dodge(width = 0.75)) +  # Increase the size of the mean point (size = 5)
  labs(title = "",
       x = "Strain",
       y = "Max normalized height") +
  theme_minimal() +
  guides(fill = "none")  # Remove the legend for fill

# Show the grouped violin plot with median and individual data points
print(plot9)

# Create a grouped violin plot with median and individual data points
plot9t <- ggplot(data_thighest, aes(x = Strain, y = Titled_height, fill = Strain)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 5, color = "black", position = position_dodge(width = 0.75)) +  # Increase the size of the mean point (size = 5)
  labs(title = "",
       x = "Strain",
       y = "Max Height") +
  theme_minimal() +
  guides(fill = FALSE)  # Remove the legend for fill

# Show the grouped violin plot with median and individual data points
print(plot9t)

# Create a grouped violin plot with median and individual data points
plot10 <- ggplot(data_highest1, aes(x = Strain, y = Straight_height, fill = Strain)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 5, color = "black", position = position_dodge(width = 0.75)) +
  labs(title = "Grouped Violin Plot for Max Straight Height",
       x = "Strain",
       y = "Max Straight Height") +
  theme_minimal() +
  guides(fill = FALSE)  # Remove the legend for fill
  
# Show the grouped violin plot with median and individual data points
print(plot10)


# Create a grouped violin plot with median and individual data points
plot11 <- ggplot(data_highest, aes(x = Strain, y = Base_length, fill = Strain)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 5, color = "black", position = position_dodge(width = 0.75)) +  # Increase the size of the mean point (size = 5)
  labs(title = "Grouped Violin Plot for Max Base Length",
       x = "Strain",
       y = "Max Base length") +
  theme_minimal() +
  guides(fill = FALSE)  # Remove the legend for fill

# Show the grouped violin plot with median and individual data points
print(plot11)

# Create a grouped violin plot with median and individual data points for specific strains
selected_strains <- c("JT11398", "JU258", "JU775")

plotsub <- ggplot(subset(data_highest, Strain %in% selected_strains),
                aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 5, color = "black", position = position_dodge(width = 0.75)) +
  labs(title = "",
       x = "Strain",
       y = "Max normalized height") +
  theme_minimal() +
  theme(
    legend.position = "none"  # Remove legend
  )

# Show the grouped violin plot with median and individual data points for specific strains
print(plotsub)

# Extract the default fill colors from the plot data
default_fill_colors <- levels(plotsub$data[[1]]$fill)

# Get unique levels of the Strain variable
unique_levels <- unique(subset(data_highest, Strain %in% selected_strains)$Strain)

# Print the unique levels and their corresponding colors
for (i in seq_along(unique_levels)) {
  cat("Level:", unique_levels[i], " - Color:", default_fill_colors[i], "\n")
}

plotsub <- ggplot(subset(data_highest, Strain %in% selected_strains),
                  aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 5, color = "black", position = position_dodge(width = 0.75)) +
  labs(title = "",
       x = "Strain",
       y = "Max normalized height") +
  theme_minimal() +
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) +  # Manually specify colors for each strain
  theme(
    legend.position = "none"  # Remove legend
  )

# Show the plot
print(plotsub)




# Create a box plot with median
plot_box <- ggplot(data_highest, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.75)) +
  labs(title = "Box Plot for Max Ratio Height",
       x = "Strain",
       y = "Max Ratio Height") +
  theme_minimal()

# Show the box plot with median and individual data points
print(plot_box)





# Filter out rows with missing Strain
boxplot_data <- data1 %>%
  filter(!is.na(Strain)) %>%
  group_by(Strain) %>%
  summarise(
    Max_Ratio_Height = max(Ratio_Height, na.rm = TRUE),
    Change_Ratio_Height = max(Ratio_Height, na.rm = TRUE) - min(Ratio_Height, na.rm = TRUE),
    Max_Width = max(Width, na.rm = TRUE),
    Change_Width = max(Width, na.rm = TRUE) - min(Width, na.rm = TRUE)
  )

# Reshape data for grouped bar plot
boxplot_data_long <- boxplot_data %>%
  pivot_longer(cols = starts_with("Max_") | starts_with("Change_"), names_to = "Measure", values_to = "Value")

# Create a grouped bar plot
plot7 <- ggplot(boxplot_data_long, aes(x = Strain, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Grouped Bar Plot for Max Ratio Height and Changes",
       x = "Strain",
       y = "Value") +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_minimal()

# Show the grouped bar plot
print(plot7)

# Create a grouped box plot for all data points with the default color palette
plot8 <- ggplot(data1, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Grouped Box Plot for Ratio Height",
       x = "Strain",
       y = "Ratio Height") +
  theme_minimal()

# Show the grouped box plot
print(plot8)

# Create a grouped violin plot with median and individual data points
plot9 <- ggplot(data1_filtered, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +  # Add individual data points with jitter
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.75)) +  # Add median points
  labs(title = "Grouped Violin Plot for Ratio Height",
       x = "Strain",
       y = "Ratio Height") +
  theme_minimal()

# Show the grouped violin plot with median and individual data points
print(plot9)

# Filter data for specific strains
selected_strains <- c("CB4856", "JU258", "JT11398")
filtered_data <- data1_filtered %>% filter(Strain %in% selected_strains)

# Create a grouped box plot for Ratio Height for selected strains
plot_selected_strains <- ggplot(filtered_data, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Grouped Box Plot for Ratio Height (Selected Strains)",
       x = "Strain",
       y = "Ratio Height") +
  theme_minimal()

# Show the grouped box plot for selected strains
print(plot_selected_strains)

# Filter data for specific strains
selected_strains <- c("CB4856", "MY23", "JT11398")
filtered_data <- data1_filtered %>% filter(Strain %in% selected_strains)

# Create a grouped violin plot with median and individual data points for Ratio Height for selected strains
plot_selected_strains <- ggplot(filtered_data, aes(x = Strain, y = Ratio_Height, fill = Strain)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +  # Add individual data points with jitter
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.75)) +  # Add median points
  labs(title = "Grouped Violin Plot for Ratio Height (Selected Strains)",
       x = "Strain",
       y = "Ratio Height") +
  theme_minimal()

# Show the grouped violin plot with median and individual data points for selected strains
print(plot_selected_strains)

# Filter data for specific strains
selected_strains <- c("JT11398","MY23", "CB4856")
filtered_data <- data1 %>% filter(Strain %in% selected_strains)

# Convert 'Strain' to a factor with ordered levels
filtered_data$Strain <- factor(filtered_data$Strain, levels = c("JT11398", "MY23", "CB4856"))

# Fit a linear regression model
model <- lm(Ratio_Height ~ Strain, data = filtered_data)

# Print the summary of the model
summary(model)

# Calculate total combinations for each strain
total_combinations <- data1 %>%
  group_by(Strain) %>%
  summarise(Total = n_distinct(interaction(Day, Plate_ID)))

# Calculate total combinations for all strains together
total_all <- data1 %>%
  summarise(Total = n_distinct(interaction(Day, Plate_ID)))

# Combine the results into one data frame
total_combinations <- bind_rows(total_combinations, total_all)

print(total_combinations)


# Calculate Max_Ratio_Height for each strain
max_ratio_height_data <- data1 %>%
  group_by(Strain) %>%
  summarise(Max_Ratio_Height = max(Ratio_Height, na.rm = TRUE)) %>%
  ungroup()

# Create a grouped box plot for Max_Ratio_Height
plot_max_ratio_height <- ggplot(max_ratio_height_data, aes(x = Strain, y = Max_Ratio_Height, fill = Strain)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Grouped Box Plot for Max Ratio Height",
       x = "Strain",
       y = "Max Ratio Height") +
  theme_minimal()

# Show the grouped box plot for Max Ratio Height
print(plot_max_ratio_height)




total_combinations <- data1 %>%
  group_by(Strain) %>%
  summarise(Total = n_distinct(interaction(Day, Plate_ID)))

print(total_combinations)

# Assuming the data frame is already loaded and named 'Percentage'
# If not, load your data first using read.csv() or other appropriate functions

# Calculate the percentage of strain for each tower
Percentage$Percentage <- (Percentage$Tower / Percentage$Total) * 100

# Define new colors for each strain
new_strain_colors <- c('#d62728', '#ff7f0e', '#bcbd22', '#4CC417', '#33FF57', '#2ca02c',
                       '#17becf', '#4682B4', '#1f77b4', '#9467bd', '#e377c2', '#D70060')

# Set y-axis limits
y_axis_limits <- c(0, 80)

# Create a bar plot with adjusted y-axis limits and new colors
barplot(Percentage$Percentage, names.arg = Percentage$Strain, col = new_strain_colors,
        main = 'Percentage of Tower', xlab = 'Strain', ylab = 'Percentage of Tower',
        ylim = y_axis_limits, cex.names = 0.8)

# Adjust title position
mtext("Percentage of Tower for Each Strain", line = 2, cex = 1.2)




# Add legend to the right of the plot
legend('right', legend = Percentage$Strain, fill = strain_colors, title = 'Strain', cex = 0.8)

# Filter out rows with missing Strain
boxplot_data <- data1 %>%
  filter(Strain %in% c("CB4856", "JU258", "JU775")) %>%
  group_by(Strain) %>%
  summarise(
    Max_Ratio_Height = max(Ratio_Height, na.rm = TRUE),
    Change_Ratio_Height = max(Ratio_Height, na.rm = TRUE) - min(Ratio_Height, na.rm = TRUE),
    Max_Width = max(Width, na.rm = TRUE),
    Change_Width = max(Width, na.rm = TRUE) - min(Width, na.rm = TRUE)
  )

# Specify the order of strains
boxplot_data$Strain <- factor(boxplot_data$Strain, levels = c("CB4856", "JU258", "JU775"))

# Reshape data for grouped bar plot
boxplot_data_long <- boxplot_data %>%
  pivot_longer(cols = starts_with("Max_") | starts_with("Change_"), names_to = "Measure", values_to = "Value")

# Create a grouped bar plot for the selected strains
plot_selected_strains <- ggplot(boxplot_data_long, aes(x = Strain, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Grouped Bar Plot for Max Ratio Height and Changes (Selected Strains)",
       x = "Strain",
       y = "Value") +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_minimal()

# Show the grouped bar plot for the selected strains
print(plot_selected_strains)

# Filter data for specific strains
selected_strains <- c("CB4856", "JU258", "JT11398")
filtered_data <- data1 %>% filter(Strain %in% selected_strains)

# Convert 'Strain' to a factor with ordered levels
filtered_data$Strain <- factor(filtered_data$Strain, levels = c( "CB4856","JU258", "JT11398"))

# Fit a linear regression model
model <- lm(Ratio_Height ~ Strain, data = filtered_data)

# Print the summary of the model
summary(model)

library(performance)

# Assuming your data frame is called 'data1'
boxplot_data <- data1 %>%
  group_by(Strain) %>%
  summarise(
    Max_Height = max(Ratio_Height, na.rm = TRUE),
    Change_Height = last(Ratio_Height, na.rm = TRUE) - first(Ratio_Height, na.rm = TRUE)
  )

# Create a bar plot with an automatically generated color palette
plot7 <- ggplot(boxplot_data, aes(x = Strain, y = Max_Height, fill = Strain)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Plot for Maximum Ratio Height",
       x = "Strain",
       y = "Maximum Ratio Height") +
  scale_fill_discrete() +  # Use an automatic color palette
  theme_minimal()

# Show the bar plot
print(plot7)




# Laden Sie die erforderlichen Bibliotheken
library(dplyr)
library(ggplot2)

average_height <- Tower1 %>%
  group_by(Time, Strain) %>%
  summarise(Average_Straight_Height = mean(Straight_height, na.rm = TRUE), .groups = 'drop')

# Ersetzen von Dezimalkommas durch Dezimalpunkte in der Spalte "Straight_height"
data$Straight_height <- as.numeric(gsub(",", ".", data$Straight_height))

# Berechnen Sie den Durchschnitt der Straight_height für jeden Strain und jede Time
average_height <- data %>%
  group_by(Time, Strain) %>%
  summarise(Average_Straight_Height = mean(Straight_height, na.rm = TRUE))

# Erstellen Sie einen ggplot für alle Strains und verbinden Sie die Punkte
plot1 <- ggplot(average_height, aes(x = Time, y = Average_Straight_Height, color = Strain, group = Strain)) +
  geom_point() +  # Punkte erstellen
  geom_line() +  # Linien zwischen den Punkten erstellen
  labs(x = "Zeit", y = "Durchschnittliche Straight_height") +  # Achsentitel festlegen
  scale_color_discrete(name = "Strain") +  # Legendentitel festlegen
  theme_minimal()  # Wählen Sie einen minimalen Plot-Stil

# Zeigen Sie den Plot an
print(plot1)

# Laden Sie die erforderlichen Bibliotheken
library(dplyr)
library(ggplot2)

# Berechnen Sie den Durchschnitt der Straight_height für jeden Strain und jede Time
average_height <- data %>%
  group_by(Time, Strain) %>%
  summarise(Average_Straight_Height = mean(Straight_height, na.rm = TRUE))

# Filtern Sie den gewünschten Strain (z.B. "CB4856")
selected_strain <- "CB4856"

# Erstellen Sie einen ggplot nur für den ausgewählten Strain
plot <- ggplot(average_height %>% filter(Strain == selected_strain), 
               aes(x = Time, y = Average_Straight_Height)) +
  geom_point() +  # Punkte erstellen
  geom_line() +  # Linien zwischen den Punkten erstellen
  labs(x = "Zeit", y = "Durchschnittliche Straight_height") +  # Achsentitel festlegen
  theme_minimal()  # Wählen Sie einen minimalen Plot-Stil

# Zeigen Sie den Plot an
print(plot)

# Ersetzen von Dezimalkommas durch Dezimalpunkte in der Spalte "Straight_height"
data$Width <- as.numeric(gsub(",", ".", data$Width))

# Berechnen Sie den Durchschnitt der Straight_height für jeden Strain und jede Time
average_width <- data %>%
  group_by(Time, Strain) %>%
  summarise(Average_Width = mean(Width, na.rm = TRUE))

# Erstellen Sie einen ggplot für alle Strains und verbinden Sie die Punkte
plot2 <- ggplot(average_width, aes(x = Time, y = Average_Width, color = Strain, group = Strain)) +
  geom_point() +  # Punkte erstellen
  geom_line() +  # Linien zwischen den Punkten erstellen
  labs(x = "Zeit", y = "Durchschnittliche Width") +  # Achsentitel festlegen
  scale_color_discrete(name = "Strain") +  # Legendentitel festlegen
  theme_minimal()  # Wählen Sie einen minimalen Plot-Stil

# Zeigen Sie den Plot an
print(plot2)

# Laden Sie die erforderlichen Bibliotheken
library(dplyr)
library(ggplot2)

# Berechnen Sie den Durchschnitt der Straight_height für jeden Strain und jede Time
average_height <- data %>%
  group_by(Time, Strain) %>%
  summarise(Average_Straight_Height = mean(Straight_height, na.rm = TRUE))

# Filtern Sie den gewünschten Strain (z.B. "CB4856")
selected_strain <- "CB4856"

# Erstellen Sie einen ggplot nur für den ausgewählten Strain
plot <- ggplot(average_height %>% filter(Strain == selected_strain), 
               aes(x = Time, y = Average_Straight_Height)) +
  geom_point() +  # Punkte erstellen
  geom_line() +  # Linien zwischen den Punkten erstellen
  labs(x = "Zeit", y = "Durchschnittliche Straight_height") +  # Achsentitel festlegen
  theme_minimal()  # Wählen Sie einen minimalen Plot-Stil

# Zeigen Sie den Plot an
print(plot)

# Laden Sie die erforderlichen Bibliotheken
library(dplyr)
library(ggplot2)

# Berechnen Sie den Durchschnitt der Straight_height für jeden Strain und jede Time
average_height <- data %>%
  group_by(Time, Strain) %>%
  summarise(Average_Straight_Height = mean(Straight_height, na.rm = TRUE))

# Ersetzen von Dezimalkommas durch Dezimalpunkte in der Spalte "Width"
data$Width <- as.numeric(gsub(",", ".", data$Width))

# Berechnen Sie den Durchschnitt der Width für jeden Strain und jede Time
average_width <- data %>%
  group_by(Time, Strain) %>%
  summarise(Average_Width = mean(Width, na.rm = TRUE))

# Erstellen Sie einen ggplot, der beide Durchschnittswerte in einem Plot anzeigt
plot_combined <- ggplot() +
  geom_line(data = average_height, aes(x = Time, y = Average_Straight_Height, color = Strain, group = Strain)) +
  geom_point(data = average_height, aes(x = Time, y = Average_Straight_Height, color = Strain)) +
  geom_line(data = average_width, aes(x = Time, y = Average_Width, color = Strain, group = Strain)) +
  geom_point(data = average_width, aes(x = Time, y = Average_Width, color = Strain)) +
  labs(x = "Zeit", y = "Durchschnittliche Höhe und Breite", title = "Durchschnittliche Höhe und Breite nach Strain und Zeit") +
  scale_color_discrete(name = "Strain") +
  theme_minimal()

# Zeigen Sie den kombinierten Plot an
print(plot_combined)

# Load the necessary libraries
library(dplyr)

# Calculate the maximum height and width for each strain
max_height <- average_height %>%
  group_by(Strain) %>%
  summarise(
    Max_Height = max(Average_Straight_Height),
    Initial_Height = first(Average_Straight_Height),
    Change_Height = last(Average_Straight_Height) - first(Average_Straight_Height)
  )

max_width <- average_width %>%
  group_by(Strain) %>%
  summarise(
    Max_Width = max(Average_Width),
    Initial_Width = first(Average_Width),
    Change_Width = last(Average_Width) - first(Average_Width)
  )

# View the results
max_height
max_width

# Example ANOVA test for height
anova_result <- aov(Average_Straight_Height ~ Strain, data = average_height)
summary(anova_result)

# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(ggsignif)  # For adding significance indicators

# Example data
data <- data.frame(
  Strain = rep(c("Strain1", "Strain2", "Strain3"), each = 20),
  Height = rnorm(60)
)

# Perform ANOVA
anova_result <- aov(Height ~ Strain, data = data)

# Perform ANOVA for height
anova_height <- aov(Average_Straight_Height ~ Strain, data = average_height)

# Perform ANOVA for width
anova_width <- aov(Average_Width ~ Strain, data = average_width)

# Load the necessary libraries
library(ggplot2)
library(ggsignif)

# Create a function to generate a plot with significance indicators
create_plot_with_signif <- function(data, response, title) {
  # Create the boxplot
  boxplot <- ggplot(data, aes(x = Strain, y = {{ response }}, fill = Strain)) +
    geom_boxplot() +
    labs(title = title) +
    theme_minimal()
  
  # Add significance indicators
  boxplot_with_signif <- boxplot +
    geom_signif(comparisons = list(c("CB4856", "JT1198"), c("CB4856", "LKC34")),
                map_signif_level = TRUE) +
    theme(legend.position = "none")  # Remove the legend
  
  return(boxplot_with_signif)
}

# Create plots with significance indicators for height and width
plot_height <- create_plot_with_signif(average_height, Average_Straight_Height, "Height by Strain")
plot_width <- create_plot_with_signif(average_width, Average_Width, "Width by Strain")

# Show the graphs
print(plot_height)
print(plot_width)

# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Calculate the maximum height and width for each strain
max_height <- average_height %>%
  group_by(Strain) %>%
  summarise(
    Max_Height = max(Average_Straight_Height),
    Initial_Height = first(Average_Straight_Height),
    Change_Height = last(Average_Straight_Height) - first(Average_Straight_Height)
  )

max_width <- average_width %>%
  group_by(Strain) %>%
  summarise(
    Max_Width = max(Average_Width),
    Initial_Width = first(Average_Width),
    Change_Width = last(Average_Width) - first(Average_Width)
  )

# Combine the data into a single data frame
combined_data <- full_join(max_height, max_width, by = "Strain")

# Create boxplots
boxplot_data <- combined_data %>%
  pivot_longer(
    cols = starts_with("Max_") | starts_with("Change_"),
    names_to = "Measure",
    values_to = "Value"
  )

# Create boxplots
plot4 <- ggplot(boxplot_data, aes(x = Strain, y = Value, fill = Measure)) +
  geom_boxplot() +
  labs(title = "Boxplots for Maximum and Change in Height/Width",
       x = "Strain",
       y = "Value") +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_minimal()

# Show the boxplots
print(plot4)

# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Calculate the maximum height and width for each strain
max_height <- average_height %>%
  group_by(Strain) %>%
  summarise(
    Max_Height = max(Average_Straight_Height),
    Initial_Height = first(Average_Straight_Height),
    Change_Height = last(Average_Straight_Height) - first(Average_Straight_Height)
  )

max_width <- average_width %>%
  group_by(Strain) %>%
  summarise(
    Max_Width = max(Average_Width),
    Initial_Width = first(Average_Width),
    Change_Width = last(Average_Width) - first(Average_Width)
  )

# Combine the data into a single data frame
combined_data <- full_join(max_height, max_width, by = "Strain")

# Prepare the data for plotting
barplot_data <- combined_data %>%
  pivot_longer(
    cols = starts_with("Max_") | starts_with("Change_"),
    names_to = "Measure",
    values_to = "Value"
  )

# Create a bar chart
plot5 <- ggplot(barplot_data, aes(x = Strain, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Bar Chart for Maximum and Change in Height/Width",
       x = "Strain",
       y = "Value") +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_minimal()

# Show the bar chart
print(plot5)

# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Calculate the maximum height and width for each strain
max_height <- average_height %>%
  group_by(Strain) %>%
  summarise(
    Max_Height = max(Average_Straight_Height),
    Initial_Height = first(Average_Straight_Height),
    Change_Height = last(Average_Straight_Height) - first(Average_Straight_Height)
  )

max_width <- average_width %>%
  group_by(Strain) %>%
  summarise(
    Max_Width = max(Average_Width),
    Initial_Width = first(Average_Width),
    Change_Width = last(Average_Width) - first(Average_Width)
  )

# Combine the data into a single data frame
combined_data2 <- full_join(max_ratio_height, max_width, by = "Strain")
combined_data2 <- full_join(combined_data2, max_height, by = "Strain")

# Now, calculate Change_Ratio_Height and Change_Width for combined_data2
combined_data2 <- combined_data2 %>%
  mutate(
    Change_Ratio_Height = Max_Ratio_Height - Initial_Height,
    Change_Width = Max_Width - Initial_Width
  )

# Remove rows with missing Strain
combined_data2 <- combined_data2 %>% filter(!is.na(Strain))

# Ensure Change_Ratio_Height is positive
combined_data2$Change_Ratio_Height <- abs(combined_data2$Change_Ratio_Height)

# Filter the data to include only CB4856, EG4725, and JU258 strains
selected_strains2 <- c("CB4856", "JU258", "JU775")
filtered_data2 <- combined_data2 %>%
  filter(Strain %in% selected_strains2)

# Prepare the data for plotting
barplot_data2 <- filtered_data2 %>%
  pivot_longer(
    cols = c("Max_Ratio_Height", "Change_Ratio_Height", "Max_Width", "Change_Width"),
    names_to = "Measure",
    values_to = "Value"
  )

# Create a bar chart
plot_filtered2 <- ggplot(barplot_data2, aes(x = Strain, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Bar Chart for Maximum and Change in Width and Max Ratio Height",
       x = "Strain",
       y = "Value") +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_minimal()

# Show the filtered bar chart
print(plot_filtered2)


# Reorder the Strain factor levels to change the position
barplot_data2$Strain <- factor(barplot_data2$Strain, levels = c("CX11314", "DL238", "JT11398"))

# Create a bar chart with reordered positions
plot_reordered2 <- ggplot(barplot_data2, aes(x = Strain, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Bar Chart for Maximum and Change in Height/Width",
       x = "Strain",
       y = "Value") +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_minimal()

# Show the reordered bar chart
print(plot_reordered2)

model <- lm(Ratio_Height ~ Strain, data=data1)
# Choose the highest value within each group
model <- data1 %>%
  group_by(Strain, Plate_ID, Day) %>%
  arrange(desc(Ratio_Height)) %>%
  slice(1) %>%
  ungroup()
summary(model)

# Assuming your data frame is called 'data1'
# Fit a linear regression model
model <- lm(Ratio_Height ~ Strain, data = data1_filtered)

# Print the summary of the model
summary(model)

# Convert Strain to a factor if it's not already
data1_filtered$Strain <- as.factor(data1_filtered$Strain)

# Change the reference level to Strain "JT11398"
data1_filtered$Strain <- relevel(data1_filtered$Strain, ref = "JT11398")

# Fit a linear regression model with the new reference level
model <- lm(Ratio_Height ~ Strain, data = data1_filtered)

# Print the summary of the model
summary(model)
 

