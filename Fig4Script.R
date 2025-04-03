# Load necessary libraries
install.packages("ineq")
library(dplyr)
library(ggplot2)
library(ineq)  # For Gini coefficient calculation
library(readr)  # For read_csv()

# Load the data using read_csv from readr package
data <- read_csv("1-s2.0-S0278416523000685-mmc1.csv")

# Rename columns by replacing spaces with underscores
colnames(data) <- gsub(" ", "_", colnames(data))

# Filter the data: Only tiles with at least two structures, and structure areas between 20 and 275 square meters
data <- data |>
  filter(Sample_Size_Structure.x >= 2, 
         Mean_Structure_Area >= 20, 
         Mean_Structure_Area <= 275)

# Filter data to only columns needed and rename columns to something more clear
mmc1 <- data |>
  select(
    Unique_Identifier, 
    Tile, 
    Mean_Structure_Area, 
    Gini_Corrected = Corrected_Gini_Structure_Area, 
    Sample_Size = Sample_Size_Structure.x, 
    Lower_Gini = Lower_Gini_Structure_Area, 
    Upper_Gini = Higher_Gini_Structure_Area
  )


# Supplemental material provied corrected Gini values and formula used to get corrected values, Gc = G * (n/n-1)
# Formula was flipped to get original uncorrected and verified on lab data where we had both corrected and uncorrected (personal communication John Waldon 2025)
# G = Gc(n-1/n)
Gini_Uncorrected <- ((mmc1$Gini_Corrected)*((mmc1$Sample_Size-1)/mmc1$Sample_Size))

mmc1 <- mmc1 |>
  mutate(Gini_Uncorrected)

# Calculate the difference between corrected and uncorrected Gini coefficients
gini_difference <- (mmc1$Gini_Corrected - mmc1$Gini_Uncorrected)

# Calculate the average between corrected and uncorrected Gini coefficients
gini_mean <- ((mmc1$Gini_Corrected + mmc1$Gini_Uncorrected)/2)

mmc1 <- mmc1 |>
  mutate(gini_difference, gini_mean)
         
# Plot the difference vs. the number of structures per tile
ggplot(mmc1, aes(x = Sample_Size, y = gini_difference)) +
  geom_point(alpha = 0.5) +  # Scatter plot with some transparency for better visualization
  geom_smooth(method = "loess", se = TRUE, color = "black") +  # Add a smooth line to visualize the trend
  labs(
    x = "Number of Structures per Tile",
    y = "Corrected - Uncorrected Gini Difference",
    title = "Comparison of Corrected and Uncorrected Gini Coefficients"
  ) +
  theme_minimal()  # Apply a clean theme to the plot


# Calculate the difference between corrected and uncorrected Gini coefficients
UpperLower <- (mmc1$Upper_Gini - mmc1$Lower_Gini)

# Add new values to data
mmc1 <- mmc1 |>
  mutate(UpperLower)

# Plot the difference vs. the number of structures per tile
ggplot(mmc1, aes(x = Sample_Size, y = UpperLower)) +
  geom_point(alpha = 0.5) +  # Scatter plot with some transparency for better visualization
  geom_smooth(method = "loess", se = TRUE, color = "black") +  # Add a smooth line to visualize the trend
  labs(
    x = "Number of Structures per Tile",
    y = "Upper - Lower Gini",
    title = "Comparison of Upper and Lower Gini Coefficients"
  ) +
  theme_minimal()  # Apply a clean theme to the plot
