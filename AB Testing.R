# Load necessary libraries
library(ggplot2)
library(ggpubr)  
library(stats)

# Importing the dataset
Data <- read.csv("C:/Users/Mus/Downloads/website_ab_test.csv")
head(Data)

# Counting missing values
nbr_missing_values <- colSums(is.na(Data))
print(nbr_missing_values)
# Comment: The dataset has no missing values.

# Display the structure of the dataset
str(Data)

# Show the dataset's dimensions
dim(Data)

# Data types of columns
sapply(Data, class)

# Descriptive statistics
summary(Data)   

# Create a scatter plot for Click Through Rate (CTR) vs Conversion Rate
ggplot(Data, aes(x = Click.Through.Rate, y = Conversion.Rate, color = Theme)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, aes(group = Theme), linetype = "dashed") +  
  labs(title = "CTR vs Conversion Rate") +  
  theme_minimal()

# Extract data for each theme
light_theme_data <- Data %>% filter(Theme == 'Light Theme')
dark_theme_data <- Data %>% filter(Theme == 'Dark Theme')

# Combine the data for the two themes into a single dataframe
combined_data <- bind_rows(
  light_theme_data %>% mutate(Theme = 'Light Theme'),
  dark_theme_data %>% mutate(Theme = 'Dark Theme')
)

# Create a grouped bar chart for Click Through Rate by theme
ggplot(combined_data, aes(x = Click.Through.Rate, fill = Theme)) +
  geom_histogram(position = "dodge", binwidth = 0.01, alpha = 0.6) +
  labs(title = 'Click Through Rate by Theme', x = 'Click Through Rate', y = 'Frequency') +
  scale_fill_manual(values = c('Light Theme' = 'blue', 'Dark Theme' = 'red')) +
  theme_minimal()


# Create a grouped bar chart for Conversion Rate by theme
ggplot(combined_data, aes(x = Conversion.Rate, fill = Theme)) +
  geom_histogram(position = "dodge", binwidth = 0.01, alpha = 0.6) +
  labs(title = 'Conversion Rate by Theme', x = 'Conversion Rate', y = 'Frequency') +
  scale_fill_manual(values = c('Light Theme' = 'blue', 'Dark Theme' = 'red')) +
  theme_minimal()


# Boxplot for Bounce Rate by Theme
ggplot(Data, aes(x = Theme, y = Bounce.Rate, fill = Theme)) +
  geom_boxplot() +
  labs(title = 'Bounce Rate Distribution by Theme', x = 'Theme', y = 'Bounce Rate') +
  scale_fill_manual(values = c('Light Theme' = 'blue', 'Dark Theme' = 'red')) +
  theme_minimal()


# Boxplot for Scroll Depth by Theme
ggplot(Data, aes(x = Theme, y = Scroll_Depth, fill = Theme)) +
  geom_boxplot() +
  labs(title = 'Scroll Depth Distribution by Theme', x = 'Theme', y = 'Scroll Depth') +
  scale_fill_manual(values = c('Light Theme' = 'blue', 'Dark Theme' = 'red')) +
  theme_minimal()

ggplot(Data, aes(x = Scroll_Depth, y = Session_Duration, color = Purchases)) +
  geom_point() +
  labs(title = 'Scroll Depth vs Session Duration by Purchases', x = 'Scroll Depth', y = 'Session Duration') +
  scale_color_manual(values = c('No' = 'red', 'Yes' = 'green')) +
  theme_minimal()

#### A/B testing for Purchases ####

# Calculate conversion counts and sample sizes
light_theme_conversions <- sum(light_theme_data$Purchases == 'Yes')
light_theme_total <- nrow(light_theme_data)

dark_theme_conversions <- sum(dark_theme_data$Purchases == 'Yes')
dark_theme_total <- nrow(dark_theme_data)

# Calculate conversion rates
light_theme_conversion_rate <- light_theme_conversions / light_theme_total
dark_theme_conversion_rate <- dark_theme_conversions / dark_theme_total

# Print conversion rates
cat("Light Theme Conversion Rate:", light_theme_conversion_rate, "\n")
cat("Dark Theme Conversion Rate:", dark_theme_conversion_rate, "\n")

# Perform two-sample proportion test
# Proportions test for two independent samples
test_result <- prop.test(x = c(light_theme_conversions, dark_theme_conversions),
                         n = c(light_theme_total, dark_theme_total))

# Print test results
cat("A/B Testing - Chi-squared Test Statistic:", test_result$statistic, "\n")
cat("p-value:", test_result$p.value, "\n")

#### A/B testing for Session Duration ####

# Extract session duration for each theme
light_theme_session_duration <- light_theme_data$Session_Duration
dark_theme_session_duration <- dark_theme_data$Session_Duration

# Calculate the average session duration for both themes
light_theme_avg_duration <- mean(light_theme_session_duration, na.rm = TRUE)
dark_theme_avg_duration <- mean(dark_theme_session_duration, na.rm = TRUE)

# Print the average session duration for both themes
cat("Light Theme Average Session Duration:", light_theme_avg_duration, "\n")
cat("Dark Theme Average Session Duration:", dark_theme_avg_duration, "\n")

# Perform two-sample t-test for session duration
t_test_result <- t.test(light_theme_session_duration, dark_theme_session_duration)

# Print the t-test results
cat("A/B Testing for Session Duration - t-statistic:", t_test_result$statistic, "\n")
cat("p-value:", t_test_result$p.value, "\n")