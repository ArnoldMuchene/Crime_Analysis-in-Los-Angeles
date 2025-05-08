### STAT5230 Final Project ###

# Loading packages
pacman::p_load(readxl,dplyr,lubridate, car, stats, reshape2, factoextra, corrplot, rstatix, stringr, bestNormalize, tidyverse, caret, GGally, MVN, skimr, magrittr, biotools)

# Load the dataset
crime <- read.csv("Crime_Data_2020_to_Present.csv")

# Select specific columns and parse dates/times
selected_data <- crime %>%
  dplyr::select(Date.Rptd, TIME.OCC, AREA, Crm.Cd.Desc) %>%
  mutate(
    # Parse Date.Rptd and convert to ISO date format (YYYY-MM-DD)
    Date.Rptd = as.Date(parse_date_time(Date.Rptd, orders = "m/d/Y H:M"), format = "%Y-%m-%d"),
    # Convert TIME.OCC to a 4-digit numeric string (handling any NAs)
    TIME.OCC = sprintf("%04d", as.numeric(as.character(TIME.OCC)))
  ) %>%
  # Convert TIME.OCC to numeric and create time categories
  mutate(
    time_numeric = as.numeric(TIME.OCC),
    time_category = case_when(
      time_numeric >= 0 & time_numeric < 600 ~ "Morning (00:00-06:00)",
      time_numeric >= 600 & time_numeric < 1200 ~ "Before_noon (06:00-12:00)",
      time_numeric >= 1200 & time_numeric < 1800 ~ "Afternoon (12:00-18:00)",
      time_numeric >= 1800 & time_numeric <= 2359 ~ "Night (18:00-23:59)",
      TRUE ~ "Unknown"
    ),
    # Simplify the time labels
    time = case_when(
      str_detect(time_category, "Morning") ~ "Morning",
      str_detect(time_category, "Before_noon") ~ "Before_noon",
      str_detect(time_category, "Afternoon") ~ "Afternoon",
      str_detect(time_category, "Night") ~ "Night",
      TRUE ~ "Unknown"
    )
  )

# Now, filter the dataset, create crime category indicators, and summarize by year, time, and AREA
filtered_data <- selected_data %>%
  filter(!is.na(Date.Rptd) & year(Date.Rptd) != 2025) %>%
  mutate(
    vehicletheft = ifelse(str_detect(Crm.Cd.Desc, regex("VEHICLE - STOLEN", ignore_case = TRUE)), 1, 0),
    theft         = ifelse(str_detect(Crm.Cd.Desc, regex("THEFT", ignore_case = TRUE)), 1, 0),
    rape          = ifelse(str_detect(Crm.Cd.Desc, regex("RAPE", ignore_case = TRUE)), 1, 0),
    burglary      = ifelse(str_detect(Crm.Cd.Desc, regex("BURGLARY", ignore_case = TRUE)), 1, 0),
    robbery       = ifelse(str_detect(Crm.Cd.Desc, regex("ROBBERY", ignore_case = TRUE)), 1, 0),
    vandalism     = ifelse(str_detect(Crm.Cd.Desc, regex("VANDALISM", ignore_case = TRUE)), 1, 0),
    year          = year(Date.Rptd)
  ) %>%
  group_by(year, time, AREA) %>%
  summarise(
    vehicletheft = sum(vehicletheft, na.rm = TRUE),
    theft        = sum(theft, na.rm = TRUE),
    rape         = sum(rape, na.rm = TRUE),
    burglary     = sum(burglary, na.rm = TRUE),
    robbery      = sum(robbery, na.rm = TRUE),
    vandalism    = sum(vandalism, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  arrange(year, time, AREA)

# Preview the filtered and summarized data
head(filtered_data)



# Save the output to a CSV file (adjust the file path as needed)
write.csv(filtered_data, file = "data_summary.csv", row.names = FALSE)

#--------------------------------------------------------------------------------------------------------
# Select only the numeric columns for correlation
numeric_cols <- filtered_data[, c("vehicletheft", "theft", "rape", "burglary", "robbery", "vandalism")]

numeric_cols |> 
  # calculating the correlation matrix
  cor() |> 
  # Plotting the correlation matrix and ordering them in terms of correlation patterns
  ggcorrplot::ggcorrplot(
    lab = T,
    colors = c("red", "white", "blue"),
    type = "lower",
    outline.color = "white",
    ggtheme = theme_void,
    hc.order = T
  ) 


# Using ggpairs() from the GGally package to create the scatterplot matrix:
ggpairs(numeric_cols)

#The density plot for "tresspassing" is very rightly skewed.

# Create a new data frame including the year column along with crime counts
crime_data <- filtered_data[, c("year", "vehicletheft", "theft","rape", 
                                 "burglary", "robbery", "vandalism")]

# Summary statistics for crime variables

# A character vector containing the names of the columns you want to summarise



# Reshape the data to long format and create boxplots
crime_data %>% 
  pivot_longer(
    cols = -year,               # Use all columns except 'year'
    names_to = "CrimeType", 
    values_to = "Count"
  ) %>% 
  mutate(Feature = as_factor(CrimeType)) %>% 
  ggplot(aes(x = as.factor(year), y = Count, fill = as.factor(year))) + 
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ Feature, ncol = 2, scales = "free_y") + 
  labs(
    x = "Year",
    title = "Crime Counts by Year for Different Crime Types"
  ) +
  theme_minimal()

# checking the assumption for multivariate normality
# First check if the variables are univariate normal.

# Creating a QQ Plot for each of the 6 crime counts
 crime_data |>  
  
  pivot_longer(
    cols = vehicletheft:vandalism, 
    values_to = "Count", 
    names_to = "CrimeType"
  ) |>  
  
  mutate(measure = as_factor(CrimeType)) |>  
  
  ggplot(
    mapping = aes(
      sample = Count, 
      color = CrimeType
    )
  ) + 
  
  stat_qq_line(color = "black") + 
  
  theme(legend.position = "none") +
  
  stat_qq(alpha = 0.50) +
  
  facet_wrap(
    facet = ~ CrimeType,
    scales = "free"
  ) 

 # Using mvn() in the MVN package to perform the Shapiro-Wilks test for each of the variables individually
 
 # Create a new data frame including the year column along with crime counts
 crime_df <- crime_data[, c("vehicletheft", "theft", "rape",
                                "burglary","robbery", "vandalism")]
 mvn(
   data = crime_df,
   covariance = FALSE,
   mvnTest = "mardia",
   univariateTest = "SW",
   desc = F
 )$univariateNormality

 # From the univariate test, there is strong evidence, from the p-values that the variables (crime types) are not normally distributed
 
 # checking multivariate normality
 mvn(
   data = crime_df,
   covariance = FALSE,
   mvnTest = "mardia",
   univariateTest = "SW",
   multivariatePlot = "qq",
   desc = F
 )$multivariateNormality
 
 # Overall, the result indicates that the crime data is not multivariate normal
 # We visualize the histogram plot to check which variable appears skewed 
 mvn(
   data = crime_df, 
   mvnTest = "mardia", 
   univariatePlot = "histogram",
   univariateTest = "SW",
   desc = F
 )
 ## From the plot, we can see that burglary, robbery and rape are very righly skewed. Rape also has 0 counts which makes Box-cox transformation invalid 

 # Apply the log(x+1) transformation and then scale the transformed values
 df_transformed <- crime_df %>%
   mutate(across(everything(), ~ log(.x + 1))) %>%
   mutate(across(everything(), ~ scale(.x)))
 
 
 # (Optional) Check the first few rows of the transformed data
 head(df_transformed)
 
 # Now, check multivariate normality on the transformed & scaled data
 mvn_result <- mvn(
   data = df_transformed,
   mvnTest = "mardia",          # Use Mardia's test for multivariate normality
   univariateTest = "SW",        # Also check univariate normality with Shapiro-Wilk
   multivariatePlot = "qq",      # Produce a QQ plot for the multivariate test
   desc = FALSE
 )
 
 # Print the outputs from the MVN test
 print(mvn_result$multivariateNormality)
 print(mvn_result$univariateNormality)
 
 
#--------------------------------------------------------------------------------------------- 
 # Research Q1: Has crime types changed in Los Angeles in the last 5 years?
 # A MANOVA test was carried out to evaluate the question.
 # We treat year as an independent variable and crime types as dependent variables.
 
 # Null Hypothesis: The mean vectors for crime types are equal across independent variable (years).
#----------------------------------------------------------------------------------------------------

 
 # Step 1: Create a new data frame for MANOVA by combining the 'year' column with the transformed crime counts.
 manova_df <- cbind(year = crime_data$year, as.data.frame(df_transformed))
 
 # Save the output to a CSV file (adjust the file path as needed)
 write.csv(manova_df, file = "manova_df.csv", row.names = FALSE)
 
 manova_df <- read.csv("manova_df.csv")
 
 
 # Data exploration
 str(manova_df)
 colSums(is.na(manova_df))
 manova_df$year <- as.factor(manova_df$year)
 table(manova_df$year)
 crime_vars <- c("vehicletheft", "theft", "rape", "burglary", "robbery", "vandalism")
 summary(manova_df[, crime_vars])
 
 # Assumption checks
 cor_matrix <- cor(manova_df[, crime_vars])
 print(round(cor_matrix, 2))
 
 # Check for Multivariate normality across the years
 normality_results <- data.frame(Year = character(), Skewness_p = numeric(), Kurtosis_p = numeric(), stringsAsFactors = FALSE)
 for (y in levels(manova_df$year)) {
   group_data <- manova_df[manova_df$year == y, crime_vars]
   mvn_result <- mvn(data = group_data, mvnTest = "mardia")
   print(mvn_result$multivariateNormality)
   skewness_p <- mvn_result$multivariateNormality$`p value`[1]
   kurtosis_p <- mvn_result$multivariateNormality$`p value`[2]
   normality_results <- rbind(normality_results, data.frame(Year = y, Skewness_p = skewness_p, Kurtosis_p = kurtosis_p))
 }

   print(normality_results)
 
 # check for Homogeneity of Covariance matrices
 box_result <- boxM(manova_df[, crime_vars], manova_df$year)
 print(box_result)
 

#-----------------------------------------------------------------------------------------------------
# Given the Box’s M test result (p = 5.806e-09), the Permutation-Based MANOVA (PERMANOVA) is the most appropriate action because:
 

 # Load required packages
 library(dplyr)    # For data manipulation
 library(rstatix)  # For Games-Howell test
 library(vegan)    # For NMDS (visualization)
 
 
 manova_df$year <- as.factor(manova_df$year)
 crime_vars <- c("vehicletheft", "theft", "rape", "burglary", "robbery", "vandalism")
 
 # Step 1: Box’s M test
 boxM_result <- boxM(manova_df[, crime_vars], manova_df$year)
 print(boxM_result)
 
 # Step 2: PERMANOVA
 permanova_result <- adonis2(as.matrix(manova_df[, crime_vars]) ~ year, 
                             data = manova_df, permutations = 999, method = "euclidean")
 print(permanova_result)

 

 # Step 3: Welch’s ANOVA (unchanged, for continuity)
 anova_results <- lapply(crime_vars, function(var) {
   oneway.test(as.formula(paste(var, "~ year")), data = manova_df, var.equal = FALSE)
 })
 p_values <- sapply(anova_results, function(x) x$p.value)
 p_adjusted <- p.adjust(p_values, method = "bonferroni")
 results_table <- data.frame(
   Crime = crime_vars,
   P_Value = round(p_values, 4),
   P_Adjusted = round(p_adjusted, 4)
 )
 cat("\nWelch’s ANOVA Results (Bonferroni Adjusted):\n")
 print(results_table)
 
 # Step 4: Games-Howell Post-Hoc Tests using rstatix
 cat("\nGames-Howell Post-Hoc Tests:\n")
 for (var in crime_vars) {
   cat("\nPost-Hoc for", var, ":\n")
   # Perform Games-Howell test
   gh_result <- games_howell_test(
     data = manova_df,
     formula = as.formula(paste(var, "~ year")),
     conf.level = 0.95,
     detailed = FALSE
   ) %>%
     filter(p.adj < 0.05)  # Show only significant comparisons
   print(gh_result)
 }
 
 
 
 

#------------------------------------------------------------------------------------------------------- 
 ## Perform LDA to seperate crime types by year
 # Set seed for reproducibility
 set.seed(123)
 
 # Convert year to factor
 manova_df$year <- as.factor(manova_df$year)
 
 # Verify standardization of crime variables
 crime_vars <- c("vehicletheft", "theft", "rape", "burglary", "robbery", "vandalism")
 cat("\nChecking standardization of crime variables (mean ~ 0, sd ~ 1):\n")
 print(sapply(manova_df[, crime_vars], function(x) c(mean = round(mean(x), 3), sd = round(sd(x), 3))))
 
 # Check for missing values
 if (anyNA(manova_df[, c("year", crime_vars)])) {
   stop("Missing values detected in manova_df. Please handle them before proceeding.")
 }
 
 # Split data into training and test sets
 train_indices <- sample(1:nrow(manova_df), 0.7 * nrow(manova_df))
 train_data <- manova_df[train_indices, ]
 test_data <- manova_df[-train_indices, ]
 
 # Check class distribution in training data
 cat("\nClass distribution in training data (year):\n")
 print(table(train_data$year))
 
 # Perform LDA
 lda_model <- lda(year ~ vehicletheft + theft + rape + burglary + robbery + vandalism, 
                  data = train_data)
 
 # Print LDA model summary
 cat("\nLDA Model Summary:\n")
 print(lda_model)
 
 # Calculate and print proportion of trace (variance explained)
 prop_trace <- lda_model$svd^2 / sum(lda_model$svd^2)
 names(prop_trace) <- paste0("LD", 1:length(prop_trace))
 cat("\nProportion of Trace (Variance Explained):\n")
 print(round(prop_trace, 3))
 
 # Predict on test data
 lda_pred <- predict(lda_model, newdata = test_data)
 
 # Confusion matrix
 conf_matrix <- table(Predicted = lda_pred$class, Actual = test_data$year)
 cat("\nConfusion Matrix:\n")
 print(conf_matrix)
 
 # Classification accuracy and misclassification rate
 accuracy <- mean(lda_pred$class == test_data$year)
 cat("\nClassification Accuracy:", round(accuracy, 3), "\n")
 cat("Misclassification Rate:", round(1 - accuracy, 3), "\n")
 
 # Visualization
 lda_scores <- as.data.frame(predict(lda_model)$x)
 lda_scores$year <- train_data$year
 ggplot(lda_scores, aes(x = LD1, y = LD2, color = year)) +
   geom_point(size = 2) +
   stat_ellipse(aes(color = year), type = "norm", level = 0.95) +
   theme_minimal() +
   labs(title = "LDA: Separation of Years by Crime Profiles",
        x = sprintf("LD1 (%.2f%% variance)", prop_trace[1] * 100),
        y = sprintf("LD2 (%.2f%% variance)", prop_trace[2] * 100)) +
   scale_color_manual(values = c("2020" = "red", "2021" = "blue", "2022" = "green", 
                                 "2023" = "purple", "2024" = "orange")) +
   theme(legend.text = element_text(size = 8))
 ggsave("lda_plot.png", width = 8, height = 6)
 
 cat("\nLDA plot saved as 'lda_plot.png'\n") 
 
#------------------------------------------------------------------------------------------------------ 

 # Robustness: LOOCV
 lda_model_cv <- lda(year ~ vehicletheft + theft + rape + burglary + robbery + vandalism, 
                     data = train_data, CV = TRUE)
 cv_accuracy <- mean(lda_model_cv$class == train_data$year)
 cat("\nLOOCV Accuracy:", round(cv_accuracy, 3), "\n")
 
 # Save results
 save(lda_model, lda_pred, conf_matrix, lda_scores, file = "lda_results.RData")
 cat("\nResults saved to 'lda_results.RData'\n")

 
#---------------------------------------------------------------------------------------------------------------
# Research Question 2: Does crime types in Los Angeles differ by time of the day?
#--------------------------------------------------------------------------------------------------------------- 

 # Clear environment
 rm(list = ls())
 
# Reshaping data
 
 # Load data and prepare
 crime_data2 <- read.csv("crime_time_data.csv")
 data_reduced <-crime_data2[, !names(crime_data2) %in% c("AREA")]
 data_reduced$time <- factor(data_reduced$time, levels = c("Morning", "Before_noon", "Afternoon", "Night"))
 
 # Data examination
 str(data_reduced)
 table(data_reduced$time)
 
 crime_vars <- c("vehicletheft", "theft", "rape", "burglary", "robbery", "vandalism")
 
 summary(data_reduced[, crime_vars])
 
 # checking standardization
 sapply(data_reduced[, crime_vars], function(x) c(mean = round(mean(x), 6), sd = round(sd(x), 6)))
 
# Checking missing variables
 colSums(is.na(data_reduced))
 
 # Assumption checks for Multivariate analysis
 mvn_result <- mvn(data = data_reduced[, crime_vars], mvnTest = "mardia")
 print(mvn_result$multivariateNormality)
 
 # Data isn't multivariate normal based on mardia's test but its pretty close (from p-values)!
 
 # Checking Box's M for equal covariance matrices
 box_m_result <- boxM(data_reduced[, crime_vars], data_reduced$time)
 print(box_m_result)
 # The condition for equal covariance matrix is satisfied (p= 0.8846 > the threshold of 0.05)
 
 # checking correlation
 cor_matrix <- cor(data_reduced[, crime_vars])
 print(round(cor_matrix, 3))
 # Does not seem to be unnecessarily high
 

 # MANOVA Analysis
 # Null Hypothesis: The mean vector of the 6 crime types are equal across all four levels of time.
 
 manova_model <- manova(as.matrix(data_reduced[, crime_vars]) ~ time, data = data_reduced)
 summary_manova <- summary(manova_model, test = "Pillai")
 print(summary_manova)
 save(manova_model, summary_manova, file = "manova_time_crime_time_data.RData")
 ## Pillai's trace (2.011), p-value = 2.2e-16 which indicates significant difference across the time levels for crimes committed.
 
 
 # Univariate ANOVAs
 cat("\nUnivariate ANOVA Results for Each Crime Variable:\n")
 anova_results <- lapply(crime_vars, function(var) {
   aov_model <- aov(data_reduced[[var]] ~ time, data = data_reduced)
   summary(aov_model)
 })
 names(anova_results) <- crime_vars
 print(anova_results)
 save(anova_results, file = "anova_time_crime_time_data.RData")
 cat("\nANOVA results saved to 'anova_time_crime_time_data.RData'\n")
 
 ## Based on individual ANOVA test, all time levels for crime types are significant.
 
 # Post-hoc Tukey HSD tests
 cat("\nTukey HSD Post-Hoc Tests for Each Crime Variable:\n")
 tukey_results <- lapply(crime_vars, function(var) {
   aov_model <- aov(data_reduced[[var]] ~ time, data = data_reduced)
   tukey <- TukeyHSD(aov_model)
   return(tukey)
 })
 names(tukey_results) <- crime_vars
 print(tukey_results)
 save(tukey_results, file = "tukey_time_crime_time_data.RData")
 cat("\nTukey HSD results saved to 'tukey_time_crime_time_data.RData'\n")
 

 # Visualization
 data_melt <- melt(data_reduced, id.vars = "time", measure.vars = crime_vars, variable.name = "Crime", value.name = "Value")
 cat("\nGenerating boxplots for crime variables by time...\n")
 ggplot(data_melt, aes(x = time, y = Value, fill = time)) +
   geom_boxplot() +
   facet_wrap(~Crime, scales = "free_y") +
   theme_minimal() +
   labs(title = "Crime Rates by Time of Day", x = "Time", y = "Standardized Value") +
   theme(legend.position = "none",
         axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
 ggsave("boxplots_crime_time.png")
 
 
 # Reshape data for  mean plotting
 # Mean plots with error bars 
 
 means <- aggregate(. ~ time, data = data_reduced[, c("time", crime_vars)], mean)
 se <- aggregate(. ~ time, data = data_reduced[, c("time", crime_vars)], function(x) sd(x)/sqrt(length(x)))
 means_melt <- melt(means, id.vars = "time", variable.name = "Crime", value.name = "Mean")
 se_melt <- melt(se, id.vars = "time", variable.name = "Crime", value.name = "SE")
 data_plot <- merge(means_melt, se_melt, by = c("time", "Crime"))
 
 ggplot(data_plot, aes(x = time, y = Mean, group = Crime, color = Crime)) +
   geom_line() +
   geom_point() +
   geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.1) +
   theme_minimal() +
   labs(title = "Mean Crime Rates by Time of Day", x = "Time", y = "Mean Standardized Value")
 ggsave("mean_plots_crime_time.png")
 
 #------------------------------------------------------------------------------------------------
# Performing LDA 
 # Perform LDA
 lda_model <- lda(time ~ vehicletheft + theft + rape + burglary + robbery + vandalism, data = data_reduced)
 
 # Summary of LDA
 print(lda_model)
 
 # Proportion of trace (variance explained by each discriminant)
 prop_trace <- lda_model$svd^2 / sum(lda_model$svd^2)
 names(prop_trace) <- paste0("LD", 1:length(prop_trace))
 print(round(prop_trace, 3))
 
 # Predict and evaluate classification accuracy
 lda_pred <- predict(lda_model)
 confusion_matrix <- table(Predicted = lda_pred$class, Actual = data_reduced$time)
 print(confusion_matrix)
 
 # Classification accuracy
 accuracy <- mean(lda_pred$class == data_reduced$time)
 print(accuracy)
 
 # Save LDA results
 save(lda_model, lda_pred, confusion_matrix, accuracy, file = "lda_time_crime_time_data.RData")
 # Extract LDA scores
 lda_scores <- as.data.frame(predict(lda_model)$x)
 lda_scores$time <- data_reduced$time
 
 # Scatterplot of LD1 vs. LD2
 ggplot(lda_scores, aes(x = LD1, y = LD2, color = time, shape = time)) +
   geom_point(size = 3) +
   theme_minimal() +
   labs(title = "LDA: Crime Profiles by Time of Day",
        x = sprintf("LD1 (%.1f%%)", prop_trace[1] * 100),
        y = sprintf("LD2 (%.1f%%)", prop_trace[2] * 100)) +
   scale_color_manual(values = c("Morning" = "blue", "Before_noon" = "green", "Afternoon" = "orange", "Night" = "red")) +
   scale_shape_manual(values = c("Morning" = 16, "Before_noon" = 17, "Afternoon" = 15, "Night" = 18))
 ggsave("lda_scatterplot_crime_time.png")
 
 
#-------------------------------------------------------------------------------------------------------
 #Research Question 3: Use KNN and classification tree to predict what time of the day, based on past information, 
 # crime types are likely to occur
 
#-----------------------------------------------------------------------------------------------------------

 # Load required libraries
 library(class)        # For KNN
 library(rpart)        # For classification tree
 library(rpart.plot)   # For tree visualization
 library(caret)        # For cross-validation and confusion matrix
 library(dplyr)        # For data manipulation
 
 
 
 # Load the dataset
 data <- read.csv("crime_time_data.csv")
 
 # Define predictors and target variable
 predictors <- c("vehicletheft", "theft", "rape", "burglary", "robbery", "vandalism")
 target <- "time"
 
 # Check for missing values
 colSums(is.na(data))
 
 # Verify data structure
 str(data[, c(target, predictors)])
 
 # Convert time to factor
 data$time <- as.factor(data$time)
 
 # Set seed for reproducibility
 set.seed(456)
 
 # Define training control for 10-fold cross-validation
 train_control <- trainControl(method = "cv", number = 10)
 
 # Train KNN model with different k values
 knn_model <- train(
   x = data[, predictors],
   y = data$time,
   method = "knn",
   trControl = train_control,
   tuneGrid = data.frame(k = seq(1, 15, by = 2)) # Test k = 1, 3, ..., 15
 )
 
 # Print model results
 print(knn_model)
 
 # Plot accuracy vs k
 plot(knn_model)
 
 # Predict using the best KNN model
 knn_pred <- predict(knn_model, newdata = data[, predictors])
 
 # Confusion matrix
 knn_conf_matrix <- confusionMatrix(knn_pred, data$time)
 print(knn_conf_matrix) 

 
 # Set seed for reproducibility
 set.seed(123)
 
 # Train classification tree
 tree_model <- rpart(
   time ~ vehicletheft + theft + rape + burglary + robbery + vandalism,
   data = data,
   method = "class",
   control = rpart.control(cp = 0.01) # Minimum complexity parameter
 )
 
 # Cross-validation to select optimal cp
 tree_cv <- train(
   time ~ vehicletheft + theft + rape + burglary + robbery + vandalism,
   data = data,
   method = "rpart",
   trControl = train_control,
   tuneGrid = data.frame(cp = seq(0.01, 0.1, by = 0.01))
 )
 
 # Print cross-validation results
 print(tree_cv)
 
 # Select best tree
 best_cp <- tree_cv$bestTune$cp
 pruned_tree <- prune(tree_model, cp = best_cp)
 
 # Plot the tree
 rpart.plot(pruned_tree, main = "Classification Tree for Time Category") 

 
 # Predict using the pruned tree
 tree_pred <- predict(pruned_tree, newdata = data, type = "class")
 
 # Confusion matrix
 tree_conf_matrix <- confusionMatrix(tree_pred, data$time)
 print(tree_conf_matrix) 

 #-----------------------------------------------------------------------------------
 
 
 
 # To change tree color/graphics
 # Select best tree
 best_cp <- tree_cv$bestTune$cp
 pruned_tree <- prune(tree_model, cp = best_cp)
 
 # Plot the tree with corrected box.palette
 rpart.plot(pruned_tree, 
            main = "Classification Tree for Time Category",
            box.palette = list("orange", "pink", "skyblue", "grey"), # List of colors in order of levels
            shadow.col = "gray",        # Optional: Add shadow for better visibility
            fallen.leaves = TRUE)       # Place leaves at the bottom
 
 