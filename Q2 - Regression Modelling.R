install.packages("openxlsx")
install.packages("knitr")
install.packages("gt")


# Load libraries
library(openxlsx)
library(readxl)
library(data.table)
library(knitr)
library(gt)


# Read in convariates and biomarkers data
covariates_data <- as.data.table(read_excel("C:/Users/sjrfb/OneDrive - University of Edinburgh/Stats Code/Assignment/covariates.xlsx"))
biomarkers_data <- as.data.table(read_excel("C:/Users/sjrfb/OneDrive - University of Edinburgh/Stats Code/Assignment/biomarkers.xlsx"))

# Split 'PatientID' in 'biomarkers_data' into separate columns, i.e. seperate patient ID and timepoint
biomarkers_data[,c("Biomarker","timepoint") := tstrsplit(Biomarker, "-", fixed = TRUE)]
biomarkers_data[, Biomarker := as.numeric(Biomarker)]
biomarkers_data[order(Biomarker)]

# Rename column
colnames(biomarkers_data)[1] <- "PatientID"

# Filter biomarker data to include only baseline values
biomarkers_data_0week <- biomarkers_data[timepoint=="0weeks"]

# Merge the biomarker and covariates datasets based on matching PatientIDS
covariates_data$PatientID <- as.character(covariates_data$PatientID)
biomarkers_data_0week$PatientID <- as.character(biomarkers_data_0week$PatientID)
data_comb <- merge(covariates_data, biomarkers_data_0week, all.x = TRUE, by = "PatientID")

# Split data into 80% for training and 20% for testing
n <- nrow(data_comb)
ntest <- round(0.2*n)
ntrain <- n - ntest

train_rows <- sample(1:n, ntrain)
data_comb_train <- data_comb[train_rows,]
data_comb_test <- data_comb[-train_rows,]

data_comb_train$PatientID <- as.numeric(data_comb_train$PatientID)
data_comb_test$PatientID <- as.numeric(data_comb_test$PatientID)


# Fit model to training set
m <- lm(`Vas-12months` ~ `PatientID`+`Age`+`Sex (1=male, 2=female)`+`Smoker (1=yes, 2=no)`+`VAS-at-inclusion`+`IL-8`+`VEGF-A`+`OPG`+`TGF-beta-1`+`IL-6`+`CXCL9`+`CXCL1`+`IL-18`+`CSF-1`, data = data_comb_train)

# Present the fitted parameter values in a table
# Extract the intercept and coefficients
coefficients_table <- summary(m)$coefficients
coefficients_table <- as.data.frame(coefficients_table)

# Display parametres in formatted table using kable
kable(coefficients_table, caption = "Fitted Model Coefficients")

# Display parameters in formatted table using gt
gt_table <- gt(coefficients_table) %>%
  tab_header(
    title = "Fitted Model Coefficients"
  ) %>%
  fmt_number(
    columns = c("Estimate", "Std. Error", "t value"),
    decimal = 3
  )

# Use fitted model to predict 12-month VAS for test data
predictions <- predict(m, newdata = data_comb_test)

#COMPARE PREDICTIONS TO ACTUAL VALUES
# Actual values of 12-month VAS in the test set
actual_values <- data_comb_test$`Vas-12months`

#Filter out any rows with NA values
non_na_indices <- !is.na(predictions) & !is.na(actual_values)
predictions_clean <- predictions[non_na_indices]
actual_values_clean <- actual_values[non_na_indices]

# Calculate Mean Squared Error and Mean Absolute Error
mse <- mean((predictions_clean - actual_values_clean)^2)
mae <- mean(abs(predictions_clean - actual_values_clean))
mape <- mean(abs((predictions_clean - actual_values_clean)/predictions_clean))*100

# Print the error metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")