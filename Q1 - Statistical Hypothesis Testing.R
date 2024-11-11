# Install packages
install.packages("openxlsx")
install.packages("openxlsx")
install.packages("data.table")
install.packages("dplyr")
install.packages("tidyr")

# Load libraries
library(openxlsx)
library(data.table)
library(dplyr)
library(tidyr)
library(readxl)

# Read in convariates and biomarkers data
covariates_data <- as.data.table(read_excel("covariates.xlsx"))
biomarkers_data <- as.data.table(read_excel("biomarkers.xlsx"))

# Check column names
colnames(covariates_data)
colnames(biomarkers_data)

# Filter out any rows with NA values
covariates_data_clean <- na.omit(covariates_data)
biomarkers_data_clean <- na.omit((biomarkers_data))

# Ensure Biomarker is character
biomarkers_data_clean[, Biomarker := as.character(Biomarker)]

# Split 'PatientID' in 'biomarkers_data' into separate columns, i.e. seperate patient ID and timepoint
biomarkers_data_clean[,c("Biomarker","timepoint") := tstrsplit(Biomarker, "-", fixed = TRUE)]
biomarkers_data_clean[, Biomarker := as.numeric(Biomarker)]
biomarkers_data_clean[order(Biomarker)]

# Rename Biomarker header to PaitentID to match covariates header
colnames(biomarkers_data_clean)[1] <- "PatientID"
head(biomarkers_data_clean)

# Filter biomarker data to only include inclusion i.e. 0 weeks
biomarkers_data_0week <- biomarkers_data_clean[timepoint=="0weeks"]

# Make sure headers are characters so can merge data
covariates_data_clean$PatientID <- as.character(covariates_data_clean$PatientID)
biomarkers_data_0week$PatientID <- as.character(biomarkers_data_0week$PatientID)

# Merge the datasets by patient ID
data_comb <- merge(covariates_data_clean, biomarkers_data_0week, all.x = TRUE, by = "PatientID")

# HYPOTHESIS TESTING

# Plot data to test normality and then pick t-test or Mann-Whitney U test depending on distribution
# IL-8 Biomarker
hist(biomarkers_data_0week[["IL-8"]]) # normal distribution
qqnorm(biomarkers_data_0week[["IL-8"]])
IL8_test_result <- t.test(`IL-8` ~ `Sex (1=male, 2=female)`, data = data_comb , var.equal = TRUE)
print(IL8_test_result)

# VEGF-A biomarker
hist(biomarkers_data_0week[["VEGF-A"]]) # non-normal distribution?
qqnorm(biomarkers_data_0week[["VEGF-A"]])
VEGFA_test_result <- wilcox.test(`VEGF-A` ~ `Sex (1=male, 2=female)`, data = data_comb , var.equal = TRUE)
print(VEGFA_test_result)

# OPG biomarker
hist(biomarkers_data_0week[["OPG"]]) # normal distribution
qqnorm(biomarkers_data_0week[["OPG"]])
OPG_test_result <- t.test(`OPG` ~ `Sex (1=male, 2=female)`, data = data_comb , var.equal = TRUE)
print(OPG_test_result)

# TGF-beta-1 biomarker
hist(biomarkers_data_0week[["TGF-beta-1"]]) # non-normal distribution
qqnorm(biomarkers_data_0week[["TGF-beta-1"]])
TGFbeta1_test_result <- wilcox.test(`TGF-beta-1` ~ `Sex (1=male, 2=female)`, data = data_comb , var.equal = TRUE)
print(TGFbeta1_test_result)

# IL-6 biomarker
hist(biomarkers_data_0week[["IL-6"]]) # non-normal distribution
qqnorm(biomarkers_data_0week[["IL-6"]])
IL6_test_result <- wilcox.test(`IL-6` ~ `Sex (1=male, 2=female)`, data = data_comb , var.equal = TRUE)
print(IL6_test_result)

# CXCL9 biomarker
hist(biomarkers_data_0week[["CXCL9"]]) # non-normal distribution
qqnorm(biomarkers_data_0week[["CXCL9"]])
CXCL9_test_result <- wilcox.test(`CXCL9` ~ `Sex (1=male, 2=female)`, data = data_comb , var.equal = TRUE)
print(CXCL9_test_result)

# CXCL1 biomarker
hist(biomarkers_data_0week[["CXCL1"]]) # non-normal distribution
qqnorm(biomarkers_data_0week[["CXCL1"]])
CXCL1_test_result <- wilcox.test(`CXCL1` ~ `Sex (1=male, 2=female)`, data = data_comb , var.equal = TRUE)
print(CXCL1_test_result)

# IL-18 biomarker
hist(biomarkers_data_0week[["IL-18"]]) # normal distribution
qqnorm(biomarkers_data_0week[["IL-18"]])
IL18_test_result <- t.test(`IL-18` ~ `Sex (1=male, 2=female)`, data = data_comb , var.equal = TRUE)
print(IL18_test_result)

# CSF-1 biomarker
hist(biomarkers_data_0week[["CSF-1"]]) # normal distribution
qqnorm(biomarkers_data_0week[["CSF-1"]])
CSF1_test_result <- t.test(`CSF-1` ~ `Sex (1=male, 2=female)`, data = data_comb , var.equal = TRUE)
print(CSF1_test_result)

# Calulating Type I Error Rate
n <- 9
alpha <- 0.05
type_I_error_rate <- 1 - (1 - alpha)^n
type_I_error_rate

# BONFERRONI-CORRECTED TESTS

# set original alpha level and calculate Bonferroni-adjusted alpha
alpha <- 0.05
num_test <- length(biomarkers_data_0week)
ajusted_alpha <- alpha / num_test

# Run t-test for each biomarker, applying the Bonferroni correction
# IL-8 biomarker
p.adjust(0.3544, method = "bonferroni", n = 9)

# VEGF-A biomarker
p.adjust(0.04402, method = "bonferroni", n = 9)

# OPG biomarker
p.adjust(0.13, method = "bonferroni", n = 9)

# TGF-beta-1 biomarker
p.adjust(0.08389, method = "bonferroni", n = 9)

# IL-6 biomarker
p.adjust(0.2652, method = "bonferroni", n = 9)

# CXCL9 biomarker
p.adjust(0.9219, method = "bonferroni", n = 9)

# CXCL1 biomarker
p.adjust(0.004647, method = "bonferroni", n = 9)

# IL-18 biomarker
p.adjust(0.1933, method = "bonferroni", n = 9)

# CSF-1 biomarker
p.adjust(0.006414, method = "bonferroni", n = 9)
