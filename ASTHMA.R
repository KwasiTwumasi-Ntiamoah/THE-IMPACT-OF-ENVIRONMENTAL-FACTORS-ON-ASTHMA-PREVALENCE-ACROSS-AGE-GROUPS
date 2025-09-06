# Load essential libraries
library(tidyverse)
library(dplyr)
library(readr)
library(caret)
library(pROC)
# seed for reproducibility
set.seed(123)

# Set the working directory to the folder where the dataset is stored, this will ensures R knows where to find the file
setwd("C:/Users/kstwumasi-ntiamoah/OneDrive - University of Plymouth/dissertation project")

# Read in the asthma dataset from a CSV file. This dataset contains patient characteristics and environmental exposure variables
asthma <- read_csv("asthma_disease_data.csv")

# Calculate the total number of patients in the dataset, this is Useful for descriptive statistics and understanding sample size
# total number of patients
total <- nrow(asthma)

# Calculate how many people have been diagnosed with asthma (Diagnosis = 1).
# number of people diagnosed with asthma
asthma_n <- sum(asthma$Diagnosis == 1, na.rm = TRUE)
asthma_n

# Calculate how many people do not have asthma (Diagnosis = 0)
# people who were not diagnosed with asthma
non_asthma <- total - asthma_n
non_asthma

# Create a new variable called 'age_group' to classify patients into age groups
asthma <- asthma %>%
  mutate(age_group = case_when(
    Age <= 12 ~ "Children",
    Age <= 17 ~ "Adolescents",
    Age <= 35 ~ "Young Adults",
    Age <= 55 ~ "Middle-aged Adults",
    Age >= 56 ~ "Older Adults"
  ))

summary(asthma)


# Table 1: Descriptive statistics by age group and asthma status
table1 <- asthma %>%
  group_by(age_group, Diagnosis) %>%
  summarise(
    Count = n(),
    Mean_Age = mean(Age, na.rm = TRUE),
    PollutionExposure = mean(PollutionExposure, na.rm = TRUE),
    PollenExposure = mean(PollenExposure, na.rm = TRUE),
    DustExposure = mean(DustExposure, na.rm = TRUE),
    PetAllergy = mean(PetAllergy, na.rm = TRUE)
  )

# View the table
print(table1)


# Plot the number of people with and without asthma by age group
asthma_case = asthma %>% filter(Diagnosis == 1) %>%
  group_by(age_group) %>% 
  summarise(Count = n())
ggplot(asthma, aes(x = age_group, fill = factor(Diagnosis))) +
  geom_bar(position = "dodge") +
  labs(x = "Age Group", y = "Count", fill = "Asthma Diagnosis",
       )

ggplot(asthma_case, aes(x = age_group, y = Count, fill = age_group)) +
  geom_bar(stat = "identity") + 
  labs(x = "Age Group", y = "Count", fill = "Asthma Diagnosis",
       )

ggplot(asthma, aes(x = age_group, y = PollutionExposure, fill = factor(Diagnosis))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(x = "Age Group", y = "Pollution Exposure",
       fill = "Diagnosis (0 = No, 1 = Yes")

ggplot(asthma, aes(x = age_group, y = PollenExposure, fill = factor(Diagnosis))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(x = "Age Group", y = "Pollen Exposure",
       fill = "Diagnosis (0 = No, 1 = Yes")

ggplot(asthma, aes(x = age_group, y = DustExposure, fill = factor(Diagnosis))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(x = "Age Group", y = "Dust Exposure",
       fill = "Diagnosis (0 = No, 1 = Yes")

ggplot(asthma, aes(x = age_group, y = PetAllergy, fill = factor(Diagnosis))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(x = "Age Group", y = "Pet Allergy",
       fill = "Diagnosis (0 = No, 1 = Yes")

as <- asthma %>% group_by(age_group, Diagnosis) %>%
  summarise(count = n(),
            Pollution = mean(PollutionExposure),
            Pollen = mean(PollenExposure),
            Dust = mean(DustExposure),
            PetAllergy = mean(PetAllergy))
as

asthma$EducationLevel <- as.factor(asthma$EducationLevel)

a <- glm(Diagnosis ~ Age + Gender + Ethnicity + EducationLevel + Smoking + PhysicalActivity +
           DietQuality + SleepQuality + BMI + FamilyHistoryAsthma + HayFever + Eczema +
           PollutionExposure + PollenExposure + DustExposure + PetAllergy +
           LungFunctionFEV1 + LungFunctionFVC, data = asthma, family = binomial)

a


summary(a)


exp(coef(a))


exp(confint(a))

#data splitting
# Seed for reproducibility
set.seed(123)

# Prepare clean dataset with needed predictors
asthma_clean <- asthma %>%
  select(Diagnosis, Age, Gender, Ethnicity, EducationLevel, Smoking, PhysicalActivity,
         DietQuality, SleepQuality, BMI, FamilyHistoryAsthma, HayFever, Eczema,
         PollutionExposure, PollenExposure, DustExposure, PetAllergy,
         LungFunctionFEV1, LungFunctionFVC) %>%
  drop_na()

# Convert relevant variables to factors
asthma_clean$Diagnosis <- as.factor(asthma_clean$Diagnosis)
asthma_clean$Gender <- as.factor(asthma_clean$Gender)
asthma_clean$Ethnicity <- as.factor(asthma_clean$Ethnicity)
asthma_clean$EducationLevel <- as.factor(asthma_clean$EducationLevel)
asthma_clean$Smoking <- as.factor(asthma_clean$Smoking)

# Stratified splitting
asthma_0 <- asthma_clean %>% filter(Diagnosis == 0) %>% mutate(row_id = row_number())
asthma_1 <- asthma_clean %>% filter(Diagnosis == 1) %>% mutate(row_id = row_number())

# Sample 300 from each class for training
train_0 <- asthma_0 %>% sample_n(300, replace = TRUE)
train_1 <- asthma_1 %>% sample_n(300, replace = TRUE)

# Remaining for validation/test
remaining_0 <- asthma_0 %>% filter(!row_id %in% train_0$row_id)
remaining_1 <- asthma_1 %>% filter(!row_id %in% train_1$row_id)

# Sample 300 from each for validation
valid_0 <- remaining_0 %>% sample_n(300, replace = TRUE)
valid_1 <- remaining_1 %>% sample_n(300, replace = TRUE)

# Remaining go to test
test_0 <- remaining_0 %>% filter(!row_id %in% valid_0$row_id)
test_1 <- remaining_1 %>% filter(!row_id %in% valid_1$row_id)

# Combine and shuffle
asthma_train   <- bind_rows(train_0, train_1) %>% select(-row_id) %>% slice_sample(n = 600)
asthma_valid   <- bind_rows(valid_0, valid_1) %>% select(-row_id) %>% slice_sample(n = 600)
asthma_test    <- bind_rows(test_0, test_1) %>% select(-row_id) %>% slice_sample(n = 600)

# Check set sizes
cat("Training set size:", nrow(asthma_train), "\n")
cat("Validation set size:", nrow(asthma_valid), "\n")
cat("Test set size:", nrow(asthma_test), "\n")

#Binary Logistic Regression
# Predict probabilities
pred_probs <- predict(a, type = "response")

# Predicted diagnosis using 0.5 cutoff
pred_class <- ifelse(pred_probs > 0.5, 1, 0)
pred_class <- factor(pred_class, levels = c(0, 1))

actual_class <- asthma$Diagnosis
# Confusion matrix

cf_m <- table(Predicted = pred_class, Actual = actual_class)
print(cf_m)

# Error rate
error_lr <- (cf_m[1,2] + 
                    cf_m[2, 1]) / sum(cf_m)

# Error percentage
cat("Logistic Regression test error rate:", round(error_lr * 100, 2), "%\n")

#Random Forest
library(randomForest)

# Set seed for reproducibility
set.seed(123)

# Train the Random Forest model using all predictors
rf_model <- randomForest(
  Diagnosis ~ .,               # Use all predictors to predict asthma
  data = asthma_train,         # Use the balanced training set
  mtry = 6,                    # Number of variables tried at each split (you can tune this)
  importance = TRUE            # To compute variable importance
)

# Print model summary
print(rf_model)

# Visualize variable importance
varImpPlot(rf_model)
table(asthma_test$Diagnosis)


# Make predictions on the test set
rf_preds <- predict(rf_model, newdata = asthma_test, type = "class")


# Confusion matrix: compare predicted vs actual
conf_matrix_rf <- table(Predicted = rf_preds, Actual = asthma_test$Diagnosis)
print(conf_matrix_rf)

# Calculate error rate
error_rf <- (conf_matrix_rf[1,2] + conf_matrix_rf[2,1]) / sum(conf_matrix_rf)
cat("Random Forest test error rate:", round(error_rf * 100, 2), "%\n")

accuracy_rf <- 1 - error_rf
accuracy_rf
cat("Random Forest Accuracy:", round(accuracy_rf * 100, 2), "%\n")

#Decision tree
library(tree)

# Fit tree
tree_model2 <- tree(Diagnosis ~ ., data = asthma_train)

# Plot
plot(tree_model2)
text(tree_model2, pretty = 0)

# Predict
tree2_preds <- predict(tree_model2, newdata = asthma_test, type = "class")

# Confusion matrix
conf_matrix_tree2 <- table(Predicted = tree2_preds, Actual = asthma_test$Diagnosis)
print(conf_matrix_tree2)

# Error rate
tree2_error <- (conf_matrix_tree2[1,2] + conf_matrix_tree2[2,1]) / sum(conf_matrix_tree2)
cat("Tree model (package 'tree') test error rate:", round(tree2_error * 100, 2), "%\n")

accuracy_tree <- 1 - tree2_error
accuracy_tree
cat("Decision Tree Accuracy:", round(accuracy_tree * 100, 2), "%\n")

#svm
# Load SVM library
library(e1071)

# Train SVM (you can tune kernel and cost if needed)
set.seed(123)
svm_model <- svm(Diagnosis ~ ., data = asthma_train, kernel = "radial", probability = TRUE)

# Predict on test data
svm_preds <- predict(svm_model, newdata = asthma_test)

# Confusion matrix
conf_matrix_svm <- table(Predicted = svm_preds, Actual = asthma_test$Diagnosis)
print(conf_matrix_svm)

# Error rate
svm_error <- (conf_matrix_svm[1,2] + conf_matrix_svm[2,1]) / sum(conf_matrix_svm)
cat("SVM test error rate:", round(svm_error * 100, 2), "%\n")

accuracy_svm <- 1 - svm_error
accuracy_svm
cat("SVM Accuracy:", round(accuracy_svm * 100, 2), "%\n")
