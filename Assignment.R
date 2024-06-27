dataset <- read.csv (file = "C:/Users/nando/OneDrive/Desktop/APU/Semester 3/PFDA/Assignment/student_prediction.csv", header = TRUE, sep = ",")

#view dataset 
View(dataset)

#Data Exploration
#structure of the dataset
str (dataset)
#checking the data 
head (dataset, 4)
#Number of rows and columns
dim (dataset)
#Column names 
names(dataset)
#Summary of the dataset
summary(dataset)

#Data Cleaning
#Searching any N/A values within the dataset
print("Position of missing values -") 
which(is.na(dataset)) 

#remove rows having NA in case any records have
dataset <- na.omit(dataset)

#slicing necessary column needed 
selected_column <- (dataset[,(
  colnames(dataset) %in% 
    c("STUDENTID", "STUDY_HRS", "LISTENS", 
      "CUML_GPA", "NOTES", "PREP_EXAM", "GRADE"))])

#rename column names 
names(selected_column) <- c("STUDENT_ID", "WEEKLY_STUDY_HOURS", "PREP_EXAM_FREQUENCY", "NOTE_CLASS",
                            "LISTEN_IN_CLASS", "CGPA_LAST_SEM","OUTPUT_GRADE")
head(selected_column)
View(selected_column)

#changing the numeric value into labels -- NOTE CLASS 
selected_column$NOTE_CLASS <- factor (selected_column$NOTE_CLASS)
selected_column$NOTE_CLASS <- factor(selected_column$NOTE_CLASS, levels=c(1,2,3), labels = c("Never", "Sometimes", "Always"))

#changing the numeric value into labels --WEEKLY STUDY HOURS
selected_column$WEEKLY_STUDY_HOURS <- factor(selected_column$WEEKLY_STUDY_HOURS, levels = c(1, 2, 3, 4, 5), 
                                             labels = c("none","<5 Hours","6-10 Hours","11-20 Hours","More Than 20 Hours"))

#changing the numeric value into labels --LISTEN IN CLASS
selected_column$LISTEN_IN_CLASS <- factor (selected_column$LISTEN_IN_CLASS)
selected_column$LISTEN_IN_CLASS <- factor(selected_column$LISTEN_IN_CLASS,levels=c(1,2,3), 
                                          labels = c("Never", "Sometimes", "Always")) 

#changing the numeric value into labels --PREP EXAM FREQUENCY
selected_column$PREP_EXAM_FREQUENCY <- factor (selected_column$PREP_EXAM_FREQUENCY)
selected_column$PREP_EXAM_FREQUENCY <- factor(selected_column$PREP_EXAM_FREQUENCY, levels = c(1, 2, 3),
                            labels = c("closest date to the exam", "regularly during the semester", "never"))

#changing the numeric value into labels --OUTPUT GRADE
selected_column$OUTPUT_GRADE <- factor(selected_column$OUTPUT_GRADE,
                                       levels=c(0,1,2,3,4,5,6,7),
                                       labels = c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA"))

#Factoring the CGPA + Output based on the attributes
selected_column$CGPA_LAST_SEM <- factor(selected_column$CGPA_LAST_SEM,
                                        levels=c(1,2,3,4,5),
                                        labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49"))

str(selected_column)


#Summary
summary(selected_column$WEEKLY_STUDY_HOURS)

WeeklyStudyHours <- table(selected_column$WEEKLY_STUDY_HOURS)

library(dplyr)
library(ggplot2)
#ANALYSIS 3.1(UNIVARIATE) : HORIZONTAL BAR CHART WEEKLY STUDY HOURS
ggplot(selected_column, aes(x = WEEKLY_STUDY_HOURS)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "WEEKLY STUDY HOURS") +
  coord_flip()

print(WeeklyStudyHours)

View(WeeklyStudyHours)

#ANALYSIS 3.2(BIVARIATE) : 1. SEGMENTED BAR CHART WEEKLY STUDY HOURS VS CGPA LAST SEM
selected_column$WEEKLY_STUDY_HOURS <- factor(selected_column$WEEKLY_STUDY_HOURS)

ggplot(selected_column, aes(x = CGPA_LAST_SEM, fill = WEEKLY_STUDY_HOURS)) +
  geom_bar(position = "fill") + geom_text(stat = "count", aes(label = after_stat(count)),
    position = position_fill(vjust = 0.5),
    color = "black"
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    y = "Count",
    fill = "WEEKLY STUDY HOURS VS CGPA LAST SEM",
    x = "Cumulative GPA",
    title = "WEEKLY STUDY HOURS VS CGPA LAST SEM"
  ) +
  theme_bw()
View(WeeklyStudyHours)

table(selected_column$WEEKLY_STUDY_HOURS, selected_column$CGPA_LAST_SEM)

#ANALYSIS 3.2(BIVARIATE) : 2. SEGMENTED BAR CHART WEEKLY STUDY HOURS VS GRADE OUTPUT
ggplot(selected_column, aes(x = OUTPUT_GRADE, fill = WEEKLY_STUDY_HOURS)) +
  geom_bar(position = "fill") + geom_text(stat = "count", aes(label = after_stat(count)),
                                          position = position_fill(vjust = 0.5),
                                          color = "black"
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    y = "Count",
    fill = "WEEKLY STUDY HOURS VS CGPA LAST SEM",
    x = "Cumulative GPA",
    title = "WEEKLY STUDY HOURS VS CGPA LAST SEM"
  ) +
  theme_bw()
View(WeeklyStudyHours)
table(selected_column$WEEKLY_STUDY_HOURS, selected_column$OUTPUT_GRADE)

#ANALYSIS 3.3(MULTIVARIATE) : 
# Create a summary table to aggregate mean OUTPUT_GRADE for each combination of WEEKLY_STUDY_HOURS and PREP_EXAM_FREQUENCY
summary_table <- selected_column %>%
  group_by(WEEKLY_STUDY_HOURS, PREP_EXAM_FREQUENCY) %>%
  summarize(mean_output_grade = mean(as.numeric(OUTPUT_GRADE), na.rm = TRUE))

# Convert WEEKLY_STUDY_HOURS and PREP_EXAM_FREQUENCY to factors for plotting
summary_table$WEEKLY_STUDY_HOURS <- factor(summary_table$WEEKLY_STUDY_HOURS)
summary_table$PREP_EXAM_FREQUENCY <- factor(summary_table$PREP_EXAM_FREQUENCY)

# Plotting grouped bar plot using ggplot2
ggplot(summary_table, aes(x = WEEKLY_STUDY_HOURS, y = mean_output_grade, fill = PREP_EXAM_FREQUENCY)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    y = "Mean Output Grade",
    x = "Weekly Study Hours",
    fill = "Prep Exam Frequency",
    title = "Mean Output Grade by Weekly Study Hours and Prep Exam Frequency"
  ) +
  theme_bw()

table(summary_table$WEEKLY_STUDY_HOURS, summary_table$mean_output_grade, summary_table$PREP_EXAM_FREQUENCY)


#Performing Random Forest on dataset

#slicing necessary column needed 
selected_column1 <- (dataset[,(
  colnames(dataset) %in% 
    c("STUDY_HRS", "LISTENS", 
      "CUML_GPA", "NOTES", "PREP_EXAM", "GRADE"))])

#rename column names 
names(selected_column1) <- c("WEEKLY_STUDY_HOURS", "PREP_EXAM_FREQUENCY", "NOTE_CLASS",
                            "LISTEN_IN_CLASS", "CGPA_LAST_SEM","OUTPUT_GRADE")
head(selected_column1)
View(selected_column1)

# Install and load the randomForest package if not already installed
# install.packages("randomForest")
library(randomForest)

selected_column1$OUTPUT_GRADE <- as.factor(selected_column1$OUTPUT_GRADE)

# Split the data into training and testing sets
set.seed(123)  # Setting a seed for reproducibility
split_index <- sample(1:nrow(selected_column1), 0.7 * nrow(selected_column1))
train_data <- selected_column1[split_index, ]
test_data <- selected_column1[-split_index, ]

# Build the Random Forest model
rf_model <- randomForest(OUTPUT_GRADE ~ ., data = train_data, ntree = 500)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance
conf_matrix <- table(predictions, test_data$OUTPUT_GRADE)
print(conf_matrix)

# You can also check the importance of each variable
var_importance <- importance(rf_model)
print(var_importance)


plot(rf_model)
varImpPlot(rf_model, main = "Variable Importance Plot")
