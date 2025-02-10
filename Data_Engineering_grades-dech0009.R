#=================================================================
# 2024 S1 COMP8031 Data Engineering Source Code
# Student name: Tanapong Dechsakul (dech0009)
# Group name: Good Friday
# Tutorial Class: Friday, 11.00 - 13.00
#=================================================================

# load all the necessary library
library(mongolite)
library(tidyverse)
library(ggplot2) # If required
library(modelr)
library(Metrics)

options(na.action = na.warn) # Setting option to warn when NA value exist.



#========================DATA WRANGLING========================#
# a) Loading the data----------
connection_string = # MongoDB connecting string

grades_collection <- mongo(collection="grades", db="sample_training", url=connection_string)

grades_collection$count() # Check for the number of records 

grades_data <- grades_collection$find() # load the data

view(grades_data) # View data as table



# b) Tidying the data----------
# Expands list-column of the data frame
grades_data <- grades_data %>%
  unnest(scores)

view(grades_data) # View data as table



# c) Handling missing data----------
# c1) Handling missing data ++++++++++
# Check for the number of NA values
grades_data %>%
  map_df(function(grades_data) sum(is.na(grades_data))) %>%
  gather(feature, num_nulls) %>%
  print(n = nrow(grades_data))

grades_data <- grades_data %>% na.omit() # omit N/A value if any

# c2) Handling incorrect or inappropriate data ++++++++++
# Checking the number of rows with a score less than 0 or greater than 100
nrow(grades_data[grades_data$score < 0 | grades_data$score > 100, ]) 

# Check the number of correct score. Accepted score is greater or equal to 0 
# and less than or equal to 100.
grades_data %>%
  filter(score >= 0) %>%
  group_by(check_score = ifelse(score >= 0 & score <= 100, "correct_score", "wrong_score")) %>%
  summarise(total = n())

# Number of student_id less than 0
nrow(grades_data[grades_data$student_id < 0, ])

# Check number of correct student_id. Accepted student_id is greater or equal to 0.
grades_data %>%
  filter(student_id >= 0) %>%
  group_by(check_studentId = ifelse(student_id >= 0, "correct_studentId", "wrong_studentId")) %>%
  summarise(total = n())

# Number of class_id less than 0
nrow(grades_data[grades_data$class_id < 0, ])

# Check number of correct class_id. Accepted class_id is greater or equal to 0.
grades_data %>%
  filter(class_id >= 0) %>%
  group_by(check_classId = ifelse(class_id >= 0, "correct_class", "wrong_class")) %>%
  summarise(total = n())

# Check unique type of scoring
unique(grades_data$type)

# Check the unique classes
unique(grades_data$class_id) 





#========================DATA TRANSFORMATION========================#
# a) Using appropriate data transformation techniques to prepare the data for analysis----------
# Calculating the mean score for every type of work
transform_grades <- grades_data %>%
  group_by(student_id, class_id, type) %>%
  mutate(avg = ifelse(type == type, mean(score), NA))

view(transform_grades)



# b) Creating new variables as needed----------
# Classify the average exam score and inserting the new table
transform_exam <- transform_grades %>%
  select(-score) %>%
  distinct(student_id, class_id, type, avg) %>%
  filter(type == "exam") %>%
  ungroup()

# Change column name to avg_exam
transform_exam <- transform_exam %>% 
  rename(avg_exam = avg) %>%
  select(-type)
view(transform_exam)

# Classify the average quiz score and inserting the new table
transform_quiz <- transform_grades %>%
  select(-score) %>%
  distinct(student_id, class_id, type, avg) %>%
  filter(type == "quiz") %>%
  ungroup()

# Change column name to avg_quiz
transform_quiz <- transform_quiz %>% 
  rename(avg_quiz = avg) %>%
  select(-type)
view(transform_quiz)

# Classify the average homework score and inserting the new table 
transform_homework <- transform_grades %>%
  select(-score) %>%
  distinct(student_id, class_id, type, avg) %>%
  filter(type == "homework")%>%
  ungroup()

# Change column name to avg_homework
transform_homework <- transform_homework %>% 
  rename(avg_homework = avg) %>%
  select(-type)
view(transform_homework)

# Merge all transformed data (exam, quiz, and homework) into one table
transformed_data <- left_join(transform_exam, transform_quiz, by = c("student_id", "class_id"))
transformed_data <- left_join(transformed_data, transform_homework, by = c("student_id", "class_id"))

# Calculate the overall and labeling grades as F, P, CR, DN, and HD
transformed_data <- transformed_data %>%
  group_by(student_id, class_id) %>%
  mutate(overall = ifelse(student_id == student_id & class_id == class_id, (avg_exam + avg_quiz + avg_homework) / 3, NA)) %>%
  mutate(grade = case_when(
    overall < 50 ~ "F",
    overall >= 50 & overall < 65 ~ "P",
    overall >= 65 & overall < 75 ~ "CR",
    overall >= 75 & overall < 85 ~ "DN",
    overall >= 85 ~ "HD",
    TRUE ~ NA_character_
  )) %>%
  ungroup()

view(transformed_data)

# Merge raw data with transformed data, to compare the set of data (if required)
tidy_grades <- grades_data %>%
  pivot_wider(names_from = type, values_from = score) %>%
  ungroup()

merged_data <- left_join(tidy_grades, transformed_data, by = c("student_id", "class_id")) %>%
  ungroup()

view(merged_data)

# The threshold for grading come from 3 score types include exam, quiz, and homework.
# Each score type in different class_id may consist more than 1 value. 
# This can be Assumed that a score type may consist more than one work or one section.
# For example, homework for class_id "0" has 2 values which mean there are 2 homework, 
# or one homework but grading for 2 sections separately. However, we will assume that all weight for
# every score type is equal. Therefore, we could find the average for every score type and combine
# all of them to recalculate for the overall before classifying the grade as F, P, CR, DN, and HD.





#========================DATA ANALYSIS========================#
# a) Using appropriate statistical methods to analyse the data----------
# The number of total students
length(unique(transformed_data$student_id))

# The number of total classes
length(unique(transformed_data$class_id))

# The frequency of student’s grades
transformed_data %>%
  count(grade)

# The number of classes with the maximum number of students
classes_NumStu <- transformed_data %>%
  group_by(class_id) %>%
  summarise(num_students = n_distinct(student_id)) 
classes_NumStu

max_stu_class_idx <- which.max(classes_NumStu$num_students)# Find the index of the class_id with the maximum number of students
classes_NumStu$class_id[max_stu_class_idx] # Get the class_id with the maximum number of students
classes_NumStu$num_students[max_stu_class_idx] # Get the maximum number of student in the class

# The number of classes with the minimum number of students
min_stu_class_idx <- which.min(classes_NumStu$num_students)# Find the index of the class_id with the minimum number of students
classes_NumStu$class_id[min_stu_class_idx] # Get the class_id with the minimum number of students
classes_NumStu$num_students[min_stu_class_idx] # Get the minimum number of student in the class



# b) Data visualisation----------
ggplot(data = transformed_data) + geom_bar(mapping = aes(x = grade)) # Show the grade frequency

# Show min, max and median for the score for every grade
ggplot(data = transformed_data) + 
  stat_summary(
    mapping = aes(x = grade, y = avg_exam),
    fun.min = min,
    fun.max = max,
    fun = median
  )

# Show box plot for every grade
ggplot(data = transformed_data, mapping = aes(x = grade, y = avg_homework)) +
  geom_boxplot()



# c) Applying analysis to the research question----------
# c1) Research question: Does the average score for homework affect the average score for quizzes 
#     and the average score for the examination of the students in class_id = 108? The more specific 
#     question would be; Will the students who have a high average score for homework also have a high 
#     average score on their quizzes and examinations?         

# Show the statistical summary of the average exam score of the students in class_id = 108 refer to their grades
# Filter class_id = 108
class_108 <- transformed_data %>%
  group_by(student_id, class_id) %>%
  filter(class_id == 108)

# Scatter plot showing the relationship between avg_homework and avg_quiz
ggplot(data = class_108, mapping = aes(x = avg_homework, y = avg_quiz, color = "blue")) + 
  geom_point() +
  geom_smooth()

# Scatter plot showing the relationship between avg_homework and avg_exam
ggplot(data = class_108, mapping = aes(x = avg_homework, y = avg_exam, color = "blue")) + 
  geom_point() +
  geom_smooth()

# c2) Research question: How the average exam score impact overall or grade of the students in class_id = 108?
# Show frequency of the average exam score related the grades
ggplot(data = class_108, mapping = aes(x = avg_exam)) +
  geom_freqpoly(mapping = aes(colour = grade), bins = 20)

# Show the average exam scores of all student in class_id = 108 compare with their overall
ggplot(data = class_108, mapping = aes(x = avg_exam, y = overall, color = "blue")) + 
  geom_point() +
  geom_smooth()


  


#========================DATA MODELLING========================#
# Filter data set specific for the class_id = 299
class <- transformed_data %>%
  group_by(student_id, class_id) %>%
  filter(class_id == 299) %>%
  ungroup()

# Mutate student attitude column by considering based on the maximum score of the different work types (avg_exam, avg_quiz, avg_homework)
class <- class %>%
  mutate (
    stu_attitude = case_when(
      avg_exam > avg_quiz & avg_exam > avg_homework ~ "Genius",
      avg_quiz > avg_exam & avg_quiz > avg_homework ~ "Smart",
      avg_homework > avg_exam & avg_homework > avg_quiz ~ "Diligent",
      .default = "Other" ))
view(class)



# a)	Simple linear model----------
ggplot(class, aes(avg_exam, overall)) + 
  geom_point()

  # Fit to linear model
mod <- lm(overall ~ avg_exam, data = class)
coef(mod)

  # Create grid
grid <- class %>% 
  data_grid(avg_exam)

  # Add prediction
grid <- grid %>%
  add_predictions(mod) 

  # Plot the predictions
ggplot(class, aes(avg_exam)) +
  geom_point(aes(y = overall)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)



# b)	General linear model----------
  # bi)	Predictors are categorical +++++
ggplot(class, aes(stu_attitude, overall)) + 
  geom_point()

  # Fit to linear model
mod1 <- lm(overall ~ stu_attitude, data = class)
mod1

  # Create grid & add prediction
grid1 <- class %>%
  data_grid(stu_attitude) %>%
  add_predictions(mod1)
grid1

  # Plot the predictions
ggplot(class, aes(stu_attitude)) +
  geom_point(aes(y = overall)) +
  geom_point(aes(y = pred), data = grid1, colour = "red", size = 4)



  # bii)	Predictors are categorical and continuous +++++
ggplot(class, aes(stu_attitude, avg_exam)) + 
  geom_point(aes(colour = grade))

  # Fit to linear model
mod2_I <- lm(overall ~ avg_exam + stu_attitude, data = class)
mod2_I

mod2_II <- lm(overall ~ avg_exam * stu_attitude, data = class)
mod2_II

  # Create grid & add prediction
grid2 <- class %>% 
  data_grid(avg_exam, stu_attitude) %>% 
  gather_predictions(mod2_I, mod2_II)
grid2

  # Plot the predictions
ggplot(class, aes(avg_exam, overall, colour = stu_attitude)) + 
  geom_point() + 
  geom_line(data = grid2, aes(y = pred)) + 
  facet_wrap(~ model)



  # biii)	Predictors are continuous +++++
ggplot(class, aes(avg_quiz, avg_exam)) + 
  geom_point()

  # Fit to linear model
mod3_I <- lm(overall ~ avg_quiz + avg_exam, data = class)
mod3_I

mod3_II <- lm(overall ~ avg_quiz * avg_exam, data = class)
mod3_II

  # Create grid & add prediction
grid3 <- class %>% 
  data_grid(avg_quiz = seq_range(avg_quiz, 5), 
            avg_exam = seq_range(avg_exam, 5)) %>% 
  gather_predictions(mod3_I, mod3_II)
grid3

  # Plot the predictions
ggplot(grid3, aes(avg_quiz, avg_exam)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

ggplot(grid3, aes(avg_quiz, pred, colour = avg_exam, group = avg_exam)) + 
  geom_line() + 
  facet_wrap(~ model)

ggplot(grid3, aes(avg_exam, pred, colour = avg_quiz, group = avg_quiz)) + 
  geom_line() + 
  facet_wrap(~ model)



# c)	Model evaluation----------
# Evaluating the performance the models a)
pred <- class %>%
  add_predictions(mod)

rmse(pred$overall, pred$pred)

# Evaluating the performance the models bi)
pred1 <- class %>%
  add_predictions(mod1)

rmse(pred1$avg_exam, pred1$pred)

# Evaluating the performance the models bii)
  # Evaluation_1
pred2_I <- class %>%
  add_predictions(mod2_I,var = "mod2_I")

rmse(pred2_I$overall, pred2_I$mod2_I)

  # Evaluation_2
pred2_II <- class %>%
  add_predictions(mod2_II,var = "mod2_II")

rmse(pred2_II$overall, pred2_II$mod2_II)

# Evaluating the performance the models biiI)
  # Evaluation_1
pred3_I <- class %>%
  add_predictions(mod3_I,var = "mod3_I")

rmse(pred3_I$overall, pred3_I$mod3_I)

  # Evaluation_2
pred3_II <- class %>%
  add_predictions(mod3_II,var = "mod3_II")

rmse(pred3_II$overall, pred3_II$mod3_II)



# d)	Model Interpretation----------

# Regarding the results from model evaluation table, the general linear model that predictors 
# are continuous variables (model “iii”, mod3_II) is outperformed other models which result from 
# the least Root Mean Square Error (RMSE) of 7.013367. This linear predictive model can be used 
# to predict the result of the overall of the “class” data set because the predictor variables 
# whether to be “avg_exam”, “avg_quiz”, and “avg_homework” are having the moderate linear relationship 
# with the overall (predicted variable).


