salary_data = data.frame(read.csv('./00_raw_data/ds_salaries.csv'))
View(salary_data)

# Wrangling of Data
# Check for duplicates
sum(duplicated(salary_data))

# Check for Na's
colnames(salary_data)[apply(salary_data, 2, anyNA)]

# Check the data structure
str(salary_data)



# VISUALIZING DATA

summary(salary_data$salary_in_usd)
# The summary of salary in usd gives a strong indication to the distribution of 
# salary in usd in the dataset
# Range: The range of salaries is quite wide. 
# The Salary ranges from 2,859.00 USD to 600,000.00 USD. The average Salary
# is 112,298.00 USD
# 1st Quartile: 25% of the salaries is lower than 62,726.00 USD(1st Quartile)
# 3rd Quartile: 75% of the salaries is higher than 150,000.00 USD(3rd Quartile)
# Skewness: The mean salary($112,298) is greater than the median salary ($101,570)which indicates that it is
# positively skewed, hence it is skewed to the right



library(ggplot2)

# Using bar chart / count plot  to visualize the experience_level

# Convert the dependent variable to a factor / levels  and view the structure
salary_data[,'experience_level'] <- as.factor(salary_data[,'experience_level'])

# Bar  plot
barplot <- ggplot(salary_data, aes(x=experience_level, fill=experience_level)) + geom_bar() +
  labs(title = "Bar plot of Experience Level ", x = "Experience level", y ="Count")
barplot

# Analysis
# This Bar plot depicts the distribution of experience levels among a group of
# individuals. The x-axis represents different experience levels, while the y-axis 
# shows the count of individuals in each category. Here's a breakdown of the information presented:
# EN (Entry): Represented by a coral/pink bar, with approximately 100 individuals.
# EX (Expert): Shown in green, this is the smallest group with about 50 individuals.
# MI (Mid-level): Depicted by a teal bar, this is the second-largest group with about 200 individuals.
# SE (Senior): Represented by a purple bar, this is the largest group with slightly over 200 individuals.
# The plot effectively illustrates the relative proportions of each experience level 
# It appears that mid-level (MI) and senior (SE) experience levels are the most common,
# while expert (EX) level is the least represented in this dataset.

# using box plot to visualize the data between experience_level and salary in usd
boxplot <- ggplot(salary_data, aes(x=experience_level, y=salary_in_usd, fill=experience_level)) + geom_boxplot() + 
  labs(title = "Box Plot of Salary in USD by Experience level", x = "Experience Level", y = "Salary(USD)")
boxplot

# Analysis
# Entry level (EN) has the lowest median salary and the smallest range.
# Expert level (EX) shows the highest median salary and the largest range, indicating more variability in pay.
# Mid-level (MI) and Senior level (SE) fall between EN and EX, with SE having a  higher median than MI.
# All levels show some outliers above their main distributions, particularly noticeable in the EX category.
# There's a clear progression in salary as experience level increases.
# The range of salaries tends to increase with experience level, suggesting more pay variability in higher positions.


# using bar chart /count plot to visualize the company size and salary_in_usd

# Convert the dependent variable to a factor / levels  and view the structure
salary_data[,'company_size'] <- as.factor(salary_data[,'company_size'])

barplot <- ggplot(salary_data, aes(x=company_size, fill=company_size)) + geom_bar() +
  labs(title = "Bar plot of Company Size", x = "Company Size", y ="Frequency")
barplot

# Analysis
# This bar plot shows the distribution of company sizes in a data set. 
# The x-axis represents different company size categories, 
# while the y-axis shows the frequency or count of companies in each category.
# The company sizes are categorized as: L for Large companies
# M for medium companies and S for small companies
# Based on the plot the medium size are the most common, with a frequency 
# of around 300
# Large companies are the second most frequent, with about 200 occurrences
# Small companies (S) are the least common in this data set, with approximately 100 instances.
# The color coding helps distinguish between the categories: 
# Large companies are represented in pink/salmon
# Medium companies are shown in green
# Small companies are depicted in light blue
# This visualization provides a quick and clear overview of the company size distribution
# in the data set, showing that medium-sized companies dominate,
# followed by large companies, with small companies being the least represented.


# using bar plot to visualize the average salary by experience level
library(dplyr)
avg_salary <- salary_data %>%
  group_by(experience_level) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE))

ggplot(avg_salary, aes(x = experience_level, y = avg_salary, fill = experience_level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("$%s", format(round(avg_salary), big.mark = ","))),
            vjust = -0.5, size = 3.5) +
  labs(title = "Average Salary by Experience Level",
       x = "Experience Level",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_brewer(palette = "Blues")


# Using bar chart / count plot  to visualize the employment type

# Convert the dependent variable to a factor / levels  and view the structure
salary_data[,'employment_type'] <- as.factor(salary_data[,'employment_type'])

# Bar  plot
barplot <- ggplot(salary_data, aes(x=employment_type, fill=employment_type)) + geom_bar() +
  labs(title = "Bar plot of Employment Type", x = "Employment Type", y ="Count")
barplot

# Analysis
# This bar plot (also a count plot) shows the distribution of different employment types in a data set. 
# The employment type CT: Likely stands for Contract
# FL: Likely stands for Freelance
# FT: Likely stands for Full-Time
# PT: Likely stands for Part-Time
# From the distribution, FT (Full-Time) is overwhelmingly the most common employment type, with around 600 counts
# The other employment types (CT, FL, and PT) have very low counts, 
# barely visible on the graph compared to FT.
# Looking at the visual representation, Each employment type is represented by a different color bar.
# The y-axis shows the count, ranging from 0 to 600.
# The x-axis lists the different employment types.
# A Key observation is that, There's a stark contrast between full-time employment and all other types,
# suggesting that in this data set, full-time positions are significantly more
# prevalent than contract, freelance, or part-time positions.
# This visualization effectively illustrates the dominance of full-time employment in the 
# data set, with other employment types being comparatively rare.



# using box plot to visualize the data between employment typea and salary in usd
boxplot <- ggplot(salary_data, aes(x=employment_type, y=salary_in_usd, fill=employment_type)) + geom_boxplot() + 
  labs(title = "Box Plot of Salary in USD by Employment Type", x = "Employment Type", y = "Salary(USD)")
boxplot

# Analysis
# This box plot compares salary distributions in USD across different employment types. 
# The y-axis represents salary in USD, ranging from 0 to 6e+05 (600,000).
# Each box represents the interquartile range (IQR) for that employment type.
# From the observations, Full-Time (FT) jobs have the highest median salary and the widest range.
# Contract (CT) positions show the second-highest median and a wide range.
# Freelance (FL) and Part-Time (PT) positions have lower median salaries.
# FT has several high outliers, indicating some very high-paying full-time positions.
# Looking at the comparisons: FT and CT show higher overall salaries compared to FL and PT.
# PT has the narrowest salary range, suggesting less variability.
# FL shows a compressed salary range, but with some high outliers.
# This visualization effectively illustrates how salary distributions vary across different employment types,
# with full-time positions generally offering higher salaries.


# Data visualization on Average Salary by Job Title

# Calculate mean salary by job title
job_salary_summary <- aggregate(salary_in_usd ~ job_title, data = salary_data, FUN = mean)

# Sort by descending salary
job_salary_summary <- job_salary_summary[order(-job_salary_summary$salary_in_usd),]

# Select top 15 job titles to make the plot readable
top_n <- 15
job_salary_top <- head(job_salary_summary, top_n)

ggplot(job_salary_top, aes(x = reorder(job_title, -salary_in_usd), y = salary_in_usd)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Salary by Job Title",
       x = "Job Title",
       y = "Average Salary (USD)") +
  scale_y_continuous(labels = scales::comma)

# Analysis
# This image shows a bar plot of average salaries for various job titles 
# The x-axis represents different job titles, while the y-axis shows the average salary in dollars.
# The y-axis ranges from 0 to $300,000 with increments of $100,000.
# Job titles are arranged in descending order of average salary from left to right.
# The highest-paying job title appears to be "Data Analytics Lead"
# Other high-paying roles include "Principal Data Architect", "Finance Data Analyst", and "Principal Data Scientist".
# The lowest-paying job title among those shown is "Applied Machine Learning Scientist"
# Leadership positions (e.g., "Lead", "Principal") tend to be higher on the salary scale.
# This visualization effectively displays the salary hierarchy among various data-related job titles,
# highlighting the financial value placed on different skills and positions within the industry.



# Further Analysis using Confirmatory Data Analysis

# Chi-Square Test
# H0 = The proportion of full-time employees does not differ significantly across small, medium
# and large companies.
# H1 = The proportion of full-time employees differs significantly across small, medium, and large companies.

chisqtest <- chisq.test(salary_data$employment_type,salary_data$company_size)
chisqtest

# Based on the Chi-squared test, the p-value = 0.3629 is greater than the significant
# level (0.05), This suggests that we fail to reject the null hypothesis. 
# There is not enough evidence to conclude that the proportion of full-time employees differs significantly across small, medium, and large companies.


# Further analysis using Logistic Regression
# H0 = There is no significant difference in salary (in USD) among different employment types (e.g., full-time, part-time, contract).
# H1 = There is a significant difference in salary (in USD) among different employment types.
logistic <- glm(employment_type ~ salary_in_usd, family = "binomial", data = salary_data)
summary(logistic)
# This output shows the results of a logistic regression model where employment_type is predicted by salary_in_usd.
# Model Formula: employment_type ~ salary_in_usd
# Coefficients: Intercept: 5.859e+00 (5.859) salary_in_usd: -7.636e-06 (-0.000007636)
# The negative coefficient for salary_in_usd (-7.636e-06) suggests that as salary increases, 
# the likelihood of being in the reference category of employment_type decreases.
# For each one-unit increase in salary_in_usd, the odds of being in the reference category decrease by 0.000007636.
# From the analysis conducted, the p-value(3.17e-14) is extremely small, indicating strong evidence against the null hypothesis.
# The p-value (3.17e-14) is much smaller than the common significance level of 0.05 (or even 0.01).
# Therefore we reject the null hypothesis and conclude There is a significant difference in salary (in USD) among different employment types.


# Machine Learning

# Load required libraries
library(rpart)
library(rpart.plot)

# Display the first few rows of the dataset
head(salary_data)

# Check the structure of the dataset to understand the data types
str(salary_data)

# Convert categorical variables to factors
salary_data$employment_type <- as.factor(salary_data$employment_type)
salary_data$company_size <- as.factor(salary_data$company_size)
salary_data$experience_level <- as.factor(salary_data$experience_level)
salary_data$employee_residence <- as.factor(salary_data$employee_residence)

# Check the levels of the experience_level factor
levels(salary_data$experience_level)

# Assign numeric levels to each unique experience_level

levels(salary_data$experience_level) = c(1,2,3,4)

# Verify the changes
levels(salary_data$experience_level)


# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(salary_data), size = (0.7 * nrow(salary_data))  , replace = FALSE)
train_data <- salary_data[train_index, ]
test_data <- salary_data[-train_index, ]

# Build the decision tree
# Build the decision decision tree
tree_model <- rpart(experience_level ~ salary_in_usd + company_size + employment_type + work_year+ remote_ratio,
                    data = train_data)


# Print the model
print(tree_model)

# Plot the decision tree with larger plot size and font size
rpart.plot(tree_model,
           extra = 101,  # Show the percentage of observations in each node
           under = TRUE,  # Print the split condition under the node
           tweak = 1.2,  # Expand the size of the tree
           box.palette = "GnBu")  # Use a green-blue color palette

# Predict on the test set
predictions <- predict(tree_model, test_data, type = "class")
predictions

# Evaluate the model
confusion_matrix <- table(Actual = test_data$experience_level, Predicted = predictions)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy*100, 2), "%"))



# this shows which variables are in important or influential in my model
importance <- tree_model$variable.importance
importance_df <- data.frame(feature = names(importance), importance = importance)
importance_df <- importance_df[order(-importance_df$importance),]
print(importance_df)

write.csv(salary_data, file = "salary_data.csv")