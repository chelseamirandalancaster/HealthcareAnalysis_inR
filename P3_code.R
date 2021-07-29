################### Q1A exploratory analysis, clean & convert data types ###################
# load libraries 
library(readr)
library(dplyr)
library(base)
library(ggplot2)
library(qicharts)
library(car)
library(corrgram)
library(EnvStats)
library(fBasics)
library(stats)
library(psych)
library(nnet)

# load data 
data <- read.csv("Acumen_data.csv", header = TRUE)
class(data) # check to ensure we are working with a data frame. 
data$Observation.Number <- NULL ; #View(data) # drops the observation number column 

# rename column headers 
colnames(data) <- c("Quarter", 
                    "EmployeeID", 
                    "Sex(Male=1)", 
                    "Race", 
                    "Age", 
                    "HospitalVisitThisQuarter(Yes=1)", 
                    "Salary", 
                    "HealthScore")

# check structure 
str(data) 
# sex, race, hospital visit need to be factors 

################### CHECK FOR MISSING VALUES ###################

table(is.na(data$Quarter)) # Quarter has no missing values

table(is.na(data$`Sex(Male=1)`)) # Sex has 71 missing values
table(data$`Sex(Male=1)`) # 9396 women 9636 men 
unique(data$`Sex(Male=1)`) # Sex has 0, 1, NA unique values 

table(is.na(data$Race)) # 2123 missing values 
table(data$Race)
data[is.na(data$Race), "Race"] <- 4 # assign values with NA to a 4th race category which will signify NA
unique(data$Race) # 3, 2, 1, 4 unique values  
# no there are no missing values in the Race column 

table(is.na(data$Age)) # Age no missing values
table(is.na(data$`HospitalVisitThisQuarter(Yes=1)`)) # HVTQ has no missing values
table(is.na(data$Salary)) # Salary has no missing values 
table(is.na(data$HealthScore)) # HS has no missing values 

# convert sex, race, HVTQ from integer to factor 
data$`Sex(Male=1)` <- as.factor(data$`Sex(Male=1)`)
data$Race <- as.factor(data$Race)
data$`HospitalVisitThisQuarter(Yes=1)` <- as.factor(data$`HospitalVisitThisQuarter(Yes=1)`)

# remove dollar sign and comma from salary column and coerce to numeric data type 
data$Salary <- as.numeric(gsub("[\\$,]", "", data$Salary))

# check the structure once more 
str(data)# remove dollar sign and comma from salary column and coerce to numeric data type 
data$Salary <- as.numeric(gsub("[\\$,]", "", data$Salary))

# check the structure once more 
str(data)

################### CHECK FOR OUTLIERS ###################

# health score 
unique(data$HealthScore) # there are values over 6 in this data set. 
# subset health score
healthscore.subset <- subset(data, HealthScore > 6.0)
#View(healthscore.subset)
unique(healthscore.subset$HealthScore)
nrow(healthscore.subset)
# 1238 rows with health scores of 10. 
# that is outside our range 0-6 and therefore I will drop these rows.
# subset the data set again to exclude specified rows
data.subset <- subset(data, HealthScore < 7) ; #View(data.subset2)
unique(data.subset$HealthScore)
# check outliers for healthscore (doesn't give min and max)
summary(data.subset$HealthScore) 
# boxplot stats 
boxplot.stats(data.subset$HealthScore)$stats # 33506 is boxplot min and 63069 is boxplot max

# check outliers for age (doesn't give min and max)
summary(data.subset$Age) 
# it appears there are extreme outliers 
# boxplot stats 
boxplot.stats(data.subset$Age)$stats # 17 is boxplot min and 41 is boxplot max
# subset to count extreme values 
under17 <- subset(data.subset, Age < 17) ; #View(under17)
over41 <- subset(data.subset, Age > 41) ; #View(over41)
under17over41 <- rbind(under17, over41) ; #View(under17over41)
nrow(under17over41)
unique(under17over41$Age)
# subset to get rid of values 7, 8, 16, 70, 71, 72, 170, 171, 172
data.subset2 <- subset(data.subset, Age > 16 & Age < 70) ; #View(data.subset)

# check outliers for salary (doesn't give min and max)
summary(data.subset2$Salary) 
# boxplot stats 
boxplot.stats(data.subset2$Salary)$stats # 33506 is boxplot min and 63069 is boxplot max
# subset to count extreme values 
below_salary.min <- subset(data.subset2, Salary < 33506.0)
above_salary.max <- subset(data.subset2, Salary > 63069.0)
outliers.salary <- rbind(below_salary.min, above_salary.max)
#View(outliers.salary)
nrow(outliers.salary)
unique(outliers.salary$Salary)

################### Q1B Characteristics of Employees###################

# ggplot2 boxplot for health score
ggplot(data.subset2) + 
  aes(x = "Spread", y = HealthScore) +
  geom_boxplot(fill = "orange") + 
  ggtitle("Box Plot for Health Score") +
  theme_minimal()
# this appears fine. there are no extreme values here now. 
# pull the statistics from the box plot 
summary(data.subset2$HealthScore)
boxplot.stats(data.subset2$HealthScore)$stats # 0.6 box plot min 6.0 box plot max 

# HEALTH SCORE CHANGE OVER TIME 
# HEALTH SCORE BY QUARTER
# split quarters 1-12 into subset df
quarter1 <- subset(data.subset2, Quarter == 1) ; #View(quarter1)
quarter2 <- subset(data.subset2, Quarter == 2) ; #View(quarter2)
quarter3 <- subset(data.subset2, Quarter == 3) ; #View(quarter3)
quarter4 <- subset(data.subset2, Quarter == 4) ; #View(quarter4)
quarter5 <- subset(data.subset2, Quarter == 5) ; #View(quarter5)
quarter6 <- subset(data.subset2, Quarter == 6) ; #View(quarter6)
quarter7 <- subset(data.subset2, Quarter == 7) ; #View(quarter7)
quarter8 <- subset(data.subset2, Quarter == 8) ; #View(quarter8)
quarter9 <- subset(data.subset2, Quarter == 9) ; #View(quarter9)
quarter10 <- subset(data.subset2, Quarter == 10) ; #View(quarter10)
quarter11 <- subset(data.subset2, Quarter == 11) ; #View(quarter11)
quarter12 <- subset(data.subset2, Quarter == 12) ; #View(quarter12)
# calculate mean from each quarter
mean.q1 <- mean(quarter1$HealthScore)
mean.q2 <- mean(quarter2$HealthScore)
mean.q3 <- mean(quarter3$HealthScore)
mean.q4 <- mean(quarter4$HealthScore)
mean.q5 <- mean(quarter5$HealthScore)
mean.q6 <- mean(quarter6$HealthScore)
mean.q7 <- mean(quarter7$HealthScore)
mean.q8 <- mean(quarter8$HealthScore)
mean.q9 <- mean(quarter9$HealthScore)
mean.q10 <- mean(quarter10$HealthScore)
mean.q11 <- mean(quarter11$HealthScore)
mean.q12 <- mean(quarter12$HealthScore)
# create average df
quarter <- (1:12)
mean_allquarters <- c(mean.q1, mean.q2, mean.q3, mean.q4, mean.q6, mean.q6, 
                      mean.q7, mean.q8, mean.q9, mean.q10, mean.q11, mean.q12)
meanHS.df <- data.frame(quarter, mean_allquarters) ; meanHS.df
# run chart to look at the ave health score per quarter, over 12 quarters
qic(y = mean_allquarters, 
    x = quarter, 
    data = meanHS.df, 
    chart= "run", 
    main = "Ave Health Score for all Employees over 12 Quarters", 
    xlab = "Quarter",
    ylab = "Health Score")
#________________________________________________________________________________

# age box plot 
# ggplot2 boxplot for age
ggplot(data.subset2) + 
  aes(x = "Spread", y = Age) +
  geom_boxplot(fill = "red") + 
  ggtitle("Boxplot for Age") +
  theme_minimal()
# This boxplot is more reasonable. I kept 17 because it remains in the lower whisker of the data.
# pull the statistics from the box plot
summary(data.subset2$Age) 
boxplot.stats(data.subset2$Age)$stats # 17 is boxplot min and 41 is boxplot max

# EMPLOYEES HEALTH SCORE BY AGE OVER 12 QUARTERS
# subset the data for 17-25 year olds
q1.employees17.to25 <- subset(data.subset2, Quarter == 1 & Age < 26) 
q2.employees17.to25 <- subset(data.subset2, Quarter == 2 & Age < 26) 
q3.employees17.to25 <- subset(data.subset2, Quarter == 3 & Age < 26) 
q4.employees17.to25 <- subset(data.subset2, Quarter == 4 & Age < 26) 
q5.employees17.to25 <- subset(data.subset2, Quarter == 5 & Age < 26) 
q6.employees17.to25 <- subset(data.subset2, Quarter == 6 & Age < 26) 
q7.employees17.to25 <- subset(data.subset2, Quarter == 7 & Age < 26) 
q8.employees17.to25 <- subset(data.subset2, Quarter == 8 & Age < 26) 
q9.employees17.to25 <- subset(data.subset2, Quarter == 9 & Age < 26) 
q10.employees17.to25 <- subset(data.subset2, Quarter == 10 & Age < 26) 
q11.employees17.to25 <- subset(data.subset2, Quarter == 11 & Age < 26) 
q12.employees17.to25 <- subset(data.subset2, Quarter == 12 & Age < 26) 
# calculate the mean for 17-25 year olds
q1.mean.employees17.to25 <- mean(q1.employees17.to25$HealthScore)
q2.mean.employees17.to25 <- mean(q2.employees17.to25$HealthScore)
q3.mean.employees17.to25 <- mean(q3.employees17.to25$HealthScore)
q4.mean.employees17.to25 <- mean(q4.employees17.to25$HealthScore)
q5.mean.employees17.to25 <- mean(q5.employees17.to25$HealthScore)
q6.mean.employees17.to25 <- mean(q6.employees17.to25$HealthScore)
q7.mean.employees17.to25 <- mean(q7.employees17.to25$HealthScore)
q8.mean.employees17.to25 <- mean(q8.employees17.to25$HealthScore)
q9.mean.employees17.to25 <- mean(q9.employees17.to25$HealthScore)
q10.mean.employees17.to25 <- mean(q10.employees17.to25$HealthScore)
q11.mean.employees17.to25 <- mean(q11.employees17.to25$HealthScore)
q12.mean.employees17.to25 <- mean(q12.employees17.to25$HealthScore)
# make df for 17-25 year olds
employees17to25.mean_allquarters <- c(q1.mean.employees17.to25, q2.mean.employees17.to25, q3.mean.employees17.to25, 
                                      q4.mean.employees17.to25, q5.mean.employees17.to25, q6.mean.employees17.to25, 
                                      q7.mean.employees17.to25, q8.mean.employees17.to25, q9.mean.employees17.to25,
                                      q10.mean.employees17.to25, q11.mean.employees17.to25, q12.mean.employees17.to25)
employees17to25.df <- data.frame(quarter, employees17to25.mean_allquarters) ; employees17to25.df
# susbet the data for 26-29 year olds
q1.employees26.to29 <- subset(data.subset2, Quarter == 1 & Age > 25 & Age < 30) 
q2.employees26.to29 <- subset(data.subset2, Quarter == 2 & Age > 25 & Age < 30) 
q3.employees26.to29 <- subset(data.subset2, Quarter == 3 & Age > 25 & Age < 30) 
q4.employees26.to29 <- subset(data.subset2, Quarter == 4 & Age > 25 & Age < 30) 
q5.employees26.to29 <- subset(data.subset2, Quarter == 5 & Age > 25 & Age < 30) 
q6.employees26.to29 <- subset(data.subset2, Quarter == 6 & Age > 25 & Age < 30) 
q7.employees26.to29 <- subset(data.subset2, Quarter == 7 & Age > 25 & Age < 30) 
q8.employees26.to29 <- subset(data.subset2, Quarter == 8 & Age > 25 & Age < 30) 
q9.employees26.to29 <- subset(data.subset2, Quarter == 9 & Age > 25 & Age < 30) 
q10.employees26.to29 <- subset(data.subset2, Quarter == 10 & Age > 25 & Age < 30) 
q11.employees26.to29 <- subset(data.subset2, Quarter == 11 & Age > 25 & Age < 30) 
q12.employees26.to29 <- subset(data.subset2, Quarter == 12 & Age > 25 & Age < 30) 
# calculate the mean for 26-29 year olds
q1.mean.employees26.to29 <- mean(q1.employees26.to29$HealthScore)
q2.mean.employees26.to29 <- mean(q2.employees26.to29$HealthScore)
q3.mean.employees26.to29 <- mean(q3.employees26.to29$HealthScore)
q4.mean.employees26.to29 <- mean(q4.employees26.to29$HealthScore)
q5.mean.employees26.to29 <- mean(q5.employees26.to29$HealthScore)
q6.mean.employees26.to29 <- mean(q6.employees26.to29$HealthScore)
q7.mean.employees26.to29 <- mean(q7.employees26.to29$HealthScore)
q8.mean.employees26.to29 <- mean(q8.employees26.to29$HealthScore)
q9.mean.employees26.to29 <- mean(q9.employees26.to29$HealthScore)
q10.mean.employees26.to29 <- mean(q10.employees26.to29$HealthScore)
q11.mean.employees26.to29 <- mean(q11.employees26.to29$HealthScore)
q12.mean.employees26.to29 <- mean(q12.employees26.to29$HealthScore)
# make df for 26-29 year olds
employees26to29.mean_allquarters <- c(q1.mean.employees26.to29, q2.mean.employees26.to29, q3.mean.employees26.to29,
                                      q4.mean.employees26.to29, q5.mean.employees26.to29, q6.mean.employees26.to29,
                                      q7.mean.employees26.to29, q8.mean.employees26.to29, q9.mean.employees26.to29, 
                                      q10.mean.employees26.to29, q11.mean.employees26.to29, q12.mean.employees26.to29)
employees26to29.df <- data.frame(quarter, employees26to29.mean_allquarters) ; employees26to29.df
# subset the data for 30-32 year olds
q1.employees30.to32 <- subset(data.subset2, Quarter == 1 & Age > 29 & Age < 33)
q2.employees30.to32 <- subset(data.subset2, Quarter == 2 & Age > 29 & Age < 33)
q3.employees30.to32 <- subset(data.subset2, Quarter == 3 & Age > 29 & Age < 33)
q4.employees30.to32 <- subset(data.subset2, Quarter == 4 & Age > 29 & Age < 33)
q5.employees30.to32 <- subset(data.subset2, Quarter == 5 & Age > 29 & Age < 33)
q6.employees30.to32 <- subset(data.subset2, Quarter == 6 & Age > 29 & Age < 33)
q7.employees30.to32 <- subset(data.subset2, Quarter == 7 & Age > 29 & Age < 33)
q8.employees30.to32 <- subset(data.subset2, Quarter == 8 & Age > 29 & Age < 33)
q9.employees30.to32 <- subset(data.subset2, Quarter == 9 & Age > 29 & Age < 33)
q10.employees30.to32 <- subset(data.subset2, Quarter == 10 & Age > 29 & Age < 33)
q11.employees30.to32 <- subset(data.subset2, Quarter == 11 & Age > 29 & Age < 33)
q12.employees30.to32 <- subset(data.subset2, Quarter == 12 & Age > 29 & Age < 33)
# calculate the mean for 30-32 year olds
q1.mean.employees30.to32 <- mean(q1.employees30.to32$HealthScore)
q2.mean.employees30.to32 <- mean(q2.employees30.to32$HealthScore)
q3.mean.employees30.to32 <- mean(q3.employees30.to32$HealthScore)
q4.mean.employees30.to32 <- mean(q4.employees30.to32$HealthScore)
q5.mean.employees30.to32 <- mean(q5.employees30.to32$HealthScore)
q6.mean.employees30.to32 <- mean(q6.employees30.to32$HealthScore)
q7.mean.employees30.to32 <- mean(q7.employees30.to32$HealthScore)
q8.mean.employees30.to32 <- mean(q8.employees30.to32$HealthScore)
q9.mean.employees30.to32 <- mean(q9.employees30.to32$HealthScore)
q10.mean.employees30.to32 <- mean(q10.employees30.to32$HealthScore)
q11.mean.employees30.to32 <- mean(q11.employees30.to32$HealthScore)
q12.mean.employees30.to32 <- mean(q12.employees30.to32$HealthScore)
# make df for 30-33 year olds
employees30to32.mean_allquarters <- c(q1.mean.employees30.to32, q2.mean.employees30.to32, q3.mean.employees30.to32, 
                                      q4.mean.employees30.to32, q5.mean.employees30.to32, q6.mean.employees30.to32, 
                                      q7.mean.employees30.to32, q8.mean.employees30.to32, q9.mean.employees30.to32,
                                      q10.mean.employees30.to32, q11.mean.employees30.to32, q12.mean.employees30.to32)
employees30to32.df <- data.frame(quarter, employees30to32.mean_allquarters) ; employees30to32.df
# subset the data for 33-41 year olds
q1.employees33.to41 <- subset(data.subset2, Quarter == 1 & Age > 32 & Age < 42)
q2.employees33.to41 <- subset(data.subset2, Quarter == 2 & Age > 32 & Age < 42)
q3.employees33.to41 <- subset(data.subset2, Quarter == 3 & Age > 32 & Age < 42)
q4.employees33.to41 <- subset(data.subset2, Quarter == 4 & Age > 32 & Age < 42)
q5.employees33.to41 <- subset(data.subset2, Quarter == 5 & Age > 32 & Age < 42)
q6.employees33.to41 <- subset(data.subset2, Quarter == 6 & Age > 32 & Age < 42)
q7.employees33.to41 <- subset(data.subset2, Quarter == 7 & Age > 32 & Age < 42)
q8.employees33.to41 <- subset(data.subset2, Quarter == 8 & Age > 32 & Age < 42)
q9.employees33.to41 <- subset(data.subset2, Quarter == 9 & Age > 32 & Age < 42)
q10.employees33.to41 <- subset(data.subset2, Quarter == 10 & Age > 32 & Age < 42)
q11.employees33.to41 <- subset(data.subset2, Quarter == 11 & Age > 32 & Age < 42)
q12.employees33.to41 <- subset(data.subset2, Quarter == 12 & Age > 32 & Age < 42)
# calculate the mean for 33-41 year olds
q1.mean.employees33.to41 <- mean(q1.employees33.to41$HealthScore)
q2.mean.employees33.to41 <- mean(q2.employees33.to41$HealthScore)
q3.mean.employees33.to41 <- mean(q3.employees33.to41$HealthScore)
q4.mean.employees33.to41 <- mean(q4.employees33.to41$HealthScore)
q5.mean.employees33.to41 <- mean(q5.employees33.to41$HealthScore)
q6.mean.employees33.to41 <- mean(q6.employees33.to41$HealthScore)
q7.mean.employees33.to41 <- mean(q7.employees33.to41$HealthScore)
q8.mean.employees33.to41 <- mean(q8.employees33.to41$HealthScore)
q9.mean.employees33.to41 <- mean(q9.employees33.to41$HealthScore)
q10.mean.employees33.to41 <- mean(q10.employees33.to41$HealthScore)
q11.mean.employees33.to41 <- mean(q12.employees33.to41$HealthScore)
q12.mean.employees33.to41 <- mean(q12.employees33.to41$HealthScore)
# make df for 33-41 year olds
employees33to41.mean_allquarters <- c(q1.mean.employees33.to41, q2.mean.employees33.to41, q3.mean.employees33.to41, 
                                      q4.mean.employees33.to41, q5.mean.employees33.to41, q6.mean.employees33.to41,
                                      q7.mean.employees33.to41, q8.mean.employees33.to41, q9.mean.employees33.to41, 
                                      q10.mean.employees33.to41, q11.mean.employees33.to41, q12.mean.employees33.to41) 
employees33to41.df <- data.frame(quarter, employees33to41.mean_allquarters) ; employees33to41.df
# subset the data for 42 and older
q1.employees42.older <- subset(data.subset2, Quarter == 1 & Age > 41) 
q2.employees42.older <- subset(data.subset2, Quarter == 2 & Age > 41) 
q3.employees42.older <- subset(data.subset2, Quarter == 3 & Age > 41) 
q4.employees42.older <- subset(data.subset2, Quarter == 4 & Age > 41) 
q5.employees42.older <- subset(data.subset2, Quarter == 5 & Age > 41) 
q6.employees42.older <- subset(data.subset2, Quarter == 6 & Age > 41) 
q7.employees42.older <- subset(data.subset2, Quarter == 7 & Age > 41) 
q8.employees42.older <- subset(data.subset2, Quarter == 8 & Age > 41) 
q9.employees42.older <- subset(data.subset2, Quarter == 9 & Age > 41) 
q10.employees42.older <- subset(data.subset2, Quarter == 10 & Age > 41) 
q11.employees42.older <- subset(data.subset2, Quarter == 11 & Age > 41) 
q12.employees42.older <- subset(data.subset2, Quarter == 12 & Age > 41) 
# calculate the mean for 42 and older
q1.mean.employees42.older <- mean(q1.employees42.older$HealthScore)
q2.mean.employees42.older <- mean(q2.employees42.older$HealthScore)
q3.mean.employees42.older <- mean(q3.employees42.older$HealthScore)
q4.mean.employees42.older <- mean(q4.employees42.older$HealthScore)
q5.mean.employees42.older <- mean(q5.employees42.older$HealthScore)
q6.mean.employees42.older <- mean(q6.employees42.older$HealthScore)
q7.mean.employees42.older <- mean(q7.employees42.older$HealthScore)
q8.mean.employees42.older <- mean(q8.employees42.older$HealthScore)
q9.mean.employees42.older <- mean(q9.employees42.older$HealthScore)
q10.mean.employees42.older <- mean(q10.employees42.older$HealthScore)
q11.mean.employees42.older <- mean(q11.employees42.older$HealthScore)
q12.mean.employees42.older <- mean(q12.employees42.older$HealthScore)
# make df for 42 and older
employees42older.mean_allquarters <- c(q1.mean.employees42.older, q2.mean.employees42.older, q3.mean.employees42.older,
                                       q4.mean.employees42.older, q5.mean.employees42.older, q6.mean.employees42.older, 
                                       q7.mean.employees42.older, q8.mean.employees42.older, q9.mean.employees42.older, 
                                       q10.mean.employees42.older, q11.mean.employees42.older, q12.mean.employees42.older)
employees42older.df <- data.frame(quarter, employees42older.mean_allquarters) ; employees42older.df
# PLOT 
# set the plot area so run charts lay side by side 
par(mfcol=c(3,2))
# run chart to look at employees 17-25, over 12 quarters
qic(y = employees17to25.mean_allquarters, 
    x = quarter, 
    data = employees17to25.df, 
    chart= "run", 
    main = "Ave Health Score of Employees 17-25 over 12 Quarters", 
    xlab = "Quarter",
    ylab = "Health Score")
# run chart to look at employees 26-29, over 12 quarters
qic(y = employees26to29.mean_allquarters, 
    x = quarter, 
    data = employees26to29.df, 
    chart= "run", 
    main = "Ave Health Score of Employees 26-29 over 12 Quarters", 
    xlab = "Quarter",
    ylab = "Health Score")
# run chart to look at employees 30-32, over 12 quarters
qic(y = employees30to32.mean_allquarters, 
    x = quarter, 
    data = employees30to32.df, 
    chart= "run", 
    main = "Ave Health Score of Employees 30-32 over 12 Quarters", 
    xlab = "Quarter",
    ylab = "Health Score")
# run chart to look at employees 33-41, over 12 quarters
qic(y = employees33to41.mean_allquarters, 
    x = quarter, 
    data = employees33to41.df, 
    chart= "run", 
    main = "Ave Health Score of Employees 33-41 over 12 Quarters", 
    xlab = "Quarter",
    ylab = "Health Score")
# run chart to look at employees 42 and older, over 12 quarters
qic(y = employees42older.mean_allquarters, 
    x = quarter, 
    data = employees42older.df, 
    chart= "run", 
    main = "Ave Health Score of Employees 42 and Older over 12 Quarters", 
    xlab = "Quarter",
    ylab = "Health Score")
#_________________________________________________________________________________

# ggplot2 boxplot for salary
ggplot(data.subset2) + 
  aes(x = "Spread", y = Salary) +
  geom_boxplot(fill = "green") + 
  ggtitle("Boxplot for Salary") +
  theme_minimal()
# the outliers appear to be reasonable
# pull the statistics from the box plot 
summary(data.subset2$Salary) 
boxplot.stats(data.subset2$Salary)$stats # 33506 is boxplot min and 63069 is boxplot max

# EACH SALARY GROUPS' HEALTH SCORE OVER 12 QUARTERS
# subset the data for salaries < 44540 
quarter1.lessthan44540 <- subset(data.subset2, Quarter == 1 & Salary < 44540) ; #View(quarter1.lessthan44540)
quarter2.lessthan44540 <- subset(data.subset2, Quarter == 2 & Salary < 44540) ; #View(quarter2.lessthan44540)
quarter3.lessthan44540 <- subset(data.subset2, Quarter == 3 & Salary < 44540) ; #View(quarter3.lessthan44540)
quarter4.lessthan44540 <- subset(data.subset2, Quarter == 4 & Salary < 44540) ; #View(quarter4.lessthan44540)
quarter5.lessthan44540 <- subset(data.subset2, Quarter == 5 & Salary < 44540) ; #View(quarter5.lessthan44540)
quarter6.lessthan44540 <- subset(data.subset2, Quarter == 6 & Salary < 44540) ; #View(quarter6.lessthan44540)
quarter7.lessthan44540 <- subset(data.subset2, Quarter == 7 & Salary < 44540) ; #View(quarter7.lessthan44540)
quarter8.lessthan44540 <- subset(data.subset2, Quarter == 8 & Salary < 44540) ; #View(quarter8.lessthan44540)
quarter9.lessthan44540 <- subset(data.subset2, Quarter == 9 & Salary < 44540) ; #View(quarter9.lessthan44540)
quarter10.lessthan44540 <- subset(data.subset2, Quarter == 10 & Salary < 44540) ; #View(quarter10.lessthan44540)
quarter11.lessthan44540 <- subset(data.subset2, Quarter == 11 & Salary < 44540) ; #View(quarter11.lessthan44540)
quarter12.lessthan44540 <- subset(data.subset2, Quarter == 12 & Salary < 44540) ; #View(quarter12.lessthan44540)
# calculate means < 44540 
mean.q1.lessthan44540 <- mean(quarter1.lessthan44540$HealthScore)
mean.q2.lessthan44540 <- mean(quarter2.lessthan44540$HealthScore)
mean.q3.lessthan44540 <- mean(quarter3.lessthan44540$HealthScore)
mean.q4.lessthan44540 <- mean(quarter4.lessthan44540$HealthScore)
mean.q5.lessthan44540 <- mean(quarter5.lessthan44540$HealthScore)
mean.q6.lessthan44540 <- mean(quarter6.lessthan44540$HealthScore)
mean.q7.lessthan44540 <- mean(quarter7.lessthan44540$HealthScore)
mean.q8.lessthan44540 <- mean(quarter8.lessthan44540$HealthScore)
mean.q9.lessthan44540 <- mean(quarter9.lessthan44540$HealthScore)
mean.q10.lessthan44540 <- mean(quarter10.lessthan44540$HealthScore)
mean.q11.lessthan44540 <- mean(quarter11.lessthan44540$HealthScore)
mean.q12.lessthan44540 <- mean(quarter12.lessthan44540$HealthScore)
# make df < 44540 
means_12q_lessthan44540 <- c(mean.q1.lessthan44540, mean.q2.lessthan44540, mean.q3.lessthan44540, 
                             mean.q4.lessthan44540, mean.q5.lessthan44540, mean.q6.lessthan44540, 
                             mean.q7.lessthan44540, mean.q8.lessthan44540, mean.q9.lessthan44540,
                             mean.q10.lessthan44540, mean.q11.lessthan44540, mean.q12.lessthan44540)
df.lessthan44540 <- data.frame(quarter, means_12q_lessthan44540) ; df.lessthan44540
# subset the data for salaries 44540-48177
quarter1.44540to48177 <- subset(data.subset2, Quarter == 1 & Salary > 44539 & Salary < 48178) ; #View(quarter1.44540to48177)
quarter2.44540to48177 <- subset(data.subset2, Quarter == 2 & Salary > 44539 & Salary < 48178) ; #View(quarter2.44540to48177)
quarter3.44540to48177 <- subset(data.subset2, Quarter == 3 & Salary > 44539 & Salary < 48178) ; #View(quarter3.44540to48177)
quarter4.44540to48177 <- subset(data.subset2, Quarter == 4 & Salary > 44539 & Salary < 48178) ; #View(quarter4.44540to48177)
quarter5.44540to48177 <- subset(data.subset2, Quarter == 5 & Salary > 44539 & Salary < 48178) ; #View(quarter5.44540to48177)
quarter6.44540to48177 <- subset(data.subset2, Quarter == 6 & Salary > 44539 & Salary < 48178) ; #View(quarter6.44540to48177)
quarter7.44540to48177 <- subset(data.subset2, Quarter == 7 & Salary > 44539 & Salary < 48178) ; #View(quarter7.44540to48177)
quarter8.44540to48177 <- subset(data.subset2, Quarter == 8 & Salary > 44539 & Salary < 48178) ; #View(quarter8.44540to48177)
quarter9.44540to48177 <- subset(data.subset2, Quarter == 9 & Salary > 44539 & Salary < 48178) ; #View(quarter9.44540to48177)
quarter10.44540to48177 <- subset(data.subset2, Quarter == 10 & Salary > 44539 & Salary < 48178) ; #View(quarter10.44540to48177)
quarter11.44540to48177 <- subset(data.subset2, Quarter == 11 & Salary > 44539 & Salary < 48178) ; #View(quarter11.44540to48177)
quarter12.44540to48177 <- subset(data.subset2, Quarter == 12 & Salary > 44539 & Salary < 48178) ; #View(quarter12.44540to48177)
# calculate means 44540 to 48177
mean.q1.44540to48177 <- mean(quarter1.44540to48177$HealthScore)
mean.q2.44540to48177 <- mean(quarter2.44540to48177$HealthScore)
mean.q3.44540to48177 <- mean(quarter3.44540to48177$HealthScore)
mean.q4.44540to48177 <- mean(quarter4.44540to48177$HealthScore)
mean.q5.44540to48177 <- mean(quarter5.44540to48177$HealthScore)
mean.q6.44540to48177 <- mean(quarter6.44540to48177$HealthScore)
mean.q7.44540to48177 <- mean(quarter7.44540to48177$HealthScore)
mean.q8.44540to48177 <- mean(quarter8.44540to48177$HealthScore)
mean.q9.44540to48177 <- mean(quarter9.44540to48177$HealthScore)
mean.q10.44540to48177 <- mean(quarter10.44540to48177$HealthScore)
mean.q11.44540to48177 <- mean(quarter11.44540to48177$HealthScore)
mean.q12.44540to48177 <- mean(quarter12.44540to48177$HealthScore)
# make df 44540 to 48177
means_12q_44540to48177 <- c(mean.q1.44540to48177, mean.q2.44540to48177, mean.q3.44540to48177, 
                            mean.q4.44540to48177, mean.q5.44540to48177, mean.q6.44540to48177, 
                            mean.q7.44540to48177, mean.q8.44540to48177, mean.q9.44540to48177, 
                            mean.q10.44540to48177, mean.q11.44540to48177, mean.q12.44540to48177)
df.44540to48177 <- data.frame(quarter, means_12q_44540to48177) ; df.44540to48177
# subset the data for salaries 48178-51923
quarter1.48178to51923 <- subset(data.subset2, Quarter == 1 & Salary > 48177 & Salary < 51924) ; #View(quarter1.48178to51923)
quarter2.48178to51923 <- subset(data.subset2, Quarter == 2 & Salary > 48177 & Salary < 51924) ; #View(quarter2.48178to51923)
quarter3.48178to51923 <- subset(data.subset2, Quarter == 3 & Salary > 48177 & Salary < 51924) ; #View(quarter3.48178to51923)
quarter4.48178to51923 <- subset(data.subset2, Quarter == 4 & Salary > 48177 & Salary < 51924) ; #View(quarter4.48178to51923)
quarter5.48178to51923 <- subset(data.subset2, Quarter == 5 & Salary > 48177 & Salary < 51924) ; #View(quarter5.48178to51923)
quarter6.48178to51923 <- subset(data.subset2, Quarter == 6 & Salary > 48177 & Salary < 51924) ; #View(quarter6.48178to51923)
quarter7.48178to51923 <- subset(data.subset2, Quarter == 7 & Salary > 48177 & Salary < 51924) ; #View(quarter7.48178to51923)
quarter8.48178to51923 <- subset(data.subset2, Quarter == 8 & Salary > 48177 & Salary < 51924) ; #View(quarter8.48178to51923)
quarter9.48178to51923 <- subset(data.subset2, Quarter == 9 & Salary > 48177 & Salary < 51924) ; #View(quarter9.48178to51923)
quarter10.48178to51923 <- subset(data.subset2, Quarter == 10 & Salary > 48177 & Salary < 51924) ; #View(quarter10.48178to51923)
quarter11.48178to51923 <- subset(data.subset2, Quarter == 11 & Salary > 48177 & Salary < 51924) ; #View(quarter11.48178to51923)
quarter12.48178to51923 <- subset(data.subset2, Quarter == 12 & Salary > 48177 & Salary < 51924) ; #View(quarter12.48178to51923)
# calculate means 48178-51923 
mean.q1.48178to51923 <- mean(quarter1.48178to51923$HealthScore)
mean.q2.48178to51923 <- mean(quarter2.48178to51923$HealthScore)
mean.q3.48178to51923 <- mean(quarter3.48178to51923$HealthScore)
mean.q4.48178to51923 <- mean(quarter4.48178to51923$HealthScore)
mean.q5.48178to51923 <- mean(quarter5.48178to51923$HealthScore)
mean.q6.48178to51923 <- mean(quarter6.48178to51923$HealthScore)
mean.q7.48178to51923 <- mean(quarter7.48178to51923$HealthScore)
mean.q8.48178to51923 <- mean(quarter8.48178to51923$HealthScore)
mean.q9.48178to51923 <- mean(quarter9.48178to51923$HealthScore)
mean.q10.48178to51923 <- mean(quarter10.48178to51923$HealthScore)
mean.q11.48178to51923 <- mean(quarter11.48178to51923$HealthScore)
mean.q12.48178to51923 <- mean(quarter12.48178to51923$HealthScore)
# make df 48178-51923
means_12q_48178to51923 <- c(mean.q1.48178to51923, mean.q2.48178to51923, mean.q3.48178to51923,
                            mean.q4.48178to51923, mean.q5.48178to51923, mean.q6.48178to51923, 
                            mean.q7.48178to51923, mean.q8.48178to51923, mean.q9.48178to51923,
                            mean.q10.48178to51923, mean.q11.48178to51923, mean.q12.48178to51923)
df.48178to51923 <- data.frame(means_12q_48178to51923) ; df.48178to51923
# subset the data for salaries < 51923 
quarter1.morethan51923 <- subset(data.subset2, Quarter == 1 & Salary > 51923) ; #View(quarter1.morethan51923)
quarter2.morethan51923 <- subset(data.subset2, Quarter == 2 & Salary > 51923) ; #View(quarter2.morethan51923)
quarter3.morethan51923 <- subset(data.subset2, Quarter == 3 & Salary > 51923) ; #View(quarter3.morethan51923)
quarter4.morethan51923 <- subset(data.subset2, Quarter == 4 & Salary > 51923) ; #View(quarter4.morethan51923)
quarter5.morethan51923 <- subset(data.subset2, Quarter == 5 & Salary > 51923) ; #View(quarter5.morethan51923)
quarter6.morethan51923 <- subset(data.subset2, Quarter == 6 & Salary > 51923) ; #View(quarter6.morethan51923)
quarter7.morethan51923 <- subset(data.subset2, Quarter == 7 & Salary > 51923) ; #View(quarter7.morethan51923)
quarter8.morethan51923 <- subset(data.subset2, Quarter == 8 & Salary > 51923) ; #View(quarter8.morethan51923)
quarter9.morethan51923 <- subset(data.subset2, Quarter == 9 & Salary > 51923) ; #View(quarter9.morethan51923)
quarter10.morethan51923 <- subset(data.subset2, Quarter == 10 & Salary > 51923) ; #View(quarter10.morethan51923)
quarter11.morethan51923 <- subset(data.subset2, Quarter == 11 & Salary > 51923) ; #View(quarter11.morethan51923)
quarter12.morethan51923 <- subset(data.subset2, Quarter == 12 & Salary > 51923) ; #View(quarter12.morethan51923)
# calculate means for < 51923
mean.q1.morethan51923 <- mean(quarter1.morethan51923$HealthScore)
mean.q2.morethan51923 <- mean(quarter2.morethan51923$HealthScore)
mean.q3.morethan51923 <- mean(quarter3.morethan51923$HealthScore)
mean.q4.morethan51923 <- mean(quarter4.morethan51923$HealthScore)
mean.q5.morethan51923 <- mean(quarter5.morethan51923$HealthScore)
mean.q6.morethan51923 <- mean(quarter6.morethan51923$HealthScore)
mean.q7.morethan51923 <- mean(quarter7.morethan51923$HealthScore)
mean.q8.morethan51923 <- mean(quarter8.morethan51923$HealthScore)
mean.q9.morethan51923 <- mean(quarter9.morethan51923$HealthScore)
mean.q10.morethan51923 <- mean(quarter10.morethan51923$HealthScore)
mean.q11.morethan51923 <- mean(quarter11.morethan51923$HealthScore)
mean.q12.morethan51923 <- mean(quarter12.morethan51923$HealthScore)
# make df < 51923
means_12q_morethan51923 <- c(mean.q1.morethan51923, mean.q2.morethan51923, mean.q3.morethan51923, 
                             mean.q4.morethan51923, mean.q5.morethan51923, mean.q6.morethan51923, 
                             mean.q7.morethan51923, mean.q8.morethan51923, mean.q9.morethan51923, 
                             mean.q10.morethan51923, mean.q11.morethan51923, mean.q12.morethan51923)
df.morethan51923 <- data.frame(quarter, means_12q_morethan51923) ; df.morethan51923

# PLOT OF EACH SALARY GROUP'S HEALTH SCORE BY QUARTER
# set the plot area so run charts lay side by side 
par(mfcol=c(2,2))
# run chart to look at salaries < 44540 ave health score per quarter, over 12 quarters
qic(y = means_12q_lessthan44540, 
    x = quarter, 
    data = df.lessthan44540, 
    chart= "run", 
    main = "Ave Health Score for Employees w/ Salary < $44,540 by Quarter", 
    xlab = "Quarter",
    ylab = "Health Score")
# run chart to look at salaries 44540-48177 ave health score per quarter, over 12 quarters
qic(y = means_12q_44540to48177, 
    x = quarter, 
    data = df.44540to48177, 
    chart= "run", 
    main = "Ave Health Score for Employees w/ Salary $44,540-$48,177 by Quarter", 
    xlab = "Quarter",
    ylab = "Health Score")
# run chart to look at salaries 48178-51923 ave health score per quarter, over 12 quarters
qic(y = means_12q_48178to51923, 
    x = quarter, 
    data = df.48178to51923, 
    chart= "run", 
    main = "Ave Health Score for Employees w/ Salary $48,178-$51,923 by Quarter", 
    xlab = "Quarter",
    ylab = "Health Score")
# run chart to look at salaries < 51923 ave health score per quarter, over 12 quarters
qic(y = means_12q_morethan51923, 
    x = quarter, 
    data = df.morethan51923, 
    chart= "run", 
    main = "Ave Health Score for Employees w/ Salary < $51,923 by Quarter", 
    xlab = "Quarter",
    ylab = "Health Score")


#__________________________________________________________________________________________

# sex
# calculate % women % men 
table(data.subset2$`Sex(Male=1)`)
prct.women <- 8858/(8858+8980) # 49.7% women 
prct.men <- 8980/(8858+8980) # 50.3% men 
gender <- c("Women", "Men")
prct.gender <- c(prct.women, prct.men)
sex.demographic <- data.frame(gender, prct.gender) ; sex.demographic

# Sex box plot 
plot(data.subset2$HealthScore ~ data.subset2$`Sex(Male=1)`,
     xlab = "Sex (Male=1)",
     ylab = "Health Score",
     main = "Sex Plotted Against Health Score")
# Women look to have slightly lower health scores, which means they appear to have slightly better health. 
#The lower whisker trends towards 0 more than the men's lower whisker. 
# There appear to be no outliers for wommen

# WOMEN'S HEALTH SCORE BY QUARTER
# subset data to look at women's health over 12 quarters 
women.quarter1 <- subset(data.subset2, Quarter == 1 & `Sex(Male=1)` == 0) ; #View(women.quarter1)
women.quarter2 <- subset(data.subset2, Quarter == 2 & `Sex(Male=1)` == 0) ; #View(women.quarter2)
women.quarter3 <- subset(data.subset2, Quarter == 3 & `Sex(Male=1)` == 0) ; #View(women.quarter3)
women.quarter4 <- subset(data.subset2, Quarter == 4 & `Sex(Male=1)` == 0) ; #View(women.quarter4)
women.quarter5 <- subset(data.subset2, Quarter == 5 & `Sex(Male=1)` == 0) ; #View(women.quarter5)
women.quarter6 <- subset(data.subset2, Quarter == 6 & `Sex(Male=1)` == 0) ; #View(women.quarter6)
women.quarter7 <- subset(data.subset2, Quarter == 7 & `Sex(Male=1)` == 0) ; #View(women.quarter7)
women.quarter8 <- subset(data.subset2, Quarter == 8 & `Sex(Male=1)` == 0) ; #View(women.quarter8)
women.quarter9 <- subset(data.subset2, Quarter == 9 & `Sex(Male=1)` == 0) ; #View(women.quarter9)
women.quarter10 <- subset(data.subset2, Quarter == 10 & `Sex(Male=1)` == 0) ; #View(women.quarter10)
women.quarter11 <- subset(data.subset2, Quarter == 11 & `Sex(Male=1)` == 0) ; #View(women.quarter11)
women.quarter12 <- subset(data.subset2, Quarter == 12 & `Sex(Male=1)` == 0) ; #View(women.quarter12)
# calculate womens health score ave for each quarter
W.mean.quarter1 <- mean(women.quarter1$HealthScore)
W.mean.quarter2 <- mean(women.quarter2$HealthScore)
W.mean.quarter3 <- mean(women.quarter3$HealthScore)
W.mean.quarter4 <- mean(women.quarter4$HealthScore)
W.mean.quarter5 <- mean(women.quarter5$HealthScore)
W.mean.quarter6 <- mean(women.quarter6$HealthScore)
W.mean.quarter7 <- mean(women.quarter7$HealthScore)
W.mean.quarter8 <- mean(women.quarter8$HealthScore)
W.mean.quarter9 <- mean(women.quarter9$HealthScore)
W.mean.quarter10 <- mean(women.quarter10$HealthScore)
W.mean.quarter11 <- mean(women.quarter11$HealthScore)
W.mean.quarter12 <- mean(women.quarter12$HealthScore)
# make df 
W.mean_allquarters <- c(W.mean.quarter1, W.mean.quarter2, W.mean.quarter3, W.mean.quarter4, W.mean.quarter5, W.mean.quarter6,
                        W.mean.quarter7, W.mean.quarter8, W.mean.quarter9, W.mean.quarter10, W.mean.quarter11, W.mean.quarter12)
W.meanHS.df <- data.frame(quarter, W.mean_allquarters) ;  W.meanHS.df
# MENS HEALTH SCORE BY QUARTER
# subset data to look at men's health over 12 quarters 
men.quarter1 <- subset(data.subset2, Quarter == 1 & `Sex(Male=1)` == 1) ; #View(men.quarter1)
men.quarter2 <- subset(data.subset2, Quarter == 2 & `Sex(Male=1)` == 1) ; #View(men.quarter2)
men.quarter3 <- subset(data.subset2, Quarter == 3 & `Sex(Male=1)` == 1) ; #View(men.quarter3)
men.quarter4 <- subset(data.subset2, Quarter == 4 & `Sex(Male=1)` == 1) ; #View(men.quarter4)
men.quarter5 <- subset(data.subset2, Quarter == 5 & `Sex(Male=1)` == 1) ; #View(men.quarter5)
men.quarter6 <- subset(data.subset2, Quarter == 6 & `Sex(Male=1)` == 1) ; #View(men.quarter6)
men.quarter7 <- subset(data.subset2, Quarter == 7 & `Sex(Male=1)` == 1) ; #View(men.quarter7)
men.quarter8 <- subset(data.subset2, Quarter == 8 & `Sex(Male=1)` == 1) ; #View(men.quarter8)
men.quarter9 <- subset(data.subset2, Quarter == 9 & `Sex(Male=1)` == 1) ; #View(men.quarter9)
men.quarter10 <- subset(data.subset2, Quarter == 10 & `Sex(Male=1)` == 1) ; #View(men.quarter10)
men.quarter11 <- subset(data.subset2, Quarter == 11 & `Sex(Male=1)` == 1) ; #View(men.quarter11)
men.quarter12 <- subset(data.subset2, Quarter == 12 & `Sex(Male=1)` == 1) ; #View(men.quarter12)
# calculate mens health score ave for each quarter
M.mean.quarter1 <- mean(men.quarter1$HealthScore)
M.mean.quarter2 <- mean(men.quarter2$HealthScore)
M.mean.quarter3 <- mean(men.quarter3$HealthScore)
M.mean.quarter4 <- mean(men.quarter4$HealthScore)
M.mean.quarter5 <- mean(men.quarter5$HealthScore)
M.mean.quarter6 <- mean(men.quarter6$HealthScore)
M.mean.quarter7 <- mean(men.quarter7$HealthScore)
M.mean.quarter8 <- mean(men.quarter8$HealthScore)
M.mean.quarter9 <- mean(men.quarter9$HealthScore)
M.mean.quarter10 <- mean(men.quarter10$HealthScore)
M.mean.quarter11 <- mean(men.quarter11$HealthScore)
M.mean.quarter12 <- mean(men.quarter12$HealthScore)
# make df 
M.mean_allquarters <- c(M.mean.quarter1, M.mean.quarter2, M.mean.quarter3, M.mean.quarter4, M.mean.quarter5, M.mean.quarter6, 
                        M.mean.quarter7, M.mean.quarter8, M.mean.quarter9, M.mean.quarter10, M.mean.quarter11, M.mean.quarter12)
M.meanHS.df <- data.frame(quarter, M.mean_allquarters) ; M.meanHS.df
# PLOT OF WOMENS AND MENS HEALTH SCORE BY QUARTER
# set the plot area so run charts lay side by side 
par(mfcol=c(1,2))
# run chart to look at womens ave health score per quarter, over 12 quarters
qic(y = W.mean_allquarters, 
    x = quarter, 
    data = W.meanHS.df, 
    chart= "run", 
    main = "Ave Health Score for Women at Company A over 12 Quarters", 
    xlab = "Quarter",
    ylab = "Health Score")
# run chart to look at mens ave health score per quarter, over 12 quarters
qic(y = M.mean_allquarters, 
    x = quarter, 
    data = M.meanHS.df, 
    chart= "run", 
    main = "Ave Health Score for Men at Company A over 12 Quarters", 
    xlab = "Quarter",
    ylab = "Health Score")
#________________________________________________________________________________

# calculate % employees who went to hospital this quarter % employees who did not 
table(data.subset2$`HospitalVisitThisQuarter(Yes=1)`)
prct.visithosp <- 1931/(1931+15907) # 11% visited hospital 
prct.novisithosp <- 15907/(1931+15907) # 89% did not visit hospital 
visit <- c("Yes", "No")
prct.visit <- c(prct.visithosp, prct.novisithosp) 
hospitalvisit.df <- data.frame(visit, prct.visit) ; hospitalvisit.df
# HVTQ box plot
plot(data.subset2$HealthScore ~ data.subset2$`HospitalVisitThisQuarter(Yes=1)`, 
     xlab = "Hospital Visit this Quarter (Yes=1)",
     ylab = "Health Score",
     main = "Hospital Visit Plotted Against Health Score")
# those who haven't visited the hospital this quarter appear to have lower health scores. 

# EMPLOYEES'S HEALTH SCORE WHO WENT TO THE HOSPITAL THIS QUARTER OVER 12 QUARTERS
# subset the df
visit.quarter1 <- subset(data.subset2, Quarter == 1 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter1)
visit.quarter2 <- subset(data.subset2, Quarter == 2 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter2)
visit.quarter3 <- subset(data.subset2, Quarter == 3 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter3)
visit.quarter4 <- subset(data.subset2, Quarter == 4 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter4)
visit.quarter5 <- subset(data.subset2, Quarter == 5 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter5)
visit.quarter6 <- subset(data.subset2, Quarter == 6 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter6)
visit.quarter7 <- subset(data.subset2, Quarter == 7 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter7)
visit.quarter8 <- subset(data.subset2, Quarter == 8 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter8)
visit.quarter9 <- subset(data.subset2, Quarter == 9 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter9)
visit.quarter10 <- subset(data.subset2, Quarter == 10 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter10)
visit.quarter11 <- subset(data.subset2, Quarter == 11 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter11)
visit.quarter12 <- subset(data.subset2, Quarter == 12 & `HospitalVisitThisQuarter(Yes=1)` == 1) ; #View(Visit.quarter12)
# calculate the mean 
V.mean.quarter1 <- mean(visit.quarter1$HealthScore)
V.mean.quarter2 <- mean(visit.quarter2$HealthScore)
V.mean.quarter3 <- mean(visit.quarter3$HealthScore)
V.mean.quarter4 <- mean(visit.quarter4$HealthScore)
V.mean.quarter5 <- mean(visit.quarter5$HealthScore)
V.mean.quarter6 <- mean(visit.quarter6$HealthScore)
V.mean.quarter7 <- mean(visit.quarter7$HealthScore)
V.mean.quarter8 <- mean(visit.quarter8$HealthScore)
V.mean.quarter9 <- mean(visit.quarter9$HealthScore)
V.mean.quarter10 <- mean(visit.quarter10$HealthScore)
V.mean.quarter11 <- mean(visit.quarter11$HealthScore)
V.mean.quarter12 <- mean(visit.quarter12$HealthScore)
# make df 
visit.mean_allquarters <- c(V.mean.quarter1, V.mean.quarter2, V.mean.quarter3, V.mean.quarter4, V.mean.quarter5, V.mean.quarter6, 
                            V.mean.quarter7, V.mean.quarter8, V.mean.quarter9, V.mean.quarter10, V.mean.quarter11, V.mean.quarter12)
visit.meanHS.df <- data.frame(quarter, visit.mean_allquarters) ; visit.meanHS.df
# EMPLOYEES'S HEALTH SCORE WHO DID NOT GO TO THE HOSPITAL THIS QUARTER OVER 12 QUARTERS
# subset the df
novisit.quarter1 <- subset(data.subset2, Quarter == 1 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter1)
novisit.quarter2 <- subset(data.subset2, Quarter == 2 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter2)
novisit.quarter3 <- subset(data.subset2, Quarter == 3 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter3)
novisit.quarter4 <- subset(data.subset2, Quarter == 4 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter4)
novisit.quarter5 <- subset(data.subset2, Quarter == 5 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter5)
novisit.quarter6 <- subset(data.subset2, Quarter == 6 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter6)
novisit.quarter7 <- subset(data.subset2, Quarter == 7 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter7)
novisit.quarter8 <- subset(data.subset2, Quarter == 8 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter8)
novisit.quarter9 <- subset(data.subset2, Quarter == 9 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter9)
novisit.quarter10 <- subset(data.subset2, Quarter == 10 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter10)
novisit.quarter11 <- subset(data.subset2, Quarter == 11 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter11)
novisit.quarter12 <- subset(data.subset2, Quarter == 12 & `HospitalVisitThisQuarter(Yes=1)` == 0) ; #View(novisit.quarter12)
# calculate the mean 
NV.mean.quarter1 <- mean(novisit.quarter1$HealthScore)
NV.mean.quarter2 <- mean(novisit.quarter2$HealthScore)
NV.mean.quarter3 <- mean(novisit.quarter3$HealthScore)
NV.mean.quarter4 <- mean(novisit.quarter4$HealthScore)
NV.mean.quarter5 <- mean(novisit.quarter5$HealthScore)
NV.mean.quarter6 <- mean(novisit.quarter6$HealthScore)
NV.mean.quarter7 <- mean(novisit.quarter7$HealthScore)
NV.mean.quarter8 <- mean(novisit.quarter8$HealthScore)
NV.mean.quarter9 <- mean(novisit.quarter9$HealthScore)
NV.mean.quarter10 <- mean(novisit.quarter10$HealthScore)
NV.mean.quarter11 <- mean(novisit.quarter11$HealthScore)
NV.mean.quarter12 <- mean(novisit.quarter12$HealthScore)
# make df
novisit.mean_allquarters <- c(NV.mean.quarter1, NV.mean.quarter2, NV.mean.quarter3, NV.mean.quarter4, NV.mean.quarter5, NV.mean.quarter6, 
                              NV.mean.quarter7, NV.mean.quarter8, NV.mean.quarter9, NV.mean.quarter10, NV.mean.quarter11, NV.mean.quarter12)
novisit.meanHS.df <- data.frame(quarter, novisit.mean_allquarters) ; novisit.meanHS.df

# PLOT OF EMPLOYEES VISIT HOSPITAL RECENTLY AND EMPLOYEES WHO HAVEN'T VISITED HOSPITAL RECENTLY
# set the plot area so run charts lay side by side 
par(mfcol=c(1,2))
# run chart to look at employees with recent hospital visit's ave health score per quarter, over 12 quarters
qic(y = visit.mean_allquarters, 
    x = quarter, 
    data = visit.meanHS.df, 
    chart= "run", 
    main = "Ave Health Score for Employees w/ Recent Hospital Visit", 
    xlab = "Quarter",
    ylab = "Health Score")
# run chart to look at employees without recent hospital visit's ave health score per quarter, over 12 quarters
qic(y = novisit.mean_allquarters, 
    x = quarter, 
    data = novisit.meanHS.df, 
    chart= "run", 
    main = "Ave Health Score for Employees w/out Recent Hospital Visit", 
    xlab = "Quarter",
    ylab = "Health Score")
#__________________________________________________________________________________

# Race box plot 
plot(data.subset2$HealthScore ~ data.subset2$Race, 
     xlab = "Race",
     ylab = "Health Score",
     main = "Race Plotted Against Health Score")
# the different ethnicities seems to have equal spread among them as far as health scores are concerned.

##################### Q2 feature correlation #####################

# look at patterns in the data ; 
data.pairpanel <- pairs.panels(data.subset2, main = "Variable Pair Correlations") ; data.pairpanel

# find the correlations ; pull values from panel  
lowerCor(data.subset2)

# heat maps of the variables 
salary.hm <- ggplot(data.subset2, aes(x=Salary, y = HealthScore)) + 
  geom_bin2d() + 
  ggtitle("Salary vs Health Score Heat Map") + 
  scale_fill_gradient(low = "blue", high = "green") ; salary.hm

age.hm <- ggplot(data.subset2, aes(x=Age, y = HealthScore)) + 
  geom_bin2d() + 
  ggtitle("Age vs Health Score Heat Map") + 
  scale_fill_gradient(low = "yellow", high = "red") ; age.hm

sex.hm <- ggplot(data.subset2, aes(x=`Sex(Male=1)`, y = HealthScore)) + 
  geom_bin2d() + 
  ggtitle("Sex vs Health Score Heat Map") + 
  scale_fill_gradient(low = "orange", high = "cyan") ; sex.hm

race.hm <- ggplot(data.subset2, aes(x=Race, y = HealthScore)) + 
  geom_bin2d() + 
  ggtitle("Race vs Health Score Heat Map") + 
  scale_fill_gradient(low = "pink", high = "purple") ; race.hm

# look at patterns in the data ; 
data.pairpanel <- pairs.panels(data.subset2, main = "Variable Pair Correlations") ; data.pairpanel

# find the correlations ; descriptively, only values 
lowerCor(data.subset2)

##################### Q3 Implementation and evaluation #####################

# row count of each quarter
quarter.count <- c(nrow(quarter1), nrow(quarter2), nrow(quarter3), nrow(quarter4), nrow(quarter5), nrow(quarter6), 
                   nrow(quarter7), nrow(quarter8), nrow(quarter9), nrow(quarter10), nrow(quarter11), nrow(quarter12))
quarter.df <- data.frame(quarter, quarter.count) ; quarter.df

# quarter 1 residual normality check 
lm.quarter1 <- lm(quarter1$HealthScore ~ quarter1$`Sex(Male=1)` + quarter1$Race + quarter1$Age +
                    quarter1$`HospitalVisitThisQuarter(Yes=1)` + quarter1$Salary)
lm.resQ1 <- residuals(lm.quarter1) 
plot(lm.resQ1) # residuals not linear 
qqnorm(lm.resQ1) ; qqline(lm.resQ1, col = "steelblue", lwd = 2) # looks to be right skewed
hist(lm.resQ1, 
     main = "Histogram for Quarter 1 Residuals", 
     xlab = "Quarter 1 Residuals", 
     ylab = "Frequency") # right skewed
boxplot(lm.resQ1, main = "Box Plot for Quarter 1 Residuals", 
        xlab = "Quarter 1 Residuals", 
        ylab = "Frequency") # not symmetric 
shapiro.test(lm.resQ1) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL



# quarter 2 residual normality check 
lm.quarter2 <- lm(quarter2$HealthScore ~ quarter2$`Sex(Male=1)` + quarter2$Race + quarter2$Age +
                    quarter2$`HospitalVisitThisQuarter(Yes=1)` + quarter2$Salary)
lm.resQ2 <- residuals(lm.quarter2) 
plot(lm.resQ2) # residuals not linear 
qqnorm(lm.resQ2) ; qqline(lm.resQ2, col = "steelblue", lwd = 2) # right skewed
hist(lm.resQ2, main = "Histogram for Quarter 2 Residuals", 
     xlab = "Quarter 2 Residuals", 
     ylab = "Frequency") # right skewed
boxplot(lm.resQ2, main = "Box Plot for Quarter 2 Residuals", 
        xlab = "Quarter 2 Residuals", 
        ylab = "Frequency") # not symmetric 
shapiro.test(lm.resQ2) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL

# quarter 3 residual normality check 
lm.quarter3 <- lm(quarter3$HealthScore ~ quarter3$`Sex(Male=1)` + quarter3$Race + quarter3$Age +
                    quarter3$`HospitalVisitThisQuarter(Yes=1)` + quarter3$Salary)
lm.resQ3 <- residuals(lm.quarter3) 
plot(lm.resQ3) # residuals not linear 
qqnorm(lm.resQ3) ; qqline(lm.resQ3, col = "steelblue", lwd = 2) # right skewed
hist(lm.resQ3, main = "Histogram for Quarter 3 Residuals", 
     xlab = "Quarter 3 Residuals", 
     ylab = "Frequency") # looks less right skewed than q1 and q2, still not normal
boxplot(lm.resQ3, main = "Box Plot for Quarter 3 Residuals", 
        xlab = "Quarter 3 Residuals", 
        ylab = "Frequency") # not symmetric 
shapiro.test(lm.resQ3) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL

# quarter 4 residual normality check 
lm.quarter4 <- lm(quarter4$HealthScore ~ quarter4$`Sex(Male=1)` + quarter4$Race + quarter4$Age +
                    quarter4$`HospitalVisitThisQuarter(Yes=1)` + quarter4$Salary)
lm.resQ4 <- residuals(lm.quarter2) 
plot(lm.resQ4) # not linear 
qqnorm(lm.resQ4) ; qqline(lm.resQ4, col = "steelblue", lwd = 2) # right skewed
hist(lm.resQ4, main = "Histogram for Quarter 4 Residuals", 
     xlab = "Quarter 4 Residuals", 
     ylab = "Frequency") # right skewed
boxplot(lm.resQ4, main = "Box Plot for Quarter 4 Residuals", 
        xlab = "Quarter 2 Residuals", 
        ylab = "Frequency") # not symmetric 
shapiro.test(lm.resQ4) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL

# quarter 5 residual normality check 
lm.quarter5 <- lm(quarter5$HealthScore ~ quarter5$`Sex(Male=1)` + quarter5$Race + quarter5$Age +
                    quarter5$`HospitalVisitThisQuarter(Yes=1)` + quarter5$Salary)
lm.resQ5 <- residuals(lm.quarter5) 
plot(lm.resQ5) # not linear 
qqnorm(lm.resQ5) ; qqline(lm.resQ5, col = "steelblue", lwd = 2) # light-tailed, right skewed
hist(lm.resQ5, main = "Histogram for Quarter 5 Residuals", 
     xlab = "Quarter 5 Residuals", 
     ylab = "Frequency") # right skewed
boxplot(lm.resQ5, main = "Box Plot for Quarter 5 Residuals", 
        xlab = "Quarter 5 Residuals", 
        ylab = "Frequency") # not symmetric 
shapiro.test(lm.resQ5) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL

# quarter 6 residual normality check 
lm.quarter6 <- lm(quarter6$HealthScore ~ quarter6$`Sex(Male=1)` + quarter6$Race + quarter6$Age +
                    quarter6$`HospitalVisitThisQuarter(Yes=1)` + quarter6$Salary)
lm.resQ6 <- residuals(lm.quarter6) 
plot(lm.resQ6) # not inear 
qqnorm(lm.resQ6) ; qqline(lm.resQ6, col = "steelblue", lwd = 2) # right skewed
hist(lm.resQ6, main = "Histogram for Quarter 6 Residuals", 
     xlab = "Quarter 6 Residuals", 
     ylab = "Frequency") # right skewed
boxplot(lm.resQ6, main = "Box Plot for Quarter 6 Residuals", 
        xlab = "Quarter 6 Residuals", 
        ylab = "Frequency") # not symmetric 
shapiro.test(lm.resQ6) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL

# quarter 7 residual normality check 
lm.quarter7 <- lm(quarter7$HealthScore ~ quarter7$`Sex(Male=1)` + quarter7$Race + quarter7$Age +
                    quarter7$`HospitalVisitThisQuarter(Yes=1)` + quarter7$Salary)
lm.resQ7 <- residuals(lm.quarter7) 
qqnorm(lm.resQ7) ; qqline(lm.resQ7, col = "steelblue", lwd = 2)
hist(lm.resQ7) # right skewed
boxplot(lm.resQ7) # not symmetric 
shapiro.test(lm.resQ7) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL
# quarter 8 residual normality check 
lm.quarter8 <- lm(quarter8$HealthScore ~ quarter8$`Sex(Male=1)` + quarter8$Race + quarter8$Age +
                    quarter8$`HospitalVisitThisQuarter(Yes=1)` + quarter8$Salary)
lm.resQ8 <- residuals(lm.quarter8) 
qqnorm(lm.resQ8) ; qqline(lm.resQ8, col = "steelblue", lwd = 2)
hist(lm.resQ8) # right skewed
boxplot(lm.resQ8) # not symmetric 
shapiro.test(lm.resQ8) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL
# quarter 9 residual normality check 
lm.quarter9 <- lm(quarter9$HealthScore ~ quarter9$`Sex(Male=1)` + quarter9$Race + quarter9$Age +
                    quarter9$`HospitalVisitThisQuarter(Yes=1)` + quarter9$Salary)
lm.resQ9 <- residuals(lm.quarter9) 
qqnorm(lm.resQ9) ; qqline(lm.resQ9, col = "steelblue", lwd = 2)
hist(lm.resQ9) # right skewed
boxplot(lm.resQ9) # not symmetric 
shapiro.test(lm.resQ9) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL
# quarter 10 residual normality check 
lm.quarter10 <- lm(quarter10$HealthScore ~ quarter10$`Sex(Male=1)` + quarter10$Race + quarter10$Age +
                     quarter10$`HospitalVisitThisQuarter(Yes=1)` + quarter10$Salary)
lm.resQ10 <- residuals(lm.quarter10) 
qqnorm(lm.resQ10) ; qqline(lm.resQ10, col = "steelblue", lwd = 2)
hist(lm.resQ10) # right skewed
boxplot(lm.resQ10) # not symmetric 
shapiro.test(lm.resQ10) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL
# quarter 10 residual normality check 
lm.quarter11 <- lm(quarter11$HealthScore ~ quarter11$`Sex(Male=1)` + quarter11$Race + quarter11$Age +
                     quarter11$`HospitalVisitThisQuarter(Yes=1)` + quarter11$Salary)
lm.resQ11 <- residuals(lm.quarter11) 
qqnorm(lm.resQ11) ; qqline(lm.resQ11, col = "steelblue", lwd = 2)
hist(lm.resQ11) # right skewed
boxplot(lm.resQ11) # not symmetric 
shapiro.test(lm.resQ11) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL
# quarter 12 residual normality check 
lm.quarter12 <- lm(quarter12$HealthScore ~ quarter12$`Sex(Male=1)` + quarter12$Race + quarter12$Age +
                     quarter12$`HospitalVisitThisQuarter(Yes=1)` + quarter12$Salary)
lm.resQ12 <- residuals(lm.quarter12) 
qqnorm(lm.resQ12) ; qqline(lm.resQ12, col = "steelblue", lwd = 2)
hist(lm.resQ12) # right skewed
boxplot(lm.resQ12) # not symmetric 
shapiro.test(lm.resQ12) # reject H0 since pvalue < 0.05 RESIDUALS NOT NORMAL

aov.lm <- aov(data.subset2$HealthScore ~ data.subset2$Quarter)

summary(aov.lm)
