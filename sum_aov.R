rm(list = ls())

# REQUIRED TO REPRODUCE: Install packages readxl, tidyverse and car.
library(readxl)
library(tidyverse)
library(car)

# REQUIRED TO REPRODUCE: Before running set working directory to the folder where you stored "AcademicResults.xlsx"
base_academic=read_xlsx("AcademicResults.xlsx")

#Define Incremental Score variable as the difference between scores in final and initial diagnostic test
base_academic$IS=base_academic$`Diag_2`-base_academic$`Diag_1`

# Define factors for ANOVA 
# Dim states for 3D experience (In Person and Avatars in VR classroom) and 2D experience (Remote to VR classroom and Video Conveference)
base_academic$Group=as.factor(base_academic$Group)
base_academic$Dim=as.factor(base_academic$Dim)

######################################################################################################################################
########### Descriptives for Final Exam Scores (over 10 points) ######################################################################
sum(is.na(base_academic$Exam)) # 4 participants failed to take final exam 
aggregate(Exam ~ Group, data = base_academic, 
          function(x) round(c(length = length(x), mean = mean(x)*10/40, sd = sd(x)*10/40, max =max(x)*10/40, min = min(x)*10/40), 2))
sum(!is.na(base_academic$Exam)) # Number of participants  who took final exam
round(mean(base_academic$Exam, na.rm = TRUE)*10/40, 2) # mean score in final exam
round(sd(base_academic$Exam, na.rm = TRUE)*10/40, 2) # standar deviation for score in final exam
round(max(base_academic$Exam, na.rm = TRUE)*10/40, 2) # maximum score in final exam
round(min(base_academic$Exam, na.rm = TRUE)*10/40, 2) # minimum score in final exam
#######################################################################################################################################

#######################################################################################################################################
########### Descriptives for Incremental Score in diagnostics tests (both over 10 points) #############################################
sum(is.na(base_academic$IS)) # 7 failed to take at least one diagnostic test
aggregate(IS ~ Group, data = base_academic,
          function(x) round(c(length = length(x), mean = mean(x), sd = sd(x), max =max(x), min = min(x)), 2))
sum(!is.na(base_academic$IS)) # Number of participants who took at least one initial and final diagnostic tests
round(mean(base_academic$IS, na.rm = TRUE), 2) # mean incremental Score
round(sd(base_academic$IS, na.rm = TRUE), 2) # standar deviation for incremental Score
round(max(base_academic$IS, na.rm = TRUE), 2) # maximum incremental Score
round(min(base_academic$IS, na.rm = TRUE), 2) # minimum incremental Score
#######################################################################################################################################

#######################################################################################################################################
#ANOVA models models were adjusted considering the data from students who completed the final diagnostic test and the final exam
baseanova=base_academic %>% select(Group, Dim, Diag_1, Diag_2, Exam, IS, Attendance_Requirement) %>% filter(Exam != "NA")
baseanova=baseanova %>% select(Group, Dim, Diag_1, Diag_2, Exam, IS, Attendance_Requirement) %>% filter(IS != "NA")
#######################################################################################################################################

#######################################################################################################################################
#### Following, 8 models adjusted to score in final exam as dependent variable ########################################################
#### Smallest p-value for residual normality test: min(0.05427, 0.05445, 0.029, 0.03915, 0.1075, 0.1236, 0.04598, 0.0648) = 0.029 #####
#### Smallest p-value for homoscedasticity test: min(0.9253, 0.89558, 0.7332, 0.57291, 0.7975, 0.48294, 0.9388, 0.80995) = 0.48294 ####
#### Smallest p-value for Group or Dim significance: min(0.542, 0.553, 0.508, 0.522, 0.398, 0.404, 0.151, 0.157) = 0.151 ##############
#######################################################################################################################################

# Model 1. ANOVA model. Dependent variable: Score in final exam. Regressors: Group. All participants.
# Normal errors at 1%, homoscedasticity at 5%, and Group is not significant at 5%:
aov_exam_1 <- aov(Exam ~ Group, data = baseanova)
#residual normality test
shapiro.test(aov_exam_1$residuals)
#homoscedasticity test
leveneTest(Exam ~ Group,data = baseanova)
aov_exam_1
summary(aov_exam_1)
#######################################################################################################################################

# Model 2. ANCOVA model. Dependent variable: Score in final exam. Regressors: Group + Diag_1. All participants.
# Normal errors at 1%, homoscedasticity at 5%, Group is not significant at 5% and Diag_1 is not significant at 5%:
aov_exam_2<- aov(Exam ~ Group + Diag_1, data = baseanova)
#residual normality test
shapiro.test(aov_exam_2$residuals)
#homoscedasticity test
model_exam_2 <- lm(Exam ~ Group + Diag_1, data = baseanova)
ncvTest(model_exam_2)
aov_exam_2
summary(aov_exam_2)
#######################################################################################################################################

# Model 3. ANOVA model. Dependent variable: Score in final exam. Regressors: Group. Only participants who met attendance requirement.
# Normal errors at 1%, homoscedasticity at 5%, and Group is not significant at 5%:
baseanova_AttYES=baseanova %>% select(Group, Dim, Diag_1, Diag_2, Exam, IS, Attendance_Requirement) %>% filter(Attendance_Requirement== "YES")
aov_exam_3 <- aov(Exam ~ Group, data = baseanova_AttYES)
#residual normality test
shapiro.test(aov_exam_3$residuals)
#homoscedasticity test
leveneTest(Exam ~ Group,data = baseanova_AttYES)
aov_exam_3
summary(aov_exam_3)
#######################################################################################################################################

# Model 4. ANCOVA model. Dependent variable: Score in final exam. Regressors: Group + Diag_1. Only participants who met attendance requirement.
# Normal errors at 1%, homoscedasticity at 5%, Group is not significant at 5% and Diag_1 is not significant at 5%:
aov_exam_4 <- aov(Exam ~ Group + Diag_1, data = baseanova_AttYES)
#residual normality test
shapiro.test(aov_exam_4$residuals)
#homoescedasticity test
model_exam_4 <- lm(Exam ~ Group + Diag_1, data = baseanova_AttYES)
ncvTest(model_exam_4)
aov_exam_4
summary(aov_exam_4)
#######################################################################################################################################

# Model 5. ANOVA model. Dependent variable: Score in final exam. Regressors: Dim. All participants.
# Normal errors at 1%, homoscedasticity at 5%, and Dim is not significant at 5%:
aov_exam_5 <- aov(Exam ~ Dim, data = baseanova)
#residual normality test
shapiro.test(aov_exam_5$residuals)
#homoscedasticity test
leveneTest(Exam ~ Dim,data = baseanova)
aov_exam_5
summary(aov_exam_5)
#######################################################################################################################################

# Model 6. ANCOVA model. Dependent variable: Score in final exam. Regressors: Dim + Diag_1. All participants.
# Normal errors at 1%, homoscedasticity at 5%, Dim is not significant at 5% and Diag_1 is not significant at 5%:
aov_exam_6<- aov(Exam ~ Dim + Diag_1, data = baseanova)
#residual normality test
shapiro.test(aov_exam_6$residuals)
#homoscedasticity test
model_exam_6 <- lm(Exam ~ Dim + Diag_1, data = baseanova)
ncvTest(model_exam_6)
aov_exam_6
summary(aov_exam_6)
#######################################################################################################################################

# Model 7. ANOVA model. Dependent variable: Score in final exam. Regressors: Dim. Only participants who met attendance requirement.
# Normal errors at 1%, homoscedasticity at 5%, and Dim is not significant at 5%:
aov_exam_7 <- aov(Exam ~ Dim, data = baseanova_AttYES)
#residual normality test
shapiro.test(aov_exam_7$residuals)
#homoscedasticity test
leveneTest(Exam ~ Dim, data = baseanova_AttYES)
aov_exam_7
summary(aov_exam_7)
#######################################################################################################################################

# Model 8. ANCOVA model. Dependent variable: Score in final exam. Regressors: Dim + Diag_1. Only participants who met attendance requirement.
# Normal errors at 1%, homoscedasticity at 5%, Dim is not significant at 5% and Diag_1 is not significant at 5%:
aov_exam_8 <- aov(Exam ~ Dim + Diag_1, data = baseanova_AttYES)
#residual normality test
shapiro.test(aov_exam_8$residuals)
#homoescedasticity test
model_exam_8 <- lm(Exam ~ Dim + Diag_1, data = baseanova_AttYES)
ncvTest(model_exam_8)
aov_exam_8
summary(aov_exam_8)
#######################################################################################################################################


#######################################################################################################################################
#### Following, 4 models adjusted to Incremental Score as dependent variable ##########################################################
#### Smallest p-value for residual normality test: min(0.6152, 0.8004, 0.6255, 0.5832) = 0.5832 #######################################
#### Smallest p-value for homoscedasticity test : min(0.3395, 0.1982, 0.7943, 0.7982) = 0.1982 ########################################
#### Smallest p-value for Group or Dim significance: min(0.277, 0.11, 0.244, 0.317) = 0.11 ############################################
#######################################################################################################################################

# Model 9. ANOVA model. Dependent variable: Incremental Score. Regressors: Group. All participants.
# Normal errors at 1%, homoscedasticity at 5%, and Group is not significant at 5%:
aov_IS_9 <- aov(IS ~ Group, data = baseanova)
#residual normality test
shapiro.test(aov_IS_9$residuals)
#homoscedasticity test
leveneTest(IS ~ Group,data = baseanova)
aov_IS_9
summary(aov_IS_9)
#######################################################################################################################################

# Model 10. ANOVA model. Dependent variable: Incremental Score. Regressors: Group. Only participants who met attendance requirement.
# Normal errors at 1%, homoscedasticity at 5%, and Group is not significant at 5%:
aov_IS_10 <- aov(IS ~ Group, data = baseanova_AttYES)
#residual normality test
shapiro.test(aov_IS_10$residuals)
#homoscedasticity test
leveneTest(IS ~ Group,data = baseanova_AttYES)
aov_IS_10
summary(aov_IS_10)
#######################################################################################################################################

# Model 11. ANOVA model. Dependent variable: Incremental Score. Regressors: Dim. All participants.
# Normal errors at 1%, homoscedasticity at 5%, and Dim is not significant at 5%:
aov_IS_11 <- aov(IS ~ Dim, data = baseanova)
#residual normality test
shapiro.test(aov_IS_11$residuals)
#homoscedasticity test
leveneTest(IS ~ Dim,data = baseanova)
aov_IS_11
summary(aov_IS_11)
#######################################################################################################################################

# Model 12. ANOVA model. Dependent variable: Incremental Score. Regressors: Dim. Only participants who met attendance requirement.
# Normal errors at 1%, homoscedasticity at 5%, and Dim is not significant at 5%:
aov_IS_12 <- aov(IS ~ Dim, data = baseanova_AttYES)
#residual normality test
shapiro.test(aov_IS_12$residuals)
#homoscedasticity test
leveneTest(IS ~ Dim, data = baseanova_AttYES)
aov_IS_12
summary(aov_IS_12)
#######################################################################################################################################

