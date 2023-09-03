rm(list=ls())

library(ggplot2)
library(dplyr)
library(ggfortify)
library(factoextra)
library(moments)

diabetes <- read.csv("diabetes.csv", header=TRUE)
head(diabetes)
sapply(diabetes[c("age", "bmi", "HbA1c_level", "blood_glucose_level")], summary)

diabetes_numeric <- diabetes %>% 
  select(c("age", "hypertension", "heart_disease", "bmi", "HbA1c_level", "blood_glucose_level"))

pairs(diabetes_numeric, lower.panel = NULL)

par(mfrow=c(2,2)) # Set up 2x2 grid of plots

hist(diabetes$age, main="Age", xlab="Years")
hist(diabetes$bmi, main="BMI", xlab="Value")
hist(diabetes$HbA1c_level, main="HbA1c Level", xlab="mg/dL")
hist(diabetes$blood_glucose_level, main="Blood Glucose Level", xlab="mg/dL")

ggplot(diabetes, aes(x = factor(gender), fill = factor(gender))) +
  geom_bar() +
  labs(title="Sex Distribution", x="Biological Sex", y="Count") +
  scale_fill_manual(name = "Biological Sex", 
                    values = c("#56B4E9", "#E69F00", "#999999"), 
                    labels = c("Male", "Female", "Unknown"))

ggplot(diabetes, aes(x = factor(diabetes), fill = factor(diabetes))) +
  geom_bar() +
  labs(title = "Diagnosed Diabetes", x = "Diabetes Status", y = "Count") +
  scale_fill_manual(name = "Diabetes Status",
                    values = c("#56B4E9", "#E69F00"), 
                    labels = c("Non-diabetic", "Diabetic"))

ggplot(diabetes, aes(x = factor(hypertension), fill = factor(hypertension))) +
  geom_bar() +
  labs(title = "Diagnosed Hypertension", x = "Hypertension", y = "Count") +
  scale_fill_manual(name = "Diagnosed Hypertension", 
                    values = c("#56B4E9", "#E69F00"), 
                    labels = c("No", "Yes"))

ggplot(diabetes, aes(x = factor(heart_disease), fill = factor(heart_disease))) +
  geom_bar() +
  labs(title = "Diagnosed Heart Disease", x = "Heart Disease", y = "Count") +
  scale_fill_manual(name = "Diagnosed Heart Disease",
                    values = c("#56B4E9", "#E69F00"), 
                    labels = c("No", "Yes"))

age_skew <- skewness(diabetes$age)
cat("Skewness of age:", age_skew, "\n")
cat("Absolute skewness of age:", abs(age_skew), "\n")


bmi_skew <- skewness(diabetes$bmi)
cat("Skewness of bmi:", bmi_skew, "\n")
cat("Absolute skewness of bmi:", abs(bmi_skew), "\n")

hba1c_skew <- skewness(diabetes$HbA1c_level)
cat("Skewness of HbA1c level:", hba1c_skew, "\n")
cat("Absolute skewness of HbA1c level:", abs(hba1c_skew), "\n")

glucose_skew <- skewness(diabetes$blood_glucose_level)
cat("Skewness of blood glucose level:", glucose_skew, "\n")
cat("Absolute skewness of blood glucose level:", abs(glucose_skew), "\n")

data_select <- diabetes %>% select(age, bmi, HbA1c_level, blood_glucose_level)

data_scale <- scale(data_select)

pca_model <- prcomp(data_scale, scale. = TRUE)
pca_model

#PC1 is made up mostly of age, bmi. PC2 is made up mostly of A1c, blood glucose. PC3 is made up mostly of a1c, blood glucose, PC4 is made up mostly of age, bmi
summary(pca_model)

pca_var <- get_pca_var(pca_model)
pca_var$contrib[,1]
pca_var$contrib[,2]

fviz_eig(pca_model, addlabels = TRUE)

pca_scores <- as.data.frame(pca_model$x)

pca_scores$outcome <- diabetes$outcome

fviz_pca_biplot(pca_model, geom = c("point", "text"), label = "var", col.var = "red", repel = TRUE)

fviz_pca_var(pca_model)

model <- glm(diabetes~pca_scores$PC1 + pca_scores$PC2 + pca_scores$PC3 + pca_scores$PC4, data = diabetes, family = binomial)

summary(model)

autoplot(
  pca_model, 
  data = diabetes, 
  colour = 'diabetes', 
  loadings=TRUE,
  size = 3, 
  loadings.label = TRUE, 
  loadings.label.size=5
  ) 

autoplot(
  pca_model,
  data = diabetes, 
  colour = 'hypertension', 
  loadings=TRUE, 
  size = 3, 
  loadings.label = TRUE, 
  loadings.label.size=5 
  ) 

autoplot(
  pca_model,
  data = diabetes, 
  colour = 'gender', 
  loadings=TRUE, 
  size = 3, 
  loadings.label = TRUE, 
  loadings.label.size=5 
  ) 

autoplot(
  pca_model, 
  data = diabetes, 
  colour = 'heart_disease', 
  loadings=TRUE, 
  size = 3, 
  loadings.label = TRUE, 
  loadings.label.size=5 
  ) 
