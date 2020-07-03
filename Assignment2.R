# a)
rm(list = ls(all = TRUE))
dat = read.table('Assignment_2_Titanic_SetA.txt', h = TRUE)

# c)
glm_fit = function(Y, X, beta_start, k = 10)
{
  # Prepare elements for the updating procedure here:
  beta = matrix(beta_start, nrow = length(X[1,]), ncol = 1)
  W    = matrix(0, nrow = length(Y), ncol = length(Y))
  
  # Evaluate the updating equation:
  for(i in 2:k)
  {
        mu = exp(X %*% beta) / (1 + exp(X %*% beta))
   diag(W) = mu * (1 - mu) 
        J  = t(X)%*%W%*%X
        U  = t(X) %*% (Y - mu)
       RHS = J %*% beta + U
      beta = solve(J, RHS)
  }
  # Process some of the results here:
  std_errors = diag(sqrt(solve(J)))
  # Return relevant content:
  return(list(estimates = beta, standard_errors = std_errors))
}

# d)
# Run R's glm and obtain results
dat$Sex = factor(dat$Sex)
dat$Pclass = factor(dat$Pclass)

# relevel - better for the odds ratios
dat$Pclass = relevel(dat$Pclass, "3")
dat$Sex = relevel(dat$Sex, "male")


fit = glm(Survived ~ Sex + Age + Pclass + Fare + Sex*Pclass, family = binomial(link = "logit"), data = dat)
check_estimates  = fit$coefficients
check_std_errors = summary(fit)$coefficients[,2] # standard errors stored in 2nd column of summary output

# Run glm_fit and obtain results
beta_start = rep(0, 6)
Y = dat$Survived
X = model.matrix(fit)
my_output = glm_fit(Y, X, beta_start)
my_estimates  = unlist(my_output["estimates"])
my_std_errors = unlist(my_output["standard_errors"])


# Create table of results
library(knitr)
library(kableExtra)
col_headings = rep(c("Estimates", "Std. Errors"),2)
results_mtx = matrix(c(check_estimates, check_std_errors, my_estimates, my_std_errors), ncol = 4)
colnames(results_mtx) = col_headings
rownames(results_mtx) = names(fit$coefficients)
kable(results_mtx, format = 'latex', booktabs = T, align="c", escape = F, caption = "Verification of Calculated Estimates") %>% kable_styling(latex_options = c("hold_position")) %>% add_header_above(c(" ", "R's glm()" = 2, "My glm_fit()" = 2))

# e)
fitOR = glm(Survived ~ Sex + Age + Pclass + Sex*Pclass, family = binomial(link = "logit"), data = dat)

beta3 = fitOR$coef['Pclass1']
OR1 = exp(beta3)
OR1

beta4 = fitOR$coef['Pclass2']
OR2 = exp(beta4)
OR2

se3 = sqrt(vcov(fitOR)['Pclass1','Pclass1'])
conf1 = exp(beta3 + c(-1,1)*1.96*se3)
conf1

se4 = sqrt(vcov(fitOR)['Pclass2','Pclass2'])
beta4
conf2 = exp(beta4 + c(-1,1)*1.96*se4)
conf2

# f)

# EDA
library(ggplot2)
c(mean(dat$Survived[dat$Sex == 'female']),
(mean(dat$Survived[dat$Sex == 'male'])))

c(mean(dat$Survived[dat$Parch < 2]),
(mean(dat$Survived[dat$Parch >= 2])))

pclass_means = c(mean(dat$Survived[dat$Pclass == 1]),
                 mean(dat$Survived[dat$Pclass == 2]),
                 mean(dat$Survived[dat$Pclass == 3]))
pclass_means

unique(dat$Embarked)
em_means = c(mean(dat$Survived[dat$Embarked== 'S']),
             mean(dat$Survived[dat$Embarked == 'C']),
             mean(dat$Survived[dat$Embarked == 'Q']))
em_means

num50     = length((dat$Age >= 50)[(dat$Age >= 50) == TRUE])
num50
age_means = c(mean(dat$Survived[dat$Age <= 50]), 
              mean(dat$Survived[dat$Age >= 50]))
age_means

# MODEL BUILDING
model_intercept = glm(Survived ~ 1, family = binomial(link = "logit"), data = dat)
model1 = glm(Survived ~ Sex, family = binomial(link = "logit"), data = dat)
anova(model_intercept, model1, test = "Chisq")

model2a =  glm(Survived ~ Sex + Pclass, family = binomial(link = "logit"), data = dat)
model2b =  glm(Survived ~ Sex + Age, family = binomial(link = "logit"), data = dat)
model2c =  glm(Survived ~ Sex + Fare, family = binomial(link = "logit"), data = dat)
model2d =  glm(Survived ~ Sex + Parch, family = binomial(link = "logit"), data = dat)
model2e =  glm(Survived ~ Sex + Embarked, family = binomial(link = "logit"), data = dat)
AIC(model1, model2a, model2b, model2c, model2d, model2e)
anova(model1, model2a, test = "Chisq")
anova(model1, model2b, test = "Chisq")
anova(model1, model2c, test = "Chisq")
anova(model1, model2d, test = "Chisq")
anova(model1, model2e, test = "Chisq")

model2 = model2a
model2i = glm(Survived ~ Sex + Pclass, family = binomial(link = "probit"), data = dat)
model2ii = glm(Survived ~ Sex + Pclass, family = binomial(link = "cloglog"), data = dat)
AIC(model2, model2i, model2ii)
summary(model2)

model3a = glm(Survived ~ Sex + Pclass + Age, family = binomial(link = "logit"), data = dat)
model3b = glm(Survived ~ Sex + Pclass + Fare, family = binomial(link = "logit"), data = dat)
model3c = glm(Survived ~ Sex + Pclass + Parch, family = binomial(link = "logit"), data = dat)
model3d = glm(Survived ~ Sex + Pclass + Embarked, family = binomial(link = "logit"), data = dat)
AIC(model2, model3a, model3b, model3c, model3d)
anova(model2, model3a, test = "Chisq")
anova(model2, model3b, test = "Chisq")
anova(model2, model3c, test = "Chisq")
anova(model2, model3d, test = "Chisq")
model3 = model3a

model4a = glm(Survived ~ Sex + Pclass + Age + Fare, family = binomial(link = "logit"), data = dat)
model4b = glm(Survived ~ Sex + Pclass + Age + Parch, family = binomial(link = "logit"), data = dat)
model4c = glm(Survived ~ Sex + Pclass + Age + Embarked, family = binomial(link = "logit"), data = dat)
AIC(model3, model4a, model4b, model4c)
anova(model3, model4a, test = "Chisq")
anova(model3, model4b, test = "Chisq")
anova(model3, model4c, test = "Chisq")
model4 = model3

model5a = glm(Survived ~ Sex + Pclass + Age + Sex*Pclass, family = binomial(link = "logit"), data = dat)
model5b = glm(Survived ~ Sex + Pclass + Age + Sex*Age, family = binomial(link = "logit"), data = dat)
model5c = glm(Survived ~ Sex + Pclass + Age + Pclass*Age, family = binomial(link = "logit"), data = dat)
AIC(model4, model5a, model5b, model5c)
anova(model4, model5a, test = "Chisq")
anova(model4, model5b, test = "Chisq")
anova(model4, model5c, test = "Chisq")
model5 = model5a

model6a = glm(Survived ~ Sex + Pclass + Age + Sex*Pclass + Sex*Age, family = binomial(link = "logit"), data = dat)
model6b = glm(Survived ~ Sex + Pclass + Age + Sex*Pclass + Pclass*Age, family = binomial(link = "logit"), data = dat)
AIC(model5, model6a, model6b)
anova(model5, model6a, test = "Chisq")
anova(model5, model6b, test = "Chisq")
model6 = model6a

model7a = glm(Survived ~ Sex + Pclass + Age + Sex*Pclass + Sex*Age + Pclass*Age, family = binomial(link = "logit"), data = dat)
AIC(model6, model7a)
anova(model6, model7a, test = "Chisq")
model7 = model6

model8a = glm(Survived ~ Sex + Pclass + Age + Sex*Pclass + Sex*Age, family = binomial(link = "probit"), data = dat)
model8b = glm(Survived ~ Sex + Pclass + Age + Sex*Pclass + Sex*Age, family = binomial(link = "cloglog"), data = dat)
AIC(model7, model8a, model8b)
model8 = model8b

final_model = model8
summary(final_model)

# g) 
# TREE BASED METHODS

library(tree)
# Fit the default model
tree_model = tree(factor(Survived) ~ ., data = dat)
# tree_model

# Plot the tree with text
plot(tree_model)
text(tree_model, pretty = 0)

tree_model

# h)
# PREDICTIONS

glm_model  = glm(Survived ~ Sex + Pclass + Age + Sex*Pclass + Sex*Age, family = binomial(link = "cloglog"), data = dat)
tree_model = tree(factor(Survived) ~ ., data = dat)

test_data = read.table('Assignment_2_Titanic_SetB.txt', h = TRUE)
N         = length(test_data[,1]) 

test_data$Sex    = factor(test_data$Sex)
test_data$Pclass = factor(test_data$Pclass)
test_data$Pclass = relevel(test_data$Pclass, "3")
test_data$Sex    = relevel(test_data$Sex, "male")

# Make GLM predictions
prediction_probs_glm = predict(glm_model, newdata = test_data, type="response")
predictions_glm      = round(prediction_probs_glm)  # change from probabilities to 0s and 1s

# Make Tree predictions
predictions_tree = predict(tree_model, newdata = test_data, type = "class")

# View first 20 predictions vs test set
# cbind(head(test_data$Survived, 20), head(predictions_glm, 20), head(predictions_tree, 20))

# GLM Prediction Accuracy
tf_vector_glm           = test_data$Survived == predictions_glm
# tf_vector_glm         = dat$Survived == round(fitted.values(glm_model)) # fitted values on orig -> 0.797
num_correct_glm         = length(tf_vector_glm[tf_vector_glm == TRUE])
prediction_accuracy_glm = num_correct_glm/N

# Tree Prediction Accuracy
tf_vector_tree           = test_data$Survived == predictions_tree
# tf_vector              = dat$Survived == round(fitted.values(tree_model)) # fitted values on orig -> 0.797
num_correct_tree         = length(tf_vector_tree[tf_vector_tree == TRUE])
prediction_accuracy_tree = num_correct_tree/N

c("GLM Accuracy"  = prediction_accuracy_glm,
  "Tree Accuracy" = prediction_accuracy_tree)

predictions_tree = as.numeric(predictions_tree) - 1
pred = data.frame(cbind(predictions_glm, predictions_tree))
write.table(pred,'Titanic_Pred_WSZSHA001.csv', quote = F, row.names = F, sep = ',')
