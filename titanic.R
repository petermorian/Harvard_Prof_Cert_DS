library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

#create train/test sets 80/20 split
y <- titanic_clean$Survived
set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean[-test_index,]
nrow(train_set); nrow(test_set); nrow(train_set[train_set$Survived=="1",])/nrow(train_set)

#random survival
set.seed(3)
outcomes <- c(0,1)
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived) # 0.475

#surivial by gender
library(data.table)
train_set[,.N,.(Sex, Survived)] %>% group_by(Sex) %>% mutate(survival_rate = N/sum(N))
set.seed(3)
y_hat_s <- ifelse(test_set$Sex == "male", 0, 1) %>% factor(levels = levels(test_set$Survived))
mean(test_set$Survived == y_hat_s)

#surivial by class
train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))
y_hat_c <- ifelse(test_set$Pclass == 1, 1, 0) %>% factor(levels = levels(test_set$Survived))
mean(test_set$Survived == y_hat_c)

#surivial by sex & class
train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1))
y_hat_sc <- ifelse((test_set$Sex == "female" & test_set$Pclass != 3), 1, 0) %>% factor(levels = levels(test_set$Survived))
mean(test_set$Survived == y_hat_sc)

sensitivity(data = y_hat_s, reference = test_set$Survived)
sensitivity(data = y_hat_c, reference = test_set$Survived)
sensitivity(data = y_hat_sc, reference = test_set$Survived)
specificity(data = y_hat_s, reference = test_set$Survived)
specificity(data = y_hat_c, reference = test_set$Survived)
specificity(data = y_hat_sc, reference = test_set$Survived)
confusionMatrix(data = y_hat_s, reference = test_set$Survived) #0.806
confusionMatrix(data = y_hat_c, reference = test_set$Survived) #0.659
confusionMatrix(data = y_hat_sc, reference = test_set$Survived) #0.771
confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))

F_meas(data = y_hat_s, reference = test_set$Survived) #0.857
F_meas(data = y_hat_c, reference = test_set$Survived) #0.78
F_meas(data = y_hat_sc, reference = test_set$Survived) #0.872

set.seed(1)
train_lda <- train(Survived ~ Fare, method = "lda", data=train_set)
y_hat_lda <- predict(train_lda, test_set)
confusionMatrix(data = y_hat_lda, reference = test_set$Survived)$overall["Accuracy"] #0.693
train_qda <- train(Survived ~ Fare, method = "qda", data=train_set)
y_hat_qda <- predict(train_qda, test_set)
confusionMatrix(data = y_hat_qda, reference = test_set$Survived)$overall["Accuracy"] #0.693

set.seed(1)
train_glm_age <- train(Survived ~ Age, method = "glm", data=train_set)
y_hat_glm_age <- predict(train_glm_age, test_set)
confusionMatrix(data = y_hat_glm_age, reference = test_set$Survived)$overall["Accuracy"] #0.693
set.seed(1)
train_glm_4 <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data=train_set)
y_hat_glm_4 <- predict(train_glm_4, test_set)
confusionMatrix(data = y_hat_glm_4, reference = test_set$Survived)$overall["Accuracy"] #0.849
set.seed(1)
train_glm_all <- train(Survived ~ ., method = "glm", data=train_set)
y_hat_glm_all <- predict(train_glm_all, test_set)
confusionMatrix(data = y_hat_glm_all, reference = test_set$Survived)$overall["Accuracy"] #0.849

set.seed(6)
ks <- seq(3, 51, 2)
train_knn <- train(Survived ~ ., method = "knn", 
                   data = train_set,
                   tuneGrid = data.frame(k = ks)) 
ggplot(train_knn, highlight = TRUE) #k=11
y_hat_knn <- predict(train_knn, test_set, type = "raw")
confusionMatrix(data = y_hat_knn, reference = test_set$Survived)$overall["Accuracy"] #0.709

set.seed(8)
control <- trainControl(method = "cv", number = 10)
train_knn <- train(Survived ~ ., method = "knn", 
                   data = train_set,
                   tuneGrid = data.frame(k = ks),
                   trControl = control) 
ggplot(train_knn, highlight = TRUE) #k=5
y_hat_knn <- predict(train_knn, test_set, type = "raw")
confusionMatrix(data = y_hat_knn, reference = test_set$Survived)$overall["Accuracy"] #0.648

set.seed(10)
cp_vector <- seq(0, 0.05, 0.002)
train_tree <- train(Survived ~ .,
                      method="rpart", 
                      data = train_set,
                      tuneGrid = data.frame(cp = cp_vector))
ggplot(train_tree, highlight = TRUE) #k=0.016 is max accuracy
y_hat_tree <- predict(train_tree, test_set, type = "raw")
confusionMatrix(data = y_hat_tree, reference = test_set$Survived)$overall["Accuracy"] #0.838
plot(train_tree$finalModel, margin = 0.1)
text(train_tree$finalModel, cex = 0.75)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(train_tree$finalModel) #very nice decision tree!

set.seed(14)
mtry_vector <-seq(1,7)
train_rf <- train(Survived ~ .,
                    method="rf", 
                    ntree=100,
                    data = train_set,
                    tuneGrid = data.frame(mtry = mtry_vector))
ggplot(train_rf, highlight = TRUE) #mtry=2 is max accuracy
y_hat_rf <- predict(train_rf, test_set, type = "raw")
confusionMatrix(data = y_hat_rf, reference = test_set$Survived)$overall["Accuracy"] #0.849
imp <- varImp(train_rf)
imp #sexmale is most important 







