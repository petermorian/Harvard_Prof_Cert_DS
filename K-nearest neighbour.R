x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x,y)
#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1] #0.76

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"] #0.815 - higher acc than logistic



y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"] #tran acc 0.8825
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"] #test acc 0.815
#fit knn with k=1
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]] #0.99625
#fit knn with k=401
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"] #0.79
#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  tibble(train = train_error, test = test_error)
})
#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)] #41
max(accuracy$test) #0.86


library(caret)
library(dslabs)
data <- heights
set.seed(1)
y <- heights$sex
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

ks <- seq(1, 101, 3)
library(purrr)
f_scores <- sapply(ks, function(k){
  fit <- knn3(sex ~ ., data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = train_set$sex)
  train_error <- F_meas(data = y_hat, reference = test_set$sex)
  y_hat <- predict(fit, test_set, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test_set$sex)
  test_error <- F_meas(data = y_hat, reference = test_set$sex)
  tibble(train = train_error, test = test_error)
})
#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(f_scores[1,])] #46

library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
d<-dist(tissue_gene_expression$x)

set.seed(1)
db <- data.frame(tissue_gene_expression)
y <- db$y
test_index_y <- createDataPartition(y, times = 1, p = 0.5, list = F)
test_set <- tissue_gene_expression[test_index_y]
train_set <- tissue_gene_expression[-test_index_y]

ks = c(1, 3, 5, 7, 9, 11)
accuracies <- sapply(ks, function(k){
  fit <- knn3(y ~ x, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = test_set$y)
  cm_train$overall["Accuracy"]
})





