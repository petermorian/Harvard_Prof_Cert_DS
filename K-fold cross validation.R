library(caret)
set.seed(1996) #if you are using R 3.5 or earlier
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]

# cross validation
fit <- train(x_subset, y, method = "glm")
fit$results

install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
signif <- tt[tt$p.value<0.01,]
ind <- rownames(signif)
# cross validation
x_subset2 <- x[ ,ind]
fit <- train(x_subset2, y, method = "glm")
fit$results

# using KNN
fit <- train(x_subset2, y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
fit$results
ggplot(fit)


# --- BOOTSTRAP (approximate the monte carlo dist without whole sample)
library(tidyverse)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))
m <- median(income)
m
set.seed(1995)
N <- 250
X <- sample(income, N)
M<- median(X)
M
library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)
mean(M)
sd(M)

B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
}) # run boot assume normal dist
tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline() # compare boot to actual dist
quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1) #CLT CI is wrong
mean(M) + 1.96 * sd(M) * c(-1,1) # actual data CI
mean(M_star) + 1.96 * sd(M_star) * c(-1, 1) #Boot CI is similar to actual

library(dslabs)
set.seed(1995)
mnist_27 <- read_mnist()
data("mnist_27")
indexes <- createResample(mnist_27$train$y, 10)

sum(indexes$Resample01==3)+sum(indexes$Resample02==3)+sum(indexes$Resample03==3)+sum(indexes$Resample04==3)+sum(indexes$Resample05==3)+
  sum(indexes$Resample06==3)+sum(indexes$Resample07==3)+sum(indexes$Resample08==3)+sum(indexes$Resample09==3)+sum(indexes$Resample10==3)
  
y <- rnorm(100, 0, 1)
mu=qnorm(0.75)
mu_hat <- quantile(y, 0.75)
B <- 10^4
set.seed(1)
M_star <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(M_star)
sd(M_star)


set.seed(1)
y <- rnorm(100, 0, 1)
B <- 10^4
set.seed(1)
indexes <- createResample(y, times=B, list=F)
quantiles_vector <- c()
for (i in 1:B) {
  data <- y[indexes[,i]]
  quantiles_vector[i] <- quantile(data, 0.75)
}
mean(quantiles_vector)
sd(quantiles_vector)


