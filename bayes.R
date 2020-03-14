# Bayes' Rule. This is a theoretical rule because in practice we don't know  𝑝(𝑥)
# Having a good estimate of the  𝑝(𝑥)  will suffice for us to build optimal prediction models,
# since we can control the balance between specificity and sensitivity however we wish. In fact, 
# estimating these conditional probabilities can be thought of as the main challenge of machine learning.


# MSE (Mean Squared Error). The reason why we care about the conditional expectation in machine learning 
# is that the expected value minimizes the MSE:
#  𝑌̂ =𝐸(𝑌|𝑋=𝑥)minimizes𝐸{(𝑌̂ −𝑌)2|𝑋=𝑥} 
#  Due to this property, a succinct description of the main task of machine learning is that we use data to 
# estimate for any set of features. The main way in which competing machine learning algorithms differ is in 
# their approach to estimating this expectation.

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

library(LaplacesDemon)
PDisease <- c(0.02,0.98)
PPositive <- c(0.115, 0.885)
BayesTheorem(PDisease, PPositive)

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

