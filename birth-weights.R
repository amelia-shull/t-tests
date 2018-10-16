# Motivaing question:
# Is there an observable difference in baby weight for smokers / non-smokers?

# Install/library the `openintro package
# install.packages('openintro')
library(openintro)
library(dplyr)
library(ggplot2)

# You should now be able to `View` the `births` dataset

# Make an overlapping histogram of the birth weights of smokers/non-smokers
# This is assessing the univariate distribution by a categorical variable
ggplot(births) +
  geom_histogram(mapping = aes(x = weight, fill=smoke))

# What are the mean birth weights for smokers/non-smokers
by_smoker <- births %>% 
  group_by(smoke) %>% 
  summarize(mean_weight = mean(weight))

# Is this difference statistically significant?

# Pre-t-test conceptaul question: is this data paired?

# Store weights from smokers/non-smokers in separate variables for easier access
smokers <- births %>% filter(smoke == "smoker") %>% select(weight)
non_smokers <- births %>% filter(smoke == "nonsmoker") %>% select(weight)


# Implement a t-test to assess the significance of this difference
t.test(smokers, non_smokers)

# Now, let's calculate the t value and confidence intervals manually
diff_means <- mean(smokers$weight) - mean(non_smokers$weight)

se <- sqrt(
  (var(smokers$weight)/length(smokers$weight)) +
    (var(non_smokers$weight)/length(non_smokers$weight))
)

t_score <- diff_means/se

# Compute CIs using t threshold
t <- 2.009 # from t table
ci_lower <- diff_means - t * se
ci_upper <- diff_means + t * se
