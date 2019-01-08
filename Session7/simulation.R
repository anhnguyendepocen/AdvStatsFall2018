# load libraries
library(tidyverse)

# Set seed
set.seed(10182018)

# Run simulation for binomial distribution

# Set Parameters
p = 0.6 # probability of success
n = 10 # number of trials
rep = 10000 # number of replications

# initialize empty df
df10 <- data_frame(obs=1:n)

for (i in 1:rep) {
  print(i)
  cola <- paste('col', i, sep= '_')
  df10[[cola]] <- rbinom(n, 1, p)
}

head(df10)

# Summarise each sample
means10 <- df10 %>% 
  select(contains("_")) %>% 
  summarise_all(mean) %>%
  t %>%
  as.data.frame

head(means10,10)

# Plot distribution
means10 %>% ggplot(aes(x=V1)) +
  geom_histogram(color='black', binwidth = 0.1)

# Now try with n=100 (p = 0.6, rep = 10000
n1 = 1000 # number of trials

# initialize empty df
df1000 <- data_frame(obs=1:n1)

for (i in 1:rep) {
  print(i)
  cola <- paste('col', i, sep= '_')
  df1000[[cola]] <- rbinom(n, 1, p)
}

head(df1000)

# Summarise each sample (n=100)
means1000 <- df1000 %>% 
  select(contains("_")) %>% 
  summarise_all(mean) %>%
  t %>%
  as.data.frame

head(means1000,10)

# Plot
means1000 %>% ggplot(aes(x=V1)) +
  geom_histogram(color='black', binwidth = 0.1)

# Plot side by side
means <- bind_cols(means10, means1000)
means %>% ggplot() +
  geom_histogram(aes(x=V1), color='red', fill = 'red', alpha = 0.2, binwidth = .1) +
  geom_histogram(aes(x=V11), color='blue', fill = 'blue', alpha = 0.2, binwidth = .1)


