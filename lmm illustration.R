# A simple mixed model illustration

# sample sizes
n_individuals <- 10
obs_per_ind <- 20

# generate data
heights <- rnorm(n_individuals, 1.6, 0.3) # true inidividual heights
measures <- rep(heights, each=obs_per_ind) + rnorm(n_individuals*obs_per_ind, sd=0.1) # actual measurements
id <- factor(rep(1:n_individuals, each=obs_per_ind)) # individual identifier
boxplot(measures~id)

# naive (overall) estimates
summary(lm(measures~1))
mean(measures) # overall mean
sd(measures)/sqrt(length(measures)) # overall standard error

# estimates based on individual means
mn_measures <- tapply(measures, id, mean)
mean(mn_measures) # mean
sd(mn_measures)/sqrt(length(mn_measures)) # standard error - higher than above

# glmm
install.packages("lme4")
library(lme4)
mm <- lmer(measures ~ 1 + (1|id)) # (1|id) means individual ID is a random intercept
summary(mm) # estimate SE is same as above based on individual means

banco_de_dados$logmass_kg <- scale(banco_de_dados$logmass_kg)
banco_de_dados$sqrt_rainfall <- scale(banco_de_dados$sqrt_rainfall)
banco_de_dados$log_Lat <- scale(log(abs(banco_de_dados$Lat)))
banco_de_dados$log_Altitude <- scale(log(banco_de_dados$Altitude))

t <- lmer(log(D) ~ method + family +
            logmass_kg + new_environment +
            log_Lat + log_Altitude + 
            sqrt_rainfall + I(sqrt_rainfall^2) + (1|family) + (1|publication), 
          data = banco_de_dados)
summary(t)

t <- lmer(log(D) ~ method + new_environment + (1|family), 
          data = banco_de_dados)
summary(t)





