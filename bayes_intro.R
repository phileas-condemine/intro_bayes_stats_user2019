library(magrittr)
library(ggplot2)
### 1. Simulating zombie cure ad clicks

proportion_clicks <- 0.1
n_ads_shown <- 100

# Simulating data
clicked_on_ad <- c()
for(nth_ad_shown in 1:n_ads_shown) {
  clicked_on_ad[nth_ad_shown] <- runif(1, min = 0, max = 1) < proportion_clicks
}
n_clicks <- sum(clicked_on_ad)
n_clicks

rbinom(1000,n_ads_shown,proportion_clicks )%>%hist



### 2. How many clicks will we get? A generative model
n_samples <- 1E+4
n_ads_shown <- 100
proportion_clicks <- .1
n_visitors =rbinom(n_samples,n_ads_shown,proportion_clicks )
n_visitors %>%hist
mean(n_visitors >5)


### 3. How many clicks will we get? Prior uncertainty
n_samples <- 1E+4
n_ads_shown <- 100

proportion_clicks <- runif(n = n_samples, min = 0.0, max = 0.2)
proportion_clicks%>%hist
n_visitors =rbinom(n_samples,n_ads_shown,proportion_clicks )
n_visitors%>%hist
mean(n_visitors >5)

prior = data.frame(proportion_clicks=proportion_clicks,n_visitors=n_visitors)
ggplot(data=prior,aes(x=proportion_clicks,y=n_visitors))+#geom_point()
  stat_density2d()

posterior=prior[prior$n_visitors==9,]

posterior$proportion_clicks %>% hist
prior$proportion_clicks %>% hist
quantile(posterior$proportion_clicks,c(.05,.5,.95))


### Bonus

data <- c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0) # 1 is cured, 0 is not cured

n_samples <- 1E+4
n_ads_shown <- length(data)

proportion_clicks <- runif(n = n_samples, min = 0.0, max = 1)
proportion_clicks%>%hist
n_visitors =rbinom(n_samples,n_ads_shown,proportion_clicks )
n_visitors%>%hist
mean(n_visitors >5)

prior = data.frame(proportion_clicks=proportion_clicks,n_visitors=n_visitors)
ggplot(data=prior,aes(x=proportion_clicks,y=n_visitors))+geom_point()
  # stat_density2d()

posterior=prior[prior$n_visitors==sum(data),]

posterior$proportion_clicks %>% hist
prior$proportion_clicks %>% hist


### Bonus 2 
n_samples=1E+5
n_successes=sum(data)
n_failures=sum(!data)
posterior_prop_success <- rbeta(n_samples, 1 + n_successes, 1 + n_failures)
posterior_prop_success%>%hist


### bonus 3 

source("prop_model.R")

data <- c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
prop_model(data)


big_data <- sample(c(TRUE, FALSE), prob = c(0.75, 0.25),
                   size = 100, replace = TRUE)
posterior <- prop_model(big_data)
