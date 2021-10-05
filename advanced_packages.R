library(prophet)
library(CausalImpact)
library(rstanarm)
library(data.table)

ice_cream_sales =fread("ice_cream.csv")

### 1. Bayesian linear regression with rstanarm

plot(ice_cream_sales )

model_lm_classic=lm(data=ice_cream_sales,ice_creams~daily_temperature)
summary(model_lm_classic)


stan_fit <- stan_lm(ice_creams ~ daily_temperature, 
                    data = data.frame(ice_cream_sales), prior = R2(0.5, "mean"))

summary(stan_fit)
plot(stan_fit, plotfun = "hist")
plot(stan_fit,plotfun = "trace")

as_data_frame(stan_fit)->

posterior


posterior_subset <- sample_n(posterior, size = 20)
ggplot(ice_cream_sales, aes(daily_temperature, ice_creams)) +
  geom_point(color = "forestgreen") +
  geom_abline(aes(intercept = `(Intercept)`, slope = daily_temperature), data = posterior_subset, alpha = 0.5)

### 2. Bayesian generalized linear models with rstanarm


stan_fit <- stan_glm(ice_creams ~ daily_temperature, 
                data = data.frame(ice_cream_sales), family = "poisson")

summary(stan_fit)
plot(stan_fit,plotfun = "hist")
plot(stan_fit,plotfun = "trace")
hist(ice_cream_sales$daily_temperature)

posterior_prediction <- posterior_predict(object=stan_fit, 
                    newdata = data.frame(daily_temperature = c(42,0)))
head(posterior_prediction)
posterior_prediction

vals=0:42
ice_creams_demand <- posterior_predict(object=stan_fit, 
                                       newdata = data.frame(daily_temperature = vals))
colnames(ice_creams_demand) <- vals
head(ice_creams_demand)
colnames(ice_creams_demand)
hist(ice_creams_demand[,'42'])

ice_creams_benefits <- pmin(ice_creams_demand,75) * 2 - 75 * .2

apply(ice_creams_benefits,2,function(x)mean(x>0))


one_scenario = ice_creams_demand[,'42']

purrr::map_dbl(1:200,function(nb_icecreams){
  mean(pmin(one_scenario,nb_icecreams) * 2 - nb_icecreams * .2)
})%>%plot


plot(stan_fit, plotfun = "trace")
launch_shinystan(stan_fit)


#### PROPHET

chicago_daily_crime  = fread("chicago_daily_crime_2009-2019.csv")
chicago_daily_crime = data.frame(chicago_daily_crime)
chicago_daily_crime$Date=as.Date(chicago_daily_crime$Date,format = "%Y-%m-%d")
chicago_daily_crime=rename(chicago_daily_crime,ds=Date,y=n)
chicago_daily_crime$y = log10(chicago_daily_crime$y)

summary(chicago_daily_crime$ds)

# Or if you like %>% pipes
m <- prophet() %>% 
  add_country_holidays(country_name = 'US') %>% 
  fit.prophet(chicago_daily_crime)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
plot(m, forecast)
dyplot.prophet(m, forecast)



future <- make_future_dataframe(m, periods = 365, include_history = FALSE)
pred_post <- predictive_samples(m, future)$yhat


pred_post <- 10^pred_post

plot(pred_post[,1], type = "l", col = "red")
lines(pred_post[, 2], col = "green")
lines(pred_post[, 3], col = "blue")
today_i <- which(as.Date(future$ds) == as.Date(Sys.time()))
reported_crimes_today <- pred_post[today_i, ]
hist(reported_crimes_today, col = "blue")

### 5. Predicting the future that never was with Google’s CausalImpact

stock_prices <- read_csv("stock_prices.csv")
head(stock_prices)
stock_prices <- read.zoo(stock_prices)
autoplot(stock_prices, facet = NULL)

pre.period <- as.Date(c("2011-01-03", "2015-09-14"))
post.period <- as.Date(c("2015-09-21", "2017-03-19"))
# seulement AR(1)
scandal_impact <- CausalImpact(stock_prices[, "volkswagen"], pre.period, post.period)
plot(scandal_impact)
# AR(1) + info des autres séries temporelles
scandal_impact <- CausalImpact(stock_prices, pre.period, post.period)
summary(scandal_impact)
plot(scandal_impact)
summary(scandal_impact, output = "report")
