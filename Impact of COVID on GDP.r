
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r warning = FALSE, message = FALSE}
library(tidyverse)
library(lubridate)

gdp_data <-
  as_tibble(read_csv("project_data.csv"))

gdp_data <- gdp_data %>%
  rename(Country = `Country Name`)
gdp <- gdp_data %>%
  filter(
    Country %in% c('Canada', "United Kingdom", "United States"),
    `Indicator Name` == "Debt to GDP Ratio"
  ) %>%
  dplyr::select(!c(`Indicator Code`, Attribute, `Country Code`, `Indicator Name`))

gdp <-
  gdp %>% gather(key = "Year", value = "Debt to GDP Ratio",-Country)

gdp$Year <- year(as.Date.character(gdp$Year, format = "%Y"))
gdp2 <- na.omit(gdp)%>%
  filter(Year>=1960)
```

#' Did some data cleaning in order to prepare the observations to transformed into time series.
filtered for the three countries of interest and their wanted observation - Debt to GDP Ratio
removed unneeded columns; leaving just the country, year and Debt to GDP Ratio columns remaining.
took all the years columns and transposed them into rows
finally removed any NAs from the data set to avoid any potential errors with the ts function in r
we filtered the years to 1945 and onwards because we believe that several global events that happened before this time that will have extreme effects on the ratio ie) Slavery Abolition, industrial revaluation, WW1, great depression and WW2.

```{r warning = FALSE, message = FALSE}
library(ggplot2)
time_series <- ggplot(gdp2,
                      aes(
                        x = Year,
                        y = `Debt to GDP Ratio`,
                        group =
                          Country,
                        color =
                          Country
                      )) +
  geom_line(aes(color =
                  Country)) + xlab("Year") + ylab("Debt to GDP Ratio")

time_series
```


```{r warning = FALSE, message = FALSE}
library(TSA)
library(tidyverse)
library(forecast)

CA <- gdp2 %>%
  filter(Country == "Canada") %>%
  dplyr::select(!(Country))

CA <- ts(CA$`Debt to GDP Ratio`,start = 1960, end = 2020)

UK <- gdp2 %>%
  filter(Country == "United Kingdom") %>%
  dplyr::select(!(Country))

UK <- ts(UK$`Debt to GDP Ratio`,start = 1960, end = 2020)

US <- gdp2 %>%
  filter(Country == "United States") %>%
  dplyr::select(!(Country))

US <- ts(US$`Debt to GDP Ratio`,start = 1960, end = 2020) 
```

#' A low debt-to-GDP ratio indicates an economy that produces and sells goods and services is sufficient to pay back debts without incurring further debt.

```{r warning = FALSE, message = FALSE}
library(tseries)
plot(CA,
     type = "o",
     ylab = "Debt to GDP Ratio",
     main = "Canada")
CA.line <- lm(CA ~ time(CA))
abline(CA.line)

plot(UK,
     type = "o",
     ylab = "Debt to GDP Ratio",
     main = "United Kingdom")
UK.line <- lm(UK ~ time(UK))
abline(UK.line)

plot(US,
     type = "o",
     ylab = "Debt to GDP Ratio",
     main = "United States")
US.line <- lm(US ~ time(US))
abline(US.line)


myhist <- hist(CA)
multiplier <- myhist$counts / myhist$density
mydensity <- density(CA)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity)

myhist2 <- hist(UK)
multiplier2 <- myhist2$counts / myhist2$density
mydensity2 <- density(UK)
mydensity2$y <- mydensity2$y * multiplier2[1]

plot(myhist2)
lines(mydensity2)

myhist3 <- hist(US)
multiplier3 <- myhist3$counts / myhist3$density
mydensity3 <- density(US)
mydensity3$y <- mydensity3$y * multiplier3[1]

plot(myhist3)
lines(mydensity3)

adf.test(CA, k = 0)
adf.test(UK, k = 0)
adf.test(US, k = 0)
```

#' Canada has slight upwards trend, there is some variation in the, however, it does pass the Augmented Dickey-Fuller Test and Canada dataset is stationary. We will perform some transformations to reduce the present variability 
Canada has a log normal distribution
UK has downwards trend, there is variation in the observations
 US has upwards trend, there is variation in the observations
 UK and US ratios are non stationary and must be transformed before model fitting.


#' Based on the plot above we can see that UK debt to gdp ratio has a downwards overtime and while both the US and Canada have an upwards trend overtime.

the variation in the ratios for all three countries is very apparent and we will need to perform transformations to stabilize the variance and the mean as well as remove the trends that are present.

it is interesting to note that Canada and US have similar Debt to gdp ratio distribution 
explanations for this are strong economic relationship ie (trade and investments) between the two countries, geographical distance (very close and share infrastructure and parks) and very similar social structures. 

```{r warning = FALSE, message = FALSE}
lambda <- BoxCox.lambda(CA)
print(lambda)
```

#' using the box cox method we are able to determine a power transformation needed to stablize the variance and mean.
the power transformation reduced the variation in the data more than the log transformation

after taking the first diff we still see some trend present in the data the second difference is needed to remove this 
```{r warning = FALSE, message = FALSE}
CA.transformed <- diff((((CA))))
plot(CA.transformed, type = 'l', ylab = 'Debt to GDP Ratio')

CA.model <- lm(CA.transformed ~ time(CA.transformed))
abline(CA.model)

eacf(CA.transformed)
acf(CA.transformed)
pacf(CA.transformed)
adf.test(CA.transformed, k = 0)

c<-Arima(CA.transformed, order = c(0,0,1))

c%>%
  residuals() %>% ggtsdisplay()

Box.test(residuals(c),type="Ljung")
shapiro.test(residuals(c))
plot(residuals(c), col = c("blue"), type = "p")
abline(h=0, col="red")
qqnorm(residuals(c))
qqline(residuals(c))

```

```{r}
auto.arima(CA.transformed)
```


#' options
arima(0,1,3) does not pass normality test
arima(2,1,0) does not pass normality test
arima(1,1,2) based on the eacf plot 
log likelihood = 299.17,  aic = -590.35
this is the best choice has meets all the requirements we need for an ideal model

there was still trend the dataset at this point
arima(0,0,3) based on the acf aic = -601.7, log likelihood = 304.85
Shapiro-Wilk normality test p-value = 0.06137
Ljung-Box test p-value = 0.3856
arima(2,0,0) based on the pacf aic = -594.64, log likelihood = 300.32 
Ljung-Box test p-value = 0.1395
Shapiro-Wilk normality test p-value = 0.1078

```{r warning = FALSE, message = FALSE}
lambda1 <- BoxCox.lambda(UK)
print(lambda1)
```

#' the power transformation has a better result on stability of the variance than the log transformation.

# ```{r warning = FALSE, message = FALSE}
# UK.transformed <- diff((diff(log(UK))))
# plot(UK.transformed, type = 'l', ylab = 'Debt to GDP Ratio')
# 
# UK.model <- lm(UK.transformed ~ time(UK.transformed))
# abline(UK.model)
# 
# ```
#the power transformation reduced the variation in the data more than the log transformation note this is a small difference, however, for our purpose the power transformation is the best choice

we need to take the second diff to reduce the present trend even more so.

```{r warning = FALSE, message = FALSE}

UK.transformed <- diff((diff((UK)^2)))
plot(UK.transformed, type = 'l', ylab = 'Debt to GDP Ratio')

UK.model <- lm(UK.transformed ~ time(UK.transformed))
abline(UK.model)

adf.test(UK.transformed, k = 0)

eacf(UK.transformed)
acf(UK.transformed)
pacf(UK.transformed)

k<-Arima(UK.transformed, order = c(0,0,0))

k%>%
  residuals() %>% ggtsdisplay()

Box.test(residuals(k),type="Ljung")
shapiro.test(residuals(k))
plot(residuals(k), col = c("blue"), type = "p")
abline(h=0, col="red")
qqnorm(residuals(k))
qqline(residuals(k))
```

```{r}
auto.arima(UK.transformed)
```


```{r warning = FALSE, message = FALSE}
lambda2 <- BoxCox.lambda(US)
print(lambda2)
```

```{r warning = FALSE, message = FALSE}
# US.transformed <- diff((((US^lambda2))))
# plot(US.transformed, type = 'l', ylab = 'Debt to GDP Ratio')
# 
# US.model <- lm(US.transformed ~ time(US.transformed))
# abline(US.model)


US.transformed2 <- diff(diff(sqrt(US^lambda2)))

plot(US.transformed2, type = 'l', ylab = 'Debt to GDP Ratio')

US.model2 <- lm(US.transformed2 ~ time(US.transformed2))
abline(US.model2)


adf.test((US.transformed2), k = 0)

eacf(US.transformed2)
acf(US.transformed2)
pacf(US.transformed2)

s<-Arima(US.transformed2, order = c(0,0,1))

s%>%
  residuals() %>% ggtsdisplay()

Box.test(residuals(s),type="Ljung")
shapiro.test(residuals(s))
plot(residuals(s), col = c("blue"), type = "p")
abline(h=0, col="red")
qqnorm(residuals(s))
qqline(residuals(s))


#' the residual has right skewed distribution 
```

```{r}
s
```


```{r}
auto.arima(US.transformed2)
```


#' the log transformation was able to reduce the reduce the variability in data much more than the power transformation

based on eacf plot the best model is arima(0,2,1)



# ```{r warning = FALSE, message = FALSE}
# US.transformed <- diff(((US)^0.5))
# plot(US.transformed, type = 'l', ylab = 'Debt to GDP Ratio')
# 
# US.model <- lm(US.transformed ~ time(US.transformed))
# abline(US.model)
# 
# ```

#' the power transformation did not handle the variation in data as well as the log transformation did




```{r warning = FALSE, message = FALSE}
library(tseries)
adf.test(US.transformed, k = 0)
adf.test(CA.transformed, k = 0)
adf.test(UK.transformed, k = 0)
```


```{r warning = FALSE, message = FALSE}

eacf(CA.transformed)
acf(CA.transformed)
pacf(CA.transformed)


```
#' based on the results of the acf, pacf and eacf there are few possible models we can use here.
ARIMA(1, 1, 2)
ARIMA(2, 1, 3)
ARIMA(3, 1, 3)
the final model we use will be based on the model that provides the best residual analysis results and AICc score.


```{r warning = FALSE, message = FALSE}
library(TSA)
library(tidyverse)
library(forecast)
fit <- CA.transformed %>%
  Arima(order = c(1, 1, 2))
fit

# fit1 <- CA.transformed %>%
#   Arima(order = c(1, 0, 2))
# fit1
# 
# fit2 <- CA.transformed %>%
#   Arima(order = c(2, 0, 3))
# fit2
# 
# fit3 <- CA.transformed %>%
#   Arima(order = c(3, 0, 3))
# fit3

```

```{r warning = FALSE, message = FALSE}
library(TSA)
library(tidyverse)
library(forecast)

checkresiduals(fit)
```


```{r warning = FALSE, message = FALSE}
shapiro.test(residuals(fit))
plot(residuals(fit), col = c("blue"), type = "p")
qqnorm(residuals(fit))
qqline(residuals(fit))
```


```{r warning = FALSE, message = FALSE}
acf(US.transformed)
pacf(US.transformed)
eacf(US.transformed)
```
#' based on the eacf an appropriate mode would ARIMA(0,0,0)

```{r warning = FALSE, message = FALSE}
library(TSA)
library(tidyverse)
library(forecast)
fit <- US.transformed %>%
  Arima(order = c(3, 0, 1), method = "CSS")
checkresiduals(fit)

```


```{r warning = FALSE, message = FALSE}
shapiro.test(residuals(fit))
plot(residuals(fit), col = c("blue"), type = "p")
qqnorm(residuals(fit))
qqline(residuals(fit))
```



```{r warning = FALSE, message = FALSE}
acf(UK.transformed)
pacf(UK.transformed)
eacf(UK.transformed)
```






```{r warning = FALSE, message = FALSE}
library(TSA)
library(tidyverse)
library(forecast)
fit <- UK.transformed %>%
  Arima(order = c(3, 0, 1), method = "CSS")
checkresiduals(fit)

```

```{r warning = FALSE, message = FALSE}
shapiro.test(residuals(fit))
plot(residuals(fit), col = c("blue"), type = "p")
qqnorm(residuals(fit))
qqline(residuals(fit))
```


```{r warning = FALSE, message = FALSE}
auto.arima(CA.transformed)
```


```{r}
(3.453e-12)<(4.182e-12)
```

```{r}
library(forecast)
shapiro.test(US.transformed2)
americatimeseriesforecast <- forecast(US.transformed2, h= (5))
plot(canadatimeseriesforecast, main = "American GDP-Debt Ratio with 5 Year Forecast", xlab= "Year", ylab="GDP to Debt Ratio")

```
#' thank you!
