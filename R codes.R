library(mosaic)
library(ggplot2)
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(ggplot2)
library(datarium)
library(qqplotr)
library(datarium)
library(RVAideMemoire)
library(caret)
library(TTR)
library(forecast)
library(dplyr)
library(moments)
library(nortest)
library(stats)
library(car)
library(tseries)

birth <- read.csv(file = 'birth_rate.csv', header= TRUE)
glimpse(birth)
names(birth)
head(birth) 
tail(birth) 
str(birth)
summary(birth)
dim(birth)

colSums(is.na(birth))

birth$Country <-as.factor(birth$Country)
birth$Year <-as.factor(birth$Year)
birth$Region <-as.factor(birth$Region)
summary(birth)
birth_num <- birth %>% select(-Country,-Year,-Region)

birth_num_europe <- birth %>%
  filter(Region == "Europe") %>%
  select(AFR, TFR, POV, PAWR, PG, MMR, NoMD, BRC, SBR, MRI)

birth_num_asia <- birth %>%
  filter(Region == "Asia") %>%
  select(AFR, TFR, POV, PAWR, PG, MMR, NoMD, BRC, SBR, MRI)



boxplot(BRC~Region, data=birth,col=rainbow(8))
boxplot(BRC~Year, data=birth,col=rainbow(8))

boxplot(TFR~Region, data=birth,col=rainbow(8))
boxplot(TFR~Year, data=birth,col=rainbow(8))

boxplot(POV~Region, data=birth,col=rainbow(8))
boxplot(POV~Year, data=birth,col=rainbow(8))

boxplot(PG~Region, data=birth,col=rainbow(8))
boxplot(PG~Year, data=birth,col=rainbow(8))

boxplot(MMR~Region, data=birth,col=rainbow(8))
boxplot(AFR~Region, data=birth,col=rainbow(8))


hist(birth$AFR, freq=FALSE, xlab="Adolescent birth rate (births per 1,000 women ages 15-19)",col="green")
hist(birth$TFR, freq=FALSE, xlab="Fertility rate, total (births per woman)",col="green")
hist(birth$POV, freq=FALSE, xlab="Prevalence of overweight (% of adults)",col="green")
hist(birth$PAWR, freq=FALSE, xlab="Prevalence of anemia among women of reproductive age (% of women ages 15-49)",col="green")
hist(birth$PG, freq=FALSE, xlab="Population growth (annual %)",col="green")
hist(birth$MMR, freq=FALSE, xlab="Maternal mortality ratio (modeled estimate, per 100,000 live births)",col="green")
hist(birth$NoMD, freq=FALSE, xlab="Number of maternal deaths)",col="green")
hist(birth$BRC, freq=FALSE, xlab="Birth rate, crude (per 1,000 people)",col="green")
hist(birth$SBR, freq=FALSE, xlab="Stillbirth rate (per 1,000 total births)",col="green")
hist(birth$MRI, freq=FALSE, xlab="Mortality rate, infant (per 1,000 live births)",col="green")

qplot(Region,TFR,data=birth)
qplot(Region,BRC,data=birth, color = Year)
qplot(Region,PG,data=birth, color = Year, size = I(10))

qplot(TFR,data=birth, geom = "density",fill=I("blue"))

qplot(POV,data=birth, geom = "density",fill=I("blue"))
qplot(PG,data=birth, geom = "density",fill=I("blue"))

qplot(BRC,data=birth, geom = "density",fill=I("blue"))

qplot(MMR,data=birth, geom = "density",fill=I("blue"))
qplot(PAWR,data=birth, geom = "density",fill=I("blue"))

ggplot(birth, aes(x = PG, fill = Region)) +
  geom_density() +
  labs(title = "Density of Population Growth (PG) by Region",
       x = "Population Growth (PG)",
       y = "Density")

ggplot(birth, aes(x = TFR, fill = Region)) +
  geom_density() +
  labs(title = "Density of Fertility Rate, Total (TFR) by Region",
       x = "Fertility Rate, Total (TFR)",
       y = "Density")

ggplot(birth, aes(x = BRC, fill = Region)) +
  geom_density() +
  labs(title = "Density of Birth rate, total (births per woman) by Region",
       x = "Birth rate, total (births per woman),TFR",
       y = "Density")

#Calculating the mean for each numerical variable
data_mean <- birth %>%
  summarise(
    mean_AFR = mean(AFR),
    mean_TFR = mean(TFR),
    mean_POV = mean(POV),
    mean_PAWR = mean(PAWR),
    mean_PG = mean(PG),
    mean_MMR = mean(MMR),
    mean_NoMD = mean(NoMD),
    mean_BRC = mean(BRC),
    mean_SBR = mean(SBR),
    mean_MRI = mean(MRI)
  )

data_mean_arranged <- data_mean %>%
  arrange(across(everything(), desc))

for (col_name in colnames(data_mean_arranged)) {
  cat(col_name, ": ", data_mean_arranged[[col_name]], "\n")
}


#Calculating the birth rate average for each region.
average_birth_rate_asia <- birth %>%
  filter(Region == "Asia") %>%
  summarise(mean_BRC_asia = mean(BRC))
average_birth_rate_asia 

average_birth_rate_europe <- birth %>%
  filter(Region == "Europe") %>%
  summarise(mean_BRC_europe = mean(BRC))
average_birth_rate_europe 


data_median <- birth %>%
  summarise(
    median_AFR = median(AFR),
    median_TFR = median(TFR),
    median_POV = median(POV),
    median_PAWR = median(PAWR),
    median_PG = median(PG),
    median_MMR = median(MMR),
    median_NoMD = median(NoMD),
    median_BRC = median(BRC),
    median_SBR = median(SBR),
    median_MRI = median(MRI)
  )

# Sorting the columns in descending order
data_median_arranged <- data_median %>%
  arrange(across(everything(), desc))

for (col_name in colnames(data_median_arranged)) {
  cat(col_name, ": ", data_median_arranged[[col_name]], "\n")
}

#Calculating the median birth rate for each region.
median_birth_rate_asia <- birth %>%
  filter(Region == "Asia") %>%
  summarise(median_BRC_asia = median(BRC))
median_birth_rate_asia 

median_birth_rate_europe <- birth %>%
  filter(Region == "Europe") %>%
  summarise(median_BRC_europe = median(BRC))
median_birth_rate_europe


#Computing the mode of each numerical variable
mode_AFR <- as.numeric(names(sort(table(birth$AFR), decreasing = TRUE)[1]))
mode_TFR <- as.numeric(names(sort(table(birth$TFR), decreasing = TRUE)[1]))
mode_POV <- as.numeric(names(sort(table(birth$POV), decreasing = TRUE)[1]))
mode_PAWR <- as.numeric(names(sort(table(birth$PAWR), decreasing = TRUE)[1]))
mode_PG <- as.numeric(names(sort(table(birth$PG), decreasing = TRUE)[1]))
mode_MMR <- as.numeric(names(sort(table(birth$MMR), decreasing = TRUE)[1]))
mode_NoMD <- as.numeric(names(sort(table(birth$NoMD), decreasing = TRUE)[1]))
mode_BRC <- as.numeric(names(sort(table(birth$BRC), decreasing = TRUE)[1]))
mode_SBR <- as.numeric(names(sort(table(birth$SBR), decreasing = TRUE)[1]))
mode_MRI <- as.numeric(names(sort(table(birth$MRI), decreasing = TRUE)[1]))

mode_BRC_asia <- as.numeric(names(sort(table(birth_num_asia$BRC), decreasing = TRUE)[1]))
mode_BRC_europe <- as.numeric(names(sort(table(birth_num_europe$BRC), decreasing = TRUE)[1]))

mode_AFR
mode_TFR 
mode_POV
mode_PAWR
mode_PG
mode_MMR 
mode_NoMD 
mode_BRC 
mode_SBR 
mode_MRI

mode_BRC_asia
mode_BRC_europe

#computing the standard deviation of each variable
data_sd <- birth %>%
  summarise(
    sd_AFR = sd(AFR),
    sd_TFR = sd(TFR),
    sd_POV = sd(POV),
    sd_PAWR = sd(PAWR),
    sd_PG = sd(PG),
    sd_MMR = sd(MMR),
    sd_NoMD = sd(NoMD),
    sd_BRC = sd(BRC),
    sd_SBR = sd(SBR),
    sd_MRI = sd(MRI)
  )

data_sd_arranged <- data_sd %>%
  arrange(across(everything(), desc))

for (col_name in colnames(data_sd_arranged)) {
  cat(col_name, ": ", data_sd_arranged[[col_name]], "\n")
}

#Calculating the median birth rate for each region.
sd_birth_rate_asia <- birth %>%
  filter(Region == "Asia") %>%
  summarise(sd_BRC_asia = sd(BRC))
sd_birth_rate_asia 

sd_birth_rate_europe <- birth %>%
  filter(Region == "Europe") %>%
  summarise(sd_BRC_europe = sd(BRC))
sd_birth_rate_europe 




#calculating the skewness of each indicator
birth_num_skewness <- sapply(birth_num, skewness)
# Sort the skewness values in ascending order
birth_skewness_sorted <- sort(birth_num_skewness, decreasing = )
birth_skewness_sorted

#calculating the kurtosis of each indicator
birth_num_kurtosis <- sapply(birth_num, kurtosis)
# Sort the kurtosis values in ascending order
birth_kurtosis_sorted <- sort(birth_num_kurtosis, decreasing = )
birth_kurtosis_sorted


#Calculating the skewness of birth rate for both regions
birth_num_skewness_asia <- skewness(birth_num_asia$BRC, na.rm = TRUE)
birth_num_skewness_europe <- skewness(birth_num_europe$BRC, na.rm = TRUE)
birth_num_skewness_asia
birth_num_skewness_europe

birth_num_kurtosis_asia <- kurtosis(birth_num_asia$BRC, na.rm = TRUE)
birth_num_kurtosis_europe <- kurtosis(birth_num_europe$BRC, na.rm = TRUE)
birth_num_kurtosis_asia
birth_num_kurtosis_europe


#-------------------------------------------------------------------#


# multiple scatter plots
pairs(birth_num[, c("TFR", "MMR", "PG","BRC")])

# Pearson correlation between variables TFR and BRC
cor(birth_num_asia$TFR, birth_num_asia$BRC)
cor(birth_num_europe$TFR, birth_num_europe$BRC)
#--------
birth_cont_asia <- birth_num_asia%>% select(-NoMD)
birth_cont_europe <- birth_num_europe%>% select(-NoMD)

round(cor(birth_cont_asia), digits = 2)
corrplot(cor(birth_cont_asia), method = "number", type = "upper")

round(cor(birth_cont_europe), digits = 2)
corrplot(cor(birth_cont_europe), method = "number", type = "upper")

#----------

round(cor(birth_num_asia, method = "spearman"), digit=2)
round(cor(birth_num_europe, method = "spearman"), digit=2)
#----------

cramerV(birth$Region, birth$NoMD)
cramerV(birth$Region, birth$Country)
cramerV(birth$Country, birth$NoMD)

#-----------
cor.test(birth$NoMD, birth$TFR)
cor.test(birth$NoMD, birth$BRC)
#----------------

# Visually confirm if the plot appears to be on a normal curve
ggplot(mapping = aes(sample=birth$POV[birth$Region=="Europe"])) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theorectical") + ylab("Europe")

hist(birth$POV[birth$Region=="Europe"])
summary(birth$POV[birth$Region=="Europe"])
shapiro.test(birth$POV[birth$Region=="Europe"])

t.test(birth$POV[birth$Region=="Europe"], mu=55.58, alternative="greater")
#p-value > 0.05, hence, the Ho cannot be rejected.
#----------------------------#

#Ho: The distribution of total fertility Rate is identical for both Europe and Asia
#Ha: The distribution of total fertility Rate is NOT identical for both Europe and Asia

# Visually confirm if the plot appears to be on a normal curve

ggplot(mapping = aes(sample=birth$TFR[birth$Region=="Europe"])) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Europe")

hist(birth$TFR[birth$Region=="Europe"])

ggplot(mapping = aes(sample=birth$TFR[birth$Region=="Asia"])) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Asia")

hist(birth$TFR[birth$Region=="Asia"])

# Hypothesis test
wilcox.test(TFR ~ Region, data=birth)

#p-value > 0.05, hence, the Ho cannot be rejected.
#----------------------------#


#Ho: Average Population growth in Asia = Average Population growth in Europe
#Ha:  Average Population growth in Asia /= Average Population growth in Europe


t.test(PG ~ Region, birth)
#p-value > 0.05, hence, the Ho is NOT rejected.


#-------------------------------------------------------

#Ho: The two variables are not associated
#Ha: The two variables are associated




contingency_table <- table(birth$Region, birth$Country)
print(contingency_table)

chisq.test(contingency_table)

#p-value < 0.05, hence, the Ho is rejected.




#-----------------------------------------------------------
str(birth_num)
plot(birth_num)
corrplot(cor(birth_num), method = "number", type = "upper")
data.frame(colnames(birth_num))
pairs(birth_num[,c(8,2,1,6,5)], lower.panel = NULL, upper.panel = panel.smooth, pch = 19,cex = 0.2)
birth_model_1 <-lm(BRC ~ TFR, birth_num)
summary.lm(birth_model_1)


plot(BRC ~ TFR, birth_num,
     col = "blue",
     main = "Regression: Birth Rate, Crude and Fertility Rate, Total",
     xlab = "Fertility Rate, Total",
     ylab = "Birth Rate, Crude")

abline(birth_model_1, col="red")
plot(birth_model_1, 1)
plot(birth_model_1, 2)
plot(birth_model_1, 3)

#Birthrate = -1.4092 + 7.8768 × TFR
#--------------------------------------------------


birth_model_2 <-lm(BRC ~ TFR + MMR, birth_num)
summary.lm(birth_model_2)

plot(birth_model_2, 1)
plot(birth_model_2, 2)
plot(birth_model_2, 3)
vif(birth_model_2)

#Birthrate = -0.08619 + (6.61052*TFR) + (0.05195*AFR)
#------------------------------------------------#
BRC_mean_grouped <- birth %>%
  group_by(Year) %>%
  summarise(
    BRC_means = mean(BRC)
  )

BRC_mean_grouped
BRC_ts <- BRC_mean_grouped[c("BRC_means")]
BRC_ts

BRC_timeseries <- ts(BRC_ts[c("BRC_means")], start=c(2005,1))
BRC_timeseries
plot.ts(BRC_timeseries)

adf.test(BRC_timeseries)

BRC_timeseriesforecasts <- HoltWinters(BRC_timeseries, gamma=FALSE)
BRC_timeseriesforecasts
BRC_timeseriesforecasts$SSE
plot(BRC_timeseriesforecasts)

BRC_timeseriesforecasts2 <- forecast(BRC_timeseriesforecasts, h=5)
plot(BRC_timeseriesforecasts2)

BRC_timeseriesforecasts2

acf(BRC_timeseriesforecasts2$residuals, lag.max=5, na.action = 
      na.pass)
pacf(BRC_timeseriesforecasts2$residuals, lag.max=5, na.action = 
       na.pass)

Box.test(BRC_timeseriesforecasts2$residuals, lag=4, type="Ljung-Box")

plot.ts(BRC_timeseriesforecasts2$residuals) # make time series plot

BRC_timeseriesforecasts2$residuals <- BRC_timeseriesforecasts2$residuals[!is.na(BRC_timeseriesforecasts2$residuals)]

hist(BRC_timeseriesforecasts2$residuals, freq = FALSE, xlab = "Forecast Errors", col = "red")
# Add a blue line plot for the density
lines(density(BRC_timeseriesforecasts2$residuals), col = "blue", lwd = 3)

#-------------------------------------------#
#ARIMA

BRC_timeseriesdiff1 <- diff(BRC_timeseries, differences=1)
plot.ts(BRC_timeseriesdiff1)

BRC_timeseriesdiff2 <- diff(BRC_timeseries, differences=2)
plot.ts(BRC_timeseriesdiff2)

acf(BRC_timeseriesdiff2, lag.max=5) # plot a correlogram
acf(BRC_timeseriesdiff2, lag.max=5, plot=FALSE) # get the autocorrelation values 
#Autocorrelations of series 'BRC_timeseriesdiff2', by lag

pacf(BRC_timeseriesdiff2, lag.max=5) # plot a correlogram
pacf(BRC_timeseriesdiff2, lag.max=5, plot=FALSE) # get the autocorrelation values 
#Autocorrelations of series 'BRC_timeseriesdiff2', by lag

library(forecast)
auto.arima(BRC_timeseries)

BRC_timeseriesdiff2_arima <- arima(BRC_timeseriesdiff2, order=c(1,2,1)) # fit an ARIMA(1,2,1) model
BRC_timeseriesdiff2_arima 

BRC_timeseriesdiff2_arima_forecasts <- forecast(BRC_timeseriesdiff2_arima, h=5)
BRC_timeseriesdiff2_arima_forecasts
plot(BRC_timeseriesdiff2_arima_forecasts)

acf(BRC_timeseriesdiff2_arima_forecasts$residuals, lag.max=5)
Box.test(BRC_timeseriesdiff2_arima_forecasts$residuals, lag=5, type="Ljung-Box")
plot.ts(BRC_timeseriesdiff2_arima_forecasts$residuals) # time plot forecast error
# Create a histogram
hist(BRC_timeseriesdiff2_arima_forecasts$residuals, freq = FALSE, xlab = "Forecast Errors", col = "red")

# Add a blue line plot for the density
lines(density(BRC_timeseriesdiff2_arima_forecasts$residuals), col = "blue", lwd = 3)
#--------------------------------#

