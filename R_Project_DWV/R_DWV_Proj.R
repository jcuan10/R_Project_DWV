
#######  R programming project: Daily Website Visitors.  #######

# Packages/Libraies ###########################################################
install.packages("ggthemes")
install.packages("forecast")
install.packages("tseries")
install.packages("Hmisc")
install.packages("corrplot")

library(ggplot2)
library(ggthemes)
library(forecast)
library(tseries)
library(xts)
library(lubridate)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(dplyr)
library(ggfortify)
###############################################################################

#Loadind dataset and getting some basic information ###########################
setwd("~/R_Project_DWV")
DWV <- read.csv("daily-website-visitors.csv")     # Loading Dataset 
str(DWV)                                          # Checking structure of DWV
head(DWV)                                         # Original "DWV" head
#tail(DWV)
###############################################################################

# Initial data preparation and transformation #################################

# 4 important variables need to be transformed as numeric variables:
# "Page.Loads" "Unique.Visits" "First.Time.Visits" "Returning.Visits"

DWV$Page.Loads <- as.numeric(gsub(",","",DWV$Page.Loads))
DWV$Unique.Visits <- as.numeric(gsub(",","",DWV$Unique.Visits))
DWV$First.Time.Visits <- as.numeric(gsub(",","",DWV$First.Time.Visits))
DWV$Returning.Visits <- as.numeric(gsub(",","",DWV$Returning.Visits))

DWV <- DWV %>%
           rename(
                  A_Page.Loads = Page.Loads,
                  B_Unique.Visits = Unique.Visits,
                  C_First.Time.Visits = First.Time.Visits,
                  D_Returning.Visits = Returning.Visits
                 )

Date <- as.Date(DWV$Date, format = "%m/%d/%Y") # Time series base (TS)
DWV$Date <- NULL                               # Removing TS from DWV
DWV$Row <- NULL                                # Removing some garbage

DWV2 <- DWV                                    # Saving DWV, still Dataframe 

DWV$Day <- NULL                                # Removing "Day" cat. var.

DWV_XTS <- as.xts(DWV, Date)        # Convert DWV into xts() object "DWV_XTS"

sum(is.na(DWV_XTS))                            # Checking for NA values
summary(DWV_XTS)                               # Summary of our xTS object
str(DWV_XTS)                                   # Verifying Structure of xTS
head(DWV_XTS)                                  # Visualizing head of the xTS

DWV2_XTS <- DWV_XTS                            # Saving DWV_XTS just in case
DWV2_XTS$Day.Of.Week <- NULL                   # Removing unnecessary var.
head(DWV2_XTS)

###############################################################################

# Initial data visualization #####################################
# Variables: Page.Loads, Unique.Visits, First.Time.Visits, Returning.Visits

data_long <- DWV[,2:5] %>%  pivot_longer(colnames(DWV[,2:5])) %>% as.data.frame()
ggplot(data_long, aes(x = value)) + geom_histogram(aes(y = ..density..)) + geom_histogram(color="black", fill="lightblue") + labs(title = "Histogram", x = "Values", y = "Density") + facet_wrap(~ name, scales = "free")

DWV2$Day[DWV2$Day == "Sunday"] <- "Sun" 
DWV2$Day[DWV2$Day == "Monday"] <- "Mon" 
DWV2$Day[DWV2$Day == "Tuesday"] <- "Tue" 
DWV2$Day[DWV2$Day == "Wednesday"] <- "Wed" 
DWV2$Day[DWV2$Day == "Thursday"] <- "Thu" 
DWV2$Day[DWV2$Day == "Friday"] <- "Fri" 
DWV2$Day[DWV2$Day == "Saturday"] <- "Sat" 

grp_bday <- group_by(DWV2, Day)
SUM_bday <- summarise_all(grp_bday, funs(mean))
SUM_bday$Day.Of.Week <- NULL
require(tidyr)
df.long <- gather(SUM_bday, variable, value, -Day)
ggplot(data = df.long, aes(x = reorder(Day, -value), y = value, fill = variable)) + geom_col(position = position_dodge()) + labs(title = "Average by Variable ", x = "Day of the week", y = "average")


cor(DWV2_XTS) # Pearson Correlation
cor(DWV2_XTS, method = "spearman")

#corrplot.mixed(cor(DWV2_XTS), lower = 'shade', upper = 'pie', order = 'hclust')
corrplot.mixed(cor(DWV2_XTS), order = 'AOE')




# Time Series analisys ########################################################

plot(as.zoo(DWV2_XTS), screens = 1, lty = 1:4,  main = "Page Loads & Visitors over time ", xlab = "Date", ylab = "value", col = rainbow(4)) 
legend("topright", c("Page.Loads", "Unique.Visits", "First.Time.Visits", "Returning.Visits"), lty = 1:5, cex = 0.4, col = rainbow(4))


# Original Time Series
par(mfrow = c(4, 1), mex = 1, cex = 0.4)
plot(DWV2_XTS$A_Page.Loads, main = "Page Loads", col = "#FF0000")
plot(DWV2_XTS$B_Unique.Visits, main = "Unique Visits", col = "#80FF00")
plot(DWV2_XTS$C_First.Time.Visits, main = "First Time Visits", col = "#00FFFF")
plot(DWV2_XTS$D_Returning.Visits, main = "Returning Visits", col = "#8000FF")

# Monthly Trend (Seasonal)
par(mfrow = c(4, 1), mex = 1, cex = 0.4)
month_tr = apply.monthly(DWV2_XTS, mean)
barplot(month_tr$A_Page.Loads, main = "Page Loads / month", ylab = "Average" , col = "#FF0000")
barplot(month_tr$B_Unique.Visits, main = "Unique Visits / month", ylab = "Average" , col = "#80FF00")
barplot(month_tr$C_First.Time.Visits, main = "First Time Visits / month", ylab = "Average" , col = "#00FFFF")
barplot(month_tr$D_Returning.Visits, main = "Returning Visits / month", ylab = "Average" , col = "#8000FF")

# Quartely Trend (Trend)
par(mfrow = c(4, 1), mex = 1, cex = 0.4)
quar_tr = apply.quarterly(DWV2_XTS, mean)
barplot(quar_tr$A_Page.Loads, main = "Page Loads / quarter", ylab = "Average" , col = "#FF0000")
barplot(quar_tr$B_Unique.Visits, main = "Unique Visits / quarter", ylab = "Average" , col = "#80FF00")
barplot(quar_tr$C_First.Time.Visits, main = "First Time Visits / quarter", ylab = "Average" , col = "#00FFFF")
barplot(quar_tr$D_Returning.Visits, main = "Returning Visits / quarter", ylab = "Average" , col = "#8000FF")

# Yearly Trend (Trend)
par(mfrow = c(4, 1), mex = 1, cex = 0.4)
year_tr = apply.yearly(DWV2_XTS, mean)
barplot(year_tr$A_Page.Loads, main = "Page Loads / year", ylab = "Average" , col = "#FF0000")
barplot(year_tr$B_Unique.Visits, main = "Unique Visits / year", ylab = "Average" , col = "#80FF00")
barplot(year_tr$C_First.Time.Visits, main = "First Time Visits / year", ylab = "Average" , col = "#00FFFF")
barplot(year_tr$D_Returning.Visits, main = "Returning Visits / year", ylab = "Average" , col = "#8000FF")


# 
# Analizing just the "Page Loads" and "Unique Visits"

# Moving Average
par(mfrow = c(2, 1), mex = 1, cex = 0.4)
PL_MA <- rollmean(DWV2_XTS$A_Page.Loads, 7, align = "right")
PL_MA <- merge(PL_MA, DWV2_XTS$A_Page.Loads)
plot(PL_MA, main = "Moving Average - Page Loads")

UV_MA <- rollmean(DWV2_XTS$B_Unique.Visits, 7, align = "right")
UV_MA <- merge(UV_MA, DWV2_XTS$B_Unique.Visits)
plot(UV_MA, main = "Moving Average - Unique Visits")

# Autocorrelation
acf(DWV2_XTS$A_Page.Loads, main = "Autocorrelation - Page Loads")

acf(DWV2_XTS$B_Unique.Visits, main = "Autocorrelation - Unique Visits")

# Autocorrelation Test with Box.test()
AT_PL <- Box.test(DWV2_XTS$A_Page.Loads)
AT_UV <- Box.test(DWV2_XTS$B_Unique.Visits)

# Partial Autocorrelation
pacf(DWV2_XTS$A_Page.Loads, main = "Partial Autocorrelation - Page Loads")

pacf(DWV2_XTS$B_Unique.Visits, main = "Partial Autocorrelation - Unique Visits")

#Lagged Correlation Between Page Loads and Unique Visits "Cross-Correlation Function
par(mfrow = c(2, 1), mex = 0.5, cex = 0.5)
plot(coredata(DWV2_XTS$A_Page.Loads)[,1], coredata(DWV2_XTS$B_Unique.Visits)[,1], main = "Page Loads vs Unique Visits", xlab =  "Page Loads", ylab = "Unique Visits")
abline(reg = lm(coredata(DWV2_XTS$B_Unique.Visits)[,1] ~ coredata(DWV2_XTS$A_Page.Loads)[,1]), col = "red")
Ccf(coredata(DWV2_XTS$A_Page.Loads)[,1], coredata(DWV2_XTS$B_Unique.Visits)[,1], main = "Page Loads vs Unique Visits")


# Detrending the Time Series
par(mfrow = c(2, 1), mex = 0.5, cex = 0.5)
plot(PL_MA, main = "Page Loads Trend")
m_PL <- lm(coredata(DWV2_XTS$A_Page.Loads) ~ index(DWV2_XTS))
detr_PL <- zoo(resid(m_PL), index(DWV2_XTS))
plot(detr_PL, xlab ="Year", ylab = "Residual from Linear Model", main = "Page Loads Deviation from Trend")

par(mfrow = c(2, 1), mex = 0.5, cex = 0.5)
plot(UV_MA, main = "Unique Visitd Trend")
m_UV <- lm(coredata(DWV2_XTS$B_Unique.Visits) ~ index(DWV2_XTS))
detr_UV <- zoo(resid(m_UV), index(DWV2_XTS))
plot(detr_UV, xlab ="Year", ylab = "Residual from Linear Model", main = "Unique Visits Deviation from Trend")

# ARIMA Model
AAm_PL <- auto.arima(DWV2_XTS$A_Page.Loads)
checkresiduals(AAm_PL)

AAm_UV <- auto.arima(DWV2_XTS$B_Unique.Visits)
checkresiduals(AAm_UV)

# Forecast
fc_PL <- forecast(AAm_PL, h=100)
autoplot(fc_PL)

fc_UV <- forecast(AAm_UV, h=100)
autoplot(fc_UV)
