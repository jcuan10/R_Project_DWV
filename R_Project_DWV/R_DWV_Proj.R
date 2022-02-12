
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
###############################################################################

#Loadind dataset and getting some basic information ###########################
setwd("~/R_Project_DWV")
DWV <- read.csv("daily-website-visitors.csv") #Dataset 
str(DWV)
head(DWV)
tail(DWV)
###############################################################################

# Initial data preparation ####################################################

DWV$Page.Loads <- as.numeric(gsub(",","",DWV$Page.Loads))
DWV$Unique.Visits <- as.numeric(gsub(",","",DWV$Unique.Visits))
DWV$First.Time.Visits <- as.numeric(gsub(",","",DWV$First.Time.Visits))
DWV$Returning.Visits <- as.numeric(gsub(",","",DWV$Returning.Visits))

Date <- as.Date(DWV$Date, format = "%m/%d/%Y")
DWV$Date <- NULL
DWV$Row <- NULL

DWV2 <- DWV

DWV$Day <- NULL

DWV_XTS <- as.xts(DWV, Date) # Convert dataframe DWV into time series DWV_XTS

sum(is.na(DWV_XTS))
summary(DWV_XTS)
str(DWV_XTS)
head(DWV_XTS)

DWV2_XTS <- DWV_XTS
DWV2_XTS$Day.Of.Week <- NULL
head(DWV2_XTS)

###############################################################################

# Initial data visualization #####################################
# Variables to be analized: Page.Loads, Unique.Visits, First.Time.Visits, Returning.Visits

#hist.data.frame(DWV[,2:5])
grp_bday <- group_by(DWV2, Day)
SUM_bday <- summarise_all(grp_bday, funs(sum))
SUM_bday$Day.Of.Week <- NULL
require(tidyr)
df.long <- gather(SUM_bday, variable, value, -Day)
ggplot(data = df.long, aes(x = reorder(Day, -value), y = value, fill = variable)) + geom_col(position = position_dodge()) 


data_long <- DWV[,2:5] %>%  pivot_longer(colnames(DWV[,2:5])) %>% as.data.frame()
ggplot(data_long, aes(x = value)) + geom_histogram(aes(y = ..density..)) + geom_density(col = "red") + facet_wrap(~ name, scales = "free")


cor(DWV2_XTS) # Pearson Correlation
cor(DWV2_XTS, method = "spearman")

corrplot.mixed(cor(DWV2_XTS), lower = 'shade', upper = 'pie', order = 'hclust')
corrplot.mixed(cor(DWV2_XTS), order = 'AOE')

# Time Series analisys ########################################################

# Calculate Periodicity, Cycle 
#periodicity(DWV2_XTS)
#cycle(DWV2_XTS)
#par(mfrow = c(1, 1))
#barplot(DWV2_XTS$Page.Loads)

plot(as.zoo(DWV2_XTS), screens = 1, lty = 1:4, xlab = "Date", ylab = "Val", col = rainbow(4)) 
legend("topright", c("Page.Loads", "Unique.Visits", "First.Time.Visits", "Returning.Visits"), lty = 1:5, cex = 0.5, col = rainbow(4))


#layout.matrix <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1)
par(mfrow = c(4, 1), mex = 0.5, cex = 0.5)
plot(DWV2_XTS$Page.Loads, main = "Page Loads", col = '#FF0000')
plot(DWV2_XTS$Unique.Visits, main = "Unique Visits", col = "#80FF00")
plot(DWV2_XTS$First.Time.Visits, main = "First Time Visits", col = "#00FFFF")
plot(DWV2_XTS$Returning.Visits, main = "Returning Visits", col = "#8000FF")
