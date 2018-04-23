## Setup ##
library(haven)
library(tidyr)
library(Hmisc)
library(reshape2)
library(magrittr)
library(dplyr)

# set directory to GitHub repo
setwd("C:/Users/bjr21/Documents/GitHub/DunnEconData")

## Import Dataset ##
wages <- read_sas("cps_00018.sas7bdat")

wages <- subset(wages, wages$INCWAGE != 9999999)

wages$ADJINCWAGE <- wages$CPI99*wages$INCWAGE*1.43 #adjusts to 2015 dollars
wages <- subset(wages, wages$EMPSTAT == 10 | wages$EMPSTAT ==11)

wage_percentile <- aggregate(INCWAGE ~ YEAR, data = wages, FUN = wtd.quantile, weights = wages2$ASECWT, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

wage_percentile <- cbind(wage_percentile[-ncol(wage_percentile)], wage_percentile[[ncol(wage_percentile)]])

wage_percentile$YEAR <- as.factor(wage_percentile$YEAR)
wage_percentilem <- melt(wage_percentile)

names(wage_percentilem)[2] <- "Percentile"
names(wage_percentilem)[3] <- "Wages"
wage_percentilem$Percentile <- as.factor(wage_percentilem$Percentile)

ggplot(data = wage_percentilem, aes(x = YEAR, y = Wages, group = Percentile, colour = Percentile))+
  geom_line()+
  geom_point()+
  theme_minimal()

names(wage_percentile)[2] <- "Ten"
names(wage_percentile)[3] <- "Twenty"
names(wage_percentile)[4] <- "Thirty"
names(wage_percentile)[5] <- "Forty"
names(wage_percentile)[6] <- "Fifty"
names(wage_percentile)[7] <- "Sixty"
names(wage_percentile)[8] <- "Seventy"
names(wage_percentile)[9] <- "Eighty"
names(wage_percentile)[10] <- "Ninety"

wage_percentile2 <- wage_percentile %>%
  arrange(YEAR) %>%
  mutate(yearOverYear10=((Ten-lag(Ten,1))/lag(Ten,1)))

wage_percentile2 <- wage_percentile2 %>%
  arrange(YEAR) %>%
  mutate(yearOverYear20=((Twenty-lag(Twenty,1))/lag(Twenty,1)))

wage_percentile2 <- wage_percentile2 %>%
  arrange(YEAR) %>%
  mutate(yearOverYear30=((Thirty-lag(Thirty,1))/lag(Thirty,1)))

wage_percentile2 <- wage_percentile2 %>%
  arrange(YEAR) %>%
  mutate(yearOverYear40=((Forty-lag(Forty,1))/lag(Forty,1)))

wage_percentile2 <- wage_percentile2 %>%
  arrange(YEAR) %>%
  mutate(yearOverYear50=((Fifty-lag(Fifty,1))/lag(Fifty,1)))

wage_percentile2 <- wage_percentile2 %>%
  arrange(YEAR) %>%
  mutate(yearOverYear60=((Sixty-lag(Fifty,1))/lag(Sixty,1)))

wage_percentile2 <- wage_percentile2 %>%
  arrange(YEAR) %>%
  mutate(yearOverYear70=((Seventy-lag(Seventy,1))/lag(Seventy,1)))

wage_percentile2 <- wage_percentile2 %>%
  arrange(YEAR) %>%
  mutate(yearOverYear80=((Eighty-lag(Eighty,1))/lag(Eighty,1)))

wage_percentile2 <- wage_percentile2 %>%
  arrange(YEAR) %>%
  mutate(yearOverYear90=((Ninety-lag(Ninety,1))/lag(Ninety,1)))

wage_growth <- wage_percentile2[,c(1,12:19)]

wage_growth <- na.omit(wage_growth)

wage_growth$YEAR <- as.factor(wage_growth$YEAR)
wage_growthm <- melt(wage_growth)

names(wage_growthm)[2] <- "Percentile"
names(wage_growthm)[3] <- "Growth"
wage_growthm$Percentile <- as.factor(wage_growthm$Percentile)


ggplot(data = wage_growthm, aes(x = YEAR, y = Growth, group = Percentile, colour = Percentile))+
  geom_line()+
  geom_point()+
  theme_minimal()