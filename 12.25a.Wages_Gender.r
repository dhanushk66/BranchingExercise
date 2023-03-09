# 12.25 Wages-and Gender
# 
library(Sleuth3)
library(mosaic)
library(leaps)
library(tidyverse)
library(gmodels)
library(car)
library(ggthemes)
library(ggfortify)
library(GGally)
library(psych)
library(pastecs)
library(DescTools)

#options to avoid scientific notation
options(scipen=100)
options(digits=4)

#Reading  data
data(ex1225)
summary(ex1225)
names(ex1225)
#create a new variables 
wages <-transform(ex1225, logWE=log(WeeklyEarnings)) 
names(wages)



shapiro.test(wages$logWE [wages$Sex=="Female"])   
shapiro.test(wages$WeeklyEarnings [wages$Sex=="Female"])

shapiro.test(wages$WeeklyEarnings [wages$Sex=="Female"])   
shapiro.test(wages$WeeklyEarnings [wages$Sex=="Male"])

#Sort labels order for Categorical variables
wages <-wages %>%
  mutate(MetropolitanStatus=factor(MetropolitanStatus,levels=c('Not Identified','Metropolitan','Not Metropolitan')))
levels(wages$MetropolitanStatus)
levels(wages$Sex)
ggplot(data = wages,mapping = aes(x=WeeklyEarnings))+
  geom_histogram()
ggplot(data = wages,mapping = aes(x=EdCode))+
  geom_histogram()

## Step 1: Look at data (describe it) and Make lot of scatterplots
ggplot(data = wages,mapping = aes(x=Age,y=WeeklyEarnings,color=Sex))+
  geom_point()

ggplot(data = wages,mapping = aes(x=Age,y=WeeklyEarnings,color=Sex))+
  geom_point()+
  facet_wrap(~MaritalStatus)
leveneTest(WeeklyEarnings~Sex,data = wages)

ggplot(data = wages,mapping = aes(x=Age,y=WeeklyEarnings,color=Sex))+
  geom_point()+
  facet_wrap(~JobClass)
ggplot(data=wages,mapping = aes(x=Age,y=logWE,color=Sex))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(data=wages,mapping = aes(x=Age,y=WeeklyEarnings,color=Sex))+
  geom_point()
#using a pipe and mosaic functions
wages %>%
  group_by(Sex) %>%
  summarise(n = n(), Average = mean(WeeklyEarnings), Median =median(WeeklyEarnings),
            SD = sd(WeeklyEarnings),  IQR = IQR(WeeklyEarnings)) %>%
  knitr::kable()


wages %>%
  group_by(JobClass,Sex) %>%
  summarise(n = n(), Average = mean(WeeklyEarnings), Median =median(WeeklyEarnings),
            SD = sd(WeeklyEarnings),  IQR = IQR(WeeklyEarnings)) %>%
  knitr::kable()

wages %>%
  group_by(Region,Sex) %>%
  summarise(n = n(), Average = mean(WeeklyEarnings), Median =median(WeeklyEarnings),
            SD = sd(WeeklyEarnings),  IQR = IQR(WeeklyEarnings)) %>%
  knitr::kable()
wages %>%
  group_by(Education,Sex) %>%
  summarise(n = n(), Average = mean(WeeklyEarnings), Median =median(WeeklyEarnings),
            SD = sd(WeeklyEarnings),  IQR = IQR(WeeklyEarnings)) %>%
  knitr::kable()

wages %>%
  group_by(MaritalStatus,Sex) %>%
  summarise(n = n(), Average = mean(WeeklyEarnings), Median =median(WeeklyEarnings),
            SD = sd(WeeklyEarnings),  IQR = IQR(WeeklyEarnings)) %>%
  knitr::kable()

#Performing T Test only for only sex
t.test(WeeklySalary~Sex,data = wages)
exp(0.20202)

lm1<-lm(logWE~Sex,data = wages)
lm2<-lm(logWE~Region+Age,data = wages)
lm3<-lm(logWE~MetropolitanStatus+Age,data=wages)
lm4<-lm(WeeklyEarnings~Region+MetropolitanStatus+Age+MaritalStatus+EdCode+Education+JobClass,data=wages)
lm5<-lm(logWE~Age+MaritalStatus*Sex+EdCode+JobClass,data=wages)
lm6<-lm(logWE~Sex+Region,data=wages)
lm7<-lm(WeeklyEarnings~Sex*MaritalStatus,data = wages)
lm8<-lm(WeeklyEarnings~Sex+MaritalStatus,data = wages)
lm4$residuals
residualPlot(lm5)
residualPlot(lm4)
summary(wages)
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)
summary(lm7)
summary(lm8)

exp(0.22941)
anova(lm8,lm7)
exp(0.22941)
exp(c(0.238904,0.297191))
confint(lm5)
ggplot(lm1, mapping = aes(sample =lm5$residuals)) + 
  stat_qq() +
  stat_qq_line() 
wages$Residuals <- resid(lm5)
wages$FittedValues <- fitted(lm5)

ggplot(data = wages, mapping = aes(x = FittedValues, y = Residuals)) +
  geom_point(color="blue") +
  geom_hline(yintercept = 0, color ="red") +
  xlab("Fitted Values") +
  ylab("Residuals)") +
  ggtitle("Residual plot (Rich Model)") +
  theme_bw()

residualPlot(lm5)
#getting correlations coefficients #remove the cat variables.
cor(wages$WeeklyEarnings,wages$EdCode)
cor(wages[,-c(1,2,4,5,6,7,8)])
cor(wages[,-c(1,2,4,5,7,8)])
names(wages)
#drawing a scatterplot 
ggplot(data = wages, mapping = aes(x = EdCode, y = WeeklyEarnings, color = Sex)) +
  geom_point() 
 # facet_wrap(~Region, nrow =2)+
  #xlab("EducationCode") +
  #ylab("Weekly Earnings (log scale)") +
  #ggtitle(" Wages and Race study") +
  #theme_bw()
exp(0.268048)
ggplot(data = wages, mapping = aes(x = Sex, y =WeeklyEarnings)) +
  geom_boxplot(color = "dark blue", fill = "yellow") + 
  theme_bw()
ggplot(data = wages, mapping = aes(x = Region, y =WeeklyEarnings/100)) +
  geom_boxplot(color = "dark blue", fill = "yellow") + 
  theme_bw()
ggplot(data = wages, mapping = aes(x =MetropolitanStatus, y =WeeklyEarnings/100)) +
  geom_boxplot(color = "dark blue", fill = "yellow") + 
  theme_bw()
