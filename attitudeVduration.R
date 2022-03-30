

#Data

data= read.csv(file="D:/RWorks/MarketingResearch/attitude_duration_data.csv")
head(data)
nrow(data)

library(ggplot2)
library(gridExtra)
library(kableExtra)

kable(data,digits = 2) %>% kable_styling(bootstrap_options = "striped", full_width = F)

ggplot(data=data,aes(x=duration, y=attitude, col=resp))+geom_point(size=4)+labs(title = "Scatterplot: Attitude vs Duration", caption = "Source: MR Texbook", x = "Duration (years)",y = "Attitude (11 point scale)",colour = "respondants")

ggplot(data, aes(x=duration,y=attitude))+geom_point(col='salmon',size=4)+xlab("Duration (years)")+ylab("Attitude (11 point scale)")+ggtitle("FITTED REGRESSION LINE EXPLAINING ATTITUDE USING DURATION")+geom_smooth(method='lm', se=2)

fit=lm(data=data,attitude~duration)

summary(fit)
qt(0.95,4)
(1-pt(4.001,4))*2
anova(fit)

mean(data$money_supply_growth_rate)
sd(data$money_supply_growth_rate)/(nrow(data)^(.5))


