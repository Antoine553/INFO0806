library(data.table)
library(tidyverse)
library(car)

dat <- fread(file='data/prestige.csv')
dat_tible <- as_tibble(dat)

scatterplot(prestige~education, data=dat_tible)

scatterplot(prestige~income, data=dat_tible)

prest.lm1 <- lm(prestige~education, data=dat_tible)

acf(residuals(prest.lm1), main="prest.lm1")

durbinWatsonTest(prest.lm1)

plot(prest.lm1,2)

shapiro.test(residuals(prest.lm1))

prest.lm2 <- lm(prestige~income, data=Prestige)

plot(prest.lm2,2)

shapiro.test(residuals(prest.lm2))

plot(prest.lm1, 3)

ncvTest(prest.lm1)

plot(prest.lm1,1)

summary(prest.lm1)

confint(prest.lm1)

influenceIndexPlot(prest.lm1)

outlierTest(prest.lm1)

prest.lm1bis <- lm(prestige~education, data=Prestige[-c(53,67),])
compareCoefs(prest.lm1 ,prest.lm1bis) 

my_df <- data.frame(education=c(10.25))
predict(prest.lm1, newdata=my_df)

predict(prest.lm1, newdata=my_df, interval="prediction")
predict(prest.lm1, newdata=my_df, interval="confidence")



my_df <- data.frame(education=c(10.25, 11.25, 12.25))
predict(prest.lm1, newdata=my_df,interval="confidence")




my_pres <- Prestige
my_pres$res <-residuals(prest.lm1)
head(my_pres)



my_pres$fitted <-fitted(prest.lm1)
head(my_pres)


head(predict(prest.lm1))
head(fitted(prest.lm1))



vcov(prest.lm1)


ggplot(Prestige, aes(y=prestige, x=education))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Prestige")+
  xlab("education") +
  theme_classic()+
  annotate("text", x = 9, y = 80, label = "prestige = -10.73 + 5.36 * education\n (pval<0.001)")






int_pred <- predict(prest.lm1, interval="prediction")
my_prest2 <-cbind(Prestige, int_pred)
head(my_prest2)

ggplot(my_prest2, aes(y=prestige, x=education))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+    
  ylab("Prestige")+
  xlab("education") +
  theme_classic()+
  annotate("text", x = 9, y = 80, label = "prestige = -10.73 + 5.36 * education\n (pval<0.001)")