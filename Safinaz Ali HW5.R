#Safinaz Ali
#Econometric HW5
#TEAM: John R & Victoria K


load("~/Desktop/Statistic & Econmetric/Data/acs2017_ny/acs2017_ny_data.RData")
attach(acs2017_ny)
#created a subset for age since that is the age of the most working profession. The lab force and horus of work is consider to be a part time or full time worker
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
data_use <- subset(acs2017_ny,use_varb)  
detach()
attach(data_use)

model_temp1 <- lm(INCWAGE ~ AGE + educ_hs + educ_somecoll + educ_college + educ_advdeg + VETSTAT, data = data_use)
summary(model_temp1)
plot(model_temp1)

#veteran status can cause an increase in income because many people that were veterans and now work at a normal job do get the benefits 
#of getting paid more then a non veteran. So i wanted to see the difference of income wage for veterans vs people that have a normal degree
# in which i learned that there was not a positive affect as degree status had for veteran. As it was a very small estimate compared to the rest since it
# constantly increased within the higher education; so we can assume veterans will make more money because of their status in education. Since the college degree 
#was a 99% confidence and veteran has only a 95%

model_temp2 <- lm(INCWAGE ~ AGE + in_Manhattan + in_Bronx + in_Brooklyn + in_Nassau + in_Queens + in_StatenI + in_Westchester + educ_hs + educ_somecoll + educ_college + educ_advdeg, data = data_use)
summary(model_temp2)
plot(model_temp1)
                  
#here i wanted to see if the region they were located affected their wage including their degree status and so far only living in the bronx had a negative impact on their income
# compared to the other boroughs however for queens it is not significant on any level as the rest are .001 for significance which is 99% confidence interval

install.packages("stargazer")
require(stargazer)
stargazer(model_temp1, type = "text")

install.packages("AER")
library(AER)
require(AER)

NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1) 
data_graph <-subset(data_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 15), pch = 20, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = data_graph)
plot(INCWAGE ~ jitter(AGE, factor = 6), pch = 20, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = data_graph)

install.packages("yhat")
require(yhat)

to_be_predicted2 <- data.frame(AGE = 25:55, veteran = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

detach()

