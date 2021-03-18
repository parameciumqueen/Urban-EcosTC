#LOOKING AT NITROGEN,WF AND PERIPHYTON BIOMASS
#data cleaning for nitrogen
#seeing what variables I have and the totals
library(dplyr)
library(tidyr)
biop <- Results %>% group_by(PARM_NM) %>% summarise(count = n())
#removing all other columns except the ones that I want 
cleanres <- Results %>% select(PARM_NM,PARM_CD,COUNTY_NM,RESULT_VA,RESULT_SG,RESULT_RD)
#removing the other methods for periphyton calculation
cleanres <- cleanres %>% filter(PARM_NM == 'Periphyton biomass' | PARM_NM == 'Total nitrogen, wf')
#dropping all the NAs
cleanres = cleanres %>% drop_na()

#problem - need to find where the periphyon biomass and the nitrogen match
#first finding out how many different counties there are and the name
CNAME <- Results %>% group_by(COUNTY_NM) %>% summarise(count = n())
#looking at one county to see if it had any periphyton and nitrogen data - nope
blaht <- Results %>% filter(COUNTY_NM == 'Alameda County')
#might be easier to do this if i separate them into two different data frames
periphyton <- cleanres %>% filter(PARM_NM == 'Periphyton biomass')
nitrogenp <- cleanres %>% filter(PARM_NM == 'Total nitrogen, wf')
#find the different counties in each
nperi <- periphyton %>% group_by(COUNTY_NM) %>% summarise(count = n())
nnitr <- nitrogenp %>% group_by(COUNTY_NM) %>% summarise(count = n())
# simular - cobb, fairfax , franklin

#matching the variables - from mohanad
# found the average for each periphyton method and nitrogen for each site that had that data 
cleanres2 <- Results %>% group_by(SITE_NO, PARM_NM) %>% summarize(median_val = median(RESULT_VA)) %>% ungroup()
# made a df with the site numbers with the new averaged df that ONLY had nitrogen and periphyton data together
data <- pivot_wider(cleanres2,id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val) %>% 
  select(c(1,4,5)) %>% na.omit()

#want to look at the nitrogen and periphyton data again but you cleaned the env?
#first import the Results from "nitrogen, wf and periphyton (wo MSQA)"
library(dplyr)
library(tidyr)
cleanres2 <- Results %>% group_by(SITE_NO, PARM_NM) %>% summarize(median_val = median(RESULT_VA)) %>% ungroup()
data <- pivot_wider(cleanres2,id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val) %>% 
  select(c(1,4,5)) %>% na.omit()

#LOOKING AT PHOSPHORUS AND PERIPHYTON BIOMASS
#what kind of data do we have and how much of it?
phosnm <- phospbRES %>% group_by(PARM_NM) %>% summarise(count = n())
#obtaining the average for each variable for each site 
phoscleanres <- phospbRES %>% group_by(SITE_NO,COUNTY_NM,PARM_NM) %>% summarize(median_val = median(RESULT_VA),sd_val = sd(RESULT_VA)) %>% ungroup()
#only getting the averages for having phos and periphyton
phosdata <- pivot_wider(phoscleanres,id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val) %>% 
  na.omit()
#creating a plot so I can visualize the data 
library(ggplot2)
# might need to change the col names because its hard to do a ggplot with the long names 
phosdata <- phosdata %>% rename(peribio = `Periphyton biomass`,phostot = `Phosphorus, bs,total`)
#the scatterplot with a line of best fit
ggplot(phosdata, aes(phostot,peribio)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm", se = FALSE)

#GLM WITH PHOSPHOROUS
my_glm <- lm(peribio ~ phostot, data=phosdata)
#first need to meet assumptions - linearity: WHY ARE THE TWO PLOTS SO DIFFERENT?
plot(phosdata$phostot, phosdata$peribio, xlab='phosporous', ylab= 'periphyton', pch=5)
#normaility of residuals- right tailed skweded - might need to be changed 
hist(my_glm$residuals, main='Model Residuals', xlab='Residual', col='light grey', right=F)
#confirming equal varience - not entirely equal tbh
plot(my_glm$fitted.values, my_glm$residuals, xlab= 'Fitted Values', ylab='Residuals', main='Residual Plot', pch=20)
abline(h=0, col='red')
#the GLM with no interaction
summary(my_glm)
#the effect size
summary(my_glm)$adj.r.squared
#finding the tau coeffeicent for phosphorus 0.09
cor(x=phosdata$phostot,y=phosdata$peribio, method = c("kendall"), use="pairwise")
#finding pearson coeff - 0.1477
cor(x=phosdata$phostot,y=phosdata$peribio)


# LOOK AT DISSOLVED OXYGEN AND PERIPHYTON
#what kind of data do we have and how much of it?
oxynm <- dissoxypbmRES %>% group_by(PARM_NM) %>% summarise(count = n())
#obtaining the average for each variable for each site 
oxycleanres <- dissoxypbmRES %>% group_by(SITE_NO,COUNTY_NM,PARM_NM) %>% summarize(median_val = median(RESULT_VA),sd_val = sd(RESULT_VA)) %>% ungroup()
#only getting the averages for having phos and periphyton
oxydata <- pivot_wider(oxycleanres,id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val) %>% 
  na.omit()
#creating a plot so I can visualize the data and first changing the col names
oxydata <- oxydata %>% rename(dissoxyper = `Diss oxygen,%saturtn`,peribio = `Periphyton biomass`)
ggplot(oxydata, aes(dissoxyper,peribio)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm", se = FALSE)
#looking at my assumptions for oxygen as well as how badly skewed everything is
oxy_glm <- lm(peribio ~ dissoxyper, data=oxydata)
hist(oxy_glm$residuals, main='Model Residuals', xlab='Residual', col='light grey', right=F)
#finding the tau coefficient for oxygen - gave me 0.120
cor(x=oxydata$dissoxyper,y=oxydata$peribio, method = c("kendall"), use="pairwise")
# finding r gave me 0.0103
cor(oxydata$dissoxyper,oxydata$peribio)


#NITRATES AND PERIPHYTON
#creating the df with average for each variable
nitratecleanres <- nitratepbm %>% group_by(SITE_NO,PARM_NM) %>% summarize(median_val = median(RESULT_VA),sd_val = sd(RESULT_VA)) %>% ungroup()
#
nitratedata <- pivot_wider(nitratecleanres,id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val) %>% 
  na.omit()
#
nitratedata <- nitratedata %>% rename(peribio = `Periphyton biomass`,nitrate = `Nitrate, wf`)

ggplot(nitratedata, aes(peribio,nitrate)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm", se = FALSE)

plot(nitratedata$nitrate)

#phosphates 
#GLM of oxygen
#removing outliers - the 4 high points 




