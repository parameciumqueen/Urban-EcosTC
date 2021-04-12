#changing to algae data instead of periphyton
#merging the algae and the nitrate data 
library(dplyr)
library(tidyr)
##taking only the columns i want from the nitrate data 
cleanres <- justnitratRES %>% select(PARM_NM,SITE_NO,RESULT_VA,RESULT_SG,RESULT_RD)
##merging the data together - first averaging out the data 
cleanres = cleanres %>% drop_na()
##created a long data frame with the nitrate data and their median values for each site
cleanres2 <- cleanres %>% group_by(SITE_NO, PARM_NM) %>% summarize(median_val = median(RESULT_VA)) %>% ungroup()
cleanres3 <- pivot_wider(cleanres2,id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val) %>% na.omit()

##NITRATES
## need to merge the data frames and matched them via site numbers (i think?)
total <- merge(cleanres3,aldata,by=c("SITE_NO")) %>% drop_na()
#changed the name of nitrates because it was hard to work with
library(data.table)
setnames(total, "Nitrate, wf", "nitrates")
#try to do GAM on it 
library(mgcv)
gam_mod <- gam(nitrates ~ s(A_MMI), data = total)
gam_mod
plot(gam_mod, residuals = TRUE, pch = 1)
#it worked but it does not look good- there is a large outlier that I need to remove from the nitrates
library(ggplot2)
ggplot(total,aes(A_MMI,nitrates)) + geom_point()
#removing a large outlier in the data 
total <- total[total$nitrates < 10, ]
ggplot(total,aes(A_MMI,nitrates)) + geom_point()
#do GAM over again
gam_mod <- gam(nitrates ~ s(A_MMI), data = total)
gam_mod
plot(gam_mod, residuals = TRUE, pch = 1)
coef(gam_mod)
# trying different methods to smooth out the function
gam_wig <- gam(nitrates ~ s(A_MMI, k = 70), data = total, method = "REML")
plot(gam_wig, residuals = TRUE, pch = 1)

# removing 3 more outliers
total <- total[total$nitrates < 6, ]
ggplot(total,aes(A_MMI,nitrates)) + geom_point()

#redoing GAM AND SUCCEDDING
gam_mod <- gam(nitrates ~ s(A_MMI), data = total, method = "REML")
gam_mod$sp
gam_mod_s2 <- gam(nitrates ~ s(A_MMI, k = 30), data = total, sp = 0.00001)
plot(gam_mod_s2, residuals = TRUE, pch = 1)
summary(gam_mod_s2)
# in the summary, it is telling me that the p-value is 0.295 so that means it is not a
#significant predictor 

#visualizing the GAM
plot(gam_mod_s2, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE)
plot(gam_mod_s2, residuals = TRUE, pch = 1, cex = 1,shade = TRUE)

#checking to see if the model fits - it doesn't
gam.check(gam_mod_s2)

#increasing the k - gives me a p-value of 0.249
gam_mod_s2 <- gam(nitrates ~ s(A_MMI, k = 10), data = total, sp = 0.0001)
plot(gam_mod_s2, residuals = TRUE, pch = 1)
summary(gam_mod_s2)
gam.check(gam_mod_s2)

##new technique for finding the right model for nitrates 
gam_mod <- gam(nitrates ~ s(A_MMI), data = total)
coef(gam_mod)
# I have 9 coeffeicents, and that should be my k value
gam_mod2 <- gam(nitrates ~ s(A_MMI, k = 9), data = total, method = "REML")
plot(gam_mod2, residuals = TRUE, pch = 1)
summary(gam_mod2)
gam.check(gam_mod2)
# says that the k is too low
# look at sp - smoothing parameter - it is 54766.89
gam_mod <- gam(nitrates ~ s(A_MMI), data = total, method = "REML")
gam_mod$sp
#now creating a GAM with this sp
gam_mod3 <- gam(nitrates ~ s(A_MMI), data = total, sp = 10000)
plot(gam_mod3, residuals = TRUE, pch = 1)
summary(gam_mod3)

# plotting the partial effects of our GAM plot
plot(gam_mod3, pages = 1, all.terms = TRUE)
plot(gam_mod3, rug = TRUE)
# plotting with the residuals - basically the same as the regular plot except you have
# control over the shape 'pch' and the size 'cex'
plot(gam_mod3, residuals = TRUE, pch = 1, cex = 1)
# the plot of the 95% confi interval for mean shape of effect
plot(gam_mod3, se = TRUE)
# add some shading
plot(gam_mod3, residuals = TRUE, pch = 1, shade = TRUE)
# shifting the graphs to have the intercepts as the first number; Now, the partial effect plot 
# has a more natural interpretation - it shows us the prediction of the output, 
#assuming other variables are at their average value. 
plot(gam_mod3, seWithMean = TRUE, shift = coef(gam_mod3)[1])

# combining the k and the sp - not different from the sp one without the k (the gam3 agrument) 
gam_mod4 <- gam(nitrates ~ s(A_MMI, k=9), data = total, sp = 10000)
summary(gam_mod4)
plot(gam_mod4, residuals = TRUE, pch = 1)
#increasing the k to change things - does not change anything
# only decreasing the sp made the model more wiggly 
gam_mod5 <- gam(nitrates ~ s(A_MMI, k=9), data = total, sp = 0.00001)
summary(gam_mod5)
plot(gam_mod5, residuals = TRUE, pch = 1)

##since it is saying that the function is linear, might as well try a GLM on it
my_glm <- lm(A_MMI ~ nitrates, data=total)
summary(my_glm)
plot(my_glm)


##taking out the non detects 
bib <- read.csv("/Users/claudiaasmann/Documents/nitrates - all regions - RSQA/justnitratRES.csv")
bob <- bib %>% mutate(resultsbib = ifelse(REMARK_CD == '<',0,RESULT_VA)) 

#redo every fucking thing for the non detects 
# cleaning up the data again and making a long data form where the col are SITENO and nitrates 
# these nitrates values for each site is averaged
cleanres <- bob %>% select(PARM_NM,SITE_NO,resultsbib)
cleanres = cleanres %>% drop_na()
cleanres2 <- cleanres %>% group_by(SITE_NO, PARM_NM) %>% summarize(median_val = median(resultsbib)) %>% ungroup()
cleanres3 <- pivot_wider(cleanres2,id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val) %>% na.omit()
# merging data 
total <- merge(cleanres3,aldata,by=c("SITE_NO")) %>% drop_na()
# somehow the number of data points went from 87 to 272
library(data.table)
setnames(total, "Nitrate, wf", "nitrates")
plot(total$A_MMI,total$nitrates)
# removing the outlier
total <- total[total$nitrates < 25, ]
plot(total$A_MMI,total$nitrates)

#GLM on nitrates - barely signifcantly linear 0.04
my_glm <- lm(A_MMI ~ nitrates, data=total)
summary(my_glm)
plot(my_glm)
#visualizing the GLM
termplot(my_glm, partial.resid = TRUE, se = TRUE)
termplot(my_glm, partial.resid = TRUE, col.res = "blue",smooth=panel.smooth)


## ALAGAE DATA 
library(readr)
aldata <- read_csv("~/Documents/allalgaedata .xlsx - just what I need.csv")
aldata <- aldata %>% drop_na()
View(aldata)


## DISSOLVED OXYGEN
#first cleaning up the data 
cleanoxy1 <- doxyRES %>% select(PARM_NM,SITE_NO,RESULT_VA)
cleanoxy1 = cleanoxy1 %>% drop_na()
cleanoxy2 <- cleanoxy1 %>% group_by(SITE_NO, PARM_NM) %>% summarize(median_val = median(RESULT_VA)) %>% ungroup()
cleanoxy3 <- pivot_wider(cleanoxy2,id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val) %>% na.omit()
#merging the data
totaloxy <- merge(cleanoxy3,aldata,by=c("SITE_NO")) %>% drop_na()
#GLM to see if it is linear or not
setnames(totaloxy, "Dissolved oxygen", "disoxy")
my_glm <- lm(A_MMI ~ disoxy, data=totaloxy)
summary(my_glm)
plot(my_glm)
termplot(my_glm, partial.resid = TRUE, col.res = "blue",smooth=panel.smooth)
termplot(my_glm)
# it is a significant linear predictor 
#GAM so that I have something nice?

gam_mod <- gam(disoxy ~ s(A_MMI), data = totaloxy)
summary(gam_mod)

## PHOSPHORUS
# the data
library(readr)
phosdata <- read.csv("~/Documents/phosph. wu all regions/Results.csv")
View(phosdata)
# cleaning up the data 
#take out the < - not working
phosdata <- read.csv("~/Documents/phosph. wu all regions/Results.csv")
cleanphos <- phosdata %>% mutate(resultsbib = ifelse(REMARK_CD == '<',0,RESULT_VA)) 


#what the hell is E? - estimated 
cleanphos1 <- phosdata %>% select(PARM_NM,SITE_NO,RESULT_VA)




