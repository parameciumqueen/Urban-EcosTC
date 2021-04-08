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
gam_mod_s2 <- gam(nitrates ~ s(A_MMI, k = 40), data = total, sp = 0.0001)
plot(gam_mod_s2, residuals = TRUE, pch = 1)
summary(gam_mod_s2)
gam.check(gam_mod_s2)

#put the outliers back? and then redo the GAM?


#Phosphates
#Dissolved Oxygen