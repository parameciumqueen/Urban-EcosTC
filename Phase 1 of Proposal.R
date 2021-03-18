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
ggplot(phosdata, aes(peribio,phostot)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm", se = FALSE)
# do a GLM - phosphorus 
# do a GLM - nitrogen 
# look at dissolved oxygen


