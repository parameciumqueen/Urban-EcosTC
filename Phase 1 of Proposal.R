#data cleaning 
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
