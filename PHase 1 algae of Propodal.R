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
## 

## need to merge the data frames
## then use mohanads code to match them again by site number 



#Nitrates
#Phosphates
#Dissolved Oxygen