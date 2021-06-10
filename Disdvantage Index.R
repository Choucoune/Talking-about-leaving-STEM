library(tidyverse)
library(plotrix)
library(ggplot2)
library(tibble)
library(na.tools)

sc_s <- read_tsv("/Users/sweaster/Documents/SEISMIC/Data/BPK_SEISMIC_COURSE_TABLE_12_Apr_2021.tab")
st_s <- read_tsv("/Users/sweaster/Documents/SEISMIC/Data/BPK_SEISMIC_STUDENT_TABLE_12_Apr_2021.tab")



# joining student table and student courses
just_phys <- sc_s %>% filter(crs_sbj == 'PHYSICS') %>% select(st_id,crs_name,gpao)
entry_phys <- just_phys %>% left_join(st_s)

# filter for just 140/160 and 240/260- common entry physics courses
# NOTE: this list will include those that took, for example, 140 AND 240. This is NOT just students' first physics course
entry_phys <- entry_phys %>% filter(crs_name %in% c('PHYSICS 140', 'PHYSICS 160', 'PHYSICS 240', 'PHYSICS 260'))
# remove NAs
entry_phys <- na.rm(entry_phys)
# remove '2 or more'
entry_phys <- entry_phys %>% filter(ethniccode_cat %in% c(0,1,2))

# create events for advantage index
entry_phys$Disdvantage_Ind <- 0

# event equals 1 if any four of the disadvantages (non-white,female,low-income,first-gen) are true
entry_phys[(!(entry_phys$ethniccode_cat==0)) | (entry_phys$female==1) | (entry_phys$firstgen==1) | (entry_phys$lowincomflag==1), "Disdvantage_Ind"] <- 1

# event equals 2 if any 2 out of the four of the disadvantages are true
entry_phys[(!(entry_phys$ethniccode_cat==0)) & (entry_phys$female==1), "Disdvantage_Ind"] <- 2
entry_phys[(!(entry_phys$ethniccode_cat==0)) & (entry_phys$firstgen==1), "Disdvantage_Ind"] <- 2
entry_phys[(!(entry_phys$ethniccode_cat==0)) & (entry_phys$lowincomflag==1), "Disdvantage_Ind"] <- 2
entry_phys[(entry_phys$female==1) & (entry_phys$firstgen==1), "Disdvantage_Ind"] <- 2
entry_phys[(entry_phys$female==1) & (entry_phys$lowincomflag==1), "Disdvantage_Ind"] <- 2
entry_phys[(entry_phys$firstgen==1) & (entry_phys$lowincomflag==1), "Disdvantage_Ind"] <- 2

# event equals 3 if any 3 out of the four of the disadvantages are true
entry_phys[(!(entry_phys$ethniccode_cat==0)) & (entry_phys$female==1) & (entry_phys$firstgen==1), "Disdvantage_Ind"] <- 3
entry_phys[(!(entry_phys$ethniccode_cat==0)) & (entry_phys$female==1) & (entry_phys$lowincomflag==1), "Disdvantage_Ind"] <- 3
entry_phys[(!(entry_phys$ethniccode_cat==0)) & (entry_phys$firstgen==1) & (entry_phys$lowincomflag==1), "Disdvantage_Ind"] <- 3
entry_phys[(entry_phys$female==1) & (entry_phys$firstgen==1) & (entry_phys$lowincomflag==1), "Disdvantage_Ind"] <- 3

# event equals 4 if all four of the disadvantages are true
entry_phys[(!(entry_phys$ethniccode_cat==0)) & (entry_phys$female==1) & (entry_phys$firstgen==1) & (entry_phys$lowincomflag==1), "Disdvantage_Ind"] <- 4



# Relationship between students’ mean course grade and disadvantage index
mngpa_disAd <- entry_phys %>% group_by(Disdvantage_Ind,crs_name) %>% summarize(mn=mean(gpao,na.rm=TRUE))
mngpa_disAd %>% ggplot(aes(x=Disdvantage_Ind,y=mn,color=crs_name))+geom_point()+ylim(0,4)


# exporting to desktop
write.csv(entry_phys, "/Users/sweaster/Documents/SEISMIC/Data/entryphys.csv", row.names = TRUE)





