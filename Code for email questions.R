#########################################################################
# The code is already set up to find the first physics course taken by 
# physics majors. Let's ask 'who starts with those courses'? So among the 
# same physics majors, compute the numbers of, say, males and females 
# or first generation (FIRST_GEN) physics majors that started in Physics 140, 
# 160, etc. Do different groups start in different physics courses? How 
# about URM/non-URM?
#########################################################################



# General Gatherings ------------------------------------------------------
library(tidyverse)
library(car)
sc <- read_tsv("/Users/sweaster/Documents/SEISMIC/Data/BPK_LARC_STUDENT_COURSE_20210209.tab")
sr <- read_tsv("/Users/sweaster/Documents/SEISMIC/Data/BPK_LARC_STUDENT_RECORD_20210209.tab") 

# reads from the student record to get all physics majors
n_physics <- sr %>% filter(UM_DGR_1_MAJOR_1_DES  == 'Physics BS') %>% mutate(POC = STDNT_ETHNC_GRP_SHORT_DES != 'White') %>% select(STDNT_ID,UM_DGR_1_MAJOR_1_DES,FIRST_TERM_ATTND_SHORT_DES,STDNT_SEX_SHORT_DES,FIRST_GEN,EST_GROSS_FAM_INC_DES,POC)

# gets all physics 140 course enrollments
phys_course <- sc %>% filter(SBJCT_CD == 'PHYSICS' & CATLG_NBR == 140) %>% select(STDNT_ID,TERM_SHORT_DES,GRD_PNTS_PER_UNIT_NBR)

# let's identify every phys majors first course in physics
# get all the first physics course that are lectures
all_phys <- sc %>% filter(SBJCT_CD == 'PHYSICS' & CRSE_CMPNT_CD == "LEC")

# get the first phys course!
all_phys <- all_phys %>% arrange(TERM_CD) %>% group_by(STDNT_ID) %>% filter(row_number() == 1)

# now just get out the physics majors and their first physics course
fcourse <- n_physics %>% left_join(all_phys)

# recoding integer responses to character responses for 'FIRST_GEN'
fcourse$FIRST_GEN <- recode(fcourse$FIRST_GEN, "'1'='FIRST GEN'; '0'='NON-FIRST GEN'")



# Making Lists for Top 5 First-Physics Courses ----------------------------
fcourse1 <- fcourse %>% mutate(FIRST_PHYS_CRSE = str_c(SBJCT_CD,CATLG_NBR)) %>% select(STDNT_ID,FIRST_PHYS_CRSE,TERM_SHORT_DES,STDNT_SEX_SHORT_DES,FIRST_GEN,POC,EST_GROSS_FAM_INC_DES)

# physics majors that took 160 first
fcourse_160 <- fcourse1 %>% filter(FIRST_PHYS_CRSE == 'PHYSICS160')

# physics majors that took 140 first
fcourse_140 <- fcourse1 %>% filter(FIRST_PHYS_CRSE == 'PHYSICS140')

# physics majors that took 340 first
fcourse_340 <- fcourse1 %>% filter(FIRST_PHYS_CRSE == 'PHYSICS340')

# physics majors that took 240 first
fcourse_240 <- fcourse1 %>% filter(FIRST_PHYS_CRSE == 'PHYSICS240')

# physics majors that took 260 first
fcourse_260 <- fcourse1 %>% filter(FIRST_PHYS_CRSE == 'PHYSICS260')



# Computing by Gender -----------------------------------------------------
# proportions + barplots

# Creating a plane to fit 6 different barplots
par(mfrow=c(1,6))

# proporion Male and Female physics majors total
tabgndr_tot <- table(fcourse$STDNT_SEX_SHORT_DES)
propgndr_tot <- prop.table(tabgndr_tot)
bpgndr_tot <- barplot(propgndr_tot, beside = TRUE, col = c("blue", "red"), xlab = "Total", 
                      legend = c("Female", "Male"),args.legend = list(title = "Gender", x = "topright", 
                                                                      cex = .6), ylim = c(0, 1))
text(bpgndr_tot, 0, round(propgndr_tot, 2),cex=1,pos=3, col = 'white')
# barplot(prop_tot, ylim=c(0,1), xlab = 'Total')

# proporion Male and Female that took 160 first
tabgndr_160 <- table(fcourse_160$STDNT_SEX_SHORT_DES)
propgndr_160 <- prop.table(tabgndr_160)
bpgndr_160 <- barplot(propgndr_160, beside = TRUE, col = c("blue", "red"), xlab = "Phys160", 
                      legend = c("Female", "Male"),args.legend = list(title = "Gender", x = "topright", 
                                                                      cex = .6), ylim = c(0, 1))
text(bpgndr_160, 0, round(propgndr_160, 2),cex=1,pos=3, col = 'white')
# barplot(propgndr_160, ylim=c(0,1), xlab = 'Phys160')

# proporion Male and Female that took 140 first
tabgndr_140 <- table(fcourse_140$STDNT_SEX_SHORT_DES)
propgndr_140 <- prop.table(tabgndr_140)
bpgndr_140 <- barplot(propgndr_140, beside = TRUE, col = c("blue", "red"), xlab = "Phys140", 
                      legend = c("Female", "Male"),args.legend = list(title = "Gender", x = "topright", 
                                                                      cex = .6), ylim = c(0, 1))
text(bpgndr_140, 0, round(propgndr_140, 2),cex=1,pos=3, col = 'white')
# barplot(propgndr_140, ylim=c(0,1), xlab = 'Phys140')

# proporion Male and Female that took 340 first
tabgndr_340 <- table(fcourse_340$STDNT_SEX_SHORT_DES)
propgndr_340 <- prop.table(tabgndr_340)
bpgndr_340 <- barplot(propgndr_340, beside = TRUE, col = c("blue", "red"), xlab = "Phys340", 
                      legend = c("Female", "Male"),args.legend = list(title = "Gender", x = "topright", 
                                                                      cex = .6), ylim = c(0, 1))
text(bpgndr_340, 0, round(propgndr_340, 2),cex=1,pos=3, col = 'white')
# barplot(propgndr_340, ylim=c(0,1), xlab = 'Phys340')

# proporion Male and Female that took 240 first
tabgndr_240 <- table(fcourse_240$STDNT_SEX_SHORT_DES)
propgndr_240 <- prop.table(tabgndr_240)
bpgndr_240 <- barplot(propgndr_240, beside = TRUE, col = c("blue", "red"), xlab = "Phys240", 
                      legend = c("Female", "Male"),args.legend = list(title = "Gender", x = "topright", 
                                                                      cex = .6), ylim = c(0, 1))
text(bpgndr_240, 0, round(propgndr_240, 2),cex=1,pos=3, col = 'white')
# barplot(propgndr_240, ylim=c(0,1), xlab = 'Phys240')

# proporion Male and Female that took 260 first
tabgndr_260 <- table(fcourse_260$STDNT_SEX_SHORT_DES)
propgndr_260 <- prop.table(tabgndr_260)
bpgndr_260 <- barplot(propgndr_260, beside = TRUE, col = c("blue", "red"), xlab = "Phys260", 
                      legend = c("Female", "Male"),args.legend = list(title = "Gender", x = "topright", 
                                                                      cex = .6), ylim = c(0, 1))
text(bpgndr_260, 0, round(propgndr_260, 2),cex=1,pos=3, col = 'white')
# barplot(propgndr_260, ylim=c(0,1), xlab = 'Phys260')



# Computing by First Gen --------------------------------------------------
# proportions + barplots

# Creating a plane to fit 6 different barplots
par(mfrow=c(1,6))

# proporion First Gen and Non-First Gen physics majors total
tabfgen_tot <- table(fcourse$FIRST_GEN)
propfgen_tot <- prop.table(tabfgen_tot)
bpfgen_tot <- barplot(propfgen_tot, beside = TRUE, col = c("blue", "red"), xlab = "Total", 
                      legend = c("FIRST", "NON"),args.legend = list(title = "First Gen Status", x = "topright", 
                                                                      cex = .6), ylim = c(0, 1))
text(bpfgen_tot, 0, round(propfgen_tot, 2),cex=1,pos=3, col = 'white')

# proporion First Gen and Non-First Gen that took 160 first
tabfgen_160 <- table(fcourse_160$FIRST_GEN)
propfgen_160 <- prop.table(tabfgen_160)
bpfgen_160 <- barplot(propfgen_160, beside = TRUE, col = c("blue", "red"), xlab = "Phys160", 
                      legend = c("FIRST", "NON"),args.legend = list(title = "First Gen Status", x = "topright", 
                                                                    cex = .6), ylim = c(0, 1))
text(bpfgen_160, 0, round(propfgen_160, 2),cex=1,pos=3, col = 'white')
# barplot(propfgen_160, ylim=c(0,1), xlab = 'Phys160')

# proporion First Gen and Non-First Gen that took 140 first
tabfgen_140 <- table(fcourse_140$FIRST_GEN)
propfgen_140 <- prop.table(tabfgen_140)
bpfgen_140 <- barplot(propfgen_140, beside = TRUE, col = c("blue", "red"), xlab = "Phys140", 
                      legend = c("FIRST", "NON"),args.legend = list(title = "First Gen Status", x = "topright", 
                                                                    cex = .6), ylim = c(0, 1))
text(bpfgen_140, 0, round(propfgen_140, 2),cex=1,pos=3, col = 'white')
# barplot(propfgen_140, ylim=c(0,1), xlab = 'Phys140')

# proporion First Gen and Non-First Gen that took 340 first
tabfgen_340 <- table(fcourse_340$FIRST_GEN)
propfgen_340 <- prop.table(tabfgen_340)
bpfgen_340 <- barplot(propfgen_340, beside = TRUE, col = c("blue", "red"), xlab = "Phys340", 
                      legend = c("FIRST", "NON"),args.legend = list(title = "First Gen Status", x = "topright", 
                                                                    cex = .6), ylim = c(0, 1))
text(bpfgen_340, 0, round(propfgen_340, 2),cex=1,pos=3, col = 'white')
# barplot(propfgen_340, ylim=c(0,1), xlab = 'Phys340')

# proporion First Gen and Non-First Gen that took 240 first
tabfgen_240 <- table(fcourse_240$FIRST_GEN)
propfgen_240 <- prop.table(tabfgen_240)
bpfgen_240 <- barplot(propfgen_240, beside = TRUE, col = c("blue", "red"), xlab = "Phys240", 
                      legend = c("FIRST", "NON"),args.legend = list(title = "First Gen Status", x = "topright", 
                                                                    cex = .6), ylim = c(0, 1))
text(bpfgen_240, 0, round(propfgen_240, 2),cex=1,pos=3, col = 'white')
# barplot(propfgen_240, ylim=c(0,1), xlab = 'Phys240')

# proporion First Gen and Non-First Gen that took 260 first
tabfgen_260 <- table(fcourse_260$FIRST_GEN)
propfgen_260 <- prop.table(tabfgen_260)
bpfgen_260 <- barplot(propfgen_260, beside = TRUE, col = c("blue", "red"), xlab = "Phys260", 
                      legend = c("FIRST", "NON"),args.legend = list(title = "First Gen Status", x = "topright", 
                                                                    cex = .6), ylim = c(0, 1))
text(bpfgen_260, 0, round(propfgen_260, 2),cex=1,pos=3, col = 'white')
# barplot(propfgen_260, ylim=c(0,1), xlab = 'Phys260')



# Computing by URM --------------------------------------------------------
# People of Color
fcourse_POC <- fcourse1 %>% filter(POC == 'TRUE')

# White low income 
fcourse_NONPOC_lowinc1 <- fcourse1 %>% filter(EST_GROSS_FAM_INC_DES == 'Less than $25,000' & POC == 'FALSE')
fcourse_NONPOC_lowinc2 <- fcourse1 %>% filter(EST_GROSS_FAM_INC_DES == '$25,000 - $49,999' & POC == 'FALSE')
fcourse_NONPOC_lowinc_1_2 <- fcourse_NONPOC_lowinc1 %>% full_join(fcourse_NONPOC_lowinc2)

# White med income
fcourse_NONPOC_medinc <- fcourse1 %>% filter(EST_GROSS_FAM_INC_DES == '$50,000 - $74,999' & POC == 'FALSE')

# Full list White-URM
fcourse_NONPOC_URM_FullList <- fcourse_NONPOC_lowinc_1_2 %>% full_join(fcourse_NONPOC_medinc)

# Full list URM
fcourse_URM <- fcourse_POC %>% full_join(fcourse_NONPOC_URM_FullList)

# White high income
fcourse_NONPOC_highinc1 <- fcourse1 %>% filter(EST_GROSS_FAM_INC_DES == '$75,000 - $99,999' & POC == 'FALSE')
fcourse_NONPOC_highinc2 <- fcourse1 %>% filter(EST_GROSS_FAM_INC_DES == '$100,000 - $149,999' & POC == 'FALSE')
fcourse_NONPOC_highinc3 <- fcourse1 %>% filter(EST_GROSS_FAM_INC_DES == 'More than $100,000' & POC == 'FALSE')
fcourse_NONPOC_highinc4 <- fcourse1 %>% filter(EST_GROSS_FAM_INC_DES == '$150,000 - $199,999' & POC == 'FALSE')
fcourse_NONPOC_highinc5 <- fcourse1 %>% filter(EST_GROSS_FAM_INC_DES == 'More than $200,000' & POC == 'FALSE')

# Joining groups 1&2
fcourse_NONPOC_highinc_1_2 <- fcourse_NONPOC_highinc1 %>% full_join(fcourse_NONPOC_highinc2)

# Joining groups 1&2&3
fcourse_NONPOC_highinc_1_2_3 <- fcourse_NONPOC_highinc_1_2 %>% full_join(fcourse_NONPOC_highinc3)

# Joining groups 1&2&3&4
fcourse_NONPOC_highinc_1_2_3_4 <- fcourse_NONPOC_highinc_1_2_3 %>% full_join(fcourse_NONPOC_highinc4)

# Full list non-URM
fcourse_NONURM <- fcourse_NONPOC_highinc_1_2_3_4 %>% full_join(fcourse_NONPOC_highinc5)

# creating URM/NON-URM variable to make plots
# fcourse$URM <- recode(fcourse_URM)