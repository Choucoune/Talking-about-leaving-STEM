count_physics_majors <- function(sc,sr)
{

  library(tidyverse)
  #sc <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_COURSE_20210209.tab")
  #sr <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_RECORD_20210209.tab") 
 
  #reads from the student record to get all physics majors
  n_physics <- sr %>% filter(UM_DGR_1_MAJOR_1_DES  == 'Physics BS') %>% select(STDNT_ID,UM_DGR_1_MAJOR_1_DES,FIRST_TERM_ATTND_SHORT_DES)
  
  #gets all physics 140 course enrollments
  phys_course <- sc %>% filter(SBJCT_CD == 'PHYSICS' & CATLG_NBR == 140) %>% select(STDNT_ID,TERM_SHORT_DES,GRD_PNTS_PER_UNIT_NBR)
  
  #join the last two tables
  out <- n_physics %>% left_join(phys_course,by='STDNT_ID')
  print(out %>% filter(TERM_SHORT_DES != 'NA') %>% tally())
  
  
  phys_course <- sc %>% filter(SBJCT_CD == 'PHYSICS' & CATLG_NBR == 160) %>% select(STDNT_ID,TERM_SHORT_DES,GRD_PNTS_PER_UNIT_NBR)
  out <- n_physics %>% left_join(phys_course,by='STDNT_ID')
  print(out %>% filter(TERM_SHORT_DES != 'NA') %>% tally())
  
  phys_course <- sc %>% filter(SBJCT_CD == 'PHYSICS' & CATLG_NBR == 135) %>% select(STDNT_ID,TERM_SHORT_DES,GRD_PNTS_PER_UNIT_NBR)
  out <- n_physics %>% left_join(phys_course,by='STDNT_ID')
  print(out %>% filter(TERM_SHORT_DES != 'NA') %>% tally())
  
  
  #let's identify every phys majors first course in physics
  #1) get all the first physics course
  all_phys <- sc %>% filter(SBJCT_CD == 'PHYSICS' & CRSE_CMPNT_CD == "LEC") # get all physics courses that are lectures
  all_phys <- all_phys %>% arrange(TERM_CD) %>% group_by(STDNT_ID) %>% filter(row_number() == 1) #get the first phys course!
  
  #now just get out the physics majors
  fcourse <- n_physics %>% left_join(all_phys)
  
  #and count up their favorite first physics course
  tab     <- fcourse %>% group_by(CATLG_NBR) %>% tally() %>% arrange(desc(n))
  print(tab)
  
  #find the most commonly taken courses (at all!) after your first physics course
  first_phys <- fcourse %>% mutate(FIRST_PHYS_TERM=TERM_CD) %>% select(STDNT_ID,FIRST_PHYS_TERM)
  all_course <- first_phys %>% left_join(sc,by='STDNT_ID') %>% filter(TERM_CD > FIRST_PHYS_TERM)
  
  #now count up the courses taken by ALL physics students after their first class in Phyiscs.
  ctab <- all_course %>% mutate(CRSE=str_c(SBJCT_CD,CATLG_NBR)) %>% group_by(CRSE) %>% tally() %>% arrange(desc(n))
  View(ctab)
  
  
  #find the most commonly taken courses (at all!) after your first physics course LABELED!!!
  first_phys <- fcourse %>% mutate(FIRST_PHYS_TERM=TERM_CD) %>% mutate(FIRST_PHYS_CRSE = str_c(SBJCT_CD,CATLG_NBR)) %>% 
                select(STDNT_ID,FIRST_PHYS_TERM,FIRST_PHYS_CRSE)
  all_course <- first_phys %>% left_join(sc,by='STDNT_ID') %>% filter(TERM_CD > FIRST_PHYS_TERM)
  
  #now count up the courses taken by ALL physics students after their first class in Phyiscs.
  ctab <- all_course %>% mutate(CRSE=str_c(SBJCT_CD,CATLG_NBR)) %>% group_by(FIRST_PHYS_CRSE,CRSE) %>% tally() %>% arrange(desc(n))
  View(ctab)
  
  return(out)
   
}