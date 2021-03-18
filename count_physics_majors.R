count_physics_majors <- function(sc,sr)
{

  library(tidyverse)
  #sc <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_COURSE_20210209.tab")
  #sr <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_RECORD_20210209.tab") 
 
  n_physics <- sr %>% filter(UM_DGR_1_MAJOR_1_DES  == 'Physics BS') %>% select(STDNT_ID,UM_DGR_1_MAJOR_1_DES,FIRST_TERM_ATTND_SHORT_DES)
  phys_course <- sc %>% filter(SBJCT_CD == 'PHYSICS' & CATLG_NBR == 140) %>% select(STDNT_ID,TERM_SHORT_DES,GRD_PNTS_PER_UNIT_NBR)
  
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
  
  return(out)
   
}