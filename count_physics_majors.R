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
  
  
  #here's a comment, but it wont' do anhthing yet.
  
  return(out)
   
}