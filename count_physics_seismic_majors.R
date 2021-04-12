count_physics_seismic_majors <- function(sc,sr)
{
  
  library(tidyverse)
  #sc <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_COURSE_20210209.tab")
  #sr <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_RECORD_20210209.tab") 
  
  #reads from the student record to get all physics majors
  n_physics <- sr %>% filter(current_major  == 'Physics BS') %>% select(st_id,current_major,cohort)
  
  #gets all physics 140 course enrollments
  phys_course <- sc %>% filter(crs_sbj == 'PHYSICS' & crs_catalog == 140) %>% select(st_id,crs_term,numgrade)
  
  #join the last two tables
  out <- n_physics %>% left_join(phys_course,by='st_id')
  print(out %>% filter(crs_term != 'NA') %>% tally())
  
  
  phys_course <- sc %>% filter(crs_sbj == 'PHYSICS' & crs_catalog == 160) %>% select(st_id,crs_term,numgrade)
  out <- n_physics %>% left_join(phys_course,by='st_id')
  print(out %>% filter(crs_term != 'NA') %>% tally())
  
  phys_course <- sc %>% filter(crs_sbj == 'PHYSICS' & crs_catalog == 135) %>% select(st_id,crs_term,numgrade)
  out <- n_physics %>% left_join(phys_course,by='st_id')
  print(out %>% filter(crs_term != 'NA') %>% tally())
  
  
  #let's identify every phys majors first course in physics
  #1) get all the first physics course
  all_phys <- sc %>% filter(crs_sbj == 'PHYSICS' & crs_component == "LEC") # get all physics courses that are lectures
  all_phys <- all_phys %>% arrange(crs_termcd) %>% group_by(st_id) %>% filter(row_number() == 1) #get the first phys course!
  
  #now just get out the physics majors
  fcourse <- n_physics %>% left_join(all_phys)
  
  #and count up their favorite first physics course
  tab     <- fcourse %>% group_by(crs_catalog) %>% tally() %>% arrange(desc(n))
  print(tab)
  
  #find the most commonly taken courses (at all!) after your first physics course
  first_phys <- fcourse %>% mutate(FIRST_PHYS_TERM=crs_termcd) %>% select(st_id,FIRST_PHYS_TERM)
  all_course <- first_phys %>% left_join(sc,by='st_id') %>% filter(crs_term > FIRST_PHYS_TERM)
  
  #now count up the courses taken by ALL physics students after their first class in Phyiscs.
  ctab <- all_course %>% mutate(CRSE=str_c(crs_sbj,crs_catalog)) %>% group_by(CRSE) %>% tally() %>% arrange(desc(n))
  View(ctab)
  
  
  #find the most commonly taken courses (at all!) after your first physics course LABELED!!!
  first_phys <- fcourse %>% mutate(FIRST_PHYS_TERM=crs_termcd) %>% mutate(FIRST_PHYS_CRSE = str_c(crs_sbj,crs_catalog)) %>% 
    select(st_id,FIRST_PHYS_TERM,FIRST_PHYS_CRSE)
  all_course <- first_phys %>% left_join(sc,by='st_id') %>% filter(crs_termcd > FIRST_PHYS_TERM)
  
  #now count up the courses taken by ALL physics students after their first class in Phyiscs.
  ctab <- all_course %>% mutate(CRSE=str_c(crs_sbj,crs_catalog)) %>% group_by(FIRST_PHYS_CRSE,CRSE) %>% tally() %>% arrange(desc(n))
  View(ctab)
  
  return(out)
  
}