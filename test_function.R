test_function <- function(sr,sc)
{
  #sr <- read_csv('/Users/bkoester/temp/PLA/data/student.record.csv')
  #sc <- read_csv('/Users/bkoester/temp/PLA/data/student.course.csv')
  
  counts  <- sc %>% filter(SUBJECT == 'PHYSICS' & CATALOG_NBR == 140) %>% tally()
  counts2 <- sc %>% filter(SUBJECT == 'PHYSICS' & CATALOG_NBR == 340) %>% tally()
  
  sc140  <- sc %>% filter(SUBJECT == 'PHYSICS' & CATALOG_NBR == 140)
  full   <- left_join(sc140,sr,by='ANONID')
  
  
  pl <- new_function(full)
  
  print(pl)
  
  return(pl)
}

new_function <- function(data)
{
  
  p <- data %>% group_by(TERM) %>% summarize(MNGRD=mean(GRD_PTS_PER_UNIT))
  pl <- p %>% ggplot(aes(x=TERM,y=MNGRD))+geom_point()
  return(pl)
  
}