st <- read_tsv("/Users/sweaster/Documents/SEISMIC/Data/BPK_SEISMIC_STUDENT_TABLE_16_Jun_2021.tab")
sc <- read_tsv("/Users/sweaster/Documents/SEISMIC/Data/BPK_SEISMIC_COURSE_TABLE_16_Jun_2021.tab")


st$is_phys_major <- 0

# populating column
st[(st$current_major %in% c('Physics BS','Physics BSChem','Interdisciplinary Physics BA',
                            'Interdisciplinary Physics BS','General Physics BS')),'is_phys_major']<- 1


phys <- sc %>% filter(crs_sbj == 'PHYSICS' & crs_catalog %in% c(140,160,240,260,340,390)) %>% left_join(st)


st_s <- st %>% select(st_id,female,ethniccode_cat,firstgen,is_phys_major,lowincomflag)

# creating tables for class enrollment 
sc140 <- phys %>% filter(crs_name == 'PHYSICS 140') %>% mutate(Phys140_enrl=str_c(class_number,'_',crs_termcd)) %>% 
  rename(Phys140_GPA=numgrade) %>% select(st_id,Phys140_enrl,Phys140_GPA)
sc160 <- phys %>% filter(crs_name == 'PHYSICS 160') %>% mutate(Phys160_enrl=str_c(class_number,'_',crs_termcd)) %>% 
  rename(Phys160_GPA=numgrade) %>% select(st_id,Phys160_enrl,Phys160_GPA)
sc240 <- phys %>% filter(crs_name == 'PHYSICS 240') %>% mutate(Phys240_enrl=str_c(class_number,'_',crs_termcd)) %>% 
  rename(Phys240_GPA=numgrade) %>% select(st_id,Phys240_enrl,Phys240_GPA)
sc260 <- phys %>% filter(crs_name == 'PHYSICS 260') %>% mutate(Phys260_enrl=str_c(class_number,'_',crs_termcd)) %>% 
  rename(Phys260_GPA=numgrade) %>% select(st_id,Phys260_enrl,Phys260_GPA)
sc340 <- phys %>% filter(crs_name == 'PHYSICS 340') %>% mutate(Phys340_enrl=str_c(class_number,'_',crs_termcd)) %>% 
  rename(Phys340_GPA=numgrade) %>% select(st_id,Phys340_enrl,Phys340_GPA)
sc390 <- phys %>% filter(crs_name == 'PHYSICS 390') %>% mutate(Phys390_enrl=str_c(class_number,'_',crs_termcd)) %>% 
  rename(Phys390_GPA=numgrade) %>% select(st_id,Phys390_enrl,Phys390_GPA)

# making one table with student record info
st_s    <- st_s %>% left_join(sc140,by='st_id')
st_s    <- st_s %>% left_join(sc160,by='st_id')
st_s    <- st_s %>% left_join(sc240,by='st_id')
st_s    <- st_s %>% left_join(sc260,by='st_id')
st_s    <- st_s %>% left_join(sc340,by='st_id')
st_s    <- st_s %>% left_join(sc390,by='st_id')


# changing enrollment status to 0-1 for didn't enroll-did enroll for regression purposes
st_s$Phys140_enrl[!is.na(st_s$Phys140_enrl)] <- 1
st_s$Phys140_enrl[is.na(st_s$Phys140_enrl)] <- 0
st_s$Phys160_enrl[!is.na(st_s$Phys160_enrl)] <- 1
st_s$Phys160_enrl[is.na(st_s$Phys160_enrl)] <- 0
st_s$Phys240_enrl[!is.na(st_s$Phys240_enrl)] <- 1
st_s$Phys240_enrl[is.na(st_s$Phys240_enrl)] <- 0
st_s$Phys260_enrl[!is.na(st_s$Phys260_enrl)] <- 1
st_s$Phys260_enrl[is.na(st_s$Phys260_enrl)] <- 0
st_s$Phys340_enrl[!is.na(st_s$Phys340_enrl)] <- 1
st_s$Phys340_enrl[is.na(st_s$Phys340_enrl)] <- 0
st_s$Phys390_enrl[!is.na(st_s$Phys390_enrl)] <- 1
st_s$Phys390_enrl[is.na(st_s$Phys390_enrl)] <- 0

st_s$Phys140_enrl <- as.numeric(st_s$Phys140_enrl)
st_s$Phys160_enrl <- as.numeric(st_s$Phys160_enrl)
st_s$Phys240_enrl <- as.numeric(st_s$Phys240_enrl)
st_s$Phys260_enrl <- as.numeric(st_s$Phys260_enrl)
st_s$Phys340_enrl <- as.numeric(st_s$Phys340_enrl)
st_s$Phys390_enrl <- as.numeric(st_s$Phys390_enrl)


# only selecting last row of a student's record
st_s <- st_s %>% group_by(st_id) %>% slice(n())

# removing those who never took any classes in the sequence
st_s <- st_s %>% filter(!(Phys140_enrl==0 & Phys160_enrl==0 & Phys240_enrl==0 & Phys260_enrl==0 & Phys340_enrl==0 & Phys390_enrl==0))



# Defining Events ---------------------------------------------------------
# creating events for when students enroll in previous course but not the next course
st_s$P140_EVENT <- 0
st_s$P160_EVENT <- 0
st_s$P240_EVENT <- 0
st_s$P260_EVENT <- 0
st_s$P340_EVENT <- 0
st_s$P390_EVENT <- 0

# Event=1 for P140/P160 when student doesn't enroll in P240/260
# if event in P140, P160 event column will be NA
# if event in P160, P140 event column will be NA
st_s[st_s$Phys140_enrl==1 & st_s$Phys240_enrl==0 & st_s$Phys260_enrl==0, "P140_EVENT"] <- 1
st_s[st_s$Phys160_enrl==1 & st_s$Phys240_enrl==0 & st_s$Phys260_enrl==0, "P160_EVENT"] <- 1


# Event=1 for P240/P260 when student enrolls in P240/P260 and P140/P160 but doesn't enroll in P340/P390
# If student already experienced event in P140/P160, P240 event column will be NA 
st_s[st_s$Phys240_enrl==1 & st_s$Phys340_enrl==0 & st_s$Phys390_enrl==0, "P240_EVENT"] <- 1
st_s[st_s$P140_EVENT==1 | st_s$P160_EVENT==1, "P240_EVENT"] <- NA
st_s[st_s$Phys260_enrl==1 & st_s$Phys340_enrl==0 & st_s$Phys390_enrl==0, "P260_EVENT"] <- 1
st_s[st_s$P140_EVENT==1 | st_s$P160_EVENT==1, "P260_EVENT"] <- NA


# Event=1 for P340 when student enrolls in P140/P160, P240/P260, and P340 but doesn't enroll in 390
# if event already experienced in either P140/P160 or P240/P260 then event for 340 will be NA
st_s[st_s$Phys340_enrl==1 & st_s$Phys390_enrl==0, "P340_EVENT"] <- 1
st_s[st_s$P240_EVENT==1 | st_s$P260_EVENT==1, "P340_EVENT"] <- NA


# Event=1 for P390 when student enrolls in P140/P160, P240/P260, P340 and P390 but gets GPA less than 1.7 in P390 (dfw)
# if event already experienced in either P140/P160 or P240/P260 or 340 then event for 390 will be NA
st_s$Phys390_GPA[is.na(st_s$Phys390_GPA)] = 0
st_s[st_s$Phys390_GPA < 1.7, "P390_EVENT"] <- 1 
st_s[st_s$P340_EVENT==1, "P390_EVENT"] <- NA


# Some students have direct transition from Course 1 to Course 3 or 4
# don't count that as an Event for P140/P160
st_s[st_s$Phys140_enrl==1 & st_s$Phys240_enrl==0 & st_s$Phys260_enrl==0 & (st_s$Phys340_enrl==1 | st_s$Phys390_enrl==1), 'P140_EVENT'] <- 0
st_s[st_s$Phys160_enrl==1 & st_s$Phys240_enrl==0 & st_s$Phys260_enrl==0 & (st_s$Phys340_enrl==1 | st_s$Phys390_enrl==1), 'P160_EVENT'] <- 0
st_s[st_s$Phys140_enrl==1 & st_s$Phys240_enrl==0 & st_s$Phys260_enrl==0 & (st_s$Phys340_enrl==1 | st_s$Phys390_enrl==1), 'P240_EVENT'] <- 0
st_s[st_s$Phys140_enrl==1 & st_s$Phys240_enrl==0 & st_s$Phys260_enrl==0 & (st_s$Phys340_enrl==1 | st_s$Phys390_enrl==1), 'P260_EVENT'] <- 0
st_s[st_s$Phys160_enrl==1 & st_s$Phys240_enrl==0 & st_s$Phys260_enrl==0 & (st_s$Phys340_enrl==1 | st_s$Phys390_enrl==1), 'P240_EVENT'] <- 0
st_s[st_s$Phys160_enrl==1 & st_s$Phys240_enrl==0 & st_s$Phys260_enrl==0 & (st_s$Phys340_enrl==1 | st_s$Phys390_enrl==1), 'P260_EVENT'] <- 0

# Some students have direct transition from Course 2 to Course  4
# don't count that as an Event for P240/P260
st_s[st_s$Phys240_enrl==1 & st_s$Phys340_enrl==0 & st_s$Phys390_enrl==1, 'P240_EVENT'] <- 0
st_s[st_s$Phys260_enrl==1 & st_s$Phys340_enrl==0 & st_s$Phys390_enrl==1, 'P260_EVENT'] <- 0

# some students only take 340 and not 390 but still major in physics
# don't count that as an Event for P340
st_s[st_s$Phys140_enrl==0 & st_s$Phys160_enrl==0 & st_s$Phys240_enrl==0 & st_s$Phys260_enrl==0 & 
       st_s$Phys340_enrl==1 & st_s$Phys390_enrl==0 & st_s$is_phys_major==1, 'P340_EVENT'] <- 0
st_s[st_s$Phys140_enrl==0 & st_s$Phys160_enrl==0 & st_s$Phys240_enrl==0 & st_s$Phys260_enrl==0 & 
       st_s$Phys340_enrl==1 & st_s$Phys390_enrl==0 & st_s$is_phys_major==1, 'P390_EVENT'] <- 0

# some students take the first two courses but not the last two and end up with the major
# count these students as completed the sequence
st_s[(st_s$Phys140_enrl==1 | st_s$Phys160_enrl==1) & (st_s$Phys240_enrl==1 | st_s$Phys260_enrl==1) & st_s$Phys340_enrl==0 & 
       st_s$Phys390_enrl==0 & st_s$is_phys_major==1, 'P240_EVENT'] <- 0
st_s[(st_s$Phys140_enrl==1 | st_s$Phys160_enrl==1) & (st_s$Phys240_enrl==1 | st_s$Phys260_enrl==1) & st_s$Phys340_enrl==0 & 
       st_s$Phys390_enrl==0 & st_s$is_phys_major==1, 'P260_EVENT'] <- 0
st_s[(st_s$Phys140_enrl==1 | st_s$Phys160_enrl==1) & (st_s$Phys240_enrl==1 | st_s$Phys260_enrl==1) & st_s$Phys340_enrl==0 & 
       st_s$Phys390_enrl==0 & st_s$is_phys_major==1, 'P340_EVENT'] <- 0
st_s[(st_s$Phys140_enrl==1 | st_s$Phys160_enrl==1) & (st_s$Phys240_enrl==1 | st_s$Phys260_enrl==1) & st_s$Phys340_enrl==0 & 
       st_s$Phys390_enrl==0 & st_s$is_phys_major==1, 'P390_EVENT'] <- 0

# if enrollment in 390 is 0 then the GPA is NA, however, NAs are converted to 0s for the purposes of a dfw event. Here I'm ensuring that 
# students who never enrolled in 390 have GPAs of value NA and not 0
st_s[st_s$Phys390_enrl==0 & (!is.na(st_s$Phys390_GPA)) & st_s$Phys390_GPA!=1, 'Phys390_GPA'] <- NA

# some students don't take 390 but end up with the major
# count these students as completed the sequence
st_s[st_s$P140_EVENT==0 & st_s$P160_EVENT==0 & st_s$P240_EVENT==0 & st_s$P260_EVENT==0 & st_s$P340_EVENT==1 & 
       st_s$is_phys_major==1, 'P340_EVENT'] <- 0
st_s[st_s$P140_EVENT==0 & st_s$P160_EVENT==0 & st_s$P240_EVENT==0 & st_s$P260_EVENT==0 & st_s$P340_EVENT==0 & 
       st_s$is_phys_major==1, 'P390_EVENT'] <- 0


# create individual events
st_s$seq_compl <- 0


# completed given sequence: 140/160 -> 240/260 -> 340 -> 390
st_s[st_s$P140_EVENT!=1 & (!is.na(st_s$P140_EVENT)) & st_s$P160_EVENT!=1 & (!is.na(st_s$P160_EVENT)) & st_s$P240_EVENT!=1 & 
       (!is.na(st_s$P240_EVENT)) & st_s$P260_EVENT!=1 & (!is.na(st_s$P260_EVENT)) & st_s$P340_EVENT!=1 & 
       (!is.na(st_s$P340_EVENT)) &st_s$P390_EVENT!=1 & (!is.na(st_s$P390_EVENT)), 'seq_compl'] <- 1


st_s <- st_s %>% select(st_id, female, ethniccode_cat, firstgen, lowincomflag, Phys140_enrl, Phys140_GPA, Phys160_enrl, 
                        Phys160_GPA, Phys240_enrl, Phys240_GPA, Phys260_enrl, Phys260_GPA, Phys340_enrl, Phys340_GPA, 
                        Phys390_enrl, Phys390_GPA, P140_EVENT, P160_EVENT, P240_EVENT, P260_EVENT, P340_EVENT, P390_EVENT,
                        seq_compl, is_phys_major)

# create a table with events that reflect 1st, 2nd, 3rd, and 4th course 
