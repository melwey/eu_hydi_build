# cleanStrauss
#
# Author: M.Weynants
# Date created: 2013/01/10
######################################################################

# general
general0<-general
general[is.na(general)] <- 'ND'
general[,c(7,21,25,33,35)] <- -999
# add one character to profile_id
#general$PROFILE_ID <- as.numeric(paste(substr(as.character(general$PROFILE_ID),1,2),'0',substr(as.character(general$PROFILE_ID),3,7),sep=''))

# method

# basic
basic0<-basic
#basic$PROFILE_ID <- as.numeric(paste(substr(as.character(basic$PROFILE_ID),1,2),'0',substr(as.character(basic$PROFILE_ID),3,7),sep=''))
#basic$SAMPLE_ID <- as.numeric(paste(substr(as.character(basic$SAMPLE_ID),1,2),'0',substr(as.character(basic$SAMPLE_ID),3,9),sep=''))
basic$HOR1_NAME<-'ND'

# chemical
chemical0<-chemical
#chemical$PROFILE_ID <- as.numeric(paste(substr(as.character(chemical$PROFILE_ID),1,2),'0',substr(as.character(chemical$PROFILE_ID),3,7),sep=''))
#chemical$SAMPLE_ID <- as.numeric(paste(substr(as.character(chemical$SAMPLE_ID),1,2),'0',substr(as.character(chemical$SAMPLE_ID),3,9),sep=''))

# psize
psize0<-psize
#psize$PROFILE_ID <- as.numeric(paste(substr(as.character(psize$PROFILE_ID),1,2),'0',substr(as.character(psize$PROFILE_ID),3,7),sep=''))
#psize$SAMPLE_ID <- as.numeric(paste(substr(as.character(psize$SAMPLE_ID),1,2),'0',substr(as.character(psize$SAMPLE_ID),3,9),sep=''))

# ret
ret0<-ret
#ret$PROFILE_ID <- as.numeric(paste(substr(as.character(ret$PROFILE_ID),1,2),'0',substr(as.character(ret$PROFILE_ID),3,7),sep=''))
#ret$SAMPLE_ID <- as.numeric(paste(substr(as.character(ret$SAMPLE_ID),1,2),'0',substr(as.character(ret$SAMPLE_ID),3,9),sep=''))

# cond
cond0 <- cond
#cond$PROFILE_ID <- as.numeric(paste(substr(as.character(cond$PROFILE_ID),1,2),'0',substr(as.character(cond$PROFILE_ID),3,7),sep=''))
#cond$SAMPLE_ID <- as.numeric(paste(substr(as.character(cond$SAMPLE_ID),1,2),'0',substr(as.character(cond$SAMPLE_ID),3,9),sep=''))
