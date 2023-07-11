# cleanKvaerno

# Author: M. Weynants
# Date created: 2012/12/17
##################################################################
# GENERAL
general0 <- general
general <-general0[,-16]
general[,'PAR_MAT'] <- as.numeric(general0$PAR_MAT)
# what to do when 2 PAR_MAT? keep only first? (6 cases)
general[is.na(general$PAR_MAT),'PAR_MAT'] <- as.numeric(substr(general0[is.na(general$PAR_MAT),'PAR_MAT'],start=1,stop=4))
general$YEAR <- as.numeric(general0$YEAR)
# what to do when 2 years? first one
general[is.na(general$YEAR),'YEAR'] <- as.numeric(substr(general0[is.na(general$YEAR),'YEAR'],start=1,stop=4))
general$COMMENTS3 <- 'ND'
general <- general[!is.na(general$PROFILE_ID),]

# METHOD
meth[nchar(meth$X)>0,'METHOD']<- paste(meth$METHOD[nchar(meth$X)>0],meth$X[nchar(meth$X)>0],sep='. ')

# BASIC
# SAMPLE_ID not unique
basic0 <- basic
names(basic)[3] <- 'SAMPLE_POS'
#basic[basic$PROFILE_ID == 57800042,'SAMPLE_ID'] <- as.numeric(paste('57800042',1:3,sep='0'))
basic[basic$PROFILE_ID==57800445,'SAMPLE_POS'] <-1:6
basic[basic$PROFILE_ID==57800445,'SAMPLE_ID'] <- 57800445*100 + 1:6#as.numeric(paste('57800445',1:6,sep='0'))
basic[basic$SAMPLE_ID==5780018802,4:5] <- c(10,15)
basic[basic$SAMPLE_ID==5780018804,4:5] <- c(28,32)
basic[,4] <- as.numeric(basic[,4])
basic[,5] <- as.numeric(basic[,5])
names(basic)[4:5] <- c('SAMPLE_DEP_TOP','SAMPLE_DEP_BOT')


# CHEMICAL
# SAMPLE_ID not unique
chemical0 <- chemical
#chemical[chemical$SAMPLE_ID %in% chemical$SAMPLE_ID[duplicated(chemical$SAMPLE_ID)],1:5]
#chemical[as.numeric(substr(as.character(chemical$SAMPLE_ID),1,8)) != chemical$PROFILE_ID,1:8]
chemical <- chemical[,!grepl("BASE_CATIONS_M",names(chemical))]
#