# romano_corr

# Author: M.Weynants
# Date created: 2012/12/17
# Last update: 2013/01/18
######################################################################
# GENERAL
# coordinates?
general1$CONTACT_A <- paste('+390',as.character(general1$CONTACT_A),sep='')
general1<-general.checks(general1)

# BASIC
# SAMPLE_POS
# basic[basic$PROFILE_ID == unique(basic$PROFILE_ID)[which(sapply(unique(basic$PROFILE_ID),function(a){any(duplicated(basic[basic$PROFILE_ID==a & BASIC$SAMPLE_POS !=-999,3]))}))],1:5]
basic1[basic1$PROFILE_ID == 38000283,'SAMPLE_POS'] <- as.numeric(as.character(substr(basic1$SAMPLE_ID[basic1$PROFILE_ID == 38000283],start=10,stop=10)))
# SAMPLE_DEP_TOP
ind<-basic1$PROFILE_ID %in% basic1$PROFILE_ID[basic1$SAMPLE_DEP_TOP> basic1$SAMPLE_DEP_BOT]
basic1[ind,'SAMPLE_DEP_TOP'] <- c(0,30,60)
basic1[ind,'SAMPLE_DEP_BOT'] <- c(30,60,90)
# HOR1_NAME
#table(nchar(basic1$HOR1_NAME[basic1$HOR1_NAME != 'ND']))
basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==1]<-paste(' ',basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==1],'     ',sep='')
#
ind <- grepl("^[[:upper:]][[:digit:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind]<-paste(' ',substr(basic1$HOR1_NAME[ind],1,1),'    ',substr(basic1$HOR1_NAME[ind],2,2),sep='')
ind <- grepl("^[[:digit:]][[:upper:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind]<-paste(basic1$HOR1_NAME[ind],'     ',sep='')
ind <- grepl("^[[:upper:]][[:lower:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind]<-paste(' ',substr(basic1$HOR1_NAME[ind],1,1),'  ',substr(basic1$HOR1_NAME[ind],2,2),'  ',sep='')
#
ind <- grepl("^[[:upper:]][[:lower:]][[:digit:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind]<-paste(' ',substr(basic1$HOR1_NAME[ind],1,1),'  ',substr(basic1$HOR1_NAME[ind],2,2),' ',substr(basic1$HOR1_NAME[ind],3,3),sep='')
ind <- grepl("^[[:digit:]][[:upper:]][[:lower:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind]<-paste(substr(basic1$HOR1_NAME[ind],1,2),'  ',substr(basic1$HOR1_NAME[ind],3,3),'  ',sep='')
ind <- grepl("^[[:upper:]][[:lower:]][[:lower:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind]<-paste(' ',substr(basic1$HOR1_NAME[ind],1,1),'  ',substr(basic1$HOR1_NAME[ind],2,3),' ',sep='')
ind <- grepl("^[[:digit:]][[:upper:]][[:digit:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind]<-paste(substr(basic1$HOR1_NAME[ind],1,2),'    ',substr(basic1$HOR1_NAME[ind],3,3),sep='')
#
basic1$HOR1_NAME[basic1$HOR1_NAME=='Bwb1']<-' B  wb1'
basic1$HOR1_NAME[basic1$HOR1_NAME=='3Bw2']<-'3B  w 2'

# HOR1_TOP
# -999, OK
# HOR2_NAME
basic1$HOR2_NAME[basic1$HOR2_NAME=='Bwb2']<-' B  wb2'
basic1$HOR2_NAME[basic1$HOR2_NAME=='4C2'] <-'4C    2'
basic1$HOR2_NAME[basic1$HOR2_NAME=='Ap2'] <-' A  p 2'

basic1 <- basic.checks(basic1,general1$PROFILE_ID,meth$CODE_M)

# PSIZE
psize1[psize1$P_PERCENT <0,]
psize1 <- psize1[!is.na(psize1$P_M),]
psize1 <- psize.checks(psize1,basic1$SAMPLE_ID,meth$CODE_M)


# RET
# SAMPLE_ID
ret1[ret1$THETA>1,1:5]
ret1 <- ret.checks(ret1,basic1$SAMPLE_ID,meth$CODE_M)

# COND
cond1 <- cond.checks(cond1,basic1$SAMPLE_ID,meth$CODE_M)
