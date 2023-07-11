# houskova_corr
#
# Author: M. Weynants
# Date created: 2012/12/19
####################################################################### GENERAL
# LC
general1$LC_L1 <- 'B00'
general1$LC_L2 <- 'B10'
general1$LC_L3 <- 'B11'
# LU
general1$LU_L1 <- 'U110'
general1$LU_L2 <- 'U111'
# SRF_ERO_COV
general1$SRF_ERO_COV[15] <- 5
# WRB1998_RSG
general1$WRB1998_RSG[grepl('Fluvisol',general1$WRB1998_RSG)] <- 'FL'
general1$WRB1998_RSG[grepl('Chernozem',general1$WRB1998_RSG)] <- 'CH'
# WRB1998_ADJSPE1
general1$WRB1998_ADJSPE1[grepl('calcaric',general1$WRB1998_ADJSPE1)] <- 'ca'
general1$WRB1998_ADJSPE1[grepl('haplic',general1$WRB1998_ADJSPE1)] <- 'ha'
general1$WRB1998_ADJSPE1[grepl('mollic',general1$WRB1998_ADJSPE1)] <- 'mo'
general1$WRB1998_ADJSPE1[grepl('eutric',general1$WRB1998_ADJSPE1)] <- 'eu'
general1$WRB1998_ADJSPE2[grepl('calcaric',general1$WRB1998_ADJSPE2)] <- 'ca'
general1$WRB1998_ADJSPE2[grepl('arenic',general1$WRB1998_ADJSPE2)] <- 'ar'
general1$WRB1998_ADJSPE2[grepl('gleyic',general1$WRB1998_ADJSPE2)] <- 'gl'
general1$WRB1998_ADJSPE3[grepl('calcaric',general1$WRB1998_ADJSPE3)] <- 'ca'
# eroded?
general1$WRB1998_ADJSPE3[grepl('eroded',general1$WRB1998_ADJSPE3)] <- 'ND'

general1 <- general.checks(general1)

# METH
meth1 <- meth1[-7,]
meth1$METH_PAR[meth1$METH_PAR == 'Ks_M'] <- 'COND_M'
meth1<-meth.checks(meth1)

# BASIC
basic1$SAMPLE_DEP_TOP <- round(basic1$SAMPLE_DEP_TOP)
basic1$SAMPLE_DEP_BOT <- round(basic1$SAMPLE_DEP_BOT)
basic1[basic1$SAMPLE_DEP_TOP>=basic1$SAMPLE_DEP_BOT,c('SAMPLE_DEP_TOP','SAMPLE_DEP_BOT')] <- c(110,120)
# HOR1_NAME
basic1$HOR1_NAME <- sapply(basic1$HOR1_NAME,function(x){if (nchar(x) < 7 & x!='ND') {y <- paste(x,paste(rep(' ',7-nchar(x)),sep='',collapse=''),sep='')} else {if (!substr(x,7,7) %in% c(1:9,' ','')){x <- paste(substr(x,1,6),' ',sep='')} else {y<-x}}})

basic1<-basic.checks(basic1,general1$PROFILE_ID,meth1$CODE_M)

# CHEMICAL
# TB
chemical1[,'BASE_CATIONS']<-rowSums(chemical1[,c('EX_CA','EX_K','EX_MG','EX_NA')])
chemical1$BASE_CATIONS[chemical1$BASE_CATIONS < 0] <- -999
# CEC >= BASE_CATIONS
 chemical1[chemical1$CEC < round(chemical1$BASE_CATIONS,digit=1),'CEC'] <- chemical1[chemical1$CEC < round(chemical1$BASE_CATIONS,digit=1),'BASE_CATIONS']

chemical1 <- chemical.checks(chemical,basic1$SAMPLE_ID,meth1$CODE_M)

# PSIZE
ind <- substr(as.character(psize1$SAMPLE_ID),1,8) != as.character(psize1$PROFILE_ID)
psize1$PROFILE_ID[ind] <- as.numeric(substr(as.character(psize1$SAMPLE_ID[ind]),1,8))

psize1[psize1$P_M != 500,c('P_PERCENT','P_M')] <- c(psize1[psize1$P_M != 500,'P_M'],500)

psize1 <- psize.checks(psize1,basic1$SAMPLE_ID,meth1$CODE_M)

# RET
ret1 <- ret1[ret1$THETA != -999,]
ret1 <- ret.checks(ret1,basic1$SAMPLE_ID,meth1$CODE_M)

# COND
cond1 <- cond1[cond1$COND != -999,]
cond1 <- cond.checks(cond1,basic1$SAMPLE_ID,meth1$CODE_M)
#