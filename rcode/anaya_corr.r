# anaya_corr
# 
# Author: M.Weynants
# Date created: 2012/12/20
####################################################################
# GENERAL
general1$ISO_COUNTRY <- 'ES'
# LC
general1$LC_L1 <- general1$LC_L3
general1$LU_L1 <- general1$LC_L2
# LC_L1 to LC_L3
# codes from levels 1, 2 and 3 are mixed
s <- general1$LC_L1
s2 <- substr(s,start=2,stop=2)
s3 <- substr(s,start=3,stop=3)
general1$LC_L3[!grepl('0',s3)] <- s[!grepl('0',s3)]
general1$LC_L2 <- paste(substr(s,start=1,stop=2),'0',sep='')
general1$LC_L2[grepl('ND',s) | grepl('0',s2) | grepl('X',s2)] <- 'ND'
general1$LC_L1 <- paste(substr(s,start=1,stop=1),'00',sep='')
general1$LC_L1[grepl('ND',s)] <- 'ND'
# unique(general1$LC_L1)
# unique(general1$LC_L2)
# unique(general1$LC_L3)
# LU_L1 to LU_L2
general1$LU_L1[general1$LU_L1=='0']<-'ND'
s <- general1$LU_L1
general1$LU_L2[!grepl('0',substr(s,3,3))] <- s[!grepl('0',substr(s,3,3))]
general1$LU_L1[! grepl('ND',s)] <- paste(substr(s[! grepl('ND',s)],start=1,stop=3),'0',sep='')
# unique(general1$LU_L1)
# unique(general1$LU_L2)
# SRF_ROCK_COV
general1$SRF_ROCK_COV <- gsub(' ','',general1$SRF_ROCK_COV)
# SRF_COAR_COV
general1$SRF_COAR_COV <- gsub(' ','',general1$SRF_COAR_COV)
# SRF_ERO_COV
general1$SRF_ERO_COV <- -999
# SRF_ERO_DEG
general1$SRF_ERO_DEG <- gsub(' ','',general1$SRF_ERO_DEG)
# SRF_SEAL_THIC
general1$SRF_SEAL_THIC <- gsub(' ','',general1$SRF_SEAL_THIC)
# SRF_SAL_COV
general1$SRF_SAL_COV <- -999
# PAR_MAT
general1$PAR_MAT[general1$PAR_MAT == 'ND'] <- -999
general1$PAR_MAT <- as.numeric(general1$PAR_MAT)

general1 <- general.checks(general1)

# METHOD
meth1$METH_PAR <- gsub('PH_H20_M','PH_H2O_M',meth1$METH_PAR)
meth1 <- meth.checks(meth1)

# BASIC
# sample_id do not match profile_id
# for those: change sample_id to profile_id+sample_pos
smpl <- basic1$SAMPLE_ID
ind <- basic1$PROFILE_ID != round(basic1$SAMPLE_ID/100)
basic1$SAMPLE_ID[ind] <- basic1$PROFILE_ID[ind]*100+basic1$SAMPLE_POS[ind]
# change sample pos
basic1$SAMPLE_POS <- basic1$SAMPLE_ID - basic1$PROFILE_ID*100

#smpl[basic1$SAMPLE_ID %in% basic1$SAMPLE_ID[duplicated(basic1$SAMPLE_ID)]]

# HOR1_NAME
basic1$HOR1_NAME <- sapply(basic1$HOR1_NAME,function(x){if (nchar(x) < 7 & x!='ND') {y <- paste(x,paste(rep(' ',7-nchar(x)),sep='',collapse=''),sep='')} else {if (nchar(x)>7){y <- substr(x,1,7)} else {y<-x}};y})
nd <- basic$HOR1_NAME=="ND"
table(substr(basic1$HOR1_NAME[!nd],1,1))
table(substr(basic1$HOR1_NAME[!nd],2,2))
table(substr(basic1$HOR1_NAME[!nd],3,3))
table(substr(basic1$HOR1_NAME[!nd],4,4))
table(substr(basic1$HOR1_NAME[!nd],5,5))
table(substr(basic1$HOR1_NAME[!nd],6,6))
table(substr(basic1$HOR1_NAME[!nd],7,7))
# correcting case by case...
# POR: one value=4730
# BD: two values = 9.99 => -999
basic1$BD[basic1$BD>2.5] <- -999

basic1 <- basic.checks(basic1,general1$PROFILE_ID,meth1$CODE_M)

# CHEMICAL
# different number of rows in basic and chemical!!!
# one pH of 0.3
chemical1[chemical1$PH_H2O<3.5 & chemical1$PH_H2O != -999,'PH_H2O']<--999
# EX_CA out of range
# compute base_cations
chemical1$BASE_CATIONS <- rowSums(chemical1[,c('EX_NA','EX_MG','EX_K','EX_CA')])
chemical1[chemical1$BASE_CATIONS < 0,'BASE_CATIONS'] <- -999
#chemical1[chemical1$BASE_CATIONS > 1.1*chemical$CEC & chemical1$CEC!=-999,c('SAMPLE_ID','CEC','EX_NA','EX_MG','EX_K','EX_CA','BASE_CATIONS')]
chemical1 <- chemical.checks(chemical1,basic1$SAMPLE_ID,meth1$CODE_M)


# PSIZE
# SAMPLE_ID not in basic
# sample_id do not match profile_id
# psize1[psize1$PROFILE_ID != round(psize1$SAMPLE_ID/100),]
psize1.smpl <- psize1$SAMPLE_ID
# sample_id too large
ind <- psize1$SAMPLE_ID > 1e10
psize1$SAMPLE_ID[ind] <- psize1$SAMPLE_ID[ind] - psize1$PROFILE_ID[ind]*1000 + psize1$PROFILE_ID[ind]*100
# sample_id too small (lack hor_pos)
psize1$SAMPLE_ID[psize1$SAMPLE_ID==724808080] <- 7248080804
psize1 <- psize1[psize1$SAMPLE_ID != 724806100,]
psize1 <- psize.checks(psize1,basic1$SAMPLE_ID,meth1$CODE_M)

# RET

# SAMPLE_ID not in basic
# sample_id do not match profile_id
ret1[ret1$PROFILE_ID != round(ret1$SAMPLE_ID/100),1:5]
ret1.smpl<-ret1$SAMPLE_ID
# sample_id too large
ind <- ret1$SAMPLE_ID > 1e10
ret1$SAMPLE_ID[ind] <- ret1$SAMPLE_ID[ind] - ret1$PROFILE_ID[ind]*1000 + ret1$PROFILE_ID[ind]*100
# remove samples not in basic (29 samples!)
ret1 <- ret1[ret1$SAMPLE_ID %in% basic1$SAMPLE_ID,]

# duplicates
# tmp<-ret1[ret1$SAMPLE_ID %in% ret1$SAMPLE_ID[duplicated(ret1[,c("SAMPLE_ID","HEAD","THETA")])],1:5]
# tmp[do.call(order,tmp),]
ret1 <- ret1[!duplicated(ret1[,c("SAMPLE_ID","HEAD","THETA")]),]
ret1 <- ret.checks(ret1,basic1$SAMPLE_ID,meth1$CODE_M)

# COND
# remove duplicates
cond1 <- cond1[!duplicated(cond1[,c("SAMPLE_ID","VALUE","COND")]),]
cond1 <- cond.checks(cond1,basic1$SAMPLE_ID,meth1$CODE_M)

