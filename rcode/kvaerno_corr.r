# kvaerno_corr

# Author: M.Weynants
# Date created: 2012/12/17
# Last update: 2013/01/21
####################################################################

# GENERAL
# LC_L1:truncate
# !!! CAUTION: I am loosing info !!!
lc <- general1$LC_L1
general1$LC_L1 <- paste(substr(lc,start=1,stop=1),'00',sep='')
general1$LC_L1[lc == 'ND'] <- 'ND'
# LC_2
general1$LC_L2 <- paste(substr(lc,start=1,stop=2),'0',sep='')
general1$LC_L2[lc == 'ND' | grepl('00',general1$LC_L2)] <- 'ND'
# LC_3
general1$LC_L3 <- substr(lc,start=1,stop=3)
general1$LC_L3[lc == 'ND' | grepl('0',general1$LC_L3)] <- 'ND'
general1$LC_L3[grepl('C',general1$LC_L3)] <- paste(substr(lc[grepl('C',general1$LC_L3)],1,1),substr(lc[grepl('C',general1$LC_L3)],3,3),sep='X')
# LU_L1
lu <- general1$LU_L1
general1$LU_L1 <- paste(substr(lu,start=1,stop=3),'0',sep='')
general1$LU_L1[lu == 'ND'] <- 'ND'
# LU_L2
general1$LU_L2 <- lu
general1$LU_L2[lu == 'ND' | grepl('0',general1$LU_L2)] <- 'ND'

# SITE LDFRM
general1$SITE_LANDFORM[nchar(general1$SITE_LANDFORM)>2] <- substr(general1$SITE_LANDFORM[nchar(general1$SITE_LANDFORM)>2],start=1,stop=2)
# SITE_SLOP_POS
general1$SITE_SLOP_POS[general1$SITE_SLOP_POS =='V'] <- 'ND'
# SITE_SLOP_GRAD
general1$SITE_SLOP_GRAD <- substr(general1$SITE_SLOP_GRAD,start=1,stop=2)
# SRF_ROCK_DIS
general1$SRF_ROCK_DIS[general1$SRF_ROCK_DIS == -998] <- -999
# SRF_COAR_SIZ
general1$SRF_COAR_SIZ <- 'ND'
# SRF_ERO_CAT
general1$SRF_ERO_CAT[grepl(',',general1$SRF_ERO_CAT)] <- 'W'
# AGE
general1$AGE[nchar(general1$AGE)>3] <- substr(general1$AGE[nchar(general1$AGE)>3],1,3)
general1$AGE <- gsub('I','l',general1$AGE)
general1$AGE <- gsub('lpp','lPp',general1$AGE)
general1$AGE <- gsub('lpi','lPi',general1$AGE)
# WRB 2006
general1$WRB2006_RSG <- gsub('RE','RG',general1$WRB2006_RSG)
general1$WRB2006_RSG <- gsub('CA','CM',general1$WRB2006_RSG)
general1$WRB2006_SQ2 <- gsub('si','sl',general1$WRB2006_SQ2)

general1<-general.checks(general1)

# METHODS
meth1<-meth.checks(meth1)

# BASIC
# SAMPLE_POS
basic1$SAMPLE_POS[basic1$SAMPLE_ID==5780043504] <- 4
# SAMPLE_DEP_TOP/BOT
basic1[,4:5] <- round(basic1[,4:5])
# HOR1_NAME
#basic[basic1$HOR1_NAME != 'ND' &  nchar(basic1$HOR1_NAME)!=7, c(1,2,6:8)]
basic1$HOR1_NAME[162] <- ' L     '
basic1$HOR1_NAME[c(237,276,297,300)] <- paste(basic1$HOR1_NAME[c(237,276,297,300)],' ',sep='')
basic1$HOR1_NAME <- gsub(' D ',' R ',basic1$HOR1_NAME)
#basic1[!substr(basic1$HOR1_NAME,2,2) %in% c('H', 'O', 'A', 'E', 'B', 'C', 'R', 'I', 'L', 'W', ' ') & !grepl('ND',basic1$HOR1_NAME),c(1:2,6:8)]
basic1$HOR1_NAME[basic1$SAMPLE_ID==5780006102] <- ' B/C   '
basic1$HOR1_NAME[basic1$SAMPLE_ID==5780006504] <- ' B/E   '
basic1$HOR1_NAME[basic1$SAMPLE_ID %in% c(5780006802,5780013902)] <- ' E/B   '
#basic1[!substr(basic1$HOR1_NAME,5,5) %in% c(letters,'@',' ') & !grepl('ND',basic1$HOR1_NAME),c(1:2,6:8)]
basic1$HOR1_NAME[basic1$SAMPLE_ID == 5780049604] <- '2B C   '
# HOR1_TOP < HOR1_BOT
#basic1[basic1$HOR1_TOP <0 & basic1$HOR1_TOP !=-999,c(1,2,6:8)]
# basic1[basic1$HOR1_TOP > basic$HOR1_BOT & basic1$HOR1_BOT !=-999,c(1,2,6:8)]: empty
# HOR2_TOP
#basic1[basic1$HOR2_NAME != 'ND' & nchar(basic1$HOR2_NAME)!=7,c(1,2,6:8)]
# STRUCTURE1
# basic1[basic1$STRUCTURE1 != 'ND' & nchar(basic1$STRUCTURE1)!=6,c(1,2,15:17)]
basic1[basic1$STRUCTURE1 %in% c('MA','SG'),'STRUCTURE1'] <- paste('    ',basic1[basic1$STRUCTURE1 %in% c('MA','SG'),'STRUCTURE1'],sep='')
basic1[basic1$STRUCTURE1 %in% c('  MA'),'STRUCTURE1'] <- paste('  ',basic1[basic1$STRUCTURE1 %in% c('  MA'),'STRUCTURE1'],sep='')
basic1[basic1$STRUCTURE1 == '  SG  ','STRUCTURE1'] <- '    SG'
basic1[basic1$STRUCTURE1 == '  MA  ','STRUCTURE1'] <- '    MA'
basic1[basic1$SAMPLE_ID == 5780050102,'STRUCTURE1'] <- 'MOFIGR'
# STRUCTURE2
#basic1[grepl('MA',basic1$STRUCTURE2),c(1,2,15:17)]
basic1[basic1$STRUCTURE2 == '  SG  ','STRUCTURE2'] <- '    SG'
basic1[basic1$STRUCTURE2 == '  MA  ','STRUCTURE2'] <- '    MA'
# POR_M
# for some data POR_M is unknown (profiles 57800068 to 57800086 and 57800171 to 57800177)
# COARSE_M
# sample 5780019706: coarse=0, coarse_m=-999
basic1 <- basic.checks(basic1,general1$PROFILE_ID,meth1$CODE_M)

# NA introduced by coercion

# CHEMICAL
chemical1$OC <- as.numeric(chemical1$OC)
# NA introduced by coercion: < 0.1 under detection limit
chemical1[is.na(chemical1$OC),'OC'] <- 0
chemical1$EX_NA <- as.numeric(chemical$EX_NA)
# NA introduced by coercion: < 0.1 or <0.2 under detection limit
chemical1[is.na(chemical1$EX_NA),'EX_NA'] <- 0
chemical1$EX_MG <- as.numeric(chemical$EX_MG)
# NA introduced by coercion: < 0.1 under detection limit
chemical1[is.na(chemical1$EX_MG),'EX_MG'] <- 0
# BASE CATIONS is expressed as percentage of CEC
# change to sum of cations
chemical1$BASE_CATIONS <- rowSums(chemical1[,c('EX_NA','EX_MG','EX_CA','EX_K')])
chemical1$BASE_CATIONS[chemical1$BASE_CATIONS<0] <- -999
# CEC=BASE_CATIONS+ACIDITY_NA4O
chemical1$CEC <- rowSums(chemical1[,c("BASE_CATIONS","ACIDITY_NA4O")])
chemical1$CEC[chemical1$CEC<0] <- -999
chemical1$CEC_M[chemical1$CEC!=-999]<-190

# chemical1[chemical1$BASE_CATIONS !=-999 & chemical1$BASE_CATIONS>chemical1$CEC,c('CEC','BASE_CATIONS','ACIDITY_NA4O')]
# ACIDITY_NA4O
# chemical1[chemical1$ACIDITY_NA4O !=-999 & chemical1$ACIDITY_NA4O>chemical1$CEC,c('SAMPLE_ID','CEC','ACIDITY_NA4O')]
# ACIDITY_KCL
chemical1$ACIDITY_KCL <- as.numeric(chemical$ACIDITY_KCL)
# NA introduced by coercion: < 0.06 under detection limit
chemical1[is.na(chemical1$ACIDITY_KCL),'ACIDITY_KCL'] <- 0

chemical1 <- chemical.checks(chemical1,basic1$SAMPLE_ID,meth1$CODE_M)

# PSIZE
# remove P_SIZE >2000 and redo the sums to get 1000. COARSE already OK
psize1 <- psize.checks(psize1,basic1$SAMPLE_ID,meth1$CODE_M)

# RET
ret1$SAMPLE_ID[ret1$SAMPLE_ID == 5780054405] <- 5780044405
ret1$PROFILE_ID[ret1$SAMPLE_ID == 5780008403] <- 57800084
# duplicated
#ret1[duplicated(cbind(ret1$SAMPLE_ID,ret1$HEAD)),2] <- 5780044403
ret1 <- ret1[ret1$HEAD!=-999,]
ret1 <- ret.checks(ret1,basic1$SAMPLE_ID,meth1$CODE_M)

# COND
cond1 <- cond.checks(cond, basic1$SAMPLE_ID, meth1$CODE_M)
