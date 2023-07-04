# patyka_corr.r
# corrections to patyka's contribution to HYDI
#
# Author: M.Weynants
# Date created: 2012/12/03
######################################################################
print('Apply corrections and re-check')
# general
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
s <- general1$LU_L1
general1$LU_L2[s %in% c('U111','U112')] <- s[s %in% c('U111','U112')]
general1$LU_L1[s %in% c('U111','U112')] <- paste(substr(s[s %in% c('U111','U112')],start=1,stop=3),'0',sep='')
general1$LU_L1[grepl('U36x',s)] <- 'U360'
# unique(general1$LU_L1)
# unique(general1$LU_L2)
# SRF_SAL_COV
general1$SRF_SAL_COV <- -999
# WRB2006_RSG
general1$WRB2006_RSG <- toupper(general1$WRB2006_RSG)

general1 <- general.checks(general1)

# meth
meth1$METH_PAR <- gsub(' ' ,'',meth1$METH_PAR)
meth1$METH_PAR <- gsub('P_SIZE','P_M',meth1$METH_PAR)
meth1 <- meth.checks(meth1)

# basic
# HOR1_NAME should have 7 characters. fill with blanks or truncate
basic1$HOR1_NAME <- sapply(basic1$HOR1_NAME,function(x){if (nchar(x) < 7 & x!='ND') {y <- paste(x,paste(rep(' ',7-nchar(x)),sep='',collapse=''),sep='')} else {if (nchar(x)>7){y <- substr(x,1,7)} else {y<-x}};y})
# case to case corrections
ind<-substr(basic1$HOR1_NAME,4,4)%in%c('h','t')
basic1$HOR1_NAME[ind] <- paste(substr(basic1$HOR1_NAME[ind],1,3),substr(basic1$HOR1_NAME[ind],4,6),sep=' ')
# STRUCTURE1
basic1$STRUCTURE1[basic1$STRUCTURE1 =='       LU'] <- '    LU'
basic1$STRUCTURE1[nchar(basic1$STRUCTURE1)>6] <- substr(basic1$STRUCTURE1[nchar(basic1$STRUCTURE1)>6],start=3,stop=8)
basic1$STRUCTURE1[basic1$STRUCTURE1 =='MO'] <- 'MO    '
basic1$STRUCTURE1[basic1$STRUCTURE1 =='  SG'] <- '    SG'
basic1$STRUCTURE1[basic1$STRUCTURE1 == '    RG'] <- '    GR'

basic1$STRUCTURE2[basic1$STRUCTURE2=='GR']<-'    GR'

basic1 <- basic.checks(basic1,general1$PROFILE_ID,meth1$CODE_M)

# chemical
chemical1$PH_H2O[chemical1$PH_H2O<1] <- -999
chemical1$CEC[chemical1$SAMPLE_ID %in% c(8040062501,8040062502,8040092901)] <- c(28.8,32.7,3.6)
chemical1 <- chemical.checks(chemical1,basic1$SAMPLE_ID,meth1$CODE_M)

# psize
# Corrections are brought by Tatyana Laktionova (NSC ISSAR, Kharkiv). 2013/02/04
psize1[psize1$SAMPLE_ID %in% c(8040023504,8040031506,8040036401),"P_PERCENT"] <- c(0.1,2.1,63.8,6.7,7.7,19.6,0.0,13.8,59.2,6.4,3.2,17.4,1.5,49.8,33.1,3.5,2.9,9.2)
psize1[psize1$SAMPLE_ID %in% 8040023504,"P_M"] <- 500

psize1 <- psize.checks(psize1,basic1$SAMPLE_ID,meth1$CODE_M)

# ret
# no data

# cond
# no data