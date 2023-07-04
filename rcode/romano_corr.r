# romano_corr

# Author: M.Weynants
# Date created: 2012/12/17
# Update: 2013/01/21
######################################################################
# GENERAL
# solved by contributor
general1<-general.checks(general1)

# BASIC
# SAMPLE_POS
# solved by contributor
# SAMPLE_DEP_TOP
# solved by contributor
# HOR1_NAME
#table(nchar(basic1$HOR1_NAME[basic1$HOR1_NAME != 'ND']))
basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==6] <- paste(basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==6],' ',sep='')
basic1$HOR1_NAME <- substr(basic1$HOR1_NAME,1,7)
basic1$HOR1_NAME[basic1$HOR1_NAME == " 2C   1"] <- "2C    1"
basic1$HOR1_NAME[basic1$HOR1_NAME == " B    w"] <- " B  w  "

# HOR1_TOP
# -999, OK
# HOR2_NAME
basic1$HOR2_NAME[basic1$HOR2_NAME=='Bwb2']<-' B  wb2'
basic1$HOR2_NAME[basic1$HOR2_NAME=='4C2'] <-'4C    2'
basic1$HOR2_NAME[basic1$HOR2_NAME=='Ap2'] <-' A  p 2'

basic1 <- basic.checks(basic1,general1$PROFILE_ID,meth$CODE_M)

# chemical
chemical1$SAMPLE_ID[which(chemical1$SAMPLE_ID==3800025602)[2]] <- 3800025603
chemical1 <- chemical.checks(chemical1,basic1$SAMPLE_ID,meth$CODE_M)

# PSIZE
psize1 <- psize.checks(psize1,basic1$SAMPLE_ID,meth$CODE_M)

# RET
# SAMPLE_ID not in BASIC
ret1$SAMPLE_ID[ret1$SAMPLE_ID==3800016202]<-3800016201
# -999
ret1 <- ret1[ret1$HEAD!=-999,]
ret1 <- ret.checks(ret1,basic1$SAMPLE_ID,meth$CODE_M)

# COND
cond1<-cond1[cond1$COND!=-999,]
cond1 <- cond.checks(cond1,basic1$SAMPLE_ID,meth$CODE_M)
