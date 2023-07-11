# mako_corr
# corrections to Mako's contribution to HYDI
#
# Author: M.Weynants
# Date created: 2012/12/10
######################################################################
print('Apply corrections and re-check')
# GENERAL

# METHOD

# BASIC
# HOR1_NAME should have 7 characters. fill with blanks
basic1$HOR1_NAME <- sapply(basic1$HOR1_NAME,function(x){if (nchar(x) < 7 & x!='ND') {y <- paste(x,paste(rep(' ',7-nchar(x)),sep='',collapse=''),sep='')} else {y<-x};y})
unique(basic$POR_M)
basic1 <- basic.checks(basic1,general$PROFILE_ID,meth1[[1]])

# CHEMICAL
# Can ACIDITY_NA4O > CEC ???

# COND. remove missing values
cond1 <- cond1[cond1$COND !=-999,]
cond1 <- cond.checks(cond1, basic1$SAMPLE_ID, meth1[[1]])
