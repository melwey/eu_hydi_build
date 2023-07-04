# lamorsiki_corr
# 
# Author: M.Weynants
# Date created: 2012/12/19
##################################################################
# BASIC
basic1$BD[basic1$BD==0]<- -999
# RET
ret1[ret1$THETA>1,'THETA'] <- ret1[ret1$THETA>1,'THETA']/10
# COND: remove missing values
cond1 <- cond1[cond$COND!=-999,]

