# cleanCornelis
# 
# Author: M.Weynants
# Date created: 2013/01/18
######################################################################

# general
# basic
names(basic)[18]<-'POR'
# chemical
chemical[grepl('ND',chemical)]<--999
# cond
cond$COND_M <- as.numeric(cond$COND_M)
#cond[is.na(cond$COND_M),'COND_M'] <- -999
cond <- cond[cond$COND != -999,]


