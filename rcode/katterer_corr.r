# katterer_corr
# corrections to Katterer's contribution to HYDI
#
# Author: M.Weynants
# Date created: 2012/12/05
######################################################################
print('Apply corrections and re-check')
# method
# METH_PAR
ind <- !grepl('_M',meth1$METH_PAR)
meth1[ind,'METH_PAR'] <- paste(meth1[ind,'METH_PAR'],'_M',sep='')
meth1 <- meth1[order(meth1$CODE_M),]
meth1 <- meth.checks(meth1)

# BASIC
# ind.chem <- match(basic$SAMPLE_ID,chemical$SAMPLE_ID)
# plot(basic$BD[basic$BD!=-999 & chemical$OC[ind.chem]!=-999],chemical$OC[ind.chem][basic$BD!=-999 & chemical$OC[ind.chem]!=-999])
basic1$BD_M[basic1$BD==-999]<-999

# CHEMICAL
chemical1$OC_M[chemical1$OC==-999] <- -999

# PSIZE
# S <- sapply(unique(psize1$SAMPLE_ID),function(a){y<-sum(psize1$P_PERCENT[psize1$SAMPLE_ID==a])})

# RET
# SAMPLE_ID not in  BASIC
# remove them
ret1 <- ret1[ret1$SAMPLE_ID %in% basic1$SAMPLE_ID,]
ret1 <- ret.checks(ret1,basic1$SAMPLE_ID,meth1$CODE_M)

# COND
cond1[,7:15] <- -999
cond1 <- cond.checks(cond1,basic1[[2]],meth1[[1]])
#