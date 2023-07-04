# cleanIovino
# Adapts HYDI tables to comply to guidelines v1.1
#
# Author: M.Weynants
# Date created: 2012/12/03
####################################################################
print('Adapting to guidelines')
# GENERAL
general0 <- general
tmp <- cbind(general[,1:36],GENERAL[,37:50],general[,41:55])
# tmp$WRB1998_RSG <- general$WRB_GRP
# tmp$WRB1998_ADJSPE1 <- paste(general$WRB_ADJ,general$WRB_SPE,sep='')
general <- tmp

# BASIC
basic0 <- basic
tmp <- cbind(basic[,1:2],-999,basic[,3:13],BASIC[,15:17],basic[,18:23])
names(tmp)[3]<-"SAMPLE_POS"
tmp$STRUCTURE1 <- paste(basic$STR_GRADE,basic$STR_SIZE,basic$STR_SHAPE,sep='')
tmp$STR_COMB <- basic$STR_COMB
basic <- tmp

# CHEMICAL
chemical0 <- chemical
names(chemical)[2] <- 'SAMPLE_ID'
chemical <- chemical[,-26]

# PSIZE
psize0 <- psize
names(psize)[2] <- 'SAMPLE_ID'

# RET
ret0 <- ret
names(ret)[2] <-'SAMPLE_ID'

# COND
cond0 <- cond
names(cond)[2] <- 'SAMPLE_ID'
cond[[15]] <- -999

# METHOD, TSERMETA and TSERDATA: nothing to change
print('... done')

