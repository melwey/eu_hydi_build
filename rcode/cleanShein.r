# cleanShein
# Adapts HYDI tables to comply to guidelines v1.1
#
# Author: M.Weynants
# Date created: 2012/12/12
####################################################################
print('Adapting to guidelines')
# GENERAL
general0 <- general
tmp <- cbind(general[,1:40],GENERAL[,41:43],general[,42:46],GENERAL[,49:50],general[,47:61])
names(tmp) <- names(GENERAL)
tmp[,c(41:43,49:50)] <- 'ND'
tmp$DAY <- -999
general <- tmp

basic0 <- basic
tmp <- cbind(basic[,1:14],BASIC[,15:17],basic[,18:23])
tmp[,c(10:11,13:14)] <- -999
tmp$STRUCTURE1 <- paste(substr(basic$STR_SHAPE,start=1,stop=2),substr(basic$STR_SIZE,start=1,stop=2),substr(basic$STR_GRADE,start=1,stop=2),sep='')
tmp$STR_COMB <- substr(basic$STR_GRADE,start=3,stop=3)
tmp$STR_COMB[nchar(tmp$STR_COMB) == 0] <- '0'
tmp$STRUCTURE2 <- paste('NDND',substr(basic$STR_GRADE,start=4,stop=5),sep='')
tmp$STRUCTURE2[tmp$STR_COMB == '0'] <- 'NDNDND'
basic <- tmp

chemical <- chemical[,-26]

psize0<-psize
nm <- names(psize)[1:5]
psize <- psize[,c(1:2,4,3,5)]
nm -> names(psize)

ret[,6:14] <- -999
names(ret)[2]<-"SAMPLE_ID"

# no cond data
names(cond)[2] <- "SAMPLE_ID"
#