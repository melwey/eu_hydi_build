# cleanMatula
# 
# Author: M.Weynants
# Date created: 2013/01/17
################################################################
# general
general0 <- general
general[,5]<-as.numeric(general[,5])
general[,6]<-as.numeric(general[,6])
general[is.na(general[,5]),5:6]<- -999
general$YEAR <- as.numeric(general$YEAR)
general$YEAR[is.na(general$YEAR)] <- -999
general$MONTH <- as.numeric(general$MONTH)
general$MONTH[is.na(general$MONTH)] <- -999
general$DAY <- as.numeric(general$DAY)
general$DAY[is.na(general$DAY)] <- -999

# method
# OK

# basic
basic0 <- basic
basic$HOR2_BOT[is.na(basic$HOR2_BOT)] <- -999
basic$COARSE <- as.numeric(basic$COARSE)
basic$COARSE[is.na(basic$COARSE)] <- -999
basic$COARSE_M <- as.numeric(basic$COARSE_M)
basic$COARSE_M[is.na(basic$COARSE_M)] <- -999

# chemical
# OK

# psize
# OK

# ret
ret$TH_INV_MOD <- as.numeric(ret$TH_INV_MOD)
ret$TH_INV_MOD[is.na(ret$TH_INV_MOD)] <- -999

# cond
# OK

