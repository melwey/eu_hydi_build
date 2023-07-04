# cleanMako
# Adapts HYDI tables to comply to guidelines v1.1
#
# Author: M.Weynants
# Date created: 2012/12/05
####################################################################
print('Adapting to guidelines')
# GENERAL sp<-SpatialPoints(coords=general[,c('X_WGS84','Y_WGS84')],proj4string=CRS("+proj=longlat +datum=WGS84"))
# require('maptools')
# data(wrld_simpl)
# plot(wrld_simpl)
# sp@bbox<-bbox(wrld_simpl)
# plot(sp,add=TRUE)
# => x and y coordinates inversed
coord <- general[,c('X_WGS84','Y_WGS84')]
general[,c('X_WGS84','Y_WGS84')] <- coord[,c(2,1)]
general$REL_T_SER <- -999

general$YEAR <- as.numeric(gsub(',','',general$YEAR))

# BASIC
names(basic)[22:23] <- c('COARSE','COARSE_M')
names(basic) <- toupper(names(basic))
# CHEMICAL
#ok
# PSIZE
psize$P_SIZE <- as.numeric(gsub(',','',psize$P_SIZE))
# RET
ret$HEAD <- as.numeric(gsub(',','',ret$HEAD))
# COND
cond$COND <-as.numeric(gsub(',','',cond$COND))
# METHOD
meth$METH_PAR <- toupper(meth$METH_PAR)