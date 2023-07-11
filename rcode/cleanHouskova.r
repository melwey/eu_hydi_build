# cleanHouskova
#
# Author: M. Weynants
# Date created: 2012/12/19
###################################################################
# BASIC
names(basic)[22:23] <- c('COARSE','COARSE_M')
basic[,22:23]<- -999

# PSIZE
psize$P_SIZE <- as.numeric(gsub(',','',psize$P_SIZE))
#