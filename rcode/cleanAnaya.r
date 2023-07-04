# cleanAnaya
# update data format to new guidelines
#
# Author: M.Weynants
# Date created: 2012/11/30
# Last update: 2012/12/20
###################################################################

# general
general0 <- general
general <- cbind(general0[,1:36],GENERAL[,37:50],general0[,41:55])
general$PROFILE_ID <- as.numeric(general0$PROFILE_ID)
# LOC_COOR_SYST
general$LOC_COOR_SYST <- "ED_1950_UTM_Zone_30N. GCS_European_1950"
# WGS
general$X_WGS84 <- -999
general$Y_WGS84 <- -999
# convert NA to "ND"
general[,c('LC_L1','LU_L1','LU_L2','SITE_LANDFORM','SRF_ROCK_DIS', 'SRF_ERO_COV','SRF_ERO_ACT','SRF_SEAL_CON','SRF_CRAC_WID','SRF_CRAC_DEP','SRF_CRAC_DIS','SRF_SAL_COV','SRF_SAL_THIC','AGE')] <- "ND"
# WRB
general[,c(37:52,56:57,61:62)] <- "ND"

# method
meth0 <- meth
meth[,1] <- as.numeric(meth[,1])
meth <- meth[!is.na(meth[[1]]),1:4]

# basic
basic0 <- basic[!is.na(basic[[1]]),]
basic <- cbind(basic0[,1:2],basic0[,8],basic0[,3:4],basic0$HOR_NAME,basic0[,5:6],BASIC[,9:17],basic0[,12:17])
names(basic) <- names(BASIC)
# convert HOR_NAME
basic$HOR1_NAME <- basic0$HOR_NAME
# convert structure
basic$STRUCTURE1 <- paste(basic0$STR_GRADE,basic0$STR_SIZE,basic0$STR_SHAPE,sep='')
basic[,c(9,12,16,17)] <- 'ND'
basic[,c(7,8,10,11,13,14)] <- -999
basic[is.na(basic)] <- -999

# chemical
chemical0 <- chemical
names(chemical)[2] <- 'SAMPLE_ID'
names(chemical) <- toupper(names(chemical))
chemical[is.na(chemical)]<- -999
chemical <- chemical[,!grepl('BASE_CATIONS_M',names(chemical))]

# psize
psize0 <- psize
names(psize)[2]<-'SAMPLE_ID'
psize[is.na(psize$P_M),'P_M']<- -999

# ret
ret0<-ret
ret <- ret[ret$HEAD!=-999,]
ret <- ret[ret$THETA!=-999,]
names(ret)[2] <- "SAMPLE_ID"

# cond
cond[is.na(cond)] <- -999
names(cond)[1:2] <- c("PROFILE_ID","SAMPLE_ID")
