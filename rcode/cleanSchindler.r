# cleanSchindler
# update data format to new guidelines
#
# Author: M.Weynants
# Date created: 2012/12/12
###################################################################

# general
general0 <- as.matrix(general)
rownames(general0) <- NULL
general <- as.data.frame(general0[3:nrow(general0),2:ncol(general0)],stringsAsFactors = FALSE)
names(general) <- general0[1,2:53]
general$X_WGS84 <- as.numeric(gsub('°','',general$X_WGS84))
general$Y_WGS84 <- as.numeric(gsub('°','',general$Y_WGS84))
tmp <- cbind(general[,1:36],GENERAL[,37:50],general[,40:49],GENERAL[,61:62],general[,50:52])
general <- tmp
general[is.na(general)]<-'ND'
for (icol in c(1:7,21,25,33,35,53:55)){general[,icol] <- as.numeric(general[,icol])}
general[is.na(general)]<- -999
general[,c(17:20,22:24,26:32,34,36,56:57,59,63:65)]<-'ND'
# Assign PROFILE_ID (102 is minimum of current profile ID; 276 is counrty code)
pid <- general$PROFILE_ID
general$PROFILE_ID <- general$PROFILE_ID -101 + 27600000

# method
meth <- data.frame(CODE_M=c(605,805),METHOD=c("evaporation method on 250cm3 undisturbed soil cores with tensions measured at two points through time","evaporation method on 250cm3 undisturbed soil cores with tensions at two points. unsaturated conductivity calculated using Darcy's law and assuming quasi steady state"),METH_REF="Schindler, U. 1980. Ein Schnellverfahren zur Messung der Wasserleitf?higkeit im teilges?ttigten Boden an Stechzylinderproben.",METH_PAR=c("THETA_M","COND_M"),stringsAsFactors=FALSE)

# basic
basic0 <- as.matrix(basic)
rownames(basic0) <- NULL
basic <- as.data.frame(basic0[3:(nrow(basic0)-1),2:ncol(basic0)],stringsAsFactors = FALSE)
names(basic) <- basic0[1,2:ncol(basic0)]
tmp <- cbind(basic[,1:2],-999,basic[,3:7],BASIC[,9:19],basic[,15:18])
names(tmp)<-names(BASIC)
basic<-tmp
for (icol in c(1:2,4:5,7:8,18:23)){
basic[,icol] <- as.numeric(basic[,icol])}
for (icol in 1:23){
if (is.character(basic[,icol])) {basic[,icol] <- gsub('NA','ND',basic[,icol])}
else {basic[is.na(basic[,icol]),icol]<- -999}
}
basic$PROFILE_ID <- basic$PROFILE_ID -101 + 27600000 # starting from 1

# SAMPLE_ID
sid <- basic$SAMPLE_ID
# create new sample_id
horn <- rep(NA,nrow(basic))
prof <-unique(basic$PROFILE_ID)
for (iprof in 1:length(prof)) {ind <- basic$PROFILE_ID==prof[iprof]
# duplicated samples: same sample_id
# horu <- unique(basic$SAMPLE_DEP_TOP[ind])
horn[ind] <- order(basic$SAMPLE_DEP_TOP[ind])#match(basic$SAMPLE_DEP_TOP[ind],horu)#order(df.sp$TOP_DEPTH[ind])
}
basic$SAMPLE_ID <- basic$PROFILE_ID*100+horn
# SAMPLE_POS
basic$SAMPLE_POS <- horn

# HOR1_NAME
basic$HOR1_NAME[basic$HOR1_NAME=="AhBv"] <- ' A Bhv '
ind <- grepl("^[[:upper:]]$",basic$HOR1_NAME)
basic$HOR1_NAME[ind]<-paste(' ',basic$HOR1_NAME[ind],'     ',sep='')
ind <- grepl("^[[:upper:]][[:lower:]]$",basic$HOR1_NAME)
basic$HOR1_NAME[ind]<-paste(' ',substr(basic$HOR1_NAME[ind],1,1),'  ',substr(basic$HOR1_NAME[ind],2,2),'  ',sep='')
ind <- grepl("^[[:upper:]][[:lower:]][[:lower:]][[:digit:]]$",basic$HOR1_NAME)
basic$HOR1_NAME[ind]<-paste(' ',substr(basic$HOR1_NAME[ind],1,1),'  ',substr(basic$HOR1_NAME[ind],2,4),sep='')
##### ?????
basic$HOR1_NAME[basic$HOR1_NAME=='yc'] <- 'ND'

# BD_M & COARSE_M
basic$BD_M <- 110
basic$COARSE_M <- 120
bdm <- data.frame(CODE_M=c(110,120),METHOD=c("mass of a unit volume of oven dry (105?C) soil","sieving (wet or dry)"),METH_REF="ND",METH_PAR=c("BD_M","COARSE_M"))

# remove duplicates and average BD
basic.nd <- basic[!duplicated(basic$SAMPLE_ID),]
basic.nd$BD <- sapply(basic.nd$SAMPLE_ID,function(x){mean(basic$BD[basic$SAMPLE_ID == x  & basic$BD != -999])})
basic.nd$COARSE <- sapply(basic.nd$SAMPLE_ID,function(x){mean(basic$COARSE[basic$SAMPLE_ID == x & basic$COARSE != -999])})

basic_o <- basic
#basic <- basic.nd

# show to Schindler
#ind<-basic_o$SAMPLE_ID %in% basic_o$SAMPLE_ID[duplicated(basic_o$SAMPLE_ID)][1:10]
#pid2 <- basic_o$PROFILE_ID + 101 - 27600000
#cbind(basic_o[ind,1:2],pid2[ind],sid[ind],basic_o[ind,3:5],basic_o$BD[ind],basic_o$COARSE[ind],chemical_o$OC[ind])

# chemical
chemical0 <- as.matrix(chemical)
rownames(chemical0) <- NULL
chemical <- as.data.frame(chemical0[3:nrow(chemical0),2:ncol(chemical0)],stringsAsFactors = FALSE)
names(chemical) <- chemical0[1,2:ncol(chemical0)]
for (j in 1:(length(chemical)-1)){chemical[[j]] <- as.numeric(chemical[[j]])}
chemical$PROFILE_ID <- chemical$PROFILE_ID -101 + 27600000
names(chemical)[2] <- "SAMPLE_ID"
names(chemical)[9:10] <- c("PH_KCL","PH_KCL_M")
names(chemical) <- gsub("Na4O","NA4O",names(chemical))
chemical$SAMPLE_ID <- basic_o$SAMPLE_ID
# remove BASE_CATIONS_M
chemical <- chemical[,!grepl("BASE_CATIONS_M",names(chemical))]
# set OC_M
chemical$OC_M[chemical$OC_M == 2] <- 130
chemical$OC_M[chemical$OC_M == 3] <- 131
# add to meth
ocm <- data.frame(CODE_M=c(130,131),METHOD=c("Ash content burning in the oven at 650?C","Woesthoff method"),METH_REF="ND",METH_PAR="OC_M")
# change NA to -999
chemical[is.na(chemical)] <- -999
# remove duplicates and average OC
chemical.nd <- chemical[!duplicated(chemical$SAMPLE_ID),]
chemical.nd$OC <- sapply(chemical.nd$SAMPLE_ID,function(x){mean(chemical$OC[chemical$SAMPLE_ID==x  & chemical$OC != -999])})
chemical_o <- chemical
#chemical <- chemical.nd

# 3 last tables are completely messed up and need to be completely reworked
# psize
psize0 <- as.matrix(psize)
psize=data.frame(PROFILE_ID=NULL,SAMPLE_ID=NULL,P_PERCENT=NULL,P_M=NULL)
ps <- as.numeric(psize0[3,4:10])
for (iobs in 4:nrow(psize0)){
pc <- as.numeric(psize0[iobs,4:10])
if (any (!is.na(pc))){
if (sum(pc[!is.na(pc)]) > 100) {
df <- data.frame(PROFILE_ID=as.numeric(psize0[iobs,2]),SAMPLE_ID=as.numeric(psize0[iobs,3]),P_SIZE = ps[!is.na(pc)],P_PERCENT=c(pc[which(!is.na(pc))[1]],diff(pc[!is.na(pc)])),P_M=psize0[iobs,11])
} else {
df <- data.frame(PROFILE_ID=as.numeric(psize0[iobs,2]),SAMPLE_ID=as.numeric(psize0[iobs,3]),P_SIZE = ps[!is.na(pc)],P_PERCENT=pc[!is.na(pc)],P_M=psize0[iobs,11])
}
psize <- rbind(psize,df)
}}
psize$PROFILE_ID <- psize$PROFILE_ID -101 + 27600000
sid.p <- psize$SAMPLE_ID
psize$SAMPLE_ID <- basic_o$SAMPLE_ID[match(sid.p,sid)]
# 2023/04/18: psize$P_M is not a factor
psize$P_M <- as.factor(psize$P_M)
# indices were not correct [1:2] -> [2:3]
psm <- data.frame(CODE_M=c(500,501),METHOD=levels(psize$P_M)[2:3],METH_REF="ND",METH_PAR="P_M")
# c(500,501, -999) -> c(-999,500,501)
levels(psize$P_M) <- c(-999,500,501)
psize$P_M <-as.numeric(as.character(psize$P_M))
# missing data for organic soils
psize$P_PERCENT[psize$P_PERCENT==0 & psize$P_M == -999] <- -999
psize$ID <- sid.p

# ret
ret0 <- as.matrix(ret)
ret <- data.frame(PROFILE_ID=-999,SAMPLE_ID=-999,HEAD=-999,THETA=-999,THETA_M=-999,TH_INV_P1=-999,TH_INV_P2=-999,TH_INV_P3=-999,TH_INV_P4=-999,TH_INV_P5=-999,TH_INV_P6=-999,TH_INV_P7=-999,TH_INV_P8=-999,TH_INV_MOD=-999)
for (iobs in seq(3,nrow(ret0),by=2)){
h <- as.numeric(ret0[iobs,4:ncol(ret0)])
th <- as.numeric(ret0[iobs+1,4:ncol(ret0)])
df <- data.frame(PROFILE_ID=as.numeric(ret0[iobs,2]),SAMPLE_ID=as.numeric(ret0[iobs,3]),HEAD=h[!is.na(h)],THETA=th[!is.na(h)],THETA_M=605)
ret <- merge(ret,df,all=TRUE)
}
ret <- ret[ret$PROFILE_ID!=-999,]
ret$PROFILE_ID <- ret$PROFILE_ID -101 + 27600000
sid.r <- ret$SAMPLE_ID
ret$SAMPLE_ID <- basic_o$SAMPLE_ID[match(sid.r,sid)]
ret[is.na(ret)] <- -999
ret$ID <- sid.r

# cond
cond0 <- as.matrix(cond)
cond <- data.frame(PROFILE_ID=-999,SAMPLE_ID=-999,IND_VALUE=-999,VALUE=-999,COND=-999,COND_M=-999,K_INV_P1=-999,K_INV_P2=-999,K_INV_P3=-999,K_INV_P4=-999,K_INV_P5=-999,K_INV_P6=-999,K_INV_P7=-999,K_INV_P8=-999,K_INV_MOD=-999)
for (iobs in seq(3,nrow(cond0),by=2)){
h <- as.numeric(cond0[iobs,5:ncol(cond0)])
k <- as.numeric(cond0[iobs+1,5:ncol(cond0)])
if (!(all(is.na(h)) | all(is.na(k)))){
df <- data.frame(PROFILE_ID=as.numeric(cond0[iobs,2]),SAMPLE_ID=as.numeric(cond0[iobs,3]),IND_VALUE=1,VALUE=h[!is.na(h)],COND=k[!is.na(h)],COND_M=805)
cond <- merge(cond,df,all=TRUE)
}}
cond <- cond[cond$PROFILE_ID!=-999,]
cond$PROFILE_ID <- cond$PROFILE_ID -101 + 27600000
sid.k <- cond$SAMPLE_ID
cond$SAMPLE_ID <- basic_o$SAMPLE_ID[match(sid.k,sid)]
cond[is.na(cond)] <- -999
cond$ID <- sid.k

meth <- rbind(meth, ocm, bdm, psm)
#