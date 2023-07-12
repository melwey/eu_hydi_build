# QA_meth.r: harmonization of methods' codes and description
# based on Excel workbook: "Methods HYDI - QA all properties_NEX.xlsx" created by Sigrun Kvaerno
#

# Author: M. Weynants
# Date created: 2013/05/21
######################################################################
# require(XLConnect)
# load quality assessed HYDI
load("../output/HYDI_SOURCE_nd_qa2.Rdata")
meth <- hydi$METHOD
anaya.meth.ref <- "USDA, 1984. Procedures for Collecting Soil Samples and Methods of Analysis for Soil Survey; Rayment and Higginson, 1992. Australian Laboratory Handbook of Soil and Water Chemical Methods; Klute, 1986. Methods of Soil Analysis: Part 1, Physical and Mineralogical Methods. SSSA; Weaver et al.,1994. Methods of Soil Analysis: Part 2, Chemical and Microbiological Properties. SSSA."


# CAUTION: add Cranfield (done?) and Wosten methods
# wb <- loadWorkbook("../../QAmeeting/METHOD/Methods HYDI - QA all properties_NEW.xlsx", create = FALSE)
# basic <- readWorksheet(wb , "BASIC_M")
# chem <- readWorksheet(wb,"CHEMICAL_M")
# psize <- readWorksheet(wb,"P_M")
# theta <- readWorksheet(wb,"THETA_M")
# cond <- readWorksheet(wb,"COND_M")
# inv <-  readWorksheet(wb,"INV_M")
basic <- read.csv("../data/QAmeeting/METHOD/QA_all_NEW/BASIC_M.csv")
chem <- read.csv("../data/QAmeeting/METHOD/QA_all_NEW/CHEMICAL_M.csv")
psize <-  read.csv("../data/QAmeeting/METHOD/QA_all_NEW/P_M.csv")
theta <- read.csv("../data/QAmeeting/METHOD/QA_all_NEW/THETA_M.csv")
cond <- read.csv("../data/QAmeeting/METHOD/QA_all_NEW/COND_M.csv")
inv <- read.csv("../data/QAmeeting/METHOD/QA_all_NEW/INV_M.csv")

# new metod table
basic.meth.new <- basic[!duplicated(basic$NEW_CODE_M),c("NEW_CODE_M","NEW_METHOD","NEW_METH_REF","METH_PAR")]
chem.meth.new <- chem[!duplicated(chem$NEW_CODE_M),c("NEW_CODE_M","NEW_METHOD","NEW_METH_REF","NEW_METH_PAR")]
psize.meth.new <- psize[!duplicated(psize$NEW_CODE_M),c("NEW_CODE_M","NEW_METHOD","NEW_METH_REF","METH_PAR")]
theta.meth.new <- theta[!duplicated(theta$NEW_CODE_M),c("NEW_CODE_M","NEW_METHOD","NEW_METH_REF","METH_PAR")]
cond.meth.new <- cond[!duplicated(cond$NEW_CODE_M),c("NEW_CODE_M","NEW_METHOD","NEW_METH_REF","METH_PAR")]
inv.meth.new <- inv[!duplicated(inv$NEW_CODE_M),c("NEW_CODE_M","NEW_METHOD","NEW_METH_REF","METH_PAR")]

names(basic.meth.new)<-gsub("NEW_","",names(basic.meth.new))
names(chem.meth.new)<-gsub("NEW_","",names(chem.meth.new))
names(psize.meth.new)<-gsub("NEW_","",names(psize.meth.new))
names(theta.meth.new)<-gsub("NEW_","",names(theta.meth.new))
names(cond.meth.new)<-gsub("NEW_","",names(cond.meth.new))
names(inv.meth.new)<-gsub("NEW_","",names(inv.meth.new))

meth.new <- rbind(basic.meth.new,chem.meth.new,psize.meth.new,theta.meth.new,cond.meth.new,inv.meth.new)
meth.new$METH_PAR <- gsub("ACIDITY_KCL_M","EX_AC",meth.new$METH_PAR)
meth.new$METH_PAR <- gsub("ACIDITY_NA4O_M","POT_AC",meth.new$METH_PAR)
meth.new <- meth.new[! meth.new$METH_PAR %in% c("BASE_CATIONS_M","HUM_M","PD_M","STRENGHT_M","TOT_N_M"),]

# K_INV_MOD
# THETA_INV_MOD

#str(meth.new)

# basic
# For each in unique(basic$METH_PAR)
mp <- unique(basic$METH_PAR)
for (p in mp[1:3]){
	for (l in which(basic$METH_PAR==p)){
	# find combination METH_PAR + SOURCE + CODE_M
	ind <- hydi$BASIC$SOURCE == basic$SOURCE[l] & hydi$BASIC[,p] == basic$CODE_M[l]
	# replace by new CODE_M
	hydi$BASIC[ind,p] <- basic$NEW_CODE_M[l]
	}
}

# chemical
# add LOI, TC and HOC to chemical table
CHEM <- hydi$CHEMICAL

# original OC
oOC <- CHEM[,c("OC","OC_M","SOURCE")]
# back calculate LOI Schindler and Strauss to OM
ind <- grepl("Schindler",oOC$SOURCE) & oOC$OC_M== 130 | oOC$SOURCE=="Strauss" & oOC$OC_M==131
oOC$OC[ind] <- oOC$OC[ind]*1.724

new.cols <- as.data.frame(matrix(-999,ncol=8));names(new.cols) <- c("LOI","LOI_M","OC","OC_M","TC","TC_M","HOC","HOC_M")
CHEM <- cbind(CHEM[,1:2],new.cols,CHEM[,5:ncol(CHEM)])

# split oOC into LOI, OC and TC
meth.source <- paste(chem$CODE_M,chem$SOURCE)
o.meth.source <- paste(oOC$OC_M,a=sapply(oOC$SOURCE,function(x){y<-strsplit(x,"_");return(y[[1]][1])}))
# LOI
ind.loi <- o.meth.source %in% meth.source[chem$NEW_METH_PAR=="LOI_M"]
CHEM$LOI[ind.loi] <- oOC$OC[ind.loi]
CHEM$LOI_M[ind.loi] <- sapply(o.meth.source[ind.loi],function(x){y <- chem$NEW_CODE_M[meth.source==x]})
# OC
ind.oc <- o.meth.source %in% meth.source[chem$NEW_METH_PAR=="OC_M"]
CHEM$OC[ind.oc] <- oOC$OC[ind.oc]
CHEM$OC_M[ind.oc] <- sapply(o.meth.source[ind.oc],function(x){y <- chem$NEW_CODE_M[meth.source==x]})
# TC
ind.tc <- o.meth.source %in% meth.source[chem$NEW_METH_PAR=="TC_M"]
CHEM$TC[ind.tc] <- oOC$OC[ind.tc]
CHEM$TC_M[ind.tc] <- sapply(o.meth.source[ind.tc],function(x){y <- chem$NEW_CODE_M[meth.source==x]})

# kvaerno's updated carbon
kvaerno <- read.csv("../data/Kvaerno/CARBON_update_130612.csv")
kvaerno$TC <- as.numeric(as.character(kvaerno$TC)); kvaerno$TC[is.na(kvaerno$TC)]<-0.05
kvaerno$LOI_M[kvaerno$LOI_M==133]<-130
kvaerno$TC_M[kvaerno$TC_M==140]<-150
ind.kv <- match(kvaerno$SAMPLE_ID,CHEM$SAMPLE_ID)
CHEM[ind.kv,c("LOI","LOI_M","OC","OC_M","TC","TC_M")] <- kvaerno[,c("LOI","LOI_M","OC","OC_M","TC","TC_M")]
#table(CHEM[ind.kv,c("LOI_M","OC_M","TC_M")])
loi2ocNO<-lm(OC~LOI-1,data=CHEM[CHEM$OC_M==142 & CHEM$LOI_M==130 & CHEM$SOURCE=="Kvaerno",])

# 
# Harmonization
# classical: OC = 1/1.724 OM (LOI)
loi2oc <- function(x){
# ensemble from literature
y1 <- -.936 + .633 *x
y2 <- .01025 + .5743 *x # equivalent to classic (y8)
y3 <- .065 + .43 *x
y4 <- -.0003 + .48 *x
y5 <- .624 *x # equivalent to y1
y6 <- -.1598 + .736 *x
y7 <- -.16947/2.24194 + 1/2.24194 *x
y8 <- 1/1.724 *x # classic
# average
y9<-rowMeans(cbind(y1,y2,y3,y4,y5,y6,y7))
# slightly less than classic
return(y9)}
ind.loi <- CHEM$LOI_M!=-999
CHEM$HOC[ind.loi] <- loi2oc(CHEM$LOI[ind.loi])
CHEM$HOC_M[ind.loi] <- 99

# specific for Norway
loi2tcNO <- lm(TC~LOI-1,data=CHEM[CHEM$TC_M==150 & CHEM$LOI_M==130 & CHEM$SOURCE=="Kvaerno",])
# Note on 2014/04/08: correction made: need to rerun.
CHEM$HOC_M[CHEM$LOI_M!=-999 & CHEM$TC_M!=-999 & CHEM$SOURCE=="Kvaerno"] <- 93

# corrected Walkley-Black C recovery: OC = 1.32 WB (dark green)
wb2oc <- function(x){
y1 <- 1.05 *x
y3 <- .992 *x
y5 <- .009/.89 + 1/.89 *x
y6 <- -.015 +1/1.02 *x
y8 <- 1.2 *x
y9 <- -0.1/0.918 +1/.918 *x
y10 <- 1/.947 *x
y11 <- 1/.97 *x
y13 <- -.0016 + 1/1.034 *x
y14 <- 1/1.013*x
# average
y16 <- rowMeans(cbind(y1,y3,y5,y6,y8,y9,y10,y11,y13,y14))
# a bit lower than
return(y16)
}
ind.wb <- CHEM$OC_M == 140
CHEM$HOC[ind.wb] <- wb2oc(CHEM$OC[ind.wb])
CHEM$HOC_M[ind.wb] <- 98
# uncorrected Walkley-Black (light green)
wb2oc <- function(x){
y2 <- .0126 + 1.25 *x
y4 <- 1.47 *x
y7 <- 1.58 *x
y8 <- 1.506 *x
y9 <- 1.594 *x
y10 <- 1.774 *x
y12 <- .1759/.739 + 1/.739*x
y15 <- -.028413/.79165 + 1/.79165*x
# average
y16 <- rowMeans(cbind(y2,y4,y7,y8,y9,y10,y12,y15))
# a bit lower than 1.32
return(y16)
}
ind.wb <- CHEM$OC_M == 147
CHEM$HOC[ind.wb] <- wb2oc(CHEM$OC[ind.wb])
CHEM$HOC_M[ind.wb] <- 95
# Tyurin
ty2oc <- function(x){
y1 <- -.0162/.869 + 1/.869 *x
y2 <- -.006516/.74495 + 1./.74495 *x
y3 <- -.006245/.72013 + 1/.72013 *x
y4 <- 1.28 *x
y5 <- .3105 + 1.2596 *x
y6 <- .0073 + 1.2399 *x
# average
y7 <- rowMeans(cbind(y1,y2,y3,y4,y5,y6))
# around 1.3
return(y7)
}
ind.ty <- CHEM$OC_M == 141
CHEM$HOC[ind.ty] <- ty2oc(CHEM$OC[ind.ty])
CHEM$HOC_M[ind.ty] <- 97
# modified WB
mwb2oc <- function(x){
y1 <- 1.2 *x
y2 <- -.063/.72 + 1/.72 *x
y3 <- -.058/.81 + 1/.81 *x
y4 <- .07583 + .30188*x
y5 <- 1.0066*x
y6 <- .9901*x
#average
y7 <- rowMeans(cbind(y1,y2,y3,y4,y5,y6))
return(y7)
}
ind.mwb <- CHEM$OC_M %in% c(144,145,146)
CHEM$HOC[ind.mwb] <- mwb2oc(CHEM$OC[ind.mwb])
CHEM$HOC_M[ind.mwb] <- 96
# DC pretreated or calcimetric correction (OC = TC - .12 CaCO3)
ind.dc <- CHEM$OC_M ==143
CHEM$HOC[ind.dc] <- CHEM$OC[ind.dc]
CHEM$HOC_M[ind.dc] <- 94
ind.tc <- CHEM$TC_M %in% c(150,151) # for 150, no idea whether CO3 were removed; for 151, pH<7
CHEM$HOC[ind.tc] <- CHEM$TC[ind.tc]
CHEM$HOC_M[ind.tc] <- 94
# HOC<0 transformed to 0
CHEM$HOC[CHEM$HOC<0 & CHEM$HOC!=-999] <- 0

# add column for PH_CACL2
CHEM <- cbind(CHEM[,1:16],PH_CACL2=-999,PH_CACL2_M=-999,CHEM[,17:36])
# pass Lilly's and Cranfield's PH_KCL to PH_CACL2
ind <- CHEM$PH_KCL!=-999  & (CHEM$SOURCE=="Lilly" & CHEM$PH_KCL_M==5) | (CHEM$SOURCE=="Cranfield" & CHEM$PH_KCL_M==152)
CHEM[ind,"PH_CACL2"] <- CHEM[ind,"PH_KCL"]
CHEM[ind,"PH_CACL2_M"] <- CHEM[ind,"PH_KCL_M"]
CHEM[ind,c("PH_KCL","PH_KCL_M")] <- -999
# update Kvaerno's PH_CACL2
kvaerno <- read.csv("../data/Kvaerno/CHEMICAL.csv")
ind.kv <- match(kvaerno$SAMPLE_ID,CHEM$SAMPLE_ID)
CHEM[ind.kv,c("PH_CACL2","PH_CACL2_M")] <- kvaerno[,c("PH_CACL2","PH_CACL2_M")]

# remainng chemicals
mp <- unique(chem$METH_PAR)
for (p in mp[c(3:13,15:16)]){
	for (l in which(chem$METH_PAR==p)){
	# find combination METH_PAR + SOURCE + CODE_M
	ind <- grepl(chem$SOURCE[l],CHEM$SOURCE) & CHEM[,p] == chem$CODE_M[l]
	# replace by new CODE_M
	CHEM[ind,p] <- chem$NEW_CODE_M[l]
	}
}
# ACIDITY
# table(CHEM$SOURCE[CHEM$ACIDITY_NA4O!=-999 & CHEM$ACIDITY_KCL!=-999])
# Kvaerno: 73 ; Mako: 12
#plot(ACIDITY_NA4O~ACIDITY_KCL,data=CHEM[CHEM$ACIDITY_NA4O!=-999 & CHEM$ACIDITY_KCL!=-999,])
# if the units are correct: ACIDITY NA4O (actually with OAc) >> KCL
#ind <- CHEM$ACIDITY_NA4O!=-999 & CHEM$SOURCE %in% c("Lilly","Cranfield")
#summary(CHEM$ACIDITY_NA4O[ind])
#ind <- CHEM$ACIDITY_KCL!=-999
#summary(CHEM$ACIDITY_KCL[ind])
# one order of magnitude difference in the median (KCL<OAc)
# units? OK
names(CHEM) <- gsub("ACIDITY_NA4O","POT_AC",names(CHEM))
names(CHEM) <- gsub("ACIDITY_KCL","EX_AC",names(CHEM))

hydi$CHEMICAL <- CHEM

# PSIZE
# replace by new codes
for (l in 1:nrow(psize)){
  if (nchar(psize$SOURCE[l]) > 0){
	# find combination METH_PAR + SOURCE + CODE_M
	ind <- grepl(psize$SOURCE[l],hydi$PSIZE$SOURCE) & hydi$PSIZE$P_M == psize$CODE_M[l]
	# replace by new CODE_M
	hydi$PSIZE[ind,"P_M"] <- psize$NEW_CODE_M[l]
}
}

# RET
# theta_sat
# "Kvaerno","Romano","Mako","Houskova","Lilly"
ind.ths <- hydi$RET$HEAD==0 & hydi$RET$SOURCE %in% c("Kvaerno","Romano","Mako","Houskova","Lilly")
hydi$RET$THETA_M[ind.ths] <- 599
# split Lilly THETA_M 13 into 4
# 0: ths THETA_M =13 (unchanged)
# 10, 50, 100 =15
hydi$RET$THETA_M <- replace(hydi$RET$THETA_M,hydi$RET$THETA_M== 13 & hydi$RET$HEAD %in% c(10,50,100),15)
# 500, 2000 =16
hydi$RET$THETA_M <- replace(hydi$RET$THETA_M,hydi$RET$THETA_M== 13 & hydi$RET$HEAD %in% c(500,2000),16)
# 15000 =17
hydi$RET$THETA_M <- replace(hydi$RET$THETA_M,hydi$RET$THETA_M== 13 &hydi$RET$HEAD ==15000,17)
# split Daroussin into 2
# <1000 THETA_M 601 and 602 unchanged
# >= 1000 603 and 604
hydi$RET$THETA_M <- replace(hydi$RET$THETA_M,hydi$RET$THETA_M==601 & hydi$RET$SOURCE=="Daroussin" & hydi$RET$HEAD >=1000,603)
hydi$RET$THETA_M <- replace(hydi$RET$THETA_M,hydi$RET$THETA_M==602 & hydi$RET$SOURCE=="Daroussin" & hydi$RET$HEAD >=1000,604)
# replace by new codes
for (l in 1:nrow(theta)){
  if (!is.na(theta[l,"NEW_CODE_M"])){
	# find combination METH_PAR + SOURCE + CODE_M
	ind <- grepl(theta$SOURCE[l],hydi$RET$SOURCE) & hydi$RET$THETA_M == theta$CODE_M[l]
	# replace by new CODE_M
	hydi$RET[ind,"THETA_M"] <- theta$NEW_CODE_M[l]
}}

# COND
# replace by new codes
for (l in 1:nrow(cond)){
	# find combination METH_PAR + SOURCE + CODE_M
	ind <- grepl(cond$SOURCE[l],hydi$COND$SOURCE) & hydi$COND$COND_M == cond$CODE_M[l]
	# replace by new CODE_M
	hydi$COND[ind,"COND_M"] <- cond$NEW_CODE_M[l]
}

# Add HOC conversions codes
hoc <- data.frame(CODE_M=c(93:99),
	METHOD=c("Conversion from LOI to TC for Norway","No conversion (dry combustion)","Conversion from uncorrected Walkley-Black to dry combustion","Conversion from modified Walkley-Black to dry combustion","Conversion form Tyurin to dry combustion", "Conversion from corrected Walkley-Black to dry combustion","Conversion from loss on ignition to dry combustion"),
	METH_REF=c("EU-HYDI report"),
	METH_PAR="HOC_M")

# replace table method
hydi$METHOD <- rbind(meth.new[meth.new$CODE_M!=-999,],hoc)
# remove rows with is.na(CODE_M)
hydi$METHOD <- hydi$METHOD[!is.na(hydi$METHOD$CODE_M), ] 
hydi$METHOD <- replace(hydi$METHOD,is.na(hydi$METHOD),"ND")
hydi$METHOD <- hydi$METHOD[order(hydi$METHOD$CODE_M),]

# save
save(hydi,file="../output/HYDI_SOURCE_nd_qa3.Rdata")
