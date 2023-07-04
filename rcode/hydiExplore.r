# Basic statistics on EU-HYDI
load("HYDI_SOURCE_nd_qa3.Rdata")
hydi.na <- lapply(hydi,function(tbl){replace(tbl,tbl==-999 | tbl=="ND",NA)})
attach(hydi.na)

print("DATABASE COMPLETENESS (in %)")
print("-- GENERAL --")
Np <- nrow(GENERAL)
print(paste("Number of entries:",Np))
print("Minimal requirements")
# coordinates
ind <- !is.na(GENERAL$LOC_COOR_X) | !is.na(GENERAL$X_WGS84)
pcor <- sum(ind)/Np*100
print(paste("Geographical coordinates:",round(pcor,digit=2),"%"))
require("sp")
wgs84<-CRS("+proj=latlon  +datum=WGS84")
hydi.wgs <- SpatialPoints(GENERAL[!is.na(GENERAL[,5]),5:6],proj4string=wgs84)
require(maptools)
writePointsShape(hydi.wgs, fn="hydi_wgs")
# require("maps")
# map("world",xlim=c(-12,38),ylim=c(34,75))
# points(hydi.wgs,col="red",pch=20)

print("Other data")
pG <- pcor
count=1
for (icol in 7:60){
count <- count +1
pG[count] <- round(sum(!is.na(GENERAL[,icol]))/Np*100,digit=2)
print(paste(names(GENERAL)[icol],":",pG[count],"%"))
}
attr(pG,"name") <- c("coordinates",names(GENERAL)[7:60])
#print(summary(GENERAL))

print("-- BASIC --")
Ns <- nrow(BASIC)
print(paste("Number of entries:",Ns))
print("Minimal requirements")
pB <- NA
for (icol in c(4,5,20:23)){
pB[icol] <- round(sum(!is.na(BASIC[,icol]))/Ns*100,digit=2)
print(paste(names(BASIC)[icol],":",pB[icol],"%"))
}
print("Other data")
for (icol in c(1:3,6:19)){
pB[icol] <- round(sum(!is.na(BASIC[,icol]))/Ns*100,digit=2)
print(paste(names(BASIC)[icol],":",pB[icol],"%"))
}
attr(pB,"name") <- names(BASIC)

print("-- CHEMICAL --")
N <- nrow(CHEMICAL)
print(paste("Number of entries:",N))
print("Minimal requirements")
pC <- NA
for (icol in c(3:10)){
pC[icol] <- round(sum(!is.na(CHEMICAL[,icol]))/N*100,digit=2)
print(paste(names(CHEMICAL)[icol],":",pC[icol],"%"))
}
print("Other data")
for (icol in c(1:2,11:ncol(CHEMICAL))){
pC[icol] <- round(sum(!is.na(CHEMICAL[,icol]))/N*100,digit=2)
print(paste(names(CHEMICAL)[icol],":",pC[icol],"%"))
}
attr(pC,"name") <- names(CHEMICAL)

print("-- PSIZE --")
N <- nrow(PSIZE)
print(paste("Total number of entries:",N))
print(paste("Samples with PSIZE:",round(sum(BASIC$SAMPLE_ID %in% PSIZE$SAMPLE_ID)*100/Ns,digit=2)))
load("psize_qa.rdata")# idn and sum_pn
print(paste("PSIZE summing to 100%:",round(sum(sum_pn>=99 & sum_pn<=101)*100/length(sum_pn),digit=2),"%"))
round(sum(sum_pn>=99 & sum_pn<=101)*100/Ns,digit=2)
pP <- round(sum(sum_pn>=99 & sum_pn<=101)*100/length(sum_pn),digit=2)

print("-- RET --")
Nr <- nrow(RET)
print(paste("Total number of entries:",Nr))
ret.count <- as.numeric(table(RET$SAMPLE_ID))
print(paste("Samples with RET:",round(length(ret.count)*100/Ns,digit=2)))
hist(ret.count[ret.count<65])
print(paste("Samples with at least 5 points:",round(sum(ret.count >=5)*100/Ns,digit=2),"%"))


print("-- COND --")
N <- nrow(COND)
print(paste("Total number of entries:",N))
print(paste("Samples with COND:",round(sum(BASIC$SAMPLE_ID %in% COND$SAMPLE_ID)*100/Ns,digit=2)))
cond.count <- as.numeric(table(COND$SAMPLE_ID))
print(paste("Samples with only KSAT:",round(sum(cond.count == 1)*100/Ns,digit=2),"%"))
print(summary(cond.count))

print("DESCRIPTIVE STATISTICS")
print("-- BASIC --")
sB <- summary(BASIC[,c(4:5,7:8,18:23)])
#print(sB)
sB.df <- as.data.frame(matrix(as.numeric(substr(sB,9,max(nchar(sB)))),nrow=7,ncol=10))
names(sB.df) <- gsub(" ","",attr(sB,"dimnames")[[2]])
rownames(sB.df)<- gsub(" ","",substr(sB[1:7,1],1,7))
write.csv(t(sB.df),file="basic_summary.csv")

print("-- CHEMICAL --")
sC <- summary(CHEMICAL[,c(seq(3,33,by=2),34,35)])
#print(sC)
sC.df <- as.data.frame(matrix(as.numeric(substr(sC,9,max(nchar(sC)))),nrow=7,ncol=18))
names(sC.df) <- gsub(" ","",attr(sC,"dimnames")[[2]])
rownames(sC.df)<- gsub(" ","",substr(sC[1:7,1],1,7))
write.csv(t(sC.df),file="chemical_summary.csv")


# Meet minimum requirements: add a column to BASIC (logical)
# for each BASIC$SAMPLE_ID, must have: GENERAL:coordinates; ISO_COUNTRY; RC_L1; RC_L2; CONTACT_P; EMAIL; BASIC: SAMPLE_DEP_TOP; SAMPLE_DEP_BOT; BD; BD_M; COARSE; COARSE_M; CHEMICAL: OC, OC_M; PSIZE: sum=100+/-1, P_M; RET: HEAD, THETA; MEHTOD: CODE_M from other tables

# Minimal requirements fulfilment
# vG = c("coordinates","ISO_COUNTRY","RC_L1","RC_L2","CONTACT_P","EMAIL")
# vB = c("SAMPLE_DEP_TOP","SAMPLE_DEP_BOT","BD","BD_M","COARSE","COARSE_M")
# vC = c("OC","OC_M")
# vP = "PSIZE" # sum=100+/-1, P_M;
# vR = c("HEAD","THETA","TH_M","INV_TH_M")
# vM = c("CODE_M")
# mr <- data.frame(Variable = v, Proportion = p)

# BD, HOC, PSD, RET
sid <- BASIC$SAMPLE_ID
sum(!is.na(BASIC$BD) & !is.na(CHEMICAL$HOC[match(sid,CHEMICAL$SAMPLE_ID)]) & sid %in% RET$SAMPLE_ID & sid%in%idn[sum_pn>=99 & sum_pn<=101])

# Export basic stats

