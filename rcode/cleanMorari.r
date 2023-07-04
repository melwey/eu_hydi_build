# cleanMorari.r
#
# Author: M.Weynants
# Date: 2013/06/03
##############################################################

names(general) <- gsub("SLOPE","SLOP",names(general))
# swap WGS
lat <- general$X_WGS84
general$X_WGS84 <- general$Y_WGS84
general$Y_WGS84 <- lat
# LU/LC
general$LC_L3 <- general$LC_L1
general$LC_L2 <- paste(substr(general$LC_L3,1,2),"0",sep="")
general$LC_L1 <- paste(substr(general$LC_L3,1,1),"00",sep="")
general$LU_L2 <- general$LU_L1
general$LU_L1 <- paste(substr(general$LU_L2,1,3),"0",sep="")


# HOR1_NAME
table(nchar(basic$HOR1_NAME))
unique(basic$HOR1_NAME)
ind <- grepl("^[[:upper:]] [[:lower:]]$",basic$HOR1_NAME)
basic$HOR1_NAME[ind] <- paste(" ",substr(basic$HOR1_NAME[ind],1,1),"  ",substr(basic$HOR1_NAME[ind],3,3),"  ",sep="")
ind <- basic$HOR1_NAME=="Bwk"
basic$HOR1_NAME[ind] <- " B  wk "
ind <- basic$HOR1_NAME==" A  p"
basic$HOR1_NAME[ind] <- " A  p  "
# HOR2_NAME
ind <- basic$HOR2_NAME=="B wk"
basic$HOR2_NAME[ind] <- " B  wk "
ind <- basic$HOR2_NAME=="B w"
basic$HOR2_NAME[ind] <- " B  w  "
ind <- basic$HOR2_NAME=="2Cg"
basic$HOR2_NAME[ind] <- "2C  g  "
# COURSE
names(basic) <- gsub("COURSE","COARSE",names(basic))
basic$COARSE_M<-120

# PROFILE_ID
chemical$PROFILE_ID <- floor(chemical$SAMPLE_ID/100)
# SALT_M etc
chemical[,c("SALT_M","EX_NA_M","ACIDITY_NA4O_M","ACIDITY_KCL_M")] <- -999

# PROFILE_ID
psize$PROFILE_ID <- floor(psize$SAMPLE_ID/100)
psize$P_SIZE <- as.numeric(gsub(",","",psize$P_SIZE))

ret$PROFILE_ID <- floor(ret$SAMPLE_ID/100)
ret <- replace(ret,is.na(ret),-999)
# replace HEAD=1 by HEAD=0
ret$HEAD <- replace(ret$HEAD,ret$HEAD==1,0)

cond$PROFILE_ID <- floor(cond$SAMPLE_ID/100)
# replace HEAD=1 by HEAD=0
cond$VALUE <- replace(cond$VALUE,cond$VALUE==1,0)

# average replicates: median
# samePROFILE_ID and SAMPLE_POS == replicate
mbasic <- basic[0,1:23];mchem<-chemical[0,1:29];mpsize<-psize[0,1:5];mret<-ret[0,1:14];mcond<-cond[0,]
for (prof in general$PROFILE_ID){
	indp <- basic$PROFILE_ID == prof
	pos <- table(basic$SAMPLE_POS[indp])
	for (p in 1:length(pos)){
	if (pos[p] > 1){ #need to avergae
	sid <- basic$SAMPLE_ID[indp & basic$SAMPLE_POS==p]
	# average basic
	m <- nrow(mbasic)
	mbasic[m+1,] <- basic[basic$SAMPLE_ID %in% sid,][1,1:23]
	mbasic$SAMPLE_ID[m+1] <- prof*100+p
	mbasic[m+1,c(4,5,7,8,10,11,13,14,18:23)] <- apply(basic[basic$SAMPLE_ID %in% sid,c(4,5,7,8,10,11,13,14,18:23)],2,median)
	# average chemical
	mchem[m+1,] <- apply(chemical[chemical$SAMPLE_ID %in% sid,1:29],2,median)
	mchem[m+1,"SAMPLE_ID"] <- prof*100+p
	# average psize
	s <- unique(psize[psize$SAMPLE_ID %in% sid,"P_SIZE"])
	n <- length(s)
	m <- nrow(mpsize)
	for (is in 1:n){
	mpsize[m+is,1:5]<-apply(psize[psize$SAMPLE_ID %in% sid & psize$P_SIZE==s[is],1:5],2,median)}
	mpsize[(m+1):(m+n),"SAMPLE_ID"] <- prof*100+p
	# average ret
	# plot(THETA~log10(HEAD),data=ret[ret$SAMPLE_ID %in% sid,])
	h <- unique(ret[ret$SAMPLE_ID %in% sid,"HEAD"])
	n <- length(h)
	m <- nrow(mret)
	for (ih in 1:n){
	mret[m+ih,]<-apply(ret[ret$SAMPLE_ID %in% sid & ret$HEAD==h[ih],],2,median)}
	mret[(m+1):(m+n),"SAMPLE_ID"] <- prof*100+p
	# average cond?
	m <- nrow(mcond)
	mcond[m+1,] <- apply(cond[cond$SAMPLE_ID %in% sid,],2,median)
	mcond[m+1,"SAMPLE_ID"] <- prof*100+p
	
	# } else {
	# count<-count+1
	# print("CAUTION")
	}
	}	
}
basic<-mbasic
chemical<-mchem
psize<-mpsize
psize$P_PERCENT <- round(psize$P_PERCENT,digit=1)
psize$P_PERCENT[psize$SAMPLE_ID==3800360202 & psize$P_SIZE==2000]<-46.6 #insyead of 46.8, so that sum p_percent <101.
ret<-mret[!is.na(mret[,1]),]
cond<-mcond[!is.na(mcond[,1]),]