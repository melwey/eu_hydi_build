# MVG fit
# fitting Mualem and van Genuchten models to EU-HYDI data, with optimx
# 
# Author: M. Weynsnts
# Date created: 2013/05/03
##################################################################

load("../output/HYDI_SOURCE_nd_qa3.Rdata")

# set some useful functions
pF2h <- function(pF){h <- 10^(pF)}
h2pF <- function(h){pF <- log10(h)}
source("MV.R")
source("fitTheta.R")
source("mov_filter.r")

sid.ret <- unique(hydi$RET$SAMPLE_ID)
ind.ret <- matrix(FALSE, nrow = length(sid.ret), ncol = 4)
nret <- rep(0, length(sid.ret))

# first apply filter
RETfilt <- data.frame(SAMPLE_ID=NULL,HEAD=NULL,THETA=NULL,THETA_FIT=NULL)
for (i in seq_along(sid.ret)){
	ind <- hydi$RET$SAMPLE_ID == sid.ret[i] & hydi$RET$FLAG
	nret[i] <- sum(ind)
	ind.ret[i,1] <- nret[i] >= 5
	# one point <= pF1
	ind.ret[i,2] <- any(hydi$RET[ind,"HEAD"] <= 12.5) # having less than half the sample height is impossible (classical ring: 2.5)
	# one point between pF1 and pF2.5
	ind.ret[i,3] <- any(hydi$RET[ind,"HEAD"] > 12.5 & hydi$RET[ind,"HEAD"] <= 350)
	# one point above pF 2.5
	ind.ret[i,4] <- any(hydi$RET[ind,"HEAD"] > 350)
	# for fitting, would be better to ensure that there are measurement below pF1, between pF1 and pF2 and above pF2. But...
	# if (all(ind.ret[i,]) & all(hydi$RET[ind,"THETA"] != 0)){
	if (ind.ret[i,1]){
		hi <- hydi$RET[ind,"HEAD"]
		thi <- hydi$RET[ind,"THETA"]
		# need filtering?
		if (nret[i] > 20 | any(duplicated(hi))){f <- h.filter(cbind(hi,thi)); hi<-f[,1];thi<-f[,2]}
		RETfilt <- rbind(RETfilt,data.frame(SAMPLE_ID=sid.ret[i],HEAD=hi,THETA=thi,THETA_FIT=NA))
	}
}

# # paste old THETA_FIT to new RETfilt
# RETfilt.new <- cbind(RETfilt[,1:3],RETfilt.original[match(paste(RETfilt$SAMPLE_ID,RETfilt$HEAD),paste(RETfilt$SAMPLE_ID,RETfilt$HEAD)),"THETA_FIT"])
# names(RETfilt.new)[4] <- "THETA_FIT"
# RETfilt<-RETfilt.new

param <- as.data.frame(matrix(NA,nrow=length(sid.ret),ncol=8))
names(param)<-c("thr","ths","alp","n","m","conv","OF","meth")
for (i in 1:length(sid.ret)){
# ind <- hydi$RET$SAMPLE_ID == sid.ret[i] & hydi$RET$FLAG
ind <- RETfilt$SAMPLE_ID == sid.ret[i]
if (any(ind)){
	hi <- RETfilt[ind,"HEAD"]
	thi <- RETfilt[ind,"THETA"]
	# do the fit
	#par0$alp<-log10(par0$alp);par0$n<-log10(par0$n-1)
	L<-c(0,.2,0.000001,1.001);U<-c(0.45,0.85,1-1e-5,15)
	par0 <- try(MV.SS(hi,thi),silent=TRUE)
	if (is.character(par0)){par0<-c(0.05,0.5,0.05,2.5);names(par0)<-c("thr","ths","alp","n")}
	if (any(par0[1:4] < L) | any(par0[1:4] > U)){
		indL <- par0[1:4]<L; indU <- par0[1:4]>U
		par0[1:4][indL] <- L[indL];par0[1:4][indU]<-U[indU]
		}
		MV.opt <- try(
		optimx(par=unlist(par0)[1:4],
		fn=OF.VG.ls,
		method="nlminb",#"L-BFGS-B", 
		lower=L,upper=U,
		control=list(maxit=1000#,all.methods=TRUE
		),
		h.obs=hi,theta.obs=thi))
		if (is.character(MV.opt)){next}
		if (length(MV.opt$method)>1){
		# in results, methods sorted by decreasing fvalues
		m <- length(MV.opt$method)}else{m=1}
		par.opt <- MV.opt$par[[m]] #par.opt[3]<-10^(par.opt[3]);par.opt[4]<-10^(par.opt[4]+1)
		param[i,1:5] <- c(par.opt,1-1/par.opt[4])
		param[i,6]<-MV.opt$conv[[m]]
		param[i,7]<-MV.opt$fvalues[[m]]
		param[i,8] <- MV.opt$method[[m]]
		RETfilt$THETA_FIT[RETfilt[,1]==sid.ret[i]] <- MV(par.opt,hi)$theta
		# save after each fit
		save("i","sid.ret","param","RETfilt",file="temp.rdata")
}
}
# plot(THETA~log10(HEAD),data=hydi$RET[ind,])
# points(log10(hi),thi,col="red",pch=4)
# lines(pF<-seq(0.2,5,by=0.1),MV(par.opt,10^(pF))$theta,col="green")
# save 
VG.hydi <- cbind(sid.ret,param);names(VG.hydi)[1]<-"SAMPLE_ID"
save("VG.hydi","ind.ret","RETfilt","nret",file="VG_hydi.rdata")

# # redo for all samples that did not converge (have a flat curve)
# # problem when both alp and n are large
# nretf <- table(RETfilt$SAMPLE_ID)
# vg <- merge(VG.hydi,data.frame(SAMPLE_ID=names(nretf),n=nretf))
# summary((vg$OF)/vg$n.Freq))
# ind <- vg$alp > 0.1 & vg$n>5
# sum(ind)
# ind<-RETfilt$THETA-RETfilt$THETA_FIT< -0.1 | RETfilt$THETA-RETfilt$THETA_FIT>0.1
# sum(table(RETfilt$SAMPLE_ID[ind])>=3)

# h <- 1:1e5
# th <- MV(c(0.05,0.45,0.5,10,1-1/10,NA,NA),h)
# plot(log10(h),th)

# COND
OF.MV.ls <- function(parK,h.obs,K.obs,parT){
	par <- as.list(c(parT,parK))
	if (par$alp<0){par$alp <- 10^(par$alp);par$n<-10^(par$n)+1}
	if (!("m" %in% names(par))){par$m=1-1/par$n}
	par$Ks <- 10^(par$Ks)
	mv <- MV(par,h=h.obs)
	K.sim <- mv$K
	OF <- sum((log10(K.obs)-log10(K.sim))^2)
	return(OF)
}


sid.cond <- unique(hydi$COND$SAMPLE_ID)
ind.cond <- rep(FALSE,length(sid.cond))
ncond <- rep(0,length(sid.cond))
param <- as.data.frame(matrix(NA,nrow=length(sid.cond),ncol=4))
names(param)<-c("Ks","L","conv","OF")
CONDfilt<-data.frame(SAMPLE_ID=NULL,HEAD=NULL,COND=NULL,COND_FIT=NULL)
for(i in 1:length(sid.cond)){
ind <- hydi$COND$SAMPLE_ID == sid.cond[i] & hydi$COND$COND != 0
ncond[i] <- sum(ind)
if (ncond[i] >= 5){ind.cond[i] <- TRUE
	hi <- hydi$COND[ind,"VALUE"]
	ki <- hydi$COND[ind,"COND"]
	# need filtering?
	if (ncond[i] > 20 | any(duplicated(hi))){f <- h.filter(cbind(hi,ki)); hi<-f[,1];ki<-f[,2]}
	CONDfilt <- rbind(CONDfilt,data.frame(SAMPLE_ID=sid.cond[i],HEAD=hi,K=ki,K_FIT=NA))	
}

}

for(i in 1:length(sid.cond)){
ind <- CONDfilt$SAMPLE_ID == sid.cond[i]
ncond[i] <- sum(ind)
if (ncond[i] >= 5){ind.cond[i] <- TRUE
	hi <- CONDfilt[ind,"HEAD"]
	ki <- CONDfilt[ind,"K"]
	L<-c(-2,-5);U<-c(4,5)
	par0 <- list(Ks=log10(ki[order(hi)[1]]),L=0.5)
	MV.opt <- try(
	optimx(par=unlist(par0),fn=OF.MV.ls,method="nlminb",
		lower=L,upper=U,
		control=list(maxit=1000),
		h.obs=hi,K.obs=ki,parT=VG.hydi[VG.hydi$SAMPLE_ID==sid.cond[i],2:6]),silent=TRUE)
		if (is.character(MV.opt)){next}
		par.opt <- MV.opt$par$par; #par.opt[3]<-10^(par.opt[3]);par.opt[4]<-10^(par.opt[4]+1)
		par.opt[1] <- 10^(par.opt[1])
		param[i,1:2] <- par.opt
		param[i,3]<-MV.opt$conv
		param[i,4]<-MV.opt$fvalues
		pVG <- VG.hydi[VG.hydi$SAMPLE_ID==sid.cond[i],2:6]
		CONDfilt$K_FIT[CONDfilt[,1]==sid.cond[i]] <- MV(c(pVG,param[i,1:2]),hi)$K
		# save after each fit
		save("i","sid.cond","param","CONDfilt",file="temp.rdata")
}

}


# save 
MV.hydi <- cbind(sid.cond,param);names(MV.hydi)[1]<-"SAMPLE_ID"
save("MV.hydi","ind.cond","CONDfilt","ncond",file="MV_hydi.rdata")

# look at parameters...
png(filename="../fig/VGfit_thr_ths.png")
plot(thr~ths,data=VG.hydi)
dev.off()
png(filename="../fig/VGfit_alp_n.png")
plot(log10(alp)~log10(n-1),data=VG.hydi)
dev.off()
png(filename="../fig/MVGfit_Ks_L.png")
plot(log10(Ks)~L,data=MV.hydi)
dev.off()
