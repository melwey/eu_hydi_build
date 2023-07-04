# MVG fit
# fitting Mualem and van Genuchten models to EU-HYDI data
# 
# Author: M. Weynsnts
# Date created: 2013/05/03
##################################################################

load("EUHYDI_NA_v1_1.Rdata")
hydi <- hydi.na;rm(hydi.na)

# set some useful functions
pF2h <- function(pF){h <- 10^(pF)}
h2pF <- function(h){pF <- log10(h)}
source("../../../Rwork/MyFunctions/MV.R")
source("../../../Rwork/MyFunctions/fitTheta.R")
source("mov_filter.r")

# RET
sid.ret <- unique(hydi$RET$SAMPLE_ID)

# COND
sid.cond <- unique(hydi$COND$SAMPLE_ID)
OF.MV.ls <- function(parMV,h.obs,theta.obs,h.cond.obs,K.obs){
	if (parMV[3] < 0){parMV[3] <- 10^(parMV[3]); parMV[4] <- 10^(parMV[4])+1}
	parMV[5] <- 10^(parMV[5])
	mv <- MV(c(parMV[1:4], 1 - 1/parMV[4],parMV[5:6]), h=h.obs)
	theta.sim <- mv$theta
	mv <- MV(c(parMV[1:4], 1 - 1/parMV[4],parMV[5:6]), h=h.cond.obs)
	K.sim <- mv$K
	err.theta <- theta.obs - theta.sim
	err.K <- log10(K.obs)-log10(K.sim)
	OF <- sum(err.theta^2)/var(err.theta) + sum(err.K^2)/var(err.K)
	return(OF)
}

param <- as.data.frame(matrix(NA,nrow=length(sid.ret),ncol=10))
names(param)<-c("thr","ths","alp","n","m","Ks","L","conv","OF","meth")
for (i in 1:length(sid.ret)){
indr <- hydi$RET$SAMPLE_ID == sid.ret[i] & hydi$RET$FLAG
indc <- hydi$COND$SAMPLE_ID == sid.ret[i] & hydi$COND$IND_VALUE
# ind <- RETfilt$SAMPLE_ID == sid.ret[i]
if (any(indr)){
	if (sum(indr) < 5) {next}
	hi <- hydi$RET[indr,"HEAD"]
	thi <- hydi$RET[indr,"THETA"]
	if (any(indc)){
	hci <- hydi$COND[indc,"VALUE"]
	ki <-  hydi$COND[indc,"COND"]
	}
	# do the fit
	#par0$alp<-log10(par0$alp);par0$n<-log10(par0$n-1)
	L<-c(0,.2,0.000001,1.001,-2,-5);U<-c(0.45,0.85,1-1e-5,15,4,5)
	par0 <- try(MV.SS(hi,thi),silent=TRUE)
	if (is.character(par0)){par0<-c(0.05,0.5,0.05,2.5);names(par0)<-c("thr","ths","alp","n")} else {par0 <- unlist(par0[1:4])}
	par0[5:6] <- ifelse(rep(any(indc),2),c(Ks=log10(max(ki)),L=0.5),rep(NA,2))
	np <- ifelse(any(indc),6,4)
	if (any(par0[1:np] < L[1:np]) | any(par0[1:np] > U[1:np]))
		{
		indL <- par0[1:np] < L[1:np]; indU <- par0[1:np] > U[1:np]
		par0[1:np][indL] <- L[1:np][indL]; par0[1:np][indU]<-U[1:np][indU]
		}
	if (any(indc)){
	# coupled fit
	MV.opt <- try(
		optimx(par=unlist(par0)[1:6],
		fn=OF.MV.ls,
		method="nlminb",#"L-BFGS-B", 
		lower=L,upper=U,
		control=list(maxit=1000#,all.methods=TRUE
		),
		h.obs=hi,theta.obs=thi,h.cond.obs=hci,K.obs=ki)
		)
	} else {
	MV.opt <- try(
		optimx(par=unlist(par0)[1:4],
		fn=OF.VG.ls,
		method="nlminb",#"L-BFGS-B", 
		lower=L[1:4],upper=U[1:4],
		control=list(maxit=1000#,all.methods=TRUE
		),
		h.obs=hi,theta.obs=thi)
		)
	}
	# if optimx failed
	if (is.character(MV.opt)){next}
	if (length(MV.opt$method)>1){
		# in results, methods sorted by decreasing fvalues
		m <- length(MV.opt$method)
	} else {m=1}
	par.opt <- MV.opt$par[[m]] #par.opt[3]<-10^(par.opt[3]);par.opt[4]<-10^(par.opt[4]+1)
	param[i,1:7] <- c(par.opt[1:4],1-1/par.opt[4],par.opt[5:6])
	param[i,8]<-MV.opt$conv[[m]]
	param[i,9]<-MV.opt$fvalues[[m]]
	param[i,10] <- MV.opt$method[[m]]
	# save after each fit
	save("i","sid.ret","param",file="temp.rdata")
}
}
MVG.hydi.nofilt <- cbind(sid.ret, param);names(MVG.hydi.nofilt)[1]<-"SAMPLE_ID"
save("MVG.hydi.nofilt",file="MVG_hydi_nofilt.rdata")

# on filtered data
load("VG_hydi.rdata")
load("MV_hydi.rdata")

param <- as.data.frame(matrix(NA,nrow=length(sid.ret),ncol=10))
names(param)<-c("thr","ths","alp","n","m","Ks","L","conv","OF","meth")
for (i in 1:length(sid.ret)){
indr <- RETfilt$SAMPLE_ID == sid.ret[i]
indc <- CONDfilt$SAMPLE_ID == sid.ret[i]
if (any(indr)){
	hi <- RETfilt[indr,"HEAD"]
	thi <- RETfilt[indr,"THETA"]
	if (any(indc)){
	hci <- CONDfilt[indc,"HEAD"]
	ki <-  CONDfilt[indc,"K"]
	}
	# do the fit
	#par0$alp<-log10(par0$alp);par0$n<-log10(par0$n-1)
	L<-c(0,.2,0.000001,1.001,-2,-5);U<-c(0.45,0.85,1-1e-5,15,4,5)
	par0 <- try(MV.SS(hi,thi),silent=TRUE)
	if (is.character(par0)){par0<-c(0.05,0.5,0.05,2.5);names(par0)<-c("thr","ths","alp","n")} else {par0 <- unlist(par0[1:4])}
	par0[5:6] <- ifelse(rep(any(indc),2),c(Ks=log10(max(ki)),L=0.5),rep(NA,2))
	np <- ifelse(any(indc),6,4)
	if (any(par0[1:np] < L[1:np]) | any(par0[1:np] > U[1:np]))
		{
		indL <- par0[1:np] < L[1:np]; indU <- par0[1:np] > U[1:np]
		par0[1:np][indL] <- L[1:np][indL]; par0[1:np][indU]<-U[1:np][indU]
		}
	if (any(indc)){
	# coupled fit
	MV.opt <- try(
		optimx(par=unlist(par0)[1:6],
		fn=OF.MV.ls,
		method="nlminb",#"L-BFGS-B", 
		lower=L,upper=U,
		control=list(maxit=1000#, trace = 1#,all.methods=TRUE
		),
		h.obs=hi,theta.obs=thi,h.cond.obs=hci,K.obs=ki)
		)
	} else {
	MV.opt <- try(
		optimx(par=unlist(par0)[1:4],
		fn=OF.VG.ls,
		method="nlminb",#"L-BFGS-B", 
		lower=L[1:4],upper=U[1:4],
		control=list(maxit=1000#,all.methods=TRUE
		),
		h.obs=hi,theta.obs=thi)
		)
	}
	# if optimx failed
	if (is.character(MV.opt)){next}
	if (length(MV.opt$method)>1){
		# in results, methods sorted by decreasing fvalues
		m <- length(MV.opt$method)
	} else {m=1}
	par.opt <- MV.opt$par[[m]] #par.opt[3]<-10^(par.opt[3]);par.opt[4]<-10^(par.opt[4]+1)
	param[i,1:7] <- c(par.opt[1:4],1-1/par.opt[4],par.opt[5:6])
	param[i,8]<-MV.opt$conv[[m]]
	param[i,9]<-MV.opt$fvalues[[m]]
	param[i,10] <- MV.opt$method[[m]]
	# save after each fit
	save("i","sid.ret","param",file="temp.rdata")
}
}
MVG.hydi.filt <- cbind(sid.ret, param);names(MVG.hydi.filt)[1]<-"SAMPLE_ID"
save("MVG.hydi.filt",file="MVG_hydi_filt.rdata")


# # look at parameters...
# png(filename="./fig/VGfit_thr_ths.png")
# plot(thr~ths,data=VG.hydi)
# dev.off()
# png(filename="./fig/VGfit_alp_n.png")
# plot(log10(alp)~log10(n-1),data=VG.hydi)
# dev.off()
# png(filename="./fig/MVGfit_Ks_L.png")
# plot(log10(Ks)~L,data=MV.hydi)
# dev.off()
# file.copy(from="./fig/VGfit_thr_ths.png",to="../../../MyWater/Europe/Rcode/HYDI/fig/VGfit_thr_ths.png")
# file.copy(from="./fig/VGfit_alp_n.png",to="../../../MyWater/Europe/Rcode/HYDI/fig/VGfit_alp_n.png")
# file.copy(from="./fig/MVGfit_Ks_L.png",to="E:/weyname/Documents/Documents/MyWater/Europe/Rcode/HYDI/fig/MVGfit_Ks_L.png")

# compare the approaches:
# 1) is there a bias introduced by the filtering?
# 2) coupled versus sequential: I expect conductivity to be better fitted with the coupled approach, but the retention to be worse.

# randomly select samples for the test
# test1 with only retention data that have been filtered
set.seed(2579)
s1 <- sample(sid.ret[!sid.ret %in% sid.cond & MVG.hydi.nofilt$conv == 0 & !is.na(MVG.hydi.nofilt$conv) & nret > 20], 9)
# test2 with conductivity data
set.seed(555)
s2 <- sample(sid.ret[sid.ret %in% sid.cond & MVG.hydi.nofilt$conv == 0 & !is.na(MVG.hydi.nofilt$conv)], 9)

# plot 9 examples
par(mfrow = c(3,3))
hline <- 10^(seq(0,4.5,0.1))
for (sid in s1){
  # all observations
  plot(THETA ~ HEAD, data = hydi$RET[hydi$RET$SAMPLE_ID == sid & hydi$RET$FLAG,], log = "x")
  # filtered data
  points(THETA ~ HEAD, data = RETfilt[RETfilt$SAMPLE_ID == sid,], col = "blue", pch = 4)
  # fit to full
  parMV <- MVG.hydi.nofilt[MVG.hydi.nofilt$SAMPLE_ID == sid, 2:8]
  mv <- MV(parMV, h=hline)
  lines(theta ~ h, data=mv, col = "black")
  # fit to filtered
  parMV <- MVG.hydi.filt[MVG.hydi.filt$SAMPLE_ID == sid, 2:8]
  mv <- MV(parMV, h=hline)
  lines(theta ~ h, data=mv, col = "blue", lty = 2)
  # fit to filtered (original)
  parMV <- VG.hydi[VG.hydi$SAMPLE_ID == sid, 2:8]
  mv <- MV(parMV, h=hline)
  lines(theta ~ h, data=mv, col = "green", lty = 2)
  
}
# fuck: there is something wrong now....