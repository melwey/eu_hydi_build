# MVG fit
# fitting Mualem and van Genuchten models to EU-HYDI data
# 
# Author: M. Weynants
# Date created: 2013/05/03
##################################################################

load("../output/EUHYDI_NA_v1_1.Rdata")
hydi <- hydi.na;rm(hydi.na)

# set some useful functions
pF2h <- function(pF){h <- 10^(pF)}
h2pF <- function(h){pF <- log10(h)}
source("MV.R")
source("fitTheta.R")
source("mov_filter.r")

# RET
sid_ret <- unique(hydi$RET$SAMPLE_ID)

# COND
sid_cond <- unique(hydi$COND$SAMPLE_ID)
OF_MV_ls <- function(parMV,h_obs,theta_obs,h_cond_obs,K_obs){
	if (parMV[3] < 0){parMV[3] <- 10^(parMV[3]); parMV[4] <- 10^(parMV[4])+1}
	parMV[5] <- 10^(parMV[5])
	mv <- MV(c(parMV[1:4], 1 - 1/parMV[4],parMV[5:6]), h=h_obs)
	theta_sim <- mv$theta
	mv <- MV(c(parMV[1:4], 1 - 1/parMV[4],parMV[5:6]), h=h_cond_obs)
	K_sim <- mv$K
	err_theta <- theta_obs - theta_sim
	err_K <- log10(K_obs)-log10(K_sim)
	OF <- sum(err_theta^2)/var(err_theta) + sum(err_K^2)/var(err_K)
	return(OF)
}

param <- as.data.frame(matrix(NA,nrow=length(sid_ret),ncol=10))
names(param)<-c("thr","ths","alp","n","m","Ks","L","conv","OF","meth")
for (i in 1:length(sid_ret)){
indr <- hydi$RET$SAMPLE_ID == sid_ret[i] & hydi$RET$FLAG
indc <- hydi$COND$SAMPLE_ID == sid_ret[i] & hydi$COND$IND_VALUE
# ind <- RETfilt$SAMPLE_ID == sid_ret[i]
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
	par0 <- try(MV_SS(hi,thi),silent=TRUE)
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
	MV_opt <- try(
		optimx(par=unlist(par0)[1:6],
		fn=OF_MV_ls,
		method="nlminb",#"L-BFGS-B", 
		lower=L,upper=U,
		control=list(maxit=1000#,all.methods=TRUE
		),
		h_obs=hi,theta_obs=thi,h_cond_obs=hci,K_obs=ki)
		)
	} else {
	MV_opt <- try(
		optimx(par=unlist(par0)[1:4],
		fn=OF_VG_ls,
		method="nlminb",#"L-BFGS-B", 
		lower=L[1:4],upper=U[1:4],
		control=list(maxit=1000#,all.methods=TRUE
		),
		h_obs=hi,theta_obs=thi)
		)
	}
	# if optimx failed
	if (is.character(MV_opt)){next}
	# in results, methods sorted by decreasing fvalues
	m <- nrow(MV_opt)
	if (any(indc)){
		par_opt <- MV_opt[m, 1:6] 
		param[i,1:7] <- c(par_opt[1:4],1-1/par_opt[4],par_opt[5:6])
	} else {
		par_opt <- MV_opt[m, 1:4] 
		param[i,1:5] <- c(par_opt[1:4],1-1/par_opt[4])
	}
	param[i,8]<-MV_opt[m,"convcode"]
	param[i,9]<-MV_opt[m,"value"]
	param[i,10] <- rownames(MV_opt[m,])
	# save after each fit
	save("i","sid_ret","param",file="temp.rdata")
}
}
MVG_hydi_nofilt <- cbind(sid_ret, param);names(MVG_hydi_nofilt)[1]<-"SAMPLE_ID"
save("MVG_hydi_nofilt",file="MVG_hydi_nofilt.rdata")

# on filtered data
load("../output/VG_hydi.rdata")
load("../output/MV_hydi.rdata")

param <- as.data.frame(matrix(NA,nrow=length(sid_ret),ncol=10))
names(param)<-c("thr","ths","alp","n","m","Ks","L","conv","OF","meth")
for (i in 1:length(sid_ret)){
indr <- RETfilt$SAMPLE_ID == sid_ret[i]
indc <- CONDfilt$SAMPLE_ID == sid_ret[i]
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
	par0 <- try(MV_SS(hi,thi),silent=TRUE)
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
	MV_opt <- try(
		optimx(par=unlist(par0)[1:6],
		fn=OF_MV_ls,
		method="nlminb",#"L-BFGS-B", 
		lower=L,upper=U,
		control=list(maxit=1000#, trace = 1#,all.methods=TRUE
		),
		h_obs=hi,theta_obs=thi,h_cond_obs=hci,K_obs=ki)
		)
	} else {
	MV_opt <- try(
		optimx(par=unlist(par0)[1:4],
		fn=OF_VG_ls,
		method="nlminb",#"L-BFGS-B", 
		lower=L[1:4],upper=U[1:4],
		control=list(maxit=1000#,all.methods=TRUE
		),
		h_obs=hi,theta_obs=thi)
		)
	}
	# if optimx failed
	if (is.character(MV_opt)){next}
	# in results, methods sorted by decreasing fvalues
	m <- nrow(MV_opt)
	if (any(indc)){
		par_opt <- MV_opt[m, 1:6] 
		param[i,1:7] <- c(par_opt[1:4],1-1/par_opt[4],par_opt[5:6])
	} else {
		par_opt <- MV_opt[m, 1:4] 
		param[i,1:5] <- c(par_opt[1:4],1-1/par_opt[4])
	}
	param[i,8]<-MV_opt[m,"convcode"]
	param[i,9]<-MV_opt[m,"value"]
	param[i,10] <- rownames(MV_opt[m,])
	# save after each fit
	save("i","sid_ret","param",file="temp.rdata")
}
}
MVG_hydi_filt <- cbind(sid_ret, param);names(MVG_hydi_filt)[1]<-"SAMPLE_ID"
save("MVG_hydi_filt",file="MVG_hydi_filt.rdata")


# # look at parameters...
# png(filename="./fig/VGfit_thr_ths.png")
# plot(thr~ths,data=VG_hydi)
# dev.off()
# png(filename="./fig/VGfit_alp_n.png")
# plot(log10(alp)~log10(n-1),data=VG.hydi)
# dev.off()
# png(filename="./fig/MVGfit_Ks_L.png")
# plot(log10(Ks)~L,data=MV_hydi)
# dev.off()

# compare the approaches:
# 1) is there a bias introduced by the filtering?
# 2) coupled versus sequential: I expect conductivity to be better fitted with the coupled approach, but the retention to be worse.

# randomly select samples for the test
# test1 with only retention data that have been filtered
set.seed(2579)
s1 <- sample(sid_ret[!sid_ret %in% sid_cond & MVG_hydi_nofilt$conv == 0 & !is.na(MVG.hydi.nofilt$conv)], 9)
# test2 with conductivity data
set.seed(555)
s2 <- sample(sid_ret[sid_ret %in% sid_cond & MVG_hydi_nofilt$conv == 0 & !is.na(MVG_hydi_nofilt$conv)], 9)

# plot 9 examples
par(mfrow = c(3,3))
hline <- 10^(seq(0,4.5,0.1))
for (sid in s1){
  # all observations
  p <- ggplot() + 
  	geom_line(data = hydi$RET[hydi$RET$SAMPLE_ID == sid & hydi$RET$FLAG,],
  		mapping = aes(HEAD, THETA)) +
	geom_point(data = RETfilt[RETfilt$SAMPLE_ID == sid,],
		mapping = aes(HEAD, THETA, colour = "blue", shape = 4)) +
	scale_x_log10("Suction head")
  # fit to full
  parMV <- MVG_hydi_nofilt[MVG_hydi_nofilt$SAMPLE_ID == sid, 2:8]
  mv <- MV(parMV, h=hline)
  p <- p + geom_line(mv, aes(h,theta, colour = "black"))
  # fit to filtered
  parMV <- MVG_hydi_filt[MVG_hydi_filt$SAMPLE_ID == sid, 2:8]
  mv <- MV(parMV, h=hline)
  p <- p + geom_line(mv, aes(h, theta, colour = "blue", linetype = 2))
  # fit to filtered (original)
  parMV <- VG_hydi[VG_hydi$SAMPLE_ID == sid, 2:8]
  mv <- MV(parMV, h=hline)
  p <- p + geom_line(mv, aes(h, theta, colour = "green", linetype = 2))
  ggsave(paste0("../fig/",sid,".png"),p)
}
