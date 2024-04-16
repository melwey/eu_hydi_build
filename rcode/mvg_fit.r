# MVG fit
# fitting Mualem and van Genuchten models to EU-HYDI data, with optimx
# 
# Author: M. Weynsnts
# Date created: 2013/05/03
##################################################################

load("../output/EUHYDI_NA_v1_1.Rdata")
export_dir <- "../output/EUHYDI_v1_1_csv"

# set some useful functions
pF2h <- function(pF){h <- 10^(pF)}
h2pF <- function(h){pF <- log10(h)}
source("MV.R")
source("fitTheta.R")
source("mov_filter.r")

sid_ret <- unique(hydi$RET$SAMPLE_ID)
ind_ret <- matrix(FALSE, nrow = length(sid_ret), ncol = 4)
nret <- rep(0, length(sid_ret))

# first apply filter
RETfilt <- data.frame(SAMPLE_ID=NULL,HEAD=NULL,THETA=NULL,THETA_FIT=NULL)
for (i in seq_along(sid_ret)){
	ind <- hydi$RET$SAMPLE_ID == sid_ret[i] & hydi$RET$FLAG
	nret[i] <- sum(ind)
	ind_ret[i,1] <- nret[i] >= 5
	# one point <= pF1
	ind_ret[i,2] <- any(hydi$RET[ind,"HEAD"] <= 12.5) # having less than half the sample height is impossible (classical ring: 2.5)
	# one point between pF1 and pF2.5
	ind_ret[i,3] <- any(hydi$RET[ind,"HEAD"] > 12.5 & hydi$RET[ind,"HEAD"] <= 350)
	# one point above pF 2.5
	ind_ret[i,4] <- any(hydi$RET[ind,"HEAD"] > 350)
	# for fitting, would be better to ensure that there are measurement below pF1, between pF1 and pF2 and above pF2. But...
	# if (all(ind_ret[i,]) & all(hydi$RET[ind,"THETA"] != 0)){
	if (ind_ret[i,1]){
		hi <- hydi$RET[ind,"HEAD"]
		thi <- hydi$RET[ind,"THETA"]
		# need filtering?
		if (nret[i] > 20 | any(duplicated(hi))){f <- h_filter(cbind(hi,thi)); hi<-f[,1];thi<-f[,2]}
		RETfilt <- rbind(RETfilt,data.frame(SAMPLE_ID=sid_ret[i],HEAD=hi,THETA=thi,THETA_FIT=NA))
	}
}

# # paste old THETA_FIT to new RETfilt
# RETfilt_new <- cbind(RETfilt[,1:3],RETfilt_original[match(paste(RETfilt$SAMPLE_ID,RETfilt$HEAD),paste(RETfilt$SAMPLE_ID,RETfilt$HEAD)),"THETA_FIT"])
# names(RETfilt_new)[4] <- "THETA_FIT"
# RETfilt<-RETfilt_new

param <- as.data.frame(matrix(NA,nrow=length(sid_ret),ncol=8))
names(param)<-c("thr","ths","alp","n","m","conv","OF","meth")
for (i in 1:length(sid_ret)){
# ind <- hydi$RET$SAMPLE_ID == sid_ret[i] & hydi$RET$FLAG
ind <- RETfilt$SAMPLE_ID == sid_ret[i]
if (any(ind)){
	hi <- RETfilt[ind,"HEAD"]
	thi <- RETfilt[ind,"THETA"]
	# do the fit
	#par0$alp<-log10(par0$alp);par0$n<-log10(par0$n-1)
	L<-c(0,.2,0.000001,1.001);U<-c(0.45,0.85,1-1e-5,15)
	par0 <- try(MV_SS(hi,thi),silent=TRUE)
	if (is.character(par0)){par0<-c(0.05,0.5,0.05,2.5);names(par0)<-c("thr","ths","alp","n")}
	if (any(par0[1:4] < L) | any(par0[1:4] > U)){
		indL <- par0[1:4]<L; indU <- par0[1:4]>U
		par0[1:4][indL] <- L[indL];par0[1:4][indU]<-U[indU]
		}
		MV_opt <- try(
		optimx(par=unlist(par0)[1:4],
		fn=OF_VG_ls,
		method="nlminb",#"L-BFGS-B", 
		lower=L,upper=U,
		control=list(maxit=1000#,all.methods=TRUE
		),
		h_obs=hi,theta_obs=thi))
		if (is.character(MV_opt)){next}
		# MV_opt is of class "optimx", "data.frame"
		m <- nrow(MV_opt)
		# in results, methods sorted by decreasing fvalues
		par_opt <- MV_opt[m, 1:4] #par_opt[3]<-10^(par_opt[3]);par_opt[4]<-10^(par_opt[4]+1)
		param[i,1:5] <- c(par_opt,1-1/par_opt[4])
		param[i,6]<-MV_opt[m, "convcode"]
		param[i,7]<-MV_opt[m, "value"]
		param[i,8] <- rownames(MV_opt[m,])
		RETfilt$THETA_FIT[RETfilt[,1]==sid_ret[i]] <- MV(par_opt,hi)$theta
		# save after each fit
		save("i","sid_ret","param","RETfilt",file="temp.rdata")
}
}
# plot(THETA~log10(HEAD),data=hydi$RET[ind,])
# points(log10(hi),thi,col="red",pch=4)
# lines(pF<-seq(0.2,5,by=0.1),MV(par.opt,10^(pF))$theta,col="green")
# save 
VG_hydi <- cbind(sid_ret,param);names(VG_hydi)[1]<-"SAMPLE_ID"
save("VG_hydi","ind_ret","RETfilt","nret",file="../output/VG_hydi.rdata")
# export csv with valid VG_hydi
readr::write_excel_csv(VG_hydi %>% dplyr::filter(!is.na(thr)),
                   file = paste(export_dir,"/VG_fitted.csv", sep=""),
                   # na = "ND",
                   # would that fix the problem?
                   # quote = "all"
                   )


# COND
OF_MV_ls <- function(parK,h_obs,K_obs,parT){
	par <- as.list(c(parT,parK))
	if (par$alp<0){par$alp <- 10^(par$alp);par$n<-10^(par$n)+1}
	if (!("m" %in% names(par))){par$m=1-1/par$n}
	par$Ks <- 10^(par$Ks)
	mv <- MV(par,h=h_obs)
	K_sim <- mv$K
	OF <- sum((log10(K_obs)-log10(K_sim))^2)
	return(OF)
}


sid_cond <- unique(hydi$COND$SAMPLE_ID)
ind_cond <- rep(FALSE,length(sid_cond))
ncond <- rep(0,length(sid_cond))
param <- as.data.frame(matrix(NA,nrow=length(sid_cond),ncol=4))
names(param)<-c("Ks","L","conv","OF")
CONDfilt<-data.frame(SAMPLE_ID=NULL,HEAD=NULL,COND=NULL,COND_FIT=NULL)
for(i in 1:length(sid_cond)){
ind <- hydi$COND$SAMPLE_ID == sid_cond[i] & hydi$COND$COND != 0
ncond[i] <- sum(ind)
if (ncond[i] >= 5){ind_cond[i] <- TRUE
	hi <- hydi$COND[ind,"VALUE"]
	ki <- hydi$COND[ind,"COND"]
	# need filtering?
	if (ncond[i] > 20 | any(duplicated(hi))){f <- h_filter(cbind(hi,ki)); hi<-f[,1];ki<-f[,2]}
	CONDfilt <- rbind(CONDfilt,data.frame(SAMPLE_ID=sid_cond[i],HEAD=hi,K=ki,K_FIT=NA))	
}
}

for(i in 1:length(sid_cond)){
ind <- CONDfilt$SAMPLE_ID == sid_cond[i]
ncond[i] <- sum(ind)
if (ncond[i] >= 5){ind_cond[i] <- TRUE
	hi <- CONDfilt[ind,"HEAD"]
	ki <- CONDfilt[ind,"K"]
	L<-c(-2,-5);U<-c(4,5)
	par0 <- list(Ks=log10(ki[order(hi)[1]]),L=0.5)
	MV_opt <- try(
	optimx(par=unlist(par0),fn=OF_MV_ls,method="nlminb",
		lower=L,upper=U,
		control=list(maxit=1000),
		h_obs=hi,K_obs=ki,parT=VG_hydi[VG_hydi$SAMPLE_ID==sid_cond[i],2:6]),silent=TRUE)
		if (is.character(MV_opt)){next}
		#
		par_opt <- MV_opt[1, 1:2] #par_opt[3]<-10^(par_opt[3]);par_opt[4]<-10^(par_opt[4]+1)
		par_opt[1] <- 10^(par_opt[1])
		param[i,1:2] <- par_opt
		param[i,3] <- MV_opt[1, "convcode"]
		param[i,4] <- MV_opt[1, "value"]
		pVG <- VG_hydi[VG_hydi$SAMPLE_ID==sid_cond[i],2:6]
		CONDfilt$K_FIT[CONDfilt[,1]==sid_cond[i]] <- MV(c(pVG,param[i,1:2]),hi)$K
		# save after each fit
		save("i","sid_cond","param","CONDfilt",file="temp.rdata")
}
}

# save 
MV_hydi <- cbind(sid_cond, param); names(MV_hydi)[1] <- "SAMPLE_ID"
save("MV_hydi", "ind_cond", "CONDfilt", "ncond", file = "../output/MV_hydi.rdata")
# export csv with valid VG_hydi
readr::write_excel_csv(MV_hydi %>% dplyr::filter(!is.na(OF)),
                   file = paste(export_dir,"/MV_fitted.csv", sep=""),
                   )

# look at parameters...
png(filename="../fig/VGfit_thr_ths.png")
plot(thr~ths,data=VG_hydi)
dev.off()
png(filename="../fig/VGfit_alp_n.png")
plot(log10(alp)~log10(n-1),data=VG_hydi)
dev.off()
png(filename="../fig/MVGfit_Ks_L.png")
plot(log10(Ks)~L,data=MV_hydi)
dev.off()
