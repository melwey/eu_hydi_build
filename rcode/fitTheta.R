# TODO: Add comment
# 
# Author: M. Weynants, 2012.
###############################################################################
# 
require(optimx)
# MV.SS: generate initial guess for MV optimisation
MV.SS <- function(h,theta){
	# determine par0: starting point for optimisation of par
	# h must be positive
	ind <- h <= 0;	h[ind] <- -h[ind]
	# minimum value is 0
	if (sum(ind)>0){h[!ind]=0}
	# sort data by h and average duplicates
	xy <- sortedXyData(h,theta)
	n <- nrow(xy)
	ths0 <- xy$y[1] # max(theta)
	thr0 <- ifelse(xy$y[1]-xy$y[n]< 0.2,0,xy$y[n]) # min(theta) or 0 if difference too small
	thr0<-min(c(thr0,0.35))
	# find inflection point: where slope is steepest (in log scale)
	approxD <- diff(xy$y)/diff(log(xy$x))
	i <- which(approxD==min(approxD[2:(length(approxD)-1)]))
	if (length(i)>1){i=i[2]}
	# h.infl = 1/alp*(m)^(1/n) (root of d2theta/dh2)
	h.infl <- xy$x[i]+exp(diff(log(xy$x))[i])/2
	# linear approx at inflection point
	s.infl <- approxD[i]
	# replacing alp by alp(h.infl) in dtheta/dh = -(ths-thr)*m*n*alp^n*h^n*(1+(alp*h)^n)^(-m-1)
	s.infl.x <- -s.infl/(ths0-thr0) # =m^2/(1-m)*(1+m)^(-m-1)
	m0 <- 1-0.27/(s.infl.x+0.27) # approximation from plot of s.infl=f(m)
	n0 <- 1/(1-m0) # by definition of MV
	alp0 <- 1/h.infl*m0^(1/n0)
	list(thr=thr0,ths=ths0,alp=alp0,n=n0,m=m0,Ks=NA,L=NA)	
}
OF.VG.ls <- function(par,h.obs,theta.obs,weight=rep(1,length(h.obs))){
	par <- as.list(par)
	if (par$alp<0){par$alp <- 10^(par$alp);par$n<-10^(par$n)+1}
	if (!("m" %in% names(par))){par$m=1-1/par$n}
	if (!("Ks" %in% names(par))){par$Ks=NA}
	if (!("L" %in% names(par))){par$L=NA}
	mv <- MV(par,h=h.obs)
	theta.sim <- mv$theta
	OF <- sum(weight*(theta.obs-theta.sim)^2)
	OF
}

# Use:
# MV.opt <- optimx(par=par0,fn=OF.MV.ls,h.obs=h,theta.obs=th)


#selfStart "initial" attribute in SSvan
# mCall: matched call to the function model
# data: a data frame in which to interpret the variables in mCall
# LHS expression from the lefh-hand side of model
#function (mCall, data, LHS) 
#{
#	xy <- data.frame(sortedXyData(mCall[["input"]], LHS, data))
#	if (nrow(xy) < 4) {
#		stop("Too few distinct input values to fit a van Genuchten model")
#	}
#	ndistinct <- nrow(xy)
#	nlast <- max(3, round(ndistinct/2))
#	dfirst <- xy[1, ][["y"]][1]
#	dlast <- xy[nrow(xy), ][["y"]][1]
#	Thr1 <- ifelse(((xy[1, ][["y"]] - xy[ndistinct, ][["y"]]) < 
#						(xy[1, ][["y"]])/2), 0, xy[ndistinct, ][["y"]])
#	Ths1 <- dfirst
#	dmid <- xy[(ndistinct/2 - 2):(ndistinct/2 + 1), ]
#	pars2 <- coef(lm(y ~ log(x), data = dmid))
#	ymid <- xy[1:max(3, round(nrow(xy)/2)), ][["y"]][max(3, round(nrow(xy)/2))]
#	ax <- (ymid - pars2[1])/pars2[2]
#	slopep <- pars2[2]/(dfirst - dlast)
#	m1 <- ifelse(abs(slopep) < 1, (1 - exp(-0.8 * (abs(slopep)))), 
#			(1 - (0.5755/(abs(slopep))) + (0.1/(abs(slopep))^2) + 
#						(0.025/(abs(slopep))^3)))
#	scal1 <- 1/(1 - m1)
#	alp1 <- (((2^(1/m1)) - 1)^(1 - m1))/exp(ax)
#	pars <- c(Thr = Thr1, Ths = Ths1, alp = alp1, scal = scal1)
#	val <- c(pars[1], pars[2], pars[3], pars[4])
#	names(val) <- mCall[c("Thr", "Ths", "alp", "scal")]
#	val
#}