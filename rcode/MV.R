# Mualem-van Genuchten model describing the soil moisture retention and hydraulic 
# conductivity curves
#
# Usage
# MV(par,h=10^(seq(0,5,by=0.05)),theta=NULL)
#
# Arguments
# par	a list of parameters: thr, ths, alp, n, m, Ks, L
# h		a vector of suction head values (cm water column)
# theta	a vector of volumetric water content values (cm3/cm3)
#
# Value
# A data frame with columns h, theta, K and Se
# 
# Author: M.Weynants
# reference: van Genuchten (1980)
######################################################################
MV <- function(par,h=NULL,theta=NULL){
	if (is.list(par)){par<-unlist(par)}
	if (is.matrix(par)){
		if (ncol(par)<5){
			par[,5] <- 1-1/par[,4]
		}
		if (ncol(par)<6){
			par[,6:7]<-NA
		}
	} else {
	if (length(par)<5){
		par[5] <- 1-1/par[4]
	}
	if (length(par)<6){
		par[6:7]<-NA
	}
	}
	thr<-par[1];ths<-par[2];alp<-par[3];n<-par[4];m<-par[5];Ks<-par[6];L<-par[7]
	if (is.null(theta)){
		Se <- (1 + (alp * h)^n)^(-m)
		theta <- thr + (ths-thr) * Se
	} else {
		Se <- (theta - thr)/(ths-thr)
		h <- 1/alp * (1 + Se^(-n/(n-1)))^(1/n)
	}
	K <- Ks * Se^L * (1-(1-Se^(1/m))^m)^2
	output <- data.frame(h=h,theta=theta,Se=Se,K=K)
}



