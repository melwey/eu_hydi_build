# strauss_corr
#
# Author: M.Weynants
# Datecreated: 2013/01/10
#####################################################################

# BASIC
# sample_dep_top
# basic1[basic1$SAMPLE_DEP_TOP >= basic1$SAMPLE_DEP_BOT,1:8]
# hor1_top
# basic1[basic1$HOR1_TOP >= basic1$HOR1_BOT,1:8]
basic1$HOR1_BOT[basic1$SAMPLE_ID == 4007667303] <- -999
# for PROFILE_ID == 40080253, sample and hor seem to be inverted
ind <- basic1$SAMPLE_DEP_TOP < basic1$HOR1_TOP
tmp <- basic1[ind,4:5]
basic1[ind,4:5] <- basic1[ind,7:8]
basic1[ind,7:8] <- tmp
basic1[basic1$SAMPLE_ID==4008025304,'HOR1_BOT'] <- -999
# correction of BD for sample 407156502 according to contributor's email
basic1[basic1$SAMPLE_ID == 407156502,"BD"] <- 1.5

basic1 <- basic.checks(basic1,general1[[1]],meth1[[1]])

# RET: one sample_id not in BASIC
#ret1[!ret1$SAMPLE_ID %in% basic1$SAMPLE_ID,1:5]
# Profile 4074542 has sample 01:04 in all tables except in ret: 01,02,04,05. Impossible to know whether 05 is in reality 03
# plot(log10(ret1$HEAD[ret1$SAMPLE_ID==407454201]),ret1$THETA[ret1$SAMPLE_ID==407454201],ylim=c(0,0.5))
# points(log10(ret1$HEAD[ret1$SAMPLE_ID==407454202]),ret1$THETA[ret1$SAMPLE_ID==407454202],col='red')
# points(log10(ret1$HEAD[ret1$SAMPLE_ID==407454204]),ret1$THETA[ret1$SAMPLE_ID==407454204],col='blue')
# points(log10(ret1$HEAD[ret1$SAMPLE_ID==407454205]),ret1$THETA[ret1$SAMPLE_ID==407454205],col='green')
# the curve is very different from the others: so remove it
ret1 <- ret1[ret1$SAMPLE_ID %in% basic1$SAMPLE_ID,]

# correction according to contributor's email:
# 15/02/2013
ret1$THETA[ret1$SAMPLE_ID == 407617604 & ret1$HEAD==2.5] <- 0.528
ret1$THETA[ret1$SAMPLE_ID == 406910801 & ret1$HEAD== 60] <- 0.551
# 20/02/2013
ret1$THETA[ret1$SAMPLE_ID == 406910801 & ret1$HEAD== 3000] <- 0.0339
ret1$THETA[ret1$SAMPLE_ID == 406040301 & ret1$HEAD== 100] <- 0.482


ret1 <- ret.checks(ret1,basic1[[2]], meth1[[1]])
cond1 <- cond.checks(cond,basic1[[2]], meth1[[1]])

# the rest is OK!
