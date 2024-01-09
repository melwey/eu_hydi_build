# cleanLamorski
# 
# Author: M.Weynants
# Date created: 2012/12/19
#####################################################################
# TABLES in csv2
general <- read.csv2(file.path(path2data,dirs[i],'GENERAL.csv'),header=TRUE,as.is=TRUE,blank.lines.skip = TRUE)
basic <- read.csv2(file.path(path2data,dirs[i],'BASIC.csv'),header=TRUE,as.is=TRUE,blank.lines.skip = TRUE)
chemical <- read.csv2(file.path(path2data,dirs[i],'CHEMICAL.csv'),header=TRUE,as.is=TRUE,blank.lines.skip = TRUE)
psize <- read.csv2(file.path(path2data,dirs[i],'PSIZE.csv'),header=TRUE,as.is=TRUE,blank.lines.skip = TRUE)
ret <- read.csv2(file.path(path2data,dirs[i],'RET.csv'),header=TRUE,as.is=TRUE,blank.lines.skip = TRUE)
cond <- read.csv2(file.path(path2data,dirs[i],'COND.csv'),header=TRUE,as.is=TRUE,blank.lines.skip = TRUE)
meth <- read.csv2(file.path(path2data,dirs[i],'METHOD.csv'),header=TRUE,as.is=TRUE,blank.lines.skip = TRUE)

# GENERAL
general0 <- general
general$LOC_COOR_X <- as.numeric(gsub(' ','',general0$LOC_COOR_X))
general$LOC_COOR_Y <- as.numeric(gsub(' ','',general0$LOC_COOR_Y))

general$X_WGS84 <- as.numeric(general0$X_WGS84)
general$Y_WGS84 <- as.numeric(general0$Y_WGS84)

# METHOD

# BASIC
basic0<- basic
basic$BD <- as.numeric(basic0$BD)
basic$COARSE <- as.numeric(basic0$COARSE)

# CHEMICAL
chemical0 <- chemical
chemical$OC <- as.numeric(chemical0$OC)
names(chemical)[9:10] <- toupper(names(chemical0)[9:10])

# PSIZE
psize0 <- psize
psize$P_SIZE <- as.numeric(gsub(' ','',psize0$P_SIZE))
psize$P_PERCENT <- as.numeric(psize0$P_PERCENT)

# RET
ret0<-ret
ret$HEAD <- as.numeric(gsub(' ','',ret0$HEAD))
ret$THETA <- as.numeric(ret0$THETA)

# COND
cond0 <- cond
cond$VALUE <- as.numeric(gsub(' ','',cond0$VALUE))
cond$COND <- as.numeric(gsub(' ','',cond0$COND))
