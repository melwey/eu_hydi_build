# cleanKatterer
# Adapts HYDI tables to comply to guidelines v1.1
#
# Author: M.Weynants
# Date created: 2012/12/05
####################################################################
print('Adapting to guidelines')
# GENERAL
# missing values have to be put to -999 and 'ND'
for (ifield in 1:length(general)){
if (all(is.na(general[[ifield]]))) {
	if (ifield %in% c(1,5:7,21,25,33,35,53:55,62)) general[[ifield]] <- -999
	else general[[ifield]] <- 'ND'
	}
}
general[,2] <- as.character(general[,2])
general[,3] <- as.character(general[,3])
names(general) <- gsub("SLOPE","SLOP",names(general))

# BASIC
for (ifield in 1:length(basic)){
if (all(is.na(basic[[ifield]]))) {
	if (ifield %in% c(6,9,12,15:17)) basic[[ifield]] <- 'ND'
	else basic[[ifield]] <- -999
	}
}
names(basic)[22:23] <- c('COARSE','COARSE_M')
basic[is.na(basic)] <- -999

# CHEMICAL
for (ifield in 1:length(chemical)){
if (all(is.na(chemical[[ifield]]))) {
	chemical[[ifield]] <- -999
	}
}
chemical[is.na(chemical)] <- -999

# PSIZE
psize$P_PERCENT <- as.numeric(psize$P_PERCENT)
# remove missing values (organic soils)
psize <- psize[!is.na(psize$P_PERCENT),]

# RET
ret[is.na(ret)] <- -999

# COND
# missing IND_VALUE and  VALUE!!!!
cond$IND_VALUE <- 1
cond$VALUE <- 0
cond[is.na(cond)] <- -999

# METHOD
# OK