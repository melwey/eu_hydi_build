require(R.matlab)
# MRCcat from M.Weynants PhD
data<-readMat("../Javaux/MRCcat_filtre1.mat")
# rearrange Matlab structure into R one...
L <- length(data$MRCcat)
n <- L/13
MRC <- list()
for (isample in 1:n){
MRC[[i]] <- data$MRCcat[[1]][((isample-1)*13+2):((isample-1)*13+13)]
}

attr(data$MRCcat,"dimnames")[[1]]

# no Ks... :-(

# install.packages("R.utils")
data <- readMat("../Javaux/StructData.mat")

attr(data$StructData,"dimnames")[[1]]

# still no Ksat...

