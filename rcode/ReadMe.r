# Creating, harmonizing EU-HYDI
# The following scripts have to be executed in the following order.
# Needed packages are:
library(RODBC) # to import hypres and export hydi to mdb
library(tidyverse)
library(sf)
library(optimx)
# Other needed functions are:

# set path: set current directory to eu_hydi_build/rcode
setwd("./rcode")

# create output and figs directories
if (!dir.exists("../output")) {
    dir.create("../output")
    }
if (!dir.exists("../fig")) {
    dir.create("../fig")
    }

# Execute scripts

# generates hypres_hydi.rdata
source("hypres2hydi.r")

# generates HYDI_SOURCE_nd.rdata
source("CreateHYDI.r")#

# generates HYDI_SOURCE_nd_add.rdata
source("add2hydi.r")

# generates HYDI_SOURCE_nd_qa2.Rdata
source("QA_corr.r")

# generates HYDI_SOURCE_nd_qa3.Rdata
source("QA_meth.r")

# generates set of csv files
source("HYDI2csv.r")

# generates EU-HYDI_v1.mdb/accdb
source("HYDI2mdb.r")

# filters RET and COND data and fit MVG sequentially
source("mvg_fit.r")

# correct Cornelis coords
source("hydi_corr.r")

# Basic statistics on EU-HYDI
source("hydiExplore.r")

# fit MVG coupled.
source("mvg_fit_coupled.r")