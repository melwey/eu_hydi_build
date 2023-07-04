# HYDI2csv.R: export rdata to a set of csv files

# Author: M. Weynants
# Date created: 2023/07/02
######################################################################
load("../output/HYDI_SOURCE_nd_qa3.Rdata")

# export to csv
if (!dir.exists("../output/euhydi_csv")) {dir.create("../output/euhydi_csv")}
for (name in names(hydi)){
  write.csv(hydi[[name]], paste0("../output/euhydi_csv/", name, ".csv"))
}
