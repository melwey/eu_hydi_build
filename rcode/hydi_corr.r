# EU-HYDI: errata corrections
load("HYDI_SOURCE_nd_qa3.Rdata")

# geographic coordinates from Wim Cornelis are wrong. Grades and minutes should be transformed into decimal grades.
# select data from Cornelis in table GENERAL
pid <- hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE == "Cornelis"]
hydi$GENERAL[hydi$GENERAL$PROFILE_ID %in% pid,1:8]
ind <- hydi$GENERAL$PROFILE_ID >= 5600103 & hydi$GENERAL$PROFILE_ID <= 5600120

tmp <- as.character(format(hydi$GENERAL$X_WGS84[ind],digit = 3))
x <- as.numeric(substr(tmp,start = 1,stop = 1)) + round(as.numeric( substr(tmp, start = 2, stop = 4)) /0.6, digit = 2)
hydi$GENERAL$X_WGS84[ind] <- x

tmp <- as.character(format(hydi$GENERAL$Y_WGS84[ind],digit = 4))
y <- as.numeric(substr(tmp,start = 1,stop = 2)) + round(as.numeric( substr(tmp, start = 3, stop = 5)) /0.6, digit = 2)
hydi$GENERAL$Y_WGS84[ind] <- y

hydi$GENERAL$COMMENTS2[ind] <- "geog. coord. corrected by M. Weynants"

# save new hydi
save("hydi", file = "EUHYDI_v1_1.Rdata")

# missing to NA
for (i in 1:length(hydi)){
  hydi[[i]][hydi[[i]] == -999 | hydi[[i]] == "ND"] <- NA
  }
# save hydi.na
hydi.na <- hydi
save("hydi.na", file = "EUHYDI_NA_v1_1.Rdata")

# create shapefile with GENERAL
require(rgdal); require(sp)
hydi.sp <- hydi.na$GENERAL[!is.na(hydi.na$GENERAL$X_WGS84),]
coordinates(hydi.sp)=~X_WGS84+Y_WGS84
# project with rgdal
wgs84 <- CRS("+proj=longlat +datum=WGS84")
etrs89_laea <- CRS("+init=epsg:3035 +towgs84=0,0,0")
proj4string(hydi.sp) <- wgs84
hydi.laea <- spTransform(hydi.sp,etrs89_laea)
library(maptools)
writeSpatialShape(hydi.laea,"./gis/hydi_v1_1_LAEA.shp")
