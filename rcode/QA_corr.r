# General corrections following quality assessment
# 
# Author: M. Weynants
# Date created: 2013/02/13
####################################################################
load("../output/HYDI_SOURCE_nd_add.Rdata") # HYDI_SOURCE_nd + Cranfield + Morari(add2hydi.R)

# GENERAL
# WGS: 2 decimals for 1 km degradation
# transform local coordinates into wgs84
unique(hydi$GENERAL[hydi$GENERAL$LOC_COOR_X !=-999 & hydi$GENERAL$X_WGS == -999,c("SOURCE","LOC_COOR_SYST")])
unique(hydi$GENERAL[hydi$GENERAL$X_WGS!=-999,c("SOURCE","LOC_COOR_SYST")])

# require("sp")
# require("rgdal")
# require("maptools")
# packages no longer maintained after 2023
# use sf instead
library(sf)
require("maps")
library(rnaturalearth)
library(rnaturalearthdata)
require(foreign)

loc2wgs <- function(ind, loc_crs, country, cols = 2:3){
  coord <- cbind(x = as.numeric(hydi$GENERAL[ind,cols[1]]), y = as.numeric(hydi$GENERAL[ind,cols[2]]))
  print(head(coord))
  d <- hydi$GENERAL[ind,][!is.na(coord[,1]) & !is.na(coord[,2]) & coord[,1] != -999,]
  source_loc <- st_as_sf(d, crs = loc_crs, coords = cols)
  # sfcountry = st_as_sf(map("world", regions = country, plot=FALSE, fill = TRUE), crs = loc_crs)
  sfcountry <- ne_countries(scale = 10, country = country, returnclass = "sf")
  f1 <- ggplot2::ggplot() +
    geom_sf(data = sfcountry) +
    geom_sf(data = source_loc) +
    coord_map(loc_crs) +
    ggtitle(loc_crs$input)
  source_wgs <- st_transform(source_loc, st_crs(4326))
  f2 <- ggplot() +
    geom_sf(data = sfcountry) +
    geom_sf(data = source_wgs) +
    ggtitle("EPSG:4326")
  coord[!is.na(coord[,1]) & !is.na(coord[,2]) & coord[,1] != -999,] <- st_coordinates(source_wgs)
  print(head(coord))
  return(list(coord = coord, fig1 = f1,fig2 = f2))
}

print("Houskova")
ind <- hydi$GENERAL$SOURCE=="Houskova" & grepl("JTSK recalculated to WGS",hydi$GENERAL$LOC_COOR_SYST, fixed = TRUE)
hydi$GENERAL$LOC_COOR_SYST[ind] <- "JTSK recalculated to WGS4"
print("Houskova_HYPRES")
ind <- hydi$GENERAL$SOURCE=="Houskova_HYPRES" & hydi$GENERAL$LOC_COOR_Y != "ND"
dms2wgs<-function(a){if (length(a)==3){b=as.numeric(a[[1]]) + as.numeric(a[[2]])/60 + as.numeric(a[[3]])/3600};if (length(a)==2){b=as.numeric(a[[1]]) + as.numeric(a[[2]])/60}; if (length(a)==1){b=as.numeric(a[[1]])};b}
x <- as.vector(sapply(strsplit(hydi$GENERAL$LOC_COOR_Y[ind],"/"),dms2wgs))
y <- as.vector(sapply(strsplit(hydi$GENERAL$LOC_COOR_X[ind],"/"),dms2wgs))
wgs84<-st_crs(4326) #CRS("+proj=latlon +datum=WGS84")
map("world",xlim=c(-12,30),ylim=c(35,60))
# replace sp::SpatialPoints by 
# points(SpatialPoints(cbind(x,y),proj4string=wgs84),col="blue")
points(st_multipoint(cbind(x,y), wgs84),col="blue")
hydi$GENERAL[ind,c(4:6)] <- data.frame(c1="DMS to decimal degree",c2=x,c3=y)

print("Anaya")
# ED_1950_UTM_Zone_30N. GCS_European_1950
#PROJCS["ED_1950_UTM_Zone_30N",GEOGCS["GCS_European_1950",DATUM["D_European_1950",SPHEROID["International_1924",6378388.0,297.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-3.0],PARAMETER["Scale_Factor",0.9996],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0],AUTHORITY["EPSG",23030]]
# PROJCS["ED_1950_UTM_Zone_30N",GEOGCS["GCS_European_1950",DATUM["D_European_1950",SPHEROID["International_1924",6378388.0,297.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-3.0],PARAMETER["Scale_Factor",0.9996],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]
#  Code:EPSG::1633; Name:ED50 to WGS 84 (28) 
#  Code:EPSG::1133; Name:ED50 to WGS 84 (1) 
# 2023/04/20: replace sp::readShapePoints by sf::st_read
shp <- st_read(file.path(path2data,"Anaya/Andalusia_Localization sample points/Andalusia_Localization sample points/Puntos_Andalucia.shp"))
c<-st_coordinates(shp)
pid <- shp$PROFILE_ID

ind <- hydi$GENERAL$SOURCE=="Anaya"
# something's wrong. Either false northing or latitude of origin
utm30N <- st_crs(23030) #CRS("+init=epsg:23030 +towgs84=-131,-100.3,-163.4,-1.244,-0.02,-1.144,9.39")
# utm30N <- st_crs("+init=epsg:23030 +towgs84=-87,-98,-121")

hydi$GENERAL[ind,c("LOC_COOR_X","LOC_COOR_Y")] <- coord <- c[match(hydi$GENERAL[ind,"PROFILE_ID"],pid),]
rownames(coord)<-NULL
#coord <-cbind(as.numeric(substr(hydi$GENERAL[ind,"LOC_COOR_X"],start=2,stop=8)),as.numeric(substr(hydi$GENERAL[ind,"LOC_COOR_Y"],start=2,stop=7)))
# anaya_shp <- SpatialPointsDataFrame(coord[!is.na(coord[,1]) & !is.na(coord[,2]),],data=hydi$GENERAL[ind,][!is.na(coord[,1]) & !is.na(coord[,2]),],proj4string=utm30N)
d <- hydi$GENERAL[ind,][!is.na(coord[,1]) & !is.na(coord[,2]),]
anaya_shp <- st_as_sf(d, crs = utm30N, coords = c("LOC_COOR_X","LOC_COOR_Y"))
Spain = st_as_sf(map("world", regions = "Spain", plot=FALSE, fill = TRUE), crs = utm30N)
ggplot2::ggplot() +
  geom_sf(data = Spain) +
  geom_sf(data = anaya_shp, aes(fill = PROFILE_ID))
st_write(anaya_shp,dsn="../gis/anaya.geojson", append=FALSE)
anaya_wgs <- st_transform(anaya_shp,st_crs(4326))
ggplot() +
  geom_sf(data = st_as_sf(map("world", regions = "Spain", plot=FALSE, fill = TRUE))) +
  geom_sf(data = anaya_wgs)
hydi$GENERAL[ind,5:6][!is.na(coord[,1]),] <- st_coordinates(anaya_wgs)

print("Cornelis")
# LAMBERT-BEER (what year? 1950 (epsg:21500), 1972 (epsg:31370), 2005 (epsg:3447), 2008 (epsg:3812)?)
ind <- hydi$GENERAL$SOURCE == "Cornelis"
# LBB72 <- CRS("+proj=lcc +ellps=intl +x_0=150000.01256 +y_0=5400088.4378 +lon_0=brussels +lat_0=90 +lat_1=49.8333339 +lat_2=51.16666723333333 +towgs84=-125.8,79.9,-100.5")
# LBB72 <- st_crs("+init=epsg:31370")
LBB72 <- st_crs(31370)
coord <- cbind(as.numeric(hydi$GENERAL[ind,2]),as.numeric(hydi$GENERAL[ind,3]))
d <- hydi$GENERAL[ind,][coord[,1]!=-999,]
cornelis_shp <- st_as_sf(d, crs = LBB72, coords = c("LOC_COOR_X","LOC_COOR_Y"))
# cornelis_shp <- SpatialPointsDataFrame(coord[coord[,1]!=-999,],data=hydi$GENERAL[ind,11:15][coord[,1]!=-999,],proj4string=LBB72)
be = st_as_sf(map("world", regions = "Belgium", plot=FALSE, fill = TRUE), crs = LBB72)
ggplot2::ggplot() +
  geom_sf(data = be) +
  geom_sf(data = cornelis_shp, aes(fill = PROFILE_ID))
cornelis_wgs <- st_transform(cornelis_shp,st_crs(4326))
# original wgs
cornelis_wgs1 <- st_as_sf(hydi$GENERAL %>% filter(SOURCE == "Cornelis" & X_WGS84 != -999), crs = st_crs(4326), coords = c("X_WGS84", "Y_WGS84"))
# add transformed lambert to wgs
hydi$GENERAL[ind,5:6][coord[,1]!=-999,]<- st_coordinates(cornelis_wgs)
# wgs geographic coordinates from https://doi.org/10.1016/j.still.2008.03.003 (Wim Cornelis) are wrong. Degrees and minutes should be transformed into decimal degrees
# select data from Cornelis in table GENERAL
pid <- hydi$GENERAL$PROFILE_ID[ind]
hydi$GENERAL[hydi$GENERAL$PROFILE_ID %in% pid, 1:8]
ind1 <- hydi$GENERAL$PROFILE_ID >= 5600103 & hydi$GENERAL$PROFILE_ID <= 5600120
tmp <- as.character(format(hydi$GENERAL$X_WGS84[ind1],digit = 3))
x <- as.numeric(substr(tmp,start = 1,stop = 1)) + round(as.numeric( substr(tmp, start = 2, stop = 4)) /0.6, digit = 2)
hydi$GENERAL$X_WGS84[ind1] <- x
tmp <- as.character(format(hydi$GENERAL$Y_WGS84[ind1],digit = 4))
y <- as.numeric(substr(tmp,start = 1,stop = 2)) + round(as.numeric( substr(tmp, start = 3, stop = 5)) /0.6, digit = 2)
hydi$GENERAL$Y_WGS84[ind1] <- y
hydi$GENERAL$COMMENTS2[ind1] <- "Approximate geog. coord. corrected by M. Weynants from Degree.Minute to digital degrees"
cornelis_wgs2 <- st_as_sf(hydi$GENERAL[ind1,], crs = st_crs(4326), coords = c("X_WGS84", "Y_WGS84"))
ggplot() +
  geom_sf(data = st_as_sf(map("world", regions = "Belgium", plot=FALSE, fill = TRUE))) +
  geom_sf(data = cornelis_wgs, colour = "yellow", shape = 'o') + 
  geom_sf(data = cornelis_wgs1, colour = "cyan", shape = 'o') +
  geom_sf(data = cornelis_wgs2, colour = "green", shape = 'o') + 
  geom_sf(data = st_as_sf(hydi$GENERAL %>% filter(SOURCE == "Cornelis" & X_WGS84 != -999), crs = st_crs(4326), coords = c("X_WGS84", "Y_WGS84")), colour = "black", shape = '-')

# Wosten_HYPRES
# -999

# Hennings_HYPRES
# Gauss-Kruger: some points left (no GRIDREF in HYPRES)

# Schindler_HYPRES
# Krassowsi-Ellipsoid
# -999

print("Schindler")
# have X and Y reversed
ind <- hydi$GENERAL$SOURCE=="Schindler"
hydi$GENERAL[ind,5:6] <- hydi$GENERAL[ind,c(6,5)]

print("Iovino")
# UTM 33S (why S???)
#?PROJCS["ED_1950_UTM_Zone_33N",GEOGCS["GCS_European_1950",DATUM["D_European_1950",SPHEROID["International_1924",6378388.0,297.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",15.0],PARAMETER["Scale_Factor",0.9996],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0],AUTHORITY["EPSG",23033]]
# transformation epsg:1143 (ED50 to WGS84 (11))
UTM33S <-  st_crs("+init=epsg:23033 +towgs84=-97,-88,-135") # st_crs(23033) #
ind <- hydi$GENERAL$SOURCE == "Iovino"
# # 2023 update
# coord <- cbind(as.numeric(hydi$GENERAL[ind,2]),as.numeric(hydi$GENERAL[ind,3]))
# iovino_sp <- SpatialPoints(coord[coord[,1]!=-999,],proj4string=UTM33S)
# # transform
# iovino_wgs84 <- spTransform(iovino_sp,wgs84)
# # writePointsShape(iovino_wgs84,fn="../gis/iovino_wgs84.shp")
# hydi$GENERAL[ind,5:6][coord[,1]!=-999,]<- coordinates(iovino_wgs84)
f <- loc2wgs(ind, loc_crs = UTM33S, "Italy")
f[["fig2"]]
hydi$GENERAL[ind, 5:6] <- f[["coord"]]

print("Katterer")
# SWEREF 99 TM
# PROJCS["SWEREF99_TM",GEOGCS["GCS_SWEREF99",DATUM["D_SWEREF99",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",15.0],PARAMETER["Scale_Factor",0.9996],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0],AUTHORITY["EPSG",3006]]
sweref99 <- st_crs(3006)
ind <- hydi$GENERAL$SOURCE == "Katterer"
# # X and Y reversed
# coord <- cbind(as.numeric(hydi$GENERAL[ind,3]),as.numeric(hydi$GENERAL[ind,2]))
# katterer_sp <- SpatialPoints(coord[coord[,1]!=-999,],proj4string=sweref99)
# # transform
# katterer_wgs84 <- spTransform(katterer_sp,wgs84)
# writePointsShape(katterer_wgs84,fn="../gis/katterer_wgs84")
# hydi$GENERAL[ind,5:6][coord[,1]!=-999,]<- coordinates(katterer_wgs84)
f <- loc2wgs(ind, loc_crs = sweref99, "Sweden", cols = 3:2)
f[["fig2"]]
hydi$GENERAL[ind, 5:6] <- f[["coord"]]

print("Kvaerno")
ind <- hydi$GENERAL$SOURCE=="Kvaerno" & hydi$GENERAL$X_WGS84==-999
table(hydi$GENERAL$LOC_COOR_SYST[ind])
# UTM32  ??? what datum? Somehow now they already have wgs
# NGOc (what zone?)
# NGO (what zone?)
# 1914 III ????

# ind <- hydi$GENERAL$SOURCE=="Kvaerno" & hydi$GENERAL$LOC_COOR_SYST=="UTM32" & hydi$GENERAL$X_WGS84==-999
# utm32_ed50 <- st_crs(23032) # st_crs("+init=epsg:23032 +towgs84=-87,-95,-120")
# utm32_grs <- st_crs(25832) # st_crs("+init=epsg:25832")
# utm32_wgs <- st_crs(32632) # st_crs("+init=epsg:32632")
# # choosing one datum or the other there is a shift of about 215 m between points. But I have no way to know which is correct... (samples taken in the 90's)
# ##
# # coord <- cbind(as.numeric(hydi$GENERAL[ind,2]),as.numeric(hydi$GENERAL[ind,3]))
# # 
# # norway_utm32_sp <- SpatialPoints(coord[coord[,1]!=-999,],proj4string=utm32_ed50)
# # #writePointsShape(norway_utm32_sp,fn="../gis/norway_utm32")
# # # transform
# # norway_wgs84 <- spTransform(norway_utm32_sp,wgs84)
# # writePointsShape(norway_wgs84,fn="../gis/norway_wgs84_1")
# # hydi$GENERAL[ind,5:6]<- coordinates(norway_wgs84)
# ## 
# f <- loc2wgs(ind, loc_crs = utm32_ed50, "Norway") # "Norway(?!:Svalbard)"
# f[["fig2"]]
# hydi$GENERAL[ind, 5:6] <- f[["coord"]]


ind <- hydi$GENERAL$SOURCE=="Kvaerno" & hydi$GENERAL$LOC_COOR_SYST=="NGO" & hydi$GENERAL$X_WGS84==-999
table(hydi$GENERAL$LOC_COOR_SYST[ind])

ngoI <- st_crs(27391) # st_crs("+init=epsg:27391 towgs84=278.3,93,474.5,7.889,0.05,-6.61,6.21")
ngoII <- st_crs("+init=epsg:27392")
ngoIII <- st_crs("+init=epsg:27393")
ngoIV <- st_crs("+init=epsg:27394")
ngoV <- st_crs("+init=epsg:27395")
ngoVI <- st_crs("+init=epsg:27396")
ngoVII <- st_crs("+init=epsg:27397")
ngoVIII <- st_crs("+init=epsg:27398 ")

# coord <- cbind(as.numeric(hydi$GENERAL[ind,2]),as.numeric(hydi$GENERAL[ind,3]))

# can't find the right projection...
# norway_ngo_sp <- SpatialPoints(coord[coord[,1]!=-999,],proj4string=ngoI)
# writePointsShape(norway_ngo_sp,fn="../gis/norway_ngo1")
# norway_wgs84 <- spTransform(norway_ngo_sp,wgs84)
# writePointsShape(norway_wgs84,fn="../gis/norway_wgs84_2")
# #hydi$GENERAL[ind,5:6]<- coordinates(norway_wgs84)
# 
# fI <- loc2wgs(ind, loc_crs = ngoI, "Norway(?!:Svalbard)")
# fI[[2]]
# fII <- loc2wgs(ind, loc_crs = ngoII, "Norway(?!:Svalbard)")
# fII[[2]]
# fIII <- loc2wgs(ind, loc_crs = ngoIII, "Norway(?!:Svalbard)")
# fIII[[2]]
# fIV <- loc2wgs(ind, loc_crs = ngoIV, "Norway(?!:Svalbard)")
# fIV[[2]]
# fV <- loc2wgs(ind, loc_crs = ngoV, "Norway(?!:Svalbard)")
# fV[[2]]
# fVI <- loc2wgs(ind, loc_crs = ngoVI, "Norway(?!:Svalbard)")
# fVI[[2]]
# fVII <- loc2wgs(ind, loc_crs = ngoVII, "Norway(?!:Svalbard)")
# fVII[[2]]
# fVIII <- loc2wgs(ind, loc_crs = ngoVIII, "Norway(?!:Svalbard)")
# fVIII[[2]]

# can't find the right projection...


print("Matula")
# S-JTSK / Krovak East North (epsg::5514); towgs84 epsg::15965

# jtsk <- CRS("+proj=krovak +ellps=bessel +pm=greenwich +lat_0=49.5  +lon_0=24.833333 +k_0=0.9999 +x_0=0 +y_0=0 +unit=m +towgs84=589,76,480")
ind <- hydi$GENERAL$SOURCE=="Matula" & hydi$GENERAL$X_WGS84==-999
table(hydi$GENERAL$LOC_COOR_SYST[ind])
# coord <- cbind(as.numeric(hydi$GENERAL[ind,2]),as.numeric(hydi$GENERAL[ind,3]))
# czech_sp <- SpatialPoints(coord,proj4string=jtsk)
# czech_wgs <- spTransform(czech_sp,wgs84)
# writePointsShape(czech_sp,fn="../gis/czech_jtsk")
# writePointsShape(czech_wgs,fn="../gis/czech_wgs")
# hydi$GENERAL[ind,5:6]<- coordinates(czech_wgs)


# check coordinates in box
# hydi.wgs <- SpatialPointsDataFrame(hydi$GENERAL[hydi$GENERAL[,5]!=-999,5:6],data=hydi$GENERAL[hydi$GENERAL[,5]!=-999,c("PROFILE_ID","SOURCE")],proj4string=wgs84)
hydi.wgs <-  st_as_sf(hydi$GENERAL[hydi$GENERAL[,5]!=-999,c("PROFILE_ID","SOURCE","X_WGS84", "Y_WGS84")], crs = 4326, coords = c("X_WGS84", "Y_WGS84"))
Europe <- ne_countries(scale = "medium", continent = "europe", returnclass = "sf")
# sfeurope = st_as_sf(map("world", plot=FALSE, fill = TRUE), crs = 4326)
eu_box = c(xmin = -25, xmax = 50, ymin = 23, ymax = 70)
europe  <- st_cast(Europe, 'MULTILINESTRING') %>%
  st_cast('LINESTRING', do_split=TRUE) %>%
  mutate(npts = mapview::npts(geometry, by_feature = TRUE)) %>%
  st_cast('POLYGON')
eu_crop = st_crop(europe, st_bbox(eu_box))

f1 <- ggplot2::ggplot() +
  geom_sf(data = eu_crop) +
  geom_sf(data = hydi.wgs, aes(colour = factor(SOURCE))) +
  # coord_sf(xlim = c(-11,45), ylim = c(35,72), lims_method = "box") +
  coord_sf(crs = 3035) +
  ggtitle("EU-HYDI locations") 
f1
ggsave("../fig/hydi.png", width = 14, height = 7, units = "in")
# map("world",xlim=c(-12,50),ylim=c(34,75))
# points(hydi.wgs,col="red",pch=20)
# writePointsShape(hydi.wgs,"../gis/hydi_wgs84")

# CAUTION catch duplicated coordinates example in Norway 3267:3274
pid <- hydi$GENERAL$PROFILE_ID[hydi$GENERAL$X_WGS84!=-999]
coor.dup <- hydi$GENERAL[duplicated(hydi$GENERAL[,5:6]),5:6]
dup <- hydi$GENERAL[hydi$GENERAL$X_WGS84 %in% coor.dup[,1] & hydi$GENERAL$Y_WGS84 %in% coor.dup[,2] & hydi$GENERAL$X_WGS84 !=-999,]
dup <- dup[do.call(order,dup[,5:6]),]
#print(dup[,c("PROFILE_ID","LOC_COOR_X","LOC_COOR_Y","X_WGS84","Y_WGS84","SOURCE","PAR_MAT","WRB2006_RSG","YEAR","MONTH","DAY")])

# PAR_MAT: 0 -> -999
hydi$GENERAL$PAR_MAT[hydi$GENERAL$PAR_MAT==0] <- -999

# RC : missing for HYPRES data
unique(hydi$GENERAL$SOURCE[hydi$GENERAL$RC_L1=="ND"])
# LC/LU
# LC_L1 to LC_L3
# codes from levels 1, 2 and 3 are mixed
s <- hydi$GENERAL$LC_L1
s2 <- substr(s,start=2,stop=2)
s3 <- substr(s,start=3,stop=3)
hydi$GENERAL$LC_L3[!grepl('0',s3)] <- s[!grepl('0',s3)]
hydi$GENERAL$LC_L2[!grepl("0",s2)] <- paste(substr(s[!grepl("0",s2)],start=1,stop=2),'0',sep='')
hydi$GENERAL$LC_L1 <- paste(substr(s,start=1,stop=1),'00',sep='')
hydi$GENERAL$LC_L1[grepl('ND',s)] <- 'ND'
s <- hydi$GENERAL$LC_L2
s3 <- substr(s,start=3,stop=3)
hydi$GENERAL$LC_L3[!grepl('0',s3)] <- s[!grepl('0',s3)]
hydi$GENERAL$LC_L2[!grepl("0",s3)] <- paste(substr(s[!grepl("0",s3)],start=1,stop=2),'0',sep='')
hydi$GENERAL$LC_L2[grepl('ND',s)] <- 'ND'
hydi$GENERAL$LC_L3[grepl('0',hydi$GENERAL$LC_L3)] <- 'ND'
# table(hydi$GENERAL$LC_L1)
# LU_L1 to LU_L2
s <- hydi$GENERAL$LU_L1
s2 <- substr(s,start=4,stop=4)
hydi$GENERAL$LU_L2[!grepl('0',s2)] <- s[!grepl('0',s2)]
hydi$GENERAL$LU_L2[substr(hydi$GENERAL$LU_L2,start=4,stop=4)=="0"]<-"ND"
hydi$GENERAL$LU_L1[!grepl('0',s2)] <- paste(substr(s[!grepl('0',s2)],start=1,stop=3),"0",sep="")
hydi$GENERAL$LU_L1[grepl('ND',s)] <- 'ND'

# PUBL_REF: caution with doi and http
## grepl makes R crash!!!
hydi$GENERAL$PUBL_REF[grepl("Weynants",hydi$GENERAL$PUBL_REF, fixed = TRUE)] <- "Weynants. 2011. Linking soil hydraulic properties to structure indicators Experiments and modelling. PhD thesis, UCL, Belgium. http://hdl.handle.net/2078.1/75972"
hydi$GENERAL$PUBL_REF[grepl("doi:10.2136/sssaj2004",hydi$GENERAL$PUBL_REF, fixed = TRUE)] <- "doi:10.2136/sssaj2004.0238"
hydi$GENERAL$PUBL_REF[grepl("doi:10.1016/j.still.2008.03",hydi$GENERAL$PUBL_REF, fixed = TRUE)] <- "doi:10.1016/j.still.2008.03.003"
# 2023 add Schindler
fname = curl::curl_download(url = "https://open-research-data.zalf.de/ResearchDataSets/1977_164_1.zip", "~/Downloads/1977_164_1.zip") %>% unzip(exdir = "~/Downloads")
tmp <- XML::xmlToDataFrame(fname)
tmp_sf1 <- st_as_sf(tmp%>% dplyr::filter(ZONE == 33), coords = c("X_ETRS89", "Y_ETRS89"), crs = 25833)
tmp_sf2 <- st_as_sf(tmp%>% dplyr::filter(ZONE == 32), coords = c("X_ETRS89", "Y_ETRS89"), crs = 25832)
sfgermany = st_as_sf(map("world", region = "Germany", plot=FALSE, fill = TRUE), crs = 4326)
f1 <- ggplot2::ggplot() +
  geom_sf(data = sfgermany) +
  geom_sf(data = tmp_sf1, aes(colour = ZONE), shape = 4) +
  geom_sf(data = tmp_sf2, aes(colour = ZONE),  shape = 4) +
  geom_sf(data = hydi$GENERAL %>% dplyr::filter(grepl("Schindler", SOURCE) & X_WGS84 > -999) %>% st_as_sf(coords = c("X_WGS84", "Y_WGS84"), crs = 4326),
          aes(colour = SOURCE), shape = 1)
f1
# geog locations from Schindler_HYPRES do not completely correspond with published data (they're supposed to be the same however)
hydi$GENERAL$PUBL_REF[grepl("Schindler_HYPRES",hydi$GENERAL$SOURCE)] <- "doi:10.5194/essd-2-189-2010"
# data from Schindler included in EU-HYDI HAVE NOT been published elsewhere it seems

# recoding by country?

print("CONTACT_A")
#unique(hydi$GENERAL$SOURCE[hydi$GENERAL$CONTACT_A=="ND"])
df <- data.frame(source = c("Iovino","Katterer","Schindler","Shein","Javaux","Daroussin","Romano","Lamorski","Strauss","Cornelis", "Morari"),
                 address = c("Dipartimento dei Sistemi Agro-Ambientali, Università degli Studi di Palermo",
                             "Swedish University of Agricultural Sciences",
                             "ZALF",
                             "Moscow State University",
                             "Earth and Life Institute, Université catholique de Louvain",
                             "INRA, Orléans, France",
                             "University of Naples Federico II",
                             "Institute of Agrophysics, Polish Academy of Sciences, Lublin",
                             "Federal Agency for Water Management",
                             "Ghent University",
                             "Universitá di Padova"))
ad <- hydi$GENERAL$CONTACT_A
for (i in 1:nrow(df)){ad[hydi$GENERAL$SOURCE==df$source[i] & ad =="ND"] <- df$address[i]}
ad -> hydi$GENERAL$CONTACT_A

# EMAIL
hydi$GENERAL %>% dplyr::group_by(SOURCE, EMAIL) %>% dplyr::count() %>% View()
hydi$GENERAL$EMAIL[grepl("Houskova",hydi$GENERAL$SOURCE)] <- "b.houskova@vupop.sk"
# change contact for Javaux
hydi$GENERAL[hydi$GENERAL$SOURCE=="Javaux",c("EMAIL","CONTACT_P","CONTACT_A")] <- rep(c("Mathieu.Javaux@uclouvain.be","Mathieu Javaux","Earth and Life Institute, Université catholique de Louvain"), each=sum(hydi$GENERAL$SOURCE=="Javaux"))
# Dutch contact data
hydi$GENERAL[grepl("Wosten",hydi$GENERAL$SOURCE),c("EMAIL","CONTACT_P","CONTACT_A")] <- rep(c("Henk.Wosten@wur.nl","Henk Wosten","Wageningen University & Research"),each=sum(grepl("Wosten",hydi$GENERAL$SOURCE)))
# in Iovino, some EMAIL is "MAIL"
hydi$GENERAL$EMAIL[hydi$GENERAL$EMAIL=="MAIL"] <- "massimo.iovino@unipa.it"
hydi$GENERAL$EMAIL[grepl("Schindler",hydi$GENERAL$SOURCE)] <- "schind@zalf.de"
hydi$GENERAL$EMAIL[grepl("Romano",hydi$GENERAL$SOURCE)] <-"nunzio.romano@unina.it"
# hydi$GENERAL$CONTACT_A[grepl("^[[:digit:]]+$",hydi$GENERAL$CONTACT_A)] <- "ND"
# hydi$GENERAL$CONTACT_A[is.na(hydi$GENERAL$CONTACT_A)]<-"ND"

###### 2023/04/25 i am here
# REL_T_SER as text
hydi$GENERAL$REL_T_SER <- "ND"

print("BASIC")
# Maria da Conceicao comment:
# -	For STRUCTURE1 and STRUCTURE 2 some contributors define ?Massive? and ?Single Grain? structures as ?NDNDMA? and ?NDNDSG?, respectively. Others define it as ?    MA? and ?    SG?. What should the correct form be? ND stands for ?unknown? or ?not determined?. In those type of structures the grade and size class do not exist.
# -	Sample 560008201 (Cornelis data) has ?NDMAND? for STRUCTURE1. This is according to the guidelines. However, massive structure should define the type of structure (i.e., MA should be moved to the last two letter). All other contributors placed MA in the last two spaces.

# When MA or SG, use "    MA" and "    SG" respectively
hydi$BASIC$STRUCTURE1 <- gsub("NDNDMA","    MA",hydi$BASIC$STRUCTURE1, fixed = TRUE)
hydi$BASIC$STRUCTURE1 <- gsub("NDNDSG","    SG",hydi$BASIC$STRUCTURE1, fixed = TRUE)
hydi$BASIC$STRUCTURE1 <- gsub("NDMAND","    MA",hydi$BASIC$STRUCTURE1, fixed = TRUE)


# HOR1_NAME needs to be homogenized
table(nchar(hydi$BASIC$HOR1_NAME))
ind <- nchar(hydi$BASIC$HOR1_NAME)>7
tmp <- hydi$BASIC$HOR1_NAME[ind] 
hydi$BASIC$HOR1_NAME[ind] <- paste(" ",substr(tmp,3,4)," ",substr(tmp,5,5)," ",substr(tmp,6,6),sep="")

ind <- nchar(hydi$BASIC$HOR1_NAME)<7 & nchar(hydi$BASIC$HOR1_NAME)>2
tmp <- hydi$BASIC[ind,c("SAMPLE_ID","HOR1_NAME","SOURCE")] 
hydi$BASIC$HOR1_NAME[ind][c(3,4,7:10,13,15,18,19)] <- " B  w  "
hydi$BASIC$HOR1_NAME[ind][c(1,5,12,14,16,17)] <- " B  w 1"
hydi$BASIC$HOR1_NAME[ind][c(2,6)] <- " B  w 2"
hydi$BASIC$HOR1_NAME[ind][11] <- " B C   "

hydi$BASIC$HOR1_NAME[hydi$BASIC$HOR1_NAME=="' A p '"] <- " A  p  "

# first character
ind <- !substr(hydi$BASIC$HOR1_NAME,1,1) %in% c(' ',as.character(1:9),'b') & hydi$BASIC$HOR1_NAME!="ND"
tmp <- hydi$BASIC[ind,c("SAMPLE_ID","HOR1_NAME","SOURCE")]
hydi$BASIC$HOR1_NAME[ind] <- c(" W C   "," C     "," C     ","bC  rf ","bC  rf ")

# second character
mh <- c('H', 'O', 'A', 'E', 'B', 'C', 'R', 'I', 'L', 'W','D')
ind <- !substr(hydi$BASIC$HOR1_NAME,2,2) %in% mh
tmp <- hydi$BASIC[ind,c("SAMPLE_ID","HOR1_NAME","SOURCE")]
hydi$BASIC$HOR1_NAME[hydi$BASIC$SOURCE=="Anaya" & ind] <- "ND"
hydi$BASIC$HOR1_NAME[hydi$BASIC$SAMPLE_ID == 7248106406] <- "2C/Ca  "
hydi$BASIC$HOR1_NAME[hydi$BASIC$SAMPLE_ID == 3800045401] <- " A  p  "

# third character
ind <- !substr(hydi$BASIC$HOR1_NAME,3,3) %in% c('/',' ') & hydi$BASIC$HOR1_NAME!="ND"
tmp <- hydi$BASIC[ind,c("SAMPLE_ID","HOR1_NAME","SOURCE")]
hydi$BASIC$HOR1_NAME[ind] <- paste(substr(hydi$BASIC$HOR1_NAME[ind],1,2)," ",substr(hydi$BASIC$HOR1_NAME[ind],3,6),sep="")

# fourth character
ind <- !substr(hydi$BASIC$HOR1_NAME,4,4) %in% c(mh," ") &  hydi$BASIC$HOR1_NAME!="ND"
tmp <- hydi$BASIC[ind,c("SAMPLE_ID","HOR1_NAME","SOURCE")]
hydi$BASIC$HOR1_NAME[ind][c(3,4,6:9)] <- "2C  ca2"
hydi$BASIC$HOR1_NAME[ind][2] <- "2C  ca "
hydi$BASIC$HOR1_NAME[ind][5] <- " C  ca "

# fifth character: OK
# sixth
shd <- c(letters,'@',' ')
ind <- !substr(hydi$BASIC$HOR1_NAME,6,6) %in% shd &  hydi$BASIC$HOR1_NAME!="ND"
tmp <- hydi$BASIC[ind,c("SAMPLE_ID","HOR1_NAME","SOURCE")]
hydi$BASIC$HOR1_NAME[ind] <- " B/C  3"

#seventh: vertical subdivision
ind <- !substr(hydi$BASIC$HOR1_NAME,7,7) %in% c(1:9,' ') &  hydi$BASIC$HOR1_NAME!="ND"
tmp <- hydi$BASIC[ind,c("SAMPLE_ID","HOR1_NAME","SOURCE")]
hydi$BASIC$HOR1_NAME[ind] <- sub("0"," ",hydi$BASIC$HOR1_NAME[ind])
hydi$BASIC$HOR1_NAME[hydi$BASIC$SAMPLE_ID == 8040054704] <- " B  kg "
hydi$BASIC$HOR1_NAME[hydi$BASIC$SAMPLE_ID == 8040062202] <- " B  tk "
hydi$BASIC$HOR1_NAME[hydi$BASIC$SAMPLE_ID == 8040000301] <- " A  kp "

# assign method -999 when missing data
n <- names(hydi$BASIC)
met <- n[grepl("_M",n)]
for (i in 1:length(met)){
j <- which(n==met[i])
k <- which(n==strsplit(met[i],"_")[[1]][[1]])
hydi$BASIC[[j]][hydi$BASIC[[k]]==-999]<- -999
}

# BD_M for Schindler_HYPRES, same as for Schindler
#hydi$METHOD[(grepl("Schindler",hydi$METHOD$SOURCE) | hydi$METHOD$SOURCE=="HYPRES" )& hydi$METHOD$METH_PAR=="BD_M",]
hydi$BASIC$BD_M[hydi$BASIC$SOURCE=="Schindler_HYPRES" & hydi$BASIC$BD_M != -999] <- unique(hydi$BASIC[hydi$BASIC$SOURCE=="Schindler" & hydi$BASIC$BD!=-999,"BD_M"])

print("CHEMICAL")
# Correct SAMPLE_ID in SOURCE="Patyka"
hydi$CHEMICAL$SAMPLE_ID[hydi$CHEMICAL$SAMPLE_ID %in% c(8070047701:8070047706)] <- 8040047701:8040047706
# remove SAMLPE_ID not in BASIC
# hydi$CHEMICAL <- hydi$CHEMICAL[hydi$CHEMICAL$SAMPLE_ID %in% hydi$BASIC$SAMPLE_ID,]

# assign method -999 when missing data
n <- names(hydi$CHEMICAL)
met <- n[grepl("_M",n)]
for (i in 1:length(met)){
j <- which(n==met[i])
k <- which(n==strsplit(met[i],"_M$")[[1]][[1]])
hydi$CHEMICAL[[j]][hydi$CHEMICAL[[k]]==-999]<- -999
}

# OC_M for Schindler_HYPRES, same as for Schindler
hydi$METHOD[(grepl("Schindler",hydi$METHOD$SOURCE) | hydi$METHOD$SOURCE=="HYPRES" )& hydi$METHOD$METH_PAR=="OC_M",]
# hydi$CHEMICAL$OC_M[hydi$CHEMICAL$SOURCE=="Schindler_HYPRES" & hydi$CHEMICAL$OC_M!-999] <- unique(hydi$CHEMICAL[hydi$CHEMICAL$SOURCE=="Schindler" & hydi$CHEMICAL$OC!=-999,"OC_M"])

# round data
hydi$CHEMICAL[,c(3,5,7,9,11,13,15,17,19,21,23,25,26,28)] <- round(hydi$CHEMICAL[,c(3,5,7,9,11,13,15,17,19,21,23,25,26,28)],digits=2)

# BASE_CATIONS: check sums (Brigi's email 20/04/2013)
BC <- round(rowSums(hydi$CHEMICAL[,c("EX_MG","EX_K","EX_NA","EX_CA")]),digits=2)
D <- hydi$CHEMICAL[,"BASE_CATIONS"]-BC
ind <- abs(D)>=0.01 & BC>0 & hydi$CHEMICAL$BASE_CATIONS >0
cbind(hydi$CHEMICAL[ind,c("SAMPLE_ID","SOURCE","EX_MG","EX_K","EX_NA","EX_CA","BASE_CATIONS")],BC[ind],D[ind])
tag.C1 <- abs(D)>0.05 & BC>0 & hydi$CHEMICAL$BASE_CATIONS >0

hydi$CHEMICAL$BASE_CATIONS[ind] <- BC[ind]

# CEC >= BASE_CATIONS
ind <- hydi$CHEMICAL$CEC < hydi$CHEMICAL$BASE_CATIONS & hydi$CHEMICAL$CEC!=-999
hydi$CHEMICAL[ind,c("SAMPLE_ID","BASE_CATIONS","CEC")]
# I would leave them like that: measurement methods are different and hence can lead to different values.
# The user can create a CEC_cor if they wish.

print("PSIZE")
# do all modifications on a copy:
psize <- hydi$PSIZE

# PSIZE adjustements by Attila from "SCHINDLER PSIZE QUERY FEB 17.xlsx"
ps.new <- read.csv(paste0(path2data,"/Rcode/SCHINDLER_PSIZE_adj.csv"),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
# replace values
ind <- match(paste(psize$SAMPLE_ID,psize$P_SIZE),
paste(ps.new$SAMPLE_ID,ps.new$P_SIZE))
ind <- which(!is.na(ind))
# Check by merging that indices are correct
#tmp <- cbind(hydi$PSIZE[ind,2:4],ps.new[,1:3])#OK
psize$P_PERCENT[ind] <- ps.new$P_PERCENT
psize$SOURCE[ind] <- "Schindler_Attila"
# TAG:
PS_adj <- unique(psize$SAMPLE_ID[ind])

# Schindler_HYPRES change P_SIZE==6 <- 6.3
psize$P_SIZE[psize$P_SIZE == 6 & psize$SOURCE=="Schindler_HYPRES"] <- 6.3

# add rows in cumulative curves
add1 <- data.frame(PROFILE_ID=27600569,SAMPLE_ID=c(2760056901,2760056901,2760056902,2760056903),P_SIZE=c(630,2000,2000,2000),P_PERCENT=1,P_M=-999,ID=-999,SOURCE="Attila")
# add data:
add2 <- data.frame(PROFILE_ID=52800140,SAMPLE_ID=c(5280014001,5280014002,5280014003,5280014004,5280014005),P_SIZE=2,P_PERCENT=0.18,P_M=-999,ID=-999,SOURCE="Attila")
psize <- rbind(psize,add1,add2)
# sort
psize <- psize[do.call(order,psize[,2:3]),]
#dd[ do.call(order, dd) ,]

# correction to Norwegian sample
# SAMPLE_ID		PSIZE PCENT
# 5780037403	2000  6.9
# 5780037403	600	22.5
# 5780037403	200	20.6
# 5780037403	60	11.8
# 5780037403	20	11.8
# 5780037403	6	8.8
# 5780037403	2	17.6
psize[psize$SAMPLE_ID==5780037403,3:4] <- data.frame(psize=c(2000,600,200,60,20,6,2),percent=c(6.9,22.5,20.6,11.8,11.8,8.8,17.6))

# The loop below is not very efficient...
id <- unique(psize$SAMPLE_ID)
sum_p <- rep(NA,length(id))
for (i in 1:length(id)){
ind <- psize$SAMPLE_ID == id[i]
sum_p[i] <- sum(psize$P_PERCENT[ind])
# delete records for which all PSD is -999
if (all(psize$P_PERCENT[ind]==-999))
{psize <- psize[!ind,]}
# delete records with only one point
if(sum(ind)==1){psize <- psize[!ind,]}
}

# corrections according to Attila. see xlsx file.
# fractions: * 100
ind_frac <- psize$SAMPLE_ID %in% id[sum_p==1]
psize$P_PERCENT[ind_frac] <- psize$P_PERCENT[ind_frac] * 100
psize$SOURCE[ind_frac] <- paste(psize$SOURCE[ind_frac],"Attila_f",sep="_")
# fractions and cumulative: * 100 and uncumulate
id_fcum <- id[sum_p >1 & sum_p<8]
for (i in id_fcum){
ind <- psize$SAMPLE_ID == i
# uncumulate
o <- order(psize[ind,4])
pfc <- psize[ind,4][o]
tmp <- c(pfc[1],diff(pfc))*100
if (all(tmp>0)){
psize[ind,4][o] <- tmp
psize[ind,7] <- paste(psize[ind,7],"Attila_fcum",sep="_")
} else {psize[ind,7] <- paste(psize[ind,7],"Attila_nocorr",sep="_")}
}
# cumulative: no occurrence
# id_cum <- id[sum_p > 101]
# for (i in id_cum){
# ind <- psize$SAMPLE_ID == i
# pc <- psize[ind,4]
# # uncumulate
# pu <- c(pc[1],diff(pc))
# # check cumulative
# if (all(pu>0)){psize[ind,4] <- pu;psize[ind,7] <- paste(psize[ind,7],"Attila_cum",sep="_")}
# }

print(table(psize[grepl("Attila",psize$SOURCE),7]))
# check p_sum again
idn <- unique(psize$SAMPLE_ID)
sum_pn <- sapply(id,function(sid,psize){sum(psize$P_PERCENT[psize$SAMPLE_ID==sid])},psize)
save("idn","sum_pn",file="../output/psize_qa.rdata")

# send changes to Attila for approval
write.csv(psize[grepl("Attila",psize$SOURCE),],file="../output/psize_attila.csv")


# when OK replace table
psize -> hydi$PSIZE


# Corrections following Attila's email of 2013/04/23
# see xlsx file "PSD differences and other issues betwee EU-HYDI Feb11 and Apr17" 
ind<-hydi$PSIZE$SAMPLE_ID==2760052404
hydi$PSIZE[ind,c("P_PERCENT","SOURCE")] <- cbind(c(0.126,0.0805 # original 0.1939
,0.1053,0.1528,0.3071,0.1737,0.0546)*100,"Schindler_HYPRES_Attila_f6.3")

hydi$PSIZE$P_SIZE <- as.numeric(hydi$PSIZE$P_SIZE)
hydi$PSIZE$P_PERCENT <-as.numeric(hydi$PSIZE$P_PERCENT)

ind<-hydi$PSIZE$PROFILE_ID==27600524
# M <- matrix(hydi$PSIZE[ind,4],nrow=7)
# C <- sapply(1:ncol(M),function(x){cumsum(M[,x])})
# D <- cbind(hydi$PSIZE[ind,2:4],as.numeric(C))
# names(D)[4]<-"cum"
# require(ggplot2)
# p <- ggplot(D, aes(x=P_SIZE, y=cum, group=as.character(SAMPLE_ID)))
# p + geom_line(aes(colour = as.character(SAMPLE_ID))) + scale_colour_brewer(palette="Set1")
# correction of IDs
hydi$PSIZE$SAMPLE_ID[ind] <- 2760052400 + rep(c(5,1:4),each=7)
hydi$PSIZE$FLAG[ind]<-"ID modified by M.Weynants"

ind <- hydi$PSIZE$SAMPLE_ID == 8260004501 & hydi$PSIZE$SOURCE=="Lilly"
hydi$PSIZE <- hydi$PSIZE[!ind,]

hydi$PSIZE[hydi$PSIZE$SAMPLE_ID == 560008901 & hydi$PSIZE$P_PERCENT==10.3,c("P_SIZE","SOURCE")] <- c(50,"Cornelis_Attila")

hydi$PSIZE$SAMPLE_ID[hydi$PSIZE$SAMPLE_ID==3800001201 & hydi$PSIZE$P_SIZE==2000 & hydi$PSIZE$P_PERCENT==12.15] <- 3800001101

hydi$PSIZE$P_SIZE[hydi$PSIZE$SAMPLE_ID == 3800025001 & hydi$PSIZE$P_PERCENT==3.22] <- 1000

hydi$PSIZE <- hydi$PSIZE[! (hydi$PSIZE$SAMPLE_ID == 8040004802 & hydi$PSIZE$P_PERCENT==-999),]


print("RET")
# average duplicates?
# randomly pick data in evaporation method (by range)
# pF0 instead of HEAD=0
hydi$RET$HEAD[hydi$RET$SOURCE%in%c("Mako","Houskova","Lamorski","Shein") & hydi$RET$HEAD==1]<-0

# Tagging: RET$FLAG=1 for "trusted" measurements; 0 for not trusted (non monotonous decreasing points
hydi$RET$FLAG=TRUE
hydi$RET$FLAG[hydi$RET$SAMPLE_ID==5280003802 & hydi$RET$HEAD>250 & hydi$RET$THETA<0.010] <-FALSE
hydi$RET$FLAG[hydi$RET$SAMPLE_ID%in%c(2760059502,2760059503) & hydi$RET$HEAD==15000] <- FALSE
hydi$RET$FLAG[hydi$RET$SAMPLE_ID==560007401 & hydi$RET$HEAD>1000] <- FALSE
hydi$RET$FLAG[hydi$RET$SAMPLE_ID==8260005801 & hydi$RET$HEAD==330] <- FALSE

hydi$RET$FLAG[hydi$RET$THETA==0] <- FALSE
hydi$RET$FLAG[hydi$RET$THETA>=1] <- FALSE
# THETA=1: impossible: data are probably in Se
# SAMPLE_ID == 2760059418
# multiply by porosity: 1-2.65/0.11 ? 
# better remove full curve
hydi$RET$FLAG[hydi$RET$SAMPLE_ID == 2760059418] <- FALSE

# some sample have measurements out of the rest of the curve
# 560001902
hydi$RET$FLAG[hydi$RET$SAMPLE_ID==560001902 & hydi$RET$HEAD %in% c(200,340)] <- FALSE
# 2760058702 and 03
hydi$RET$FLAG[hydi$RET$SAMPLE_ID==2760058702 & hydi$RET$HEAD >250] <- FALSE
hydi$RET$FLAG[hydi$RET$SAMPLE_ID==2760058703 & hydi$RET$HEAD >250] <- FALSE

print("COND")
# average duplicates? (to avoid too much weight in the optimization)
# filter conductivity data larger than 10000?
d <- hydi$COND[hydi$COND$VALUE <= 1,]
boxplot(log10(COND) ~ SOURCE,data=d)

# IND_VALUE==0
sid <- hydi$COND$SAMPLE_ID[!hydi$COND$IND_VALUE]
#merge(hydi$COND[hydi$COND$SAMPLE_ID %in% sid & !hydi$COND$SAMPLE_ID %in% sid[duplicated(sid)],2:5],
#hydi$RET[hydi$RET$SAMPLE_ID %in% hydi$COND[hydi$COND$SAMPLE_ID %in% sid & !hydi$COND$SAMPLE_ID %in% sid[duplicated(sid)],2] & hydi$RET$HEAD<5,c(2:4,16)])
# in COND, change IND_VALUE to 1 and VALUE to RET$HEAD (0 for Romano's sample and 1 for Houskova's)
tmp <-hydi$RET[hydi$RET$SAMPLE_ID %in% hydi$COND[hydi$COND$SAMPLE_ID %in% sid & !hydi$COND$SAMPLE_ID %in% sid[duplicated(sid)],2] & hydi$RET$HEAD<5,c("SAMPLE_ID","HEAD","THETA")]
sid.cond <- hydi$COND[hydi$COND$SAMPLE_ID %in% sid & !hydi$COND$SAMPLE_ID %in% sid[duplicated(sid)],2]
hydi$COND[hydi$COND$SAMPLE_ID %in% sid & !hydi$COND$SAMPLE_ID %in% sid[duplicated(sid)],3:4] <- cbind(1,tmp[match(sid.cond,tmp[[1]]),2])
cbind(hydi$COND[hydi$COND$SAMPLE_ID %in% sid & !hydi$COND$SAMPLE_ID %in% sid[duplicated(sid)],2:4],tmp[match(sid.cond,tmp[[1]]),1:3])

#table(hydi$COND$SOURCE[hydi$COND$VALUE==0])
hydi$COND$VALUE[hydi$COND$SOURCE%in%c("Mako","Housova","Lamorski") & hydi$COND$VALUE==1]<-0


# First attempt towards a quality assessment indicator:
# Meet minimum requirements: add a column to BASIC (logical)
# for each BASIC$SAMPLE_ID, must have: GENERAL:coordinates; ISO_COUNTRY; RC_L1; RC_L2; CONTACT_P; EMAIL; BASIC: SAMPLE_DEP_TOP; SAMPLE_DEP_BOT; BD; BD_M; COARSE; COARSE_M; CHEMICAL: OC, OC_M; PSIZE: sum=100+/-1, P_M; RET: HEAD, THETA; MEHTOD: CODE_M from other tables

print("PSD_EST: HARMONIZED PSD")
PSD_EST <- read.csv(file.path(path2data, "../QAmeeting/PSIZE/PSIZE_2_50_2000_2013apr26.csv"), as.is=TRUE)
# Morari
morari <- read.csv(file.path(path2data,"../QAmeeting/PSIZE/PSIZE_2_50_2000_2013jun11.csv"),as.is=TRUE)
PSD_EST <- rbind(PSD_EST,morari)
# correction for Norway
# require(XLConnect)
# wb <- loadWorkbook(file.path(path2data,"../QAmeeting/PSIZE/ChangeEstimJune13.xlsx"),create=FALSE)
# newps <- readWorksheet(wb , "data to be updated")
# 2023/07/02 use csv instead of xlsx
newps <- read.csv(file.path(path2data,"../QAmeeting/PSIZE/ChangeEstimJune13_data2update.csv"), as.is = TRUE)
PSD_EST[match(newps$SAMPLE_ID,PSD_EST$SAMPLE_ID),c("USSILT","USSAND")] <- newps[,4:5]
# put in hydi
hydi$PSD_EST <- cbind(PROFILE_ID=floor(PSD_EST$SAMPLE_ID/100),PSD_EST[,1:8])

# clean DB: keep in GENERAL only records that are in BASIC
hydi$GENERAL <- hydi$GENERAL[hydi$GENERAL$PROFILE_ID %in% hydi$BASIC$PROFILE_ID,]
hydi$CHEMICAL <- hydi$CHEMICAL[hydi$CHEMICAL$SAMPLE_ID %in% hydi$BASIC$SAMPLE_ID,]
hydi$PSIZE <-  hydi$PSIZE[hydi$PSIZE$SAMPLE_ID %in% hydi$BASIC$SAMPLE_ID,]

print("map")
# hydi.wgs <- SpatialPointsDataFrame(hydi$GENERAL[hydi$GENERAL[,5]!=-999,5:6],data=hydi$GENERAL[hydi$GENERAL[,5]!=-999,c("PROFILE_ID","SOURCE")],proj4string=wgs84)
# writePointsShape(hydi.wgs,"../gis/hydi_wgs84")
ind <- hydi$GENERAL[,5]!=-999
x <- hydi$GENERAL[ind,5]
y <- hydi$GENERAL[ind,6]
wgs84<-st_crs("EPSG:4326")
map("world",xlim=c(-12,30),ylim=c(35,60))
# replace sp::SpatialPoints by 
# points(SpatialPoints(cbind(x,y),proj4string=wgs84),col="blue")
points(st_multipoint(cbind(x,y), wgs84),col="blue", pch = ".", cex = 4)


europe = c("Austria", "Belgium", "Czechia", "Denmark", "Finland", "France", "Germany", "Greece", "Hungary", "Italy", "Norway", "Poland", "Portugal", "Russia", "Slovakia", "Spain", "Sweden", "Ukraine", "United Kingdom")
ggplot() +
  geom_sf(data = st_as_sf(map("world", regions = europe, plot=FALSE, fill = TRUE))) +
  # geom_sf(data = st_as_sf(map("world", xlim=c(-12,50),ylim=c(35,75), plot=FALSE, fill = TRUE))) +
  geom_sf(data = hydi$GENERAL[ind,] %>% 
            st_as_sf(coords = c("X_WGS84", "Y_WGS84"), crs = 4326), 
          aes(colour = SOURCE), shape = 1)

ggsave("../fig/HYDI_SOURCE_nd_qa2.png", width = 14, height = 7, units = "in")

save('hydi',file="../output/HYDI_SOURCE_nd_qa2.Rdata")
 