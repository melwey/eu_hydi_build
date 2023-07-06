# EU-HYDI: errata corrections
load("../output/HYDI_SOURCE_nd_qa3.Rdata")

# geographic coordinates from Wim Cornelis are wrong. Grades and minutes should be transformed into decimal grades.
# select data from Cornelis in table GENERAL
pid <- hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE == "Cornelis"]
hydi$GENERAL[hydi$GENERAL$PROFILE_ID %in% pid, 1:8]
ind <- hydi$GENERAL$PROFILE_ID >= 5600103 & hydi$GENERAL$PROFILE_ID <= 5600120

tmp <- as.character(format(hydi$GENERAL$X_WGS84[ind],digit = 3))
x <- as.numeric(substr(tmp,start = 1,stop = 1)) + round(as.numeric( substr(tmp, start = 2, stop = 4)) /0.6, digit = 2)
hydi$GENERAL$X_WGS84[ind] <- x

tmp <- as.character(format(hydi$GENERAL$Y_WGS84[ind],digit = 4))
y <- as.numeric(substr(tmp,start = 1,stop = 2)) + round(as.numeric( substr(tmp, start = 3, stop = 5)) /0.6, digit = 2)
hydi$GENERAL$Y_WGS84[ind] <- y

hydi$GENERAL$COMMENTS2[ind] <- "geog. coord. corrected by M. Weynants"

# save new hydi
save("hydi", file = "../output/EUHYDI_v1_1.Rdata")

# missing to NA
for (i in 1:length(hydi)){
  hydi[[i]][hydi[[i]] == -999 | hydi[[i]] == "ND"] <- NA
  }
# save hydi.na
hydi.na <- hydi
save("hydi.na", file = "EUHYDI_NA_v1_1.Rdata")

# create shapefile with GENERAL
library("sf")
ind <- !is.na(hydi.na$GENERAL$X_WGS84)
source_4326 <- hydi.na$GENERAL[ind, ] %>%
            st_as_sf(coords = c("X_WGS84", "Y_WGS84"), crs = 4326)

europe = c("Austria", "Belgium", "Czechia", "Denmark", "England", "Finland", "France", "Germany", "Greece", "Hungary", "Italy", "Netherlands", "Norway(?!:Svalbard)", "Poland", "Portugal", "Russia", "Slovakia", "Spain", "Sweden", "Ukraine", "UK:Great Britain")

ggplot() +
  geom_sf(data = st_as_sf(maps::map("world", regions = europe, plot=FALSE, fill = TRUE))) +
  geom_sf(data = source_4326,
          aes(colour = SOURCE), shape = 1) +
  xlim(-15,40)

ggsave("../fig/HYDI_SOURCE.png", width = 14, height = 7, units = "in")


source_3035 <- st_transform(source_4326, st_crs(3035))
ggplot() +
  geom_sf(data = st_transform(
    st_as_sf(
      maps::map("world", regions = europe, plot=FALSE, fill = TRUE)
      ), crs = 3035
      )) +
  geom_sf(data = source_3035,
          aes(colour = SOURCE), shape = 1)
ggsave("../fig/hydi_3035.png", width = 12, height = 10)

st_write(source_4326, "../output/hydi_4326.geojson")
st_write(source_3035, "../output/hydi_3035.geojson")
