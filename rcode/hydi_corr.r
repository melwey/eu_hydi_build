# EU-HYDI: errata corrections
load("../output/HYDI_SOURCE_nd_qa3.Rdata")

# hydi.METH

# save new hydi
save("hydi", file = "../output/EUHYDI_v1_1.Rdata")

# missing to NA
for (i in 1:length(hydi)){
  hydi[[i]][hydi[[i]] == -999 | hydi[[i]] == "ND"] <- NA
  }
# save hydi.na
hydi.na <- hydi
save("hydi.na", file = "../output/EUHYDI_NA_v1_1.Rdata")

# create shapefile with GENERAL
library("sf")
ind <- !is.na(hydi.na$GENERAL$X_WGS84)
source_4326 <- hydi.na$GENERAL[ind, ] %>%
            st_as_sf(coords = c("X_WGS84", "Y_WGS84"), crs = 4326)

europe = c("Albania", "Austria", "Belarus", "Belgium", "Bosnia", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "England", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Italy", "Kosovo", "Lithuania", "Latvia", "Malta", "Macedonia", "Montenegro", "Moldova", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Turkey", "Ukraine", "UK:Great Britain")

library("tidyverse")
ggplot() +
  geom_sf(data = st_as_sf(maps::map("world", regions = europe, plot=FALSE, fill = TRUE))) +
  geom_sf(data = source_4326,
          aes(colour = SOURCE), shape = 1) +
  xlim(-15, 40) +
  ylim(33, 72)

ggsave("../fig/HYDI_SOURCE.png", width = 14, height = 7, units = "in")


source_3035 <- st_transform(source_4326, st_crs(3035))
ggplot() +
  geom_sf(data = st_transform(
    st_as_sf(
      maps::map("world", regions = europe, plot=FALSE, fill = TRUE)
      ), crs = 3035
      )) +
  geom_sf(data = source_3035,
          aes(colour = SOURCE), shape = 1) +
  xlim(2500000, 6355857) +
  ylim(1557591, 5500000)
ggsave("../fig/hydi_3035.png", width = 12, height = 10)

st_write(source_4326, "../output/hydi_4326.geojson", append = FALSE)
st_write(source_3035, "../output/hydi_3035.geojson", append = FALSE)
