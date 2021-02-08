library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(gstat)
library(stars)
library(colorspace)
library(ggspatial)


# clear environment
rm(list=ls())
gc()

#########################
### predictor rasters ###

ov_sthlm <- raster(".tif") %>%
  crop(extent(659000, 684000, 6566000, 6592000))
for_sthlm <- raster(".tif") %>%
  crop(extent(ov_sthlm))
wat_sthlm <- raster(".tif") %>%
  crop(extent(ov_sthlm))

sthlm_deso <- readOGR(dsn = "", layer = "")
r <- raster(ncol=2500, nrow=2600)
extent(r) <- extent(ov_sthlm)
crs(r ) <- CRS(ov_sthlm@crs@projargs)
night_pop <- rasterize(sthlm_deso, r, "pop_dens", fun = max, crs = CRS(ov_sthlm@crs@projargs))
day_pop <- rasterize(sthlm_deso, r, "work_dens", fun = max, crs = CRS(ov_sthlm@crs@projargs))
med_inc <- rasterize(sthlm_deso, r, "median_inc", fun = max, crs = CRS(ov_sthlm@crs@projargs))

# create focal weights
w_50 <- focalWeight(ov_sthlm, 50, type='circle')
w_50 <- w_50/max(w_50)
w_250 <- focalWeight(ov_sthlm, 250, type='circle')
w_250 <- w_250/max(w_250)

ov50 <- focal(ov_sthlm, w = w_50, fun = mean, na.rm = T)
for50 <- focal(for_sthlm, w = w_50, fun = mean, na.rm = T)
wat50 <- focal(wat_sthlm, w = w_50, fun = mean, na.rm = T)
values(wat50)[values(wat50)>0.6694214] = NA
wat <- reclassify(wat50, c(0,0,0, 0,0.6694215,1))
night_pop_250 <- focal(night_pop, w = w_250, fun = mean, na.rm = T)
day_pop_250 <- focal(day_pop, w = w_250, fun = mean, na.rm = T)
night_pop_10000 <- night_pop_250/10000
day_pop_10000 <- day_pop_250/10000
med_inc_10000 <- med_inc/10000

h_int <- harm %>%
  left_join(coord, by = "id") %>%
  st_as_sf(wkt = "wkt") %>%
  distinct(id, .keep_all = T)
st_crs(h_int) <- 4326
h_int <- st_transform(h_int, crs(r))
h_int <- h_int[st_as_sfc(st_bbox(r)),]

gs_h_d <- gstat(formula = dist_log~1, locations = h_int)
idw_h_d <- interpolate(r, gs_h_d)

c_int <- cope %>%
  left_join(coord, by = "id") %>%
  st_as_sf(wkt = "wkt") %>%
  distinct(id, .keep_all = T)
st_crs(c_int) <- 4326
c_int <- st_transform(c_int, crs(r))
c_int <- c_int[st_as_sfc(st_bbox(r)),]

gs_c_d <- gstat(formula = dist_log~1, locations = c_int)
gs_c_ac_400 <- gstat(formula = ac_400~1, locations = c_int)
idw_c_d <- interpolate(r, gs_c_d)
idw_c_ac_400 <- interpolate(r, gs_c_ac_400)

################################
### predict scores in places ###

pred_data_h <- data.frame(ov50 = as.data.frame(ov50) %>% .$layer,
                          for50 = as.data.frame(for50) %>% .$layer,
                          wat = as.factor(as.data.frame(wat) %>% .$layer),
                          night_pop_10000 = as.data.frame(night_pop_10000) %>% .$layer,
                          day_pop_10000 = as.data.frame(day_pop_10000) %>% .$layer,
                          dist_log = as.data.frame(idw_h_d) %>% .$var1.pred,
                          age = mean(h_int$age),
                          woman = as.factor(1),
                          hh_1 = as.factor(0),
                          occupation = "working",
                          med_inc_10k = as.data.frame(med_inc_10000) %>% .$layer)

pred_h <- raster(ncol=2500, nrow=2600)
extent(pred_h) <- extent(ov_sthlm)
crs(pred_h) <- CRS(ov_sthlm@crs@projargs)
values(pred_h) <- predict(melr_h, pred_data_h, re.form = NA)

pred_data_c <- data.frame(ov50 = as.data.frame(ov50) %>% .$layer,
                          for50 = as.data.frame(for50) %>% .$layer,
                          wat = as.factor(as.data.frame(wat) %>% .$layer),
                          night_pop_10000 = as.data.frame(night_pop_10000) %>% .$layer,
                          day_pop_10000 = as.data.frame(day_pop_10000) %>% .$layer,
                          dist_log = as.data.frame(idw_c_d) %>% .$var1.pred,
                          ac_400 = as.data.frame(idw_c_ac_400) %>% .$var1.pred,
                          age = mean(c_int$age),
                          woman = as.factor(1),
                          hh_1 = as.factor(0),
                          occupation = "working",
                          med_inc_10k = as.data.frame(med_inc_10000) %>% .$layer)

pred_c <- raster(ncol=2500, nrow=2600)
extent(pred_c) <- extent(ov_sthlm)
crs(pred_c) <- CRS(ov_sthlm@crs@projargs)
values(pred_c) <- predict(melr_c, pred_data_c, re.form = NA)

pred_h_c <- (pred_c-pred_h)/2
pred_odds <- exp(pred_h_c)
pred_prob <- pred_odds /(1+pred_odds)
values(pred_prob)[is.na(values(pred_prob))] <- mean(na.omit(values(pred_prob)))

##############################
### accessibility analysis ###

pred_prob_30 <- aggregate(pred_prob, fact=3)
w_2000 <- focalWeight(pred_prob_30, c(500,2000), type="Gauss")
pred_nbhd <- focal(pred_prob_30, w = w_2000, fun = sum, na.rm = T)

###################
### produce map ###

buildings <- readOGR(dsn = "tatort", layer = "great_sthlm_merged")
build_nbhd_sf <- raster::extract(pred_nbhd, buildings, fun=mean, na.rm=TRUE, sp = T) %>%
  st_as_sf()

sthlm_wat <- readOGR(dsn = "tatort", layer = "great_sthlm_water") %>%
  st_as_sf()
sthlm_roads <- readOGR(dsn = "tatort", layer = "sthlm_roads") %>%
  st_as_sf()

ov_20 <- aggregate((ov_sthlm), fact = 2)
ov_sf <- st_as_sf(st_as_stars(ov_20),
                  as_points = F, merge = T)
ov_sf <- ov_sf[ov_sf$ov_sthlm==1,]
ov_sf$area <- as.numeric(st_area(ov_sf))
ov_sf <- ov_sf[ov_sf$area>2000,]
for_20 <- aggregate((for_sthlm), fact = 2)
for_sf <- st_as_sf(st_as_stars(for_20),
                   as_points = F, merge = T)
for_sf <- for_sf[for_sf$for_sthlm==1,]
for_sf$area <- as.numeric(st_area(for_sf))
for_sf <- for_sf[for_sf$area>2000,]

F.A <- ggplot(filter(build_nbhd_sf, bostad == "1")) +
  geom_sf(data = ov_sf, fill = "#9fb77d", color = NA) +
  geom_sf(data = for_sf, fill = "#68aa3f", color = NA) +
  geom_sf(aes(fill = layer.1), color = NA) +
  geom_sf(data = filter(build_nbhd_sf, bostad=="0"), fill = "#cccccc", color = NA) +
  geom_sf(data = sthlm_wat, fill = "#dddddd", color = NA) +
  geom_sf(data = sthlm_roads, fill = "#666666", color = NA, alpha = 0.8) +
  scale_fill_gradientn(colors = c("#320000", "#E62317", "#E6BEDC", "#63acbe", "#003232"))+
  coord_sf(xlim = c(662000, 681000),
           ylim = c(6569000, 6589000),
           datum = NA) +
  labs(title = "", fill = "") +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br",
                         which_north = "true", 
                         pad_x = unit(0.75, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal) +
  theme_bw() +
  scale_alpha(guide=F) +
  guides(fill = guide_colourbar(barwidth = 12)) +
  theme(legend.position = "bottom")
F.A
