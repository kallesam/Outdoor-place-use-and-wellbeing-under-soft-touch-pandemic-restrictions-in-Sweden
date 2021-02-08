library(tidyverse)
library(lme4)
library(car)
library(sf)
library(spdep)
library(directlabels)

########################
### data preparation ###

places[,c("buttonname",
          "influence")] <- lapply(places[,c("buttonname",
                                            "influence")], ordered)
places$ov50d <- places$ov50*10
places$for50d <- places$for50*10
places$wat <- as.factor(if_else(places$wat50>0, 1, 0))
places$night_pop_dens <- places$night_pop*1000000/(250*250*pi)
places$night_pop_10000 <- places$night_pop_dens/10000
places$day_pop_dens <- places$day_pop*1000000/(250*250*pi)
places$day_pop_10000 <- places$day_pop_dens/10000
places$dist_log <- log(places$dist_home+0.01)

places$age <- as.numeric(as.factor(places$age))
places$old <- as.factor(ifelse(places$age<5,0,1))
places$woman <- as.factor(places$woman)
places$occupation <- as.factor(ifelse(places$working==1, "working",
                                      ifelse(places$student==1, "student",
                                             ifelse(places$unemployed==1, "unemployed",
                                                    ifelse(places$retired==1, "retired", "none")))))
places$occupation <- relevel(places$occupation, ref = "working")
places$hh_1 <- as.factor(places$hh_1)
places$med_inc_10k <- places$med_inc/10000

avoid <- places %>%
  filter(buttonname=="avoid")
avoid$outcome <- if_else(avoid$influence=="negative", 1, 0)
harm <- places %>%
  filter(buttonname!="avoid") %>%
  filter(influence!="positive")
harm$outcome <- if_else(harm$influence=="negative", 1, 0)
cope <- places %>%
  filter(buttonname!="avoid") %>%
  filter(influence!="negative")
cope$outcome <- if_else(cope$influence=="positive", 1, 0)


##################
### regression ###

melr_m <- glmer(outcome ~ ov50 + for50 + wat +
                  night_pop_10000 + day_pop_10000 + dist_log +
                  age + woman + occupation + hh_1 + med_inc_10k +
                  (1 | Deso), data = avoid, family = binomial,
                control = glmerControl(optimizer = "bobyqa"), nAGQ = 10
)
summary(melr_m)
exp(summary(melr_m)[[10]][2:7])

melr_h <- glmer(outcome ~ ov50 + for50 + wat +
                  night_pop_10000 + day_pop_10000 + dist_log +
                  age + woman + #occupation + 
                  hh_1 + med_inc_10k +
                  (1 | Deso), data = harm, family = binomial,
                control = glmerControl(optimizer = "bobyqa"), nAGQ = 10
)
summary(melr_h)
exp(summary(melr_h)[[10]][2:7])

melr_c <- glmer(outcome ~ ov50 + for50 + wat +
                  night_pop_10000 + day_pop_10000 + dist_log +
                  age + woman + occupation + hh_1 + med_inc_10k +
                  (1 | Deso), data = cope, family = binomial,
                control = glmerControl(optimizer = "bobyqa"), nAGQ = 100
)
summary(melr_c)
exp(summary(melr_c)[[10]][2:7])

# spatial autocorrelation of residuals
coord <-read_delim("200615.csv", ";",
                   escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::select(3, 5) %>%
  distinct(geoAnswerId, .keep_all = T)
colnames(coord)[1] <- "id"

avoid_sf <- avoid %>%
  dplyr::select(id, buttonname) %>%
  left_join(coord, by = "id") %>%
  st_as_sf(wkt = "wkt") %>%
  distinct(id, .keep_all = T)
st_crs(avoid_sf) <- 4326
avoid_sf <- st_transform(avoid_sf, 3006)
harm_sf <- harm %>%
  dplyr::select(id, buttonname) %>%
  left_join(coord, by = "id") %>%
  st_as_sf(wkt = "wkt") %>%
  distinct(id, .keep_all = T)
st_crs(harm_sf) <- 4326
harm_sf <- st_transform(harm_sf, 3006)
cope_sf <- cope %>%
  dplyr::select(id, buttonname) %>%
  left_join(coord, by = "id") %>%
  st_as_sf(wkt = "wkt") %>%
  distinct(id, .keep_all = T)
st_crs(cope_sf) <- 4326
cope_sf <- st_transform(cope_sf, 3006)

moran_table <- NULL
for(i in c(1:10,
           seq(12, 20, by = 2),
           seq(25, 50, by = 5),
           seq(60, 100, by = 10)
)){
  print(i)
  m_nb <- dnearneigh(avoid_sf, 0, i*100)
  h_nb <- dnearneigh(harm_sf, 0, i*100)
  c_nb <- dnearneigh(cope_sf, 0, i*100)
  m_nb_weights <- nb2listw(m_nb, zero.policy = TRUE)
  h_nb_weights <- nb2listw(h_nb, zero.policy = TRUE)
  c_nb_weights <- nb2listw(c_nb, zero.policy = TRUE)
  m_moran <- moran.test(residuals(melr_m), m_nb_weights, zero.policy=TRUE)
  h_moran <- moran.test(residuals(melr_h), h_nb_weights, zero.policy=TRUE)
  c_moran <- moran.test(residuals(melr_c), c_nb_weights, zero.policy=TRUE)
  stats1 <- cbind(i*100, "M", m_moran[[3]][[1]], m_moran[[2]])
  stats2 <- cbind(i*100, "H", h_moran[[3]][[1]], h_moran[[2]])
  stats3 <- cbind(i*100, "C", c_moran[[3]][[1]], c_moran[[2]])
  moran_table <- rbind(moran_table, stats1, stats2, stats3)
}
colnames(moran_table) <- c("nbhd", "model", "moran", "p")
moran_table <- as.data.frame(moran_table)
moran_table$nbhd <- as.numeric(levels(moran_table$nbhd))[moran_table$nbhd]
moran_table$moran <- as.numeric(levels(moran_table$moran))[moran_table$moran]
moran_table$p <- as.numeric(levels(moran_table$p))[moran_table$p]
moran_table$sig <- as.factor(if_else(moran_table$p<0.05, "Yes", "No"))

AC.A <- ggplot(moran_table, aes(x = nbhd,
                               y = moran,
                               group = model)) +
  geom_line() +
  geom_point(aes(color = sig), size = 3) +
  labs(x = "Neighborhood size",
       y = "Residual Moran's I") +
  labs(title = "A") +
  geom_dl(aes(label = model), method = list(dl.trans(x = x + .2), "first.points")) +
  theme_bw() +
  theme(legend.position = "none")
AC.A

cope$ac_400 <- autocov_dist(as.vector(residuals(melr_c)), cope_sf,
                             nbs = 400, zero.policy = TRUE, type = "one")

melr_c <- glmer(outcome ~ ov50 + for50 + wat +
                   night_pop_10000 + day_pop_10000 + dist_log +
                   age + woman + occupation + hh_1 + med_inc_10k + ac_400 +
                   (1 | Deso), data = cope, family = binomial,
                 control = glmerControl(optimizer = "bobyqa"), nAGQ = 100
)
summary(melr_c)
exp(summary(melr_c)[[10]][2:7])

moran_table_S1 <- NULL
for(i in c(1:10,
           seq(12, 20, by = 2),
           seq(25, 50, by = 5),
           seq(60, 100, by = 10)
)){
  print(i)
  m_nb <- dnearneigh(avoid_sf, 0, i*100)
  h_nb <- dnearneigh(harm_sf, 0, i*100)
  c_nb <- dnearneigh(cope_sf, 0, i*100)
  m_nb_weights <- nb2listw(m_nb, zero.policy = TRUE)
  h_nb_weights <- nb2listw(h_nb, zero.policy = TRUE)
  c_nb_weights <- nb2listw(c_nb, zero.policy = TRUE)
  m_moran <- moran.test(residuals(melr_m), m_nb_weights, zero.policy=TRUE)
  h_moran <- moran.test(residuals(melr_h), h_nb_weights, zero.policy=TRUE)
  c_moran <- moran.test(residuals(melr_c), c_nb_weights, zero.policy=TRUE)
  stats1 <- cbind(i*100, "M", m_moran[[3]][[1]], m_moran[[2]])
  stats2 <- cbind(i*100, "H", h_moran[[3]][[1]], h_moran[[2]])
  stats3 <- cbind(i*100, "C", c_moran[[3]][[1]], c_moran[[2]])
  moran_table_S1 <- rbind(moran_table_S1, stats1, stats2, stats3)
}
colnames(moran_table_S1) <- c("nbhd", "model", "moran", "p")
moran_table_S1 <- as.data.frame(moran_table_S1)
moran_table_S1$nbhd <- as.numeric(levels(moran_table_S1$nbhd))[moran_table_S1$nbhd]
moran_table_S1$moran <- as.numeric(levels(moran_table_S1$moran))[moran_table_S1$moran]
moran_table_S1$p <- as.numeric(levels(moran_table_S1$p))[moran_table_S1$p]
moran_table_S1$sig <- as.factor(if_else(moran_table_S1$p<0.05, "Yes", "No"))

AC.B <- ggplot(filter(moran_table_S1),
              aes(x = nbhd, y = moran, group = model)) +
  geom_line() +
  geom_point(aes( color = sig), size = 3) +
  labs(x = "Neighborhood size",
       y = "Residual Moran's I") +
  labs(title = "B") +
  geom_dl(aes(label = model), method = list(dl.trans(x = x + .2), "first.points")) +
  theme_bw() +
  theme(legend.position = "none")
AC.B

############################
### autocorrelation plot ###

moran_m <- moran_table  %>% filter(model=="M") %>% dplyr::select(nbhd, moran, sig)
moran_h <- moran_table  %>% filter(model=="H") %>% dplyr::select(nbhd, moran, sig)
moran_c <- moran_table  %>% filter(model=="C") %>% dplyr::select(nbhd, moran, sig)
moran_c_S <- moran_table_S1 %>% filter(model=="C") %>% dplyr::select(moran, sig)

moran_c <- cbind(moran_c, moran_c_S)
colnames(moran_c)[-1] <- c("C", "C_sig", "C_S", "C_S_sig")

D.A <- ggplot(moran_m, aes(x = nbhd)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_line(aes(y = moran), alpha = 0.7) +
  geom_point(aes(y = moran, color = sig), size = 1) +
  labs(title = "A. Model 1") +
  scale_color_manual(values = c("darkgrey", "black")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
D.B <- ggplot(moran_h, aes(x = nbhd)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_line(aes(y = moran), alpha = 0.7) +
  geom_point(aes(y = moran, color = sig), size = 1) +
  labs(title = "B. Model 2") +
  scale_color_manual(values = c("darkgrey", "black")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
D.C <- ggplot(moran_c, aes(x = nbhd)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_ribbon(aes(ymin = C_S, ymax = C), fill="darkgrey", alpha = 0.3) +
  geom_line(aes(y = C), alpha = 0.7) +
  geom_line(aes(y = C_S), alpha = 0.7) +
  geom_point(aes(y = C, color = C_sig), size = 1) +
  geom_point(aes(y = C_S, color = C_S_sig), size = 1) +
  labs(title = "C. Model 3") +
  scale_color_manual(values = c("darkgrey", "black")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
D.AC <- ggarrange(D.A, D.B, D.C, nrow = 1)
annotate_figure(D.AC,
                bottom = text_grob("Neighborhood size", color = "black"),
                left = text_grob("Residual Moran's I", color = "black", rot = 90))

#######################
### odds ratio plot ###

# Create labels for plot
boxLabels_f = c("Lawns (0-1)", "Forest (0-1)", "Water (0-1)", "Residential pop. (10 000/km2)",
                "Working pop. (10 000/km2)", "Distance from home (×10)")

est_ci_m <- cbind(est = fixef(melr_m)[c(2:7)],
                  confint(melr_m, method = "Wald")[3:8,])
est_ci_h <- cbind(est = fixef(melr_h)[c(2:7)],
                  confint(melr_h, method = "Wald")[3:8,])
est_ci_c <- cbind(est = fixef(melr_c)[c(2:7)],
                  confint(melr_c, method = "Wald")[3:8,])

f_or_m <- est_ci_m %>%
  exp() %>%
  data.frame(yAxis = as.factor(length(boxLabels_f):1),
             model = "M",
             z = abs(summary(melr_m)[[10]][30:35]))
f_or_h <- est_ci_h %>%
  exp() %>%
  data.frame(yAxis = as.factor(length(boxLabels_f):1),
             model = "H",
             z = abs(summary(melr_h)[[10]][24:29]))
f_or_c <- est_ci_c %>%
  exp() %>%
  data.frame(yAxis = as.factor(length(boxLabels_f):1),
             model = "C",
             z = abs(summary(melr_c)[[10]][32:37]))

f_or_df <- bind_rows(f_or_m, f_or_h, f_or_c)
colnames(f_or_df)[2:3] <- c("ci_l", "ci_h")
f_or_df$model <- factor(f_or_df$model,
                        levels = c("M", "H", "C"))
f_or_df$opacity <- (f_or_df$z^(1/4))/((max(na.omit(f_or_df$z)))^(1/4))

E.A <- f_or_df %>%
  filter(model=="M") %>%
  ggplot(aes(x = est, y = yAxis)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_h, xmin = ci_l, color = model, alpha = opacity),
                 position = ggstance::position_dodgev(0.8),
                 size = .5, height = .2) +
  geom_point(aes(color = model, alpha = opacity),
             position = ggstance::position_dodgev(0.8),
             size = 3.5) +
  scale_y_discrete(labels= rev(boxLabels_f)) +
  scale_color_manual(values = "#2E70B9") +
  scale_alpha(range = c(0.56, 0.75)) +
  scale_x_continuous(trans = "log10") +
  annotation_logticks(sides = "b") +
  labs(title = "A. Model 1",
       x = "Odds ratio") +
  theme_bw() +
  theme(legend.position="none",
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank())
E.B <- f_or_df %>%
  filter(model=="H") %>%
  ggplot(aes(x = est, y = yAxis)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_h, xmin = ci_l, alpha = opacity),
                 position = ggstance::position_dodgev(0.8),
                 size = .5, height = .2, color = "#ee442f") +
  geom_point(aes(alpha = opacity),
             position = ggstance::position_dodgev(0.8),
             size = 3.5, color = "#ee442f") +
  scale_y_discrete(labels= rev(boxLabels_f)) +
  scale_alpha(range = c(0.56, 0.79)) +
  scale_x_continuous(trans = "log10") +
  annotation_logticks(sides = "b") +
  labs(title = "B. Model 2",
       x = "Odds ratio") +
  theme_bw() +
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())
E.C <- f_or_df %>%
  filter(model=="C") %>%
  ggplot(aes(x = est, y = yAxis)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_h, xmin = ci_l, alpha = opacity),
                 position = ggstance::position_dodgev(0.8),
                 size = .5, height = .2, color = "#63acbe") +
  geom_point(aes(alpha = opacity),
             position = ggstance::position_dodgev(0.8),
             size = 3.5, color = "#63acbe") +
  scale_y_discrete(labels= rev(boxLabels_f)) +
  scale_alpha(range = c(0.45, 1)) +
  scale_x_continuous(trans = "log10") +
  annotation_logticks(sides = "b") +
  labs(title = "C. Model 3",
       x = "Odds ratio") +
  theme_bw() +
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())
E.AC <- ggarrange(E.A, E.B, E.C, nrow = 1, ncol = 3, widths = c(1.7,1,1))
E.AC

##################
### parameters ###

summary(melr_m)
exp(summary(melr_m)[[10]][,1])
exp(confint(melr_m, method = "Wald"))
vif(melr_m)

summary(melr_h)
exp(summary(melr_h)[[10]][,1])
exp(confint(melr_h, method = "Wald"))
vif(melr_h)

summary(melr_c)
exp(summary(melr_c)[[10]][,1])
exp(confint(melr_c, method = "Wald"))
vif(melr_c)
