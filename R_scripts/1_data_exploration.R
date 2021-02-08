library(tidyverse)
library(scales)
library(ggpubr)

# clear environment
rm(list=ls())
gc()

#################
### read data ###

homes <- read_csv(".csv")
respondents <- read_delim(".csv",
                          ";", escape_double = FALSE, trim_ws = TRUE) %>%
  subset(respondent %in% homes$respondent) %>%
  dplyr::select(respondent, age, woman, working, student, unemployed, retired, hh_1)
places <- read_csv(".csv") %>%
  inner_join(respondents, by = "respondent")  %>%
  na.omit() %>%
  distinct(.keep_all = T)
colnames(places)[3] <- "id"
places <- places %>% inner_join(homes[,c(1,5)], by = "respondent")
places$dist_home <- places$dist_home/1000 # kms
places$buttonname <- factor(places$buttonname,
                            levels = c("avoid", "visit_similarly", "visit_more"))
places$buttonname <- plyr::revalue(places$buttonname,
                                   c("avoid"="avoid", "visit_similarly"="similarly", "visit_more"="more"))
checkboxes <- read_delim(".csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE) %>%
  select(3, 6:14)
colnames(checkboxes)[c(1,10)] <- c("id", "infl_2")
places <- places %>%
  inner_join(checkboxes, by = "id") %>%
  distinct(.keep_all = T)
places <- places %>% filter(!is.na(infl_2))
respondents <- respondents %>%
  filter(respondent %in% places$respondent)
homes <- homes %>%
  filter(respondent %in% places$respondent)
write.csv(homes, ".csv")

# sample statistics
respondents %>%
  select(-1) %>%
  map(table)
table(respondents$woman)/684
table(respondents$working)/684
table(respondents$unemployed)/684
table(respondents$student)/684
table(respondents$retired)/684
table(respondents$hh_1)/684
table(respondents$age)/684

################
### timeline ###

ggplot(places, aes(as.Date(createtime))) +
  geom_histogram(bins = 49) +
  scale_x_date() +
  labs(x = "Date", y = "Count") +
  theme_bw()

##################
### histograms ###

facet.labs <- c("Visit less", "Visit similarly", "Visit more")
names(facet.labs) <- c("avoid", "similarly", "more")
colors <- c("#bbbbbb", "#777777", "#333333")
names(colors) <- c("avoid", "similarly", "more")

A.A <- ggplot(places, aes(x = ov50, fill = buttonname)) +
  geom_histogram(bins = 30) +
  facet_grid(buttonname ~ ., scales = "free", labeller = labeller(buttonname = facet.labs)) +
  scale_fill_manual(values = colors, labels = facet.labs) +
  labs(title = "A",
       x = "Fields",
       y = "Count") +
  theme_bw() +
  theme(legend.position = "none")
A.B <- ggplot(places, aes(x = for50, fill = buttonname)) +
  geom_histogram(bins = 30) +
  facet_grid(buttonname ~ ., scales = "free", labeller = labeller(buttonname = facet.labs)) +
  scale_fill_manual(values = colors) +
  labs(title = "B",
       x = "Forest",
       y = "Count") +
  guides(fill = F) + 
  theme_bw() +
  theme(legend.position = "none")
A.C <- ggplot(places, aes(x = wat50, fill = buttonname)) +
  geom_histogram(bins = 30) +
  facet_grid(buttonname ~ ., scales = "free", labeller = labeller(buttonname = facet.labs)) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(trans = "log2") +
  labs(title = "C",
       x = "Water",
       y = "Count") +
  guides(fill = F) + 
  theme_bw() +
  theme(legend.position = "none")
A.D <- ggplot(places, aes(x = night_pop_dens, fill = buttonname)) +
  geom_histogram(bins = 30) +
  facet_grid(buttonname ~ ., scales = "free", labeller = labeller(buttonname = facet.labs)) +
  scale_fill_manual(values = colors, labels = facet.labs) +
  scale_x_sqrt(breaks = seq(0,200,50)^2) +
  labs(title = "D",
       x = "Residential pop. density",
       y = "Count") +
  theme_bw()  +
  theme(legend.position = "none")
A.E <- ggplot(places, aes(x = day_pop_dens, fill = buttonname)) +
  geom_histogram(bins = 30) +
  facet_grid(buttonname ~ ., scales = "free", labeller = labeller(buttonname = facet.labs)) +
  scale_fill_manual(values = colors, labels = facet.labs) +
  scale_x_sqrt(
    breaks = seq(0,90,15)^2
  ) +
  labs(title = "E",
       x = "Daytime pop. density",
       y = "Count") +
  theme_bw()  +
  theme(legend.position = "none")
A.F <- ggplot(filter(places, dist_home<100&dist_home>0.01),
              aes(x = dist_home, fill = buttonname)) +
  geom_histogram(bins = 30) +
  facet_grid(buttonname ~ ., scales = "free", labeller = labeller(buttonname = facet.labs)) +
  scale_fill_manual(values = colors) +
  scale_x_log10(breaks = 10^(seq(-2,2,1)),
                labels = function(x,...) {
                  format(x, ..., scientific = FALSE, drop0trailing = TRUE)
                }) +
  labs(title = "F",
       x = "Distance to home (km)",
       y = "Count") +
  theme_bw() +
  theme(legend.position = "none")
A.AF <- ggarrange(A.A, A.B, A.C, A.D, A.E, A.F, nrow = 2, ncol = 3)
A.AF

##############
### matrix ###

matrix <- as.data.frame(table(places$influence, places$buttonname))
colnames(matrix) <- c("infl", "freq", "amount")
matrix$color <- c("1", "2", "2",
                  "3", "4", "5",
                  "3", "4", "5")
matrix$freq <- plyr::revalue(matrix$freq,
                             c("avoid"="Avoid", "similarly"="Visit similarly", "more"="Visit more"))
matrix$infl <- plyr::revalue(matrix$infl,
                             c("positive"="Positive", "none"="None", "negative"="Negative"))

colors <- c("#2E70B9", "#cccccc", "#ee442f", "#601a4a", "#63acbe")

B <- matrix %>%
  ggplot(aes(freq, infl)) +
  geom_point(aes(size = amount,
                 color = color)) +
  geom_text(aes(label = amount), color = "white") +
  scale_size_continuous(range =c(7,30),
                        guide = F) +
  scale_color_manual(values = colors,
                     guide = F) +
  labs(title = "",
       y = "Influence on well-being") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title=element_blank(),
        panel.grid.major = element_blank())
B

places$group <- ifelse(places$buttonname=="avoid",
                       ifelse(places$influence=="negative", "A-", "A=/+"),
                       ifelse(places$influence=="negative", "V-",
                              ifelse(places$influence=="positive", "V+", "V=")))

checkbox_prop <- places %>%
  select(functions, work_study, service, seclusion, other_people,
         sport_culture, friends_family, accessible, group) %>%
  na.omit() %>%
  group_by(group) %>%
  summarise(functions = round(mean(functions),2),
            work_study = round(mean(work_study),2),
            service = round(mean(service),2),
            seclusion = round(mean(seclusion),2),
            other_people = round(mean(other_people),2),
            sport_culture = round(mean(sport_culture),2),
            friends_family = round(mean(friends_family),2),
            accessible = round(mean(accessible),2)) %>%
  gather("checkbox", "prop", -group)

checkbox_prop$checkbox <- plyr::revalue(checkbox_prop$checkbox,
                             c("accessible" = "The place is easily accessible",
                               "friends_family" = "I can meet friends or family here",
                               "functions" = "I use facilities at the place",
                               "other_people" = "The place is full of people",
                               "seclusion" = "I can experience seclusion",
                               "service" = "I visit shops, restaurants or public servies",
                               "sport_culture" = "I take part in sports or culture events",
                               "work_study" = "I work of study at the place"
                               ))
checkbox_prop$checkbox <- factor(checkbox_prop$checkbox,
                                    levels = c("I can experience seclusion",
                                               "The place is easily accessible",
                                               "I use facilities at the place",
                                               "I visit shops, restaurants or public servies",
                                               "I work of study at the place",
                                               "I take part in sports or culture events",
                                               "I can meet friends or family here",
                                               "The place is full of people"))
checkbox_prop$group <- factor(checkbox_prop$group,
                              levels = c("A-", "A=/+", "V-", "V=", "V+"))
B.B <- checkbox_prop %>%
ggplot(aes(group, checkbox)) +
  geom_point(aes(size = prop,
                 color = group)) +
  geom_text(aes(label = prop), color = "white", size = 2.7) +
  scale_color_manual(values = colors,
                     guide = F) +
  scale_size_continuous(range =c(5,15),
                        guide = F) +
  labs(title = "B",
       x = "",
       y = "") +
  theme_bw()
B.B
  
#####################
### density plots ###

medians <- places %>% 
  group_by(buttonname) %>% 
  summarise(ov50 = median(ov50),
            for50 = median(for50),
            night_pop_dens = median(night_pop_dens),
            day_pop_dens = median(day_pop_dens),
            dist_home = median(dist_log))
places %>% filter(buttonname!="avoid") %>% summarise(pop = median(day_pop_dens))

places$group <- if_else(places$buttonname=="avoid",
                        if_else(places$influence=="negative", "miss", "avoid"),
                        if_else(places$influence=="negative", "harm",
                                if_else(places$influence=="positive", "cope", "similarly")))
places$group <- factor(places$group,
                       levels = c("avoid", "miss", "harm", "similarly", "cope"))
write_csv(places, "places.csv")

facet.labs <- c("A=/+", "A-", "V-", "V=", "V+")
names(facet.labs) <- c("avoid", "miss", "harm", "similarly", "cope")
colors <- c("#cccccc", "#2E70B9", "#ee442f", "#601a4a", "#63acbe")
names(colors) <- c("avoid", "miss", "harm", "similarly", "cope")

C.A <- ggplot(filter(places, dist_home<50&dist_home>0.05),
              aes(x = dist_home, y = ov50, color = group)) +
  geom_point(color = "black", alpha=0.1, size = 0.5) +
  geom_vline(xintercept = median(places$dist_home)) +
  geom_hline(yintercept = median(places$ov50)) +
  geom_density_2d() +
  scale_color_manual(values = colors) +
  facet_grid(group ~ .) +
  scale_x_log10(breaks = 10^(seq(-2,1,1)),
                labels = function(x,...) {
                  format(x, ..., scientific = FALSE, drop0trailing = TRUE)
                }) +
  labs(title = "A",
       y = "Lawns, 50 m") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_blank())
C.B <- ggplot(filter(places, dist_home<50&dist_home>0.05),
              aes(x = dist_home, y = for50, color = group)) +
  geom_point(color = "black", alpha=0.1, size = 0.5) +
  geom_vline(xintercept = median(places$dist_home)) +
  geom_hline(yintercept = median(places$for50)) +
  geom_density_2d() +
  scale_color_manual(values = colors) +
  facet_grid(group ~ .) +
  scale_x_log10(breaks = 10^(seq(-2,1,1)),
                labels = function(x,...) {
                  format(x, ..., scientific = FALSE, drop0trailing = TRUE)
                }) +
  labs(title = "B",
       y = "Forest, 50 m") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_blank())
C.C <- ggplot(filter(places, dist_home<50&dist_home>0.05),
              aes(x = dist_home, y = night_pop_dens, color = group)) +
  geom_point(color = "black", alpha=0.1, size = 0.5) +
  geom_vline(xintercept = median(places$dist_home)) +
  geom_hline(yintercept = median(places$night_pop_dens)) +
  geom_density_2d() +
  scale_color_manual(values = colors) +
  facet_grid(group ~ .) +
  scale_x_log10(breaks = 10^(seq(-2,1,1)),
                labels = function(x,...) {
                  format(x, ..., scientific = FALSE, drop0trailing = TRUE)
                }) +
  scale_y_sqrt(breaks = seq(0,200,50)^2) +
  labs(title = "C",
       y = "Residential pop. density") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_blank())
C.D <- ggplot(filter(places, dist_home<50&dist_home>0.05),
              aes(x = dist_home, y = day_pop_dens, color = group)) +
  geom_point(color = "black", alpha=0.1, size = 0.5) +
  geom_vline(xintercept = median(places$dist_home)) +
  geom_hline(yintercept = median(places$day_pop_dens)) +
  geom_density_2d() +
  scale_color_manual(values = colors) +
  facet_grid(group ~ .,labeller = labeller(group = facet.labs)) +
  scale_x_log10(breaks = 10^(seq(-2,1,1)),
                labels = function(x,...) {
                  format(x, ..., scientific = FALSE, drop0trailing = TRUE)
                }) +
  scale_y_sqrt(breaks = seq(0,90,15)^2) +
  labs(title = "D",
       y = "Working pop., 250 m") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())
C.AD <- ggarrange(C.A, C.B, C.C, C.D,
                  nrow = 1, ncol = 4, widths = c(1,1,1,1.1))
annotate_figure(C.AD,
                bottom = text_grob("Distance to home (km)", color = "black"))
