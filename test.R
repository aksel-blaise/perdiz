#####
# load packages
devtools::install_github("MomX/Momocs")
library(here)
library(Momocs)

# read images
jpg.list <- list.files(here("/img.perdiz"), full.names = TRUE)

# read attribute data
att.data <- read.csv("perdiz.csv", header = TRUE, as.is = TRUE)

# attribute to factor
att.data$context <- as.factor(att.data$context)
att.data$temporal <- as.factor(att.data$temporal)
att.data$trinomial <- as.factor(att.data$trinomial)
att.data$raw.mat <- as.factor(att.data$raw.mat)
att.data$contemp <- as.factor(att.data$contemp)
att.data$contempraw <- as.factor(att.data$contempraw)

#####
# generate outlines
outlines <- jpg.list %>%
  import_jpg()

# add attributes
data.out <- Out(outlines, 
                fac = att.data)

# scale, align, rotate, and center specimens
norm.outlines <- data.out %>% 
  coo_scale() %>%
  coo_align() %>%
  coo_rotate() %>% 
  coo_center()

#####
# outline pile
stack(norm.outlines)

panel(norm.outlines, names = TRUE)
panel(norm.outlines, fac = 'trinomial')

#####
# outlines for individual groups

## silicified wood
s.wd <- filter(norm.outlines, 
               raw.mat %in% c("s.wd"))

silicified <- s.wd %>% 
  coo_scale() %>%
  coo_align() %>%
  coo_rotate() %>% 
  coo_center()

## chert
chert <- filter(norm.outlines, 
               raw.mat %in% c("chert"))

chert <- chert %>% 
  coo_scale() %>%
  coo_align() %>%
  coo_rotate() %>% 
  coo_center()

## jasper
jasper <- filter(norm.outlines, 
                raw.mat %in% c("jasper"))

jasper <- jasper %>% 
  coo_scale() %>%
  coo_align() %>%
  coo_rotate() %>% 
  coo_center()

## quartzite
quartzite <- filter(norm.outlines, 
                 raw.mat %in% c("quartzite"))

quartzite <- quartzite %>% 
  coo_scale() %>%
  coo_align() %>%
  coo_rotate() %>% 
  coo_center()

# render figure
par(mfrow=c(2, 2))
stack(silicified, title = "Silicified Wood", xy.axis = TRUE, centroid = FALSE)
stack(chert, title = "Chert", xy.axis = TRUE, centroid = FALSE)
stack(jasper, title = "Jasper", xy.axis = TRUE, centroid = FALSE)
stack(quartzite, title = "Quartzite", xy.axis = TRUE, centroid = FALSE)

#####
# calibrate how many harmonics needed
calibrate_harmonicpower_efourier(norm.outlines, 
                                 nb.h = 30)

# 11 harmonics needed to capture 99 percent of variation
calibrate_reconstructions_efourier(norm.outlines, 
                                   range = 1:11)

# generate efa outlines with 11 harmonics
efa.outlines <- efourier(norm.outlines, 
                         nb.h = 11, 
                         norm = TRUE)

# use efa.outlines for pca
pca.outlines <- PCA(efa.outlines)

#####
# pca 
scree_plot(pca.outlines)

# plot pca by site
plot_PCA(pca.outlines, 
         morphospace_position = "range",
         ~trinomial, zoom = 1)

# plot pca by raw material
plot_PCA(pca.outlines, 
         morphospace_position = "range",
         ~raw.mat, zoom = 1)

# plot pca by context
plot_PCA(pca.outlines, 
         morphospace_position = "range",
         ~context, zoom = 1)

# plot pca by temporal
plot_PCA(pca.outlines, 
         morphospace_position = "range",
         ~temporal, zoom = 1)

# plot pca by contemp
plot_PCA(pca.outlines, 
         morphospace_position = "range",
         ~contemp, zoom = 1.2)

# plot pca by contempraw
plot_PCA(pca.outlines, 
         morphospace_position = "range",
         ~contempraw, zoom = 1.2)

# contribution of each pc
# by site
boxplot(pca.outlines, ~trinomial, nax = 1:5)
# by raw material
boxplot(pca.outlines, ~raw.mat, nax = 1:5)
# by context
boxplot(pca.outlines, ~context, nax = 1:5)
# by temporal
boxplot(pca.outlines, ~temporal, nax = 1:5)
# by contemp
boxplot(pca.outlines, ~contemp, nax = 1:5)
# by contempraw
boxplot(pca.outlines, ~contempraw, nax = 1:5)

# mean shape + 2sd for the first 10 pcs
PCcontrib(pca.outlines, nax = 1:5)

#####
# manova

# shape difference between sites?
MANOVA(pca.outlines, 'trinomial')
# which differ?
MANOVA_PW(pca.outlines, 'trinomial')

# shape difference between raw.mat?
MANOVA(pca.outlines, 'raw.mat')
# which differ?
MANOVA_PW(pca.outlines, 'raw.mat')

# shape difference by context?
MANOVA(pca.outlines, 'context')

# shape difference between temporal?
MANOVA(pca.outlines, 'temporal')
# which differ?
MANOVA_PW(pca.outlines, 'temporal')

# shape difference by contemp?
MANOVA(pca.outlines, 'contemp')
# which differ?
MANOVA_PW(pca.outlines, 'contemp')

# shape difference by contempraw?
MANOVA(pca.outlines, 'contempraw')
# which differ?
MANOVA_PW(pca.outlines, 'contempraw')

#####
# mean shapes

# site
ms.2 <- MSHAPES(efa.outlines, ~trinomial)
plot_MSHAPES(ms.2, size = 0.75)

# raw material
ms.3 <- MSHAPES(efa.outlines, ~raw.mat)
plot_MSHAPES(ms.3, size = 0.75)

# context
ms.4 <- MSHAPES(efa.outlines, ~context)
plot_MSHAPES(ms.4, size = 0.75)

# temporal
ms.5 <- MSHAPES(efa.outlines, ~temporal)
plot_MSHAPES(ms.5, size = 0.75)

# contemp
ms.5 <- MSHAPES(efa.outlines, ~contemp)
plot_MSHAPES(ms.5, size = 0.75)

# contempraw
ms.6 <- MSHAPES(efa.outlines, ~contempraw)
plot_MSHAPES(ms.6, size = 0.75)
