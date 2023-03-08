
library(tidyverse)
library(readxl)

plot <- read_excel(
  path = "data/data-NFI-training.xlsx", 
  sheet = "plot"
  )

subplot <- read_excel(
  path = "data/data-NFI-training.xlsx", 
  sheet = "subplot"
)

lf <- read_excel(
  path = "data/data-NFI-training.xlsx",
  sheet = "land_feature"
)

tree <- read_excel(
  path = "data/data-NFI-training.xlsx",
  sheet = "tree"
)

## Check data type
summary(tree)

## Mutate tree table
tree2 <- tree %>%
  mutate(
    tree_distance     = as.numeric(tree_distance),
    tree_azimuth      = as.numeric(tree_azimuth),
    tree_dbh          = as.numeric(tree_dbh),
    tree_total_height = as.numeric(tree_total_height),
    subplot_id = paste(plot_no, subplot_no, sep = "_"),
    check_distance = if_else(tree_distance <= 12, "M", "L")
    )

summary(tree$tree_distance)
summary(tree2$tree_distance)
summary(tree2)

## Visualization
ggplot(tree2, aes(x = tree_no, y = tree_dbh)) +
  geom_point()

ggplot(tree2, aes(x = tree_no, y = tree_dbh)) +
  geom_point(aes(color = plot_no), size = 2)

## Make it pretty
ggplot(tree2, aes(x = tree_no, y = tree_dbh)) +
  geom_point(aes(color = subplot_id), size = 2) +
  theme_bw() +
  labs(
    x = "Numeru ai",
    y = "Diametru ai (cm)",
    color = "Numeru parcela"
  )

## TREE DBH against TREE DISTANCE
ggplot(tree2, aes(x = tree_distance, y = tree_dbh)) +
  geom_point(aes(color = subplot_id), size = 2) +
  geom_abline(aes(intercept = 30, slope = 0), col = "red") +
  theme_bw()

ggplot(tree2, aes(x = tree_distance, y = tree_dbh)) +
  geom_text(aes(color = subplot_id, label = tree_no), size = 4) +
  geom_abline(aes(intercept = 30, slope = 0), col = "red") +
  theme_bw()

ggplot(tree2, aes(x = tree_distance, y = tree_dbh)) +
  geom_label(aes(color = subplot_id, label = tree_no), size = 4) +
  geom_abline(aes(intercept = 30, slope = 0), col = "red") +
  theme_bw()

## TREE MAPS
ggplot(tree2, aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(size = tree_dbh, color = lf_no)) +
  coord_polar(theta = "y", start = 0) +
  scale_x_continuous(breaks = c(0, 12, 25)) +
  scale_y_continuous(
    breaks = c(0, 90, 180, 270), 
    labels = c("N", "E", "S", "W"),
    limits = c(0, 360)
  ) +
  theme_linedraw() +
  facet_grid(plot_no ~ subplot_no)
  #facet_wrap(~subplot_id, nrow = 2)

## MAP FOR ONE SUBPLOT
tt <- tree2 %>%
  filter(subplot_id == "T1_C")

ggplot(tt, aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(size = tree_dbh, color = check_distance)) +
  coord_polar(theta = "y", start = 0) +
  scale_x_continuous(breaks = c(0, 12, 25)) +
  scale_y_continuous(
    breaks = c(0, 90, 180, 270), 
    labels = c("N", "E", "S", "W"),
    limits = c(0, 360)
  ) +
  theme_linedraw() +
  facet_grid(plot_no ~ subplot_no)

## MAP WITH PLOT GEOMETRY
draw_circle <- function(xcenter,ycenter,radius){
  tt <- seq(0,2*pi,length.out = 100)
  xx <- xcenter + radius * cos(tt)
  yy <- ycenter + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}

tt <- tree2 %>% 
  filter(plot_no == "T2") %>%
  mutate(
    tree_x = tree_distance*cos(pi/2 - tree_azimuth * pi /180),
    tree_y = tree_distance*sin(pi/2 - tree_azimuth * pi / 180),
    tree_x_plot = case_when(
      subplot_no == "E1" ~ tree_x + 50,
      subplot_no == "E2" ~ tree_x + 100,
      TRUE ~ tree_x
    ),
    tree_y_plot = case_when(
      subplot_no == "N1" ~ tree_y + 50,
      subplot_no == "N2" ~ tree_y + 100,
      TRUE ~ tree_y
    )
    )
  

ggplot(tt, aes(x = tree_x_plot, y = tree_y_plot)) +
  geom_text(aes(label = tree_azimuth))

ggplot(tt, aes(x = tree_x_plot, y = tree_y_plot)) +
  geom_point(aes(size = tree_dbh, color = check_distance)) +
  geom_path(
    data = draw_circle(xcenter = 0, ycenter = 0, radius = 12), aes(x = x, y = y)
  ) +
  geom_path(
    data = draw_circle(xcenter = 0, ycenter = 0, radius = 25), aes(x = x, y = y)
    ) +
  geom_path(
    data = draw_circle(xcenter = 50, ycenter = 0, radius = 12), aes(x = x, y = y)
  ) +
  geom_path(
    data = draw_circle(xcenter = 50, ycenter = 0, radius = 25), aes(x = x, y = y)
  ) +
  geom_path(
    data = draw_circle(xcenter = 100, ycenter = 0, radius = 12), aes(x = x, y = y)
  ) +
  geom_path(
    data = draw_circle(xcenter = 100, ycenter = 0, radius = 25), aes(x = x, y = y)
  ) +
  geom_path(
    data = draw_circle(xcenter = 0, ycenter = 50, radius = 12), aes(x = x, y = y)
  ) +
  geom_path(
    data = draw_circle(xcenter = 0, ycenter = 50, radius = 25), aes(x = x, y = y)
  ) +
  geom_path(
    data = draw_circle(xcenter = 0, ycenter = 100, radius = 12), aes(x = x, y = y)
  ) +
  geom_path(
    data = draw_circle(xcenter = 0, ycenter = 100, radius = 25), aes(x = x, y = y)
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_fixed() +
  labs(x = "", y = "")



