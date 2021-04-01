## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "ragg_png"
)

options(warn = - 1) 

library("ragg")
library("hypoimg")

## ---- eval = FALSE------------------------------------------------------------
#  grConvert::convertPicture("image.svg","image.c.svg")

## ----basicUsage, message = FALSE, warning = FALSE, out.width = "95%", fig.width = 12----
library(tidyverse)
library(grImport2)
library(grid)
library(gridSVG)

svg_file <- system.file("extdata", "logo.c.svg", package = "hypoimg")
svg <- hypo_read_svg(svg_file)

ggplot(tibble(x=rnorm(100),y=rnorm(100)))+
  annotation_custom(svg)+
  geom_point(aes(x=x,y=y),
             color="#AA1F00")


## ----recolorSingle, out.width = "95%", fig.width = 12, message = FALSE, warning = FALSE----
svg_new <- hypo_recolor_svg(svg, layer = 1, color = "#2B5B99")
ggplot()+
  annotation_custom(svg_new)

## ----recolorTable, out.width = "95%", fig.width = 12, fig.height = 6, message = FALSE, warning = FALSE----
n = 9
tab <- hypo_recolor_grob_table(svg,
                          LETTERS[1:n],
                          scico::scico(n, palette = 'lapaz'),
                          angle = rnorm(n)*60,
                          layer = 1)

ggplot(tibble(x=1:2),aes(x=x,y=x))+
  geom_hypo_grob(data=tab,
                 aes(grob=grob,x=x,y=y,
                     angle=angle),
                 width=.6)+
  facet_wrap(grp~.,ncol = 3)+
  theme(text=element_blank(),
        axis.ticks = element_blank())

## ----recolorMultiple, out.width = "95%", fig.width = 12, fig.height = 4, message = FALSE, warning = FALSE----
svg_file_2 <- system.file("extdata", "logo2.c.svg", package = "hypoimg")
svg_2 <- hypo_read_svg(svg_file_2)

clr <- scico::scico(25)
ggplot()+
  annotation_custom(svg_2,xmax = .5)+
  annotation_custom(svg_2 %>%
                      hypo_recolor_svg(.,layer = 1,clr[1]) %>%
                      hypo_recolor_svg(.,layer = 2,clr[8]) %>%
                      hypo_recolor_svg(.,layer = 3,clr[16]) %>%
                      hypo_recolor_svg(.,layer = 4,clr[24]),
                    xmin = .5)

## ---- echo = FALSE, fig.asp = 1, out.height = "150pt", out.width = "150pt", fig.align = "center"----
knitr::include_graphics("logo.svg")

