## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "ragg_png"
)

library("ragg")
library("hypoimg")

## ----install, eval = FALSE----------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("k-hench/hypoimg")

## ----hypoGallery, echo = FALSE, out.width = "95%", fig.width = 12-------------
ggplot(tibble(x = c(0,5.2), y = c(0,0)),aes(x = x, y = y))+
  geom_blank()+theme_void()+
  coord_fixed()+
  hypo_anno_r('puella', xmin = 0, xmax = 1)+
  hypo_anno_l('providencianus', xmin = 1.2, xmax = 2.2)+
  hypo_anno_l('indigo', xmin = 2.2, xmax = 3.2)+
  hypo_anno_l('gummigutta', xmin = 3.2, xmax = 4.2)+
  hypo_anno_l('unicolor', xmin = 4.2, xmax = 5.2)+
  scale_y_continuous(limits = c(-.2,.2))+
  theme(panel.background = element_rect())

## ----hypoTibble---------------------------------------------------------------
hypo_img


## ----pureAnno, out.width = "95%", out.height = "90%", fig.width = 9-----------
ggplot(tibble(x = c(3, 3.25, 3.75),y = c(-.1, -.3, .3)),
       aes(x = x, y = y))+
  geom_line(size = 4, color = "#35B779")+
  hypo_anno_r('puella', xmin = 0, xmax = 1)+
  hypo_anno_l('indigo', xmin = 1.5, xmax = 2.5)+
  coord_fixed()+
  scale_x_continuous(limits = c(0, 3.75))+
  scale_y_continuous(limits = c(-.35, .35))

## ----singleHypoBlock, out.width = "95%", fig.width = 12-----------------------
hypo_anno_single(species = 'aberrans',
                 circle_fill = "#35B779", circle_color = 'black',
                 plot_names = TRUE, plot_name_size = 5)

## ----pairHypoBlock, out.width = "95%", fig.width = 12-------------------------
hypo_anno_pair(left = 'ecosur', right = 'maya',
               circle_fill = "#F16913", circle_color = 'black',
               plot_names = TRUE, plot_name_size = 5)

## ----singleHypoLegend, out.width = "85%", fig.width = 8,fig.height = 8--------
set.seed(5)

# For this demonstration we'll create a fake length - weight reltionship data table.
# To do this, we'll implement the relationship as stated by fishbase for H. puella:
# http://www.fishbase.se/summary/Hypoplectrus-puella.html
hamlet_length_weight <- function(x, a = 0.009, b = 3.04 , rand = 0){
  a * x^b + rnorm(length(x)) * rand
}

# The species involved in the fake data set
hamlet_species <- c('nigricans', 'unicolor', 'gemma')

# The fake dataset
hamlet_data <- tibble(species = rep(hamlet_species, each = 10),
                      len = sample(5:17, replace = TRUE , size = 30)+rnorm(30)) %>%
  mutate(weig = ifelse(species == 'nigricans',
                       hamlet_length_weight(x = len, rand = 1) + rnorm(10),
                       ifelse(species == 'unicolor',
                              hamlet_length_weight(x = len, b = 3.14,rand = 1.5),
                              hamlet_length_weight(x = len, b = 2.94,rand = 1.5))))

# Our color map
clr_single <- viridis::viridis(3)

# The custom legend is created based on the species list and the color map
legend_grob_single <- hypo_legend_single(species = hamlet_species, color_map = clr_single,
                 circle_color = 'black', plot_names = TRUE) %>%
  ggplotGrob()

# We add the custom legend using ggplot2::annotation_custom()
ggplot(hamlet_data, aes(x = len, y = weig,
                        color = species, fill = species)) +
  geom_line(size = 1.2)+
  geom_point(color = "black", size = 2.5, shape = 21)+
  annotation_custom(legend_grob_single,xmin = 3,xmax = 9.5, ymin = 42)+
  labs(x = "Length (cm)",
       y = "Weight (g)")+
  scale_color_manual(values = clr_single, guide = FALSE)+
  scale_fill_manual(values = clr_single, guide = FALSE)

## ----pairedHypoLegend, out.width = "95%", fig.width = 10,fig.height = 6-------
# The species involved in the fake data set
left <- c("aberrans", "aberrans", "chlorurus")
right <- c("aberrans", "chlorurus",  "chlorurus")

# For this demonstration we'll create a fake pairing experiment with egg counts.
# this function samples counts for 15 pairings
egg_c <- function(x){sample(x,size = 15,replace = TRUE)}

# The fake dataset
pairing_data <- tibble(pair = rep(str_c(left, right, sep = "-"),
                                 each = 15),
                      pairing_egg_counts = c(egg_c(90:110),egg_c(50:95), egg_c(60:100)),
                      time_point = factor(rep(rep(c('morning','noon','evening'),
                                                 each=5),
                                             3),
                                         levels = c('morning','noon','evening')))

# Our color map
clr_pair <- RColorBrewer::brewer.pal(3,"RdBu")

# The custom legend is created based on two species lists and the color map
legend_grob_pair <- hypo_legend_pair(left = left, right = right,
                                     color_map = clr_pair,
                                     circle_color = 'black',
                                     plot_names = TRUE,
                                     circle_lwd = .5) %>%
  ggplotGrob()

# We add the custom legend using ggplot2::annotation_custom()
ggplot(pairing_data, aes(y = pairing_egg_counts,
                         x = time_point,
                         fill = pair))+
  geom_boxplot()+
  annotation_custom(legend_grob_pair,
                    xmin = 3.25, ymin = 70)+
  coord_cartesian(xlim = c(0.75,4.6))+
  labs(x = "Time point",
       y = "Egg count / pairing (n)")+
  scale_fill_manual(values = clr_pair,guide = FALSE)


## ----flagGallery, echo = FALSE, out.width = "95%", fig.width = 12-------------
ggplot(tibble(x = c(0,5.2), y = c(0,0)),aes(x = x, y = y))+
  geom_blank()+
  theme_void()+
  coord_fixed()+
  hypo_anno_flag('cuba', xmin = 0, xmax = 1)+
  hypo_anno_flag('caribbean', xmin = 2, xmax = 3)+
  hypo_anno_flag('panama', xmin = 4, xmax = 5)+
  scale_y_continuous(limits = c(-.2,.2))+
  theme(panel.background = element_rect())

## ----flaggTibble--------------------------------------------------------------
hypo_flag

## ----flagPalette, out.width = "100%", fig.width = 11, fig.height = 12---------
hypo_flag_palette()

## ----flagShowRoom, echo = FALSE, out.width = "100%", fig.width = 11, fig.height = 4----
set.seed(3)
p1 <- ggplot(data = tibble(x=seq(0,10,length.out = 50),y=sin(x)+.3*rnorm(50)),aes(x=x,y=y))+
  geom_area(fill = alpha('black',.2))+
  geom_point()+
  hypo_anno_flag('panama',xmin = 4.2,xmax = 5.2,ymin = -.5,ymax = -.05)+
  scale_x_continuous(limits = c(0,10))+coord_fixed(ratio = 3)

p2 <- hypo_anno_flag_single(geo = 'trinidad_and_tobago',
                      flag_lwd = 1, flag_line_color = 'black',
                      flag_radius_scale = 1, circle_fill = RColorBrewer::brewer.pal(3,'Blues')[3],
                      circle_color = 'black',circle_lwd = 1,plot_names = TRUE)


p3 <- hypo_legend_flag_pair(left = c('panama','mexico','cuba'),
                      right = c('mexico','cuba','panama'),
                      color_map = RColorBrewer::brewer.pal(3,'PRGn'),
                 flag_lwd = 1, flag_line_color = 'black',
                 flag_radius_scale = 1,
                 circle_color = 'black', plot_names = TRUE)

cowplot::plot_grid(p1, p2, p3, nrow = 1)

## ---- echo = FALSE, fig.asp = 1, out.height = "150pt", out.width = "150pt", fig.align = "center"----
knitr::include_graphics("logo.svg")

