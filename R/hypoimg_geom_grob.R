hypo_geom_grob_custom <- ggproto(
  "hypo_geom_grob_custom",
  Geom,
  setup_data = function(self, data, params) {
    data <- ggproto_parent(Geom, self)$setup_data(data, params)
    data
  },

  draw_group = function(data, panel_scales, coord) {
    vp <- grid::viewport(x=data$x, y=data$y,h = data$height,width = data$width,angle = data$angle)
    g <- grid::editGrob(data$grob[[1]], vp=vp)
    ggplot2:::ggname("hypo_geom_grob", g)
  },

  required_aes = c("grob","x","y","height","width","angle")

)

hypo_geom_grob <-  function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = FALSE,
                         ...) {
  layer(
    geom = hypo_geom_grob_custom,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
