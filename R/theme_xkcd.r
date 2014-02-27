# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2014-02-27 jue 19:42 emilio on emilio-Satellite-P100>
# =====================================================================

## fibosworld 2013. Change fonts in ggplot2, and create xkcd style graphs \url{http://fibosworld.wordpress.com/2013/02/17/change-fonts-in-ggplot2-and-create-xkcd-style-graphs/}

theme_xkcd <- function(){
  ##require(sysfonts)
  if( "xkcd.ttf" %in% sysfonts::font.files())
    sysfonts::font.add("xkcd", regular = "xkcd.ttf")
  if( "xkcd" %in% sysfonts::font.families() ) {
  theme(panel.grid.major = element_blank(),
        ##axis.ticks = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        text = element_text(size = 16, family = "xkcd"))
  } else {
    warning("Not xkcd fonts installed! See the vignette")
  theme(panel.grid.major = element_blank(),
        ##axis.ticks = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        text = element_text(size = 16))} 
}
