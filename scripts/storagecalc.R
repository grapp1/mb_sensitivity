# storagecalc.R
# calculating subsurface storage over ParFlow domain
# Assumes hydrostatic pressure
storagecalc <- function(press, bot_thickness, area, porosity){
  storage <- (press+(bot_thickness/2))*area*porosity
}