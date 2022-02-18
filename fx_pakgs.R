airtemps <- c(212, 30.3, 78, 32)
celsius1 <- (airtemps[1]-32)*5/9 #convert first temp
celsius2 <- (airtemps[2]-32)*5/9 #convert second temp
celsius3 <- (airtemps[3]-32)*5/9 #convert third temp

#' Description of fahr_to_celsius
#'
#' @param fahr Temperature in Fahrenheit as a vector
#' 
#' @return Temperature in Celsius
#' @export 
#'
#' @examples
#' fahr_to_celsius(c(212, 32))
#' 
fahr_to_celsius <- function(fahr) { #fahr is abstracted name of variable that we will ultimately pass to the function, eventually that will be airtemps that we actually
  celsius <- (fahr-32)*5/9
  return(celsius)
} 

celsius4 <- fahr_to_celsius(airtemps[1])
celsius4

airtemps_c <- fahr_to_celsius(fahr=airtemps)
airtemps_c


celsius1==celsius4 # test to check if function is working correctly

celsius <- fahr_to_celsius(airtemps)
celsius

celsius_to_fahr <- function(celsius) {
  fahr <- celsius*9/5 + 32
  return(fahr)
}

result <- celsius_to_fahr(celsius)
airtemps == result

airtemps_f<-celsius_to_fahr((celsius=airtemps_c))
airtemps==airtemps_f #check function

convert_temps <- function(fahr) {
  celsius <- (fahr-32)*5/9
  kelvin <- celsius + 273.15
  return(list(fahr=fahr, celsius=celsius, kelvin=kelvin))
}

temps_df <- data.frame(convert_temps(seq(-100,100,10)))

#' Title
#'
#' @param base_size 
#'
#' @return
#' @export
#'
#' @examples
custom_theme <- function(base_size = 9) {
  ggplot2::theme(
    axis.ticks       = ggplot2::element_blank(),
    text             = ggplot2::element_text(family = 'Helvetica', color = 'gray30', size = base_size),
    plot.title       = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, face = 'bold'),
    panel.background = ggplot2::element_blank(),
    legend.position  = 'right',
    panel.border     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = 'grey90', size = .25),
    legend.key       = ggplot2::element_rect(colour = NA, fill = NA),
    axis.line        = ggplot2::element_blank()
  )
}


ggplot(temps_df, mapping=aes(x=fahr, y=celsius, color=kelvin)) +
  geom_point() +
  custom_theme(10)
ggplot
