airtemps <- c(212, 30.3, 78, 32)
celsius1 <- (airtemps[1]-32)*5/9 #convert first temp
celsius2 <- (airtemps[2]-32)*5/9 #convert second temp
celsius3 <- (airtemps[3]-32)*5/9 #convert third temp

fahr_to_celsius <- function(fahr) { #fahr is abstracted name of variable that we will ultimately pass to the function, eventually that will be airtemps that we actually run
  celsius <- (fahr-32)*5/9
  return(celsius)
} 
