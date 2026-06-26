# función para asignar el origen de los fragmentos
# recibe un vector y devuelve un vector
asign_origin <- function(code) {
  
  mapa <- c(
    "6"="Matapalo","7"="Matapalo","8"="Matapalo","9"="Matapalo","10"="Matapalo",
    "171"="Matapalo","172"="Matapalo","175"="Matapalo",
    
    "11"="Jicaro","12"="Jicaro","13"="Jicaro","14"="Jicaro",
    "15"="Jicaro","16"="Jicaro","17"="Jicaro",
    
    "18"="Marina","19"="Marina","20"="Marina",
    "173"="Marina","174"="Marina","169"="Marina","170"="Marina"
  )
  
  origen <- mapa[code]
  origen[is.na(origen) & !is.na(code)] <- NA
  origen
}