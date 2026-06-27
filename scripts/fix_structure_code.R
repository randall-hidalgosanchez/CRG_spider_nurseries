# función para corregir el número original de estructuras
# recibe un vector y devuelve un vector

fix_structure <- function(actual) {
    
    mapa <- c(
        "285" = "15",
        "90"  = "15",
        "283" = "96",
        "78"  = "7",
        "160" = "10",
        "84"  = "16",
        "101" = "12",
        "103" = "17",
        "17 (103)" = "17",
        "104" = "19",
        "19 (104)" = "19",
        "107" = "13",
        "109" = "174",
        "174 (109)" = "174",
        "161" = "20",
        "292" = "105",
        "293" = "175"
    )
    
    original <- mapa[actual]
    original[is.na(original) & !is.na(actual)] <- actual[is.na(original) & !is.na(actual)]
    
    original
}

