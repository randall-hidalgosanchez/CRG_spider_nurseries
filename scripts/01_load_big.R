# Este script carga y da formato a los bases de datos correspondientes
# con las estructuras grandes



# Cargar los datos de las estructuras grandes
blanca_big <- read_csv("data/2026_06_26_Playa_Blanca_grandes.csv")
guiri_big <- read_csv("data/2026_06_26_Guiri_grandes.csv")
pelonas_big <- read_csv("data/2026_06_26_Pelonas_grandes.csv")


# eliminar la columna spp de la base de Guiri (no esta en Blanca)
guiri_big <- guiri_big |> select(-spp)

# revisar que el nombre de las columnas sea igual en todos los datasets
names(guiri_big)==names(blanca_big)
names(guiri_big)==names(pelonas_big)


# Remover datos analizados por Patricia, Andrea C. and Carmen. 
# Los datos de Patricia fueron analizados usando una escala incorrecta
# mientras que los de Andrea y Carmen son los mismos que hizo Chiara. 
levels(as.factor(blanca_big$digitador))

blanca_big <- blanca_big |>  
  filter(!digitador %in% c("Patricia", "Andrea C.", "Carmen")) |>
  droplevels()


# Algunas etiquetas han sido reemplazadas y "str.code" representa eso
# Por lo que es necesario reasignar el numero para que haya continuidad
# y evitar errores debido a que la etiqueta dice algo como "17 (103)"
source("./Scripts/fix_structure_code.R")
blanca_big$str.code <- fix_structure(blanca_big$str.code)

# comprobar cambios
# comparacion <- data.frame(original = blanca_big$str.code,
#                           corregido = test)
# View(comprobacion)


# Para combinar las bases de datos es necesario que cada columna sea
# de la misma clase
blanca_big <- blanca_big |>  
  mutate(str.code=as.factor(str.code),
         site=as.factor(site),
         lado=as.factor(lado),
         origin=as.factor(origin),
         digitador=as.factor(digitador),
         month=as.numeric(month),
         year=as.numeric(year), 
         .keep = "unused")

guiri_big <- guiri_big |>  
  mutate(str.code=as.factor(str.code),
         site=as.factor(site),
         lado=as.factor(lado),
         origin=as.factor(origin),
         digitador=as.factor(digitador),
         month=as.numeric(month),
         year=as.numeric(year), 
         .keep = "unused")

pelonas_big <- pelonas_big |>  
  mutate(str.code=as.factor(str.code),
         site=as.factor(site),
         lado=as.factor(lado),
         origin=as.factor(origin),
         digitador=as.factor(digitador),
         month=as.numeric(month),
         year=as.numeric(year), 
         .keep = "unused")

# Unir las bases de datos
final_big <- bind_rows(blanca_big, guiri_big, pelonas_big)


# Algunos datos registrados por Gaby (y posiblemente otros mas) incluyen 
# valores demasiado altos que no deberian ser posibles. Es posible que dichos 
# datos hayan sido registrados de manera incorrecta al convertir de excel a 
# google sheets, calcular área en lugar de contar fragmentos, o algun otro 
# artefacto
cols <- c("mort.", "perd", "mort.parc", "blanq")

final_big <- final_big |> 
    filter(if_all(all_of(cols), ~ . <= 50 | is.na(.)))


# Censurar digitadores
final_n <- length(levels(final_big$digitador))
levels(final_big$digitador) <- paste("Digitador",
                                           as.character(seq(1,final_n,1)), 
                                           sep = "_")


# corregir variable gira porque diferentes personas lo registran 
# de diferente manera
final_big <- final_big |> 
  mutate(gira = make_date(year = year,
                          month = month,
                          day = 28))



# Agregar origenes, tomar en cuenta que muchas estructuras no tienen datos
# sobre el sitio de origen
source("./scripts/asign_origin.R")
final_big$origin <- asign_origin(final_big$str.code)


# guardar los datos como objeto RDS para mantener los atributos
saveRDS(final_big, "data/processed/final_big.rds")

