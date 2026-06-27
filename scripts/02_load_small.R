# Este script carga y da formato a los bases de datos correspondientes
# con las estructuras pequenas




# Cargar los datos de las estructuras pequenas
blanca_small <- read_csv("data/2026_06_26_Playa_Blanca_pequenas.csv")
cacique_small <- read_csv("data/2026_06_26_Cacique_pequenas.csv")

# remover columna lado de los datos de Cacique
cacique_small <- cacique_small |> select(-lado)

# revisar que el nombre de las columnas sea igual en todos los datasets
names(blanca_small)==names(cacique_small)

# Para combinar las bases de datos es necesario que cada columna sea
# de la misma clase
blanca_small <- blanca_small |>  
  mutate(str.code=as.factor(str.code),
         site=as.factor(site),
         origin=as.factor(origin),
         digitador=as.factor(digitador),
         month=as.numeric(month),
         year=as.numeric(year), 
         .keep = "unused")

cacique_small <- cacique_small |>  
  mutate(str.code=as.factor(str.code),
         site=as.factor(site),
         origin=as.factor(origin),
         digitador=as.factor(digitador),
         month=as.numeric(month),
         year=as.numeric(year), 
         .keep = "unused")


# Unir las bases de datos
final_small <- bind_rows(blanca_small, cacique_small)

# Censurar digitadores
digitadores <- levels(final_small$digitador)
levels(final_small$digitador)[13] <- "Saul"
final_n <- length(levels(final_small$digitador))
levels(final_small$digitador) <- paste("Digitador",
                                     as.character(seq(1,final_n,1)), 
                                     sep = "_")


# corregir variable gira porque diferentes personas lo registran 
# de diferente manera
final_small <- final_small |> 
  mutate(gira = make_date(year = year,
                          month = month,
                          day = 28))


# guardar los datos como objeto RDS para mantener los atributos
saveRDS(final_small, "data/processed/final_small.rds")

