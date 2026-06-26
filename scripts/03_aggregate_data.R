# Los datos como tal están almacenados de manera que cada fila corresponde
# con una única fotografía. Esto es, cada dato representa un lado de 
# la estructura metálica. Este script agrega los datos de manera que cada
# fila contenga la suma total de datos para cada estructura. Lo anteior
# con el propósito de facilitar el análisis e interpretación de los datos.
# Las estructuras pequenas no tienen este inconveniente

# cargar los datos a utilizar.  
final_big <- read_rds(file = "data/processed/final_big.rds")

# remover algunos NA faltantes
final_big <- final_big |> 
  filter(!is.na(str.code))


# agregar datos
big_agg <- final_big %>%
  group_by(str.code, site, year, month) %>%
  summarise(
    
    # variables de estado / conteo → SUMA
    num.frag  = sum(num.frag, na.rm = TRUE),
    mort      = sum(mort., na.rm = TRUE),
    mort.parc = sum(mort.parc, na.rm = TRUE),
    perd      = sum(perd, na.rm = TRUE),
    blanq     = sum(blanq, na.rm = TRUE),
    
    # área → SUMA (lados de una misma estructura)
    area.cm2  = sum(area.cm2, na.rm = TRUE),
    
    # metadatos (no se agregan, solo se conservan)
    site   = first(site),
    origin = first(origin),
    digitador = first(digitador),
    
    .groups = "drop"
  )

# reasignar gira correctamente
big_agg <- big_agg |> 
    mutate(gira = make_date(year = year,
                            month = month,
                            day = 28))


# guardar los datos como objeto RDS para mantener los atributos
saveRDS(big_agg, "data/processed/big_agg.rds")
