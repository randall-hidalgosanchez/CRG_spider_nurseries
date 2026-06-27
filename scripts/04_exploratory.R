# Este script tiene por objetivo generar ciertos datos exploratorios y gráficos
# básicos tanto para las estructuras grandes como las pequeñas

# cargar datos dependiendo del dataset deseado
df <- readRDS("data/processed/big_agg.rds")
# df <- readRDS("data/processed/final_small.rds")

df <- df  |>
    clean_names()

# dimensiones del dataset
dim(df)
skim(df)

# Rango temporal
range(df$gira)

# observaciones por estructura, coincide con meses porque es una observacion por mes
df |> 
    count(str_code) |> 
    arrange(str_code)

# observaciones por región
df |> 
    count(origin)


# observaciones por sitio
df |> 
    count(site)


# observaciones por año
df |> 
    count(year)


# tiempo durante el cual cada estructura presenta datos
df |> 
    group_by(str_code) |> 
    summarise(
        min_date = min(gira),
        max_date = max(gira),
        n_meses = n_distinct(gira)
    ) |> 
    arrange(n_meses)

    
# crecimiento por sitio
paleta <- c(
    "Playa Blanca" = "#192639",
    "Guiri" = "#4EBCB8",
    "Playa Pelonas" = "#FA9938",
    "Cacique" = "#7A5CFA"
)

df |>
    group_by(gira, site) |>
    summarise(area_cm2 = mean(area_cm2, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = gira, y = area_cm2, col = site, group = site)) +
    geom_point() +
    geom_line() +
    labs(x = "Fecha", y = expression("Área (cm"^2*")")) +
    scale_color_manual(values = paleta) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") + 
    theme_classic() + 
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

