# Este script tiene por objetivo generar ciertos datos exploratorios y gráficos
# básicos tanto para las estructuras grandes como las pequeñas

# cargar datos dependiendo del dataset deseado
df <- readRDS("data/processed/big_agg.rds")
# df <- readRDS("data/processed/final_small.rds")

# definir nombre del dataset (para guardar los graficos) 
dataset_name <- "big"  
# dataset_name <- "small" 
out_dir <- "results/images"

# limpiar nombre de columnas
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

area_ts_plot <- df |>
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
          axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1))

ggsave(paste0(out_dir, "/area_ts_", dataset_name, ".png"),
       area_ts_plot, width = 8, height = 5)



# mortalidad
df <- df |> 
    mutate(porc_mortalidad = if_else(num_frag > 0,
                                     round((mort / num_frag) * 100, 3),
                                     NA_real_))

# mortalidad en el tiempo
mortality_ts_plot <- df |>
    group_by(gira, site) |>
    summarise(porc_mortalidad = mean(porc_mortalidad, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = gira, y = porc_mortalidad, col = site, group = site)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5, alpha = 0.7) +
    labs(x = "Fecha", y = "Mortalidad (%)") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") + 
    scale_color_manual(values = paleta) +
    theme_classic() +
    theme(legend.position = "top", legend.title = element_blank())

ggsave(paste0(out_dir, "/mortalidad_ts_", dataset_name, ".png"),
       mortality_ts_plot, width = 8, height = 5)


# mortalidad general
mortality_general <- df |>
    group_by(site) |>
    summarise(mean_mortality = mean(porc_mortalidad, na.rm = TRUE), 
              sd_mortality = sd(porc_mortalidad, na.rm = TRUE)) |>
    ggplot(aes(x = site, y = mean_mortality, fill = site)) +
    geom_col() +
    geom_errorbar(aes(ymin = mean_mortality - sd_mortality,
                      ymax = mean_mortality + sd_mortality),
                  width = 0.2) +
    scale_fill_manual(values = paleta) +
    labs(x = "Sitio", y = "Mortalidad media (%)") +
    theme_classic() +
    theme(legend.position = "none")

ggsave(paste0(out_dir, "/mortalidad_general_", dataset_name, ".png"),
       mortality_general, width = 6, height = 4)


# blanqueamiento
df <- df |> 
    mutate(porc_blanqueamiento = if_else(num_frag > 0,
                                     round((blanq  / num_frag) * 100, 3),
                                     NA_real_))


# blanqueamiento en el tiempo
blanqueamiento_ts_plot <- df |>
    group_by(gira, site) |>
    summarise(porc_blanqueamiento = mean(porc_blanqueamiento, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = gira, y = porc_blanqueamiento, col = site, group = site)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5, alpha = 0.7) +
    labs(x = "Fecha", y = "Blanqueamiento (%)") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") + 
    scale_color_manual(values = paleta) +
    theme_classic() +
    theme(legend.position = "top", legend.title = element_blank())

ggsave(paste0(out_dir, "/blanqueamiento_ts_", dataset_name, ".png"),
       blanqueamiento_ts_plot, width = 8, height = 5)

# blanquemiento general
blanqueamiento_general <- df |>
    group_by(site) |>
    summarise(mean_blanqueamiento = mean(porc_blanqueamiento, na.rm = TRUE), 
              sd_blanquemiento = sd(porc_blanqueamiento, na.rm = TRUE)) |>
    ggplot(aes(x = site, y = mean_blanqueamiento, fill = site)) +
    geom_col() +
    geom_errorbar(aes(ymin = mean_blanqueamiento - sd_blanquemiento, 
                      ymax = mean_blanqueamiento + sd_blanquemiento), 
                  width = 0.2) +
    scale_fill_manual(values = paleta) +
    labs(x = "Sitio", y = "Blanqueamiento medio (%)") +
    theme_classic() +
    theme(legend.position = "none")

ggsave(paste0(out_dir, "/blanqueamiento_general_", dataset_name, ".png"),
       blanqueamiento_general, width = 6, height = 4)
