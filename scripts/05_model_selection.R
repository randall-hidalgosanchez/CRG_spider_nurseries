# En este script se comparan múltiples modelos para encontrar aquel con
# el mejor ajuste para luego continuar los análisis con él


#################################################################
# estructuras pequenas
#################################################################


# cargar datos
df <- readRDS("data/processed/big_agg.rds")



df <- clean_names(df) # limpiar nombres
df <- df |> mutate(gira = as.numeric(gira))



# probar normalidad
hist(df$area_cm2)
shapiro.test(df$area_cm2) 
# prueba normalidad, H0 = datos son normales
# debido a que p < 0.05, se rechaza. Datos no son normales


# modelar los datos
m0 <- lm(area_cm2 ~ gira + site, data = df)

m1 <- glmmTMB(area_cm2 ~ gira + site, 
              data = df, 
              family = tweedie(link = "log"))

m2 <- glmmTMB(area_cm2 ~ gira + site + (1|str_code),
              data = df,
              family = tweedie())

m3 <- glmmTMB(area_cm2 ~ poly(gira, 2) + site + (1|str_code),
              data = df,
              family = tweedie())

m4 <- glmmTMB(area_cm2 ~ poly(gira, 2) + site + (1|str_code),
              data = df,
              ziformula = ~ 1,
              family = tweedie())

m5 <- glmmTMB(area_cm2 ~ poly(gira, 2) * site + (1|str_code ),
              data = df,
              ziformula = ~ 1,
              family = tweedie())

m6 <- gam(area_cm2 ~ s(gira, k = 5) + site + s(str_code, bs = "re"),
          data = df,
          family = tw(link = "log"))

m7 <- glmmTMB(area_cm2 ~ poly(gira, 3) * site + (1|str_code),
              data = df,
              ziformula = ~ 1,
              family = tweedie())

AIC(m0,m1,m2,m3,m4,m5,m6, m7)
BIC(m0,m1,m2,m3,m4,m5,m6, m7)
# el modelo 7 (m7) presenta los mejores resultados


# revisar los supuestos del modelo
res <- DHARMa::simulateResiduals(m7)

plot(res)
DHARMa::testUniformity(res)
DHARMa::testDispersion(res)
DHARMa::testZeroInflation(res)
plot_model(m7, type = "est", transform = NULL)
plot_model(m7, type = "re", transform = NULL)



# graficar predicciones de m7

# generar grilla centrada alrededor de los datos
gira_range <- range(df$gira)
gira_mid <- mean(df$gira)
gira_sd <- sd(df$gira)

# crear grid fino para evitar sobredispersión
gira_grid <- seq(gira_mid - 2.5 * gira_sd, 
                 gira_mid + 2.5 * gira_sd, 
                 length.out = 100)

# crear datos de predicción
pred_df <- expand_grid(
    gira = gira_grid,
    site = unique(df$site),
    str_code = NA  # promedio sobre efecto aleatorio
)

# predicciones es la escala de la respuesta
pred_df$pred <- predict(m7, 
                        newdata = pred_df, 
                        type = "response",
                        re.form = NA,  # population-level prediction
                        se.fit = FALSE)

# agregar columna de fecha para el eje
pred_df <- pred_df |>
    mutate(
        gira_date = as.Date(gira, origin = "1970-01-01"),
        site = factor(site)
    )

# definir paleta
paleta <- c(
    "Playa Blanca" = "#192639",
    "Guiri" = "#4EBCB8",
    "Playa Pelonas" = "#FA9938",
    "Cacique" = "#7A5CFA"
)

# graficar
p1 <- ggplot(pred_df, aes(x = gira_date, y = pred, color = site, fill = site)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = paleta) +
    scale_fill_manual(values = paleta) +
    labs(
        x = "Fecha",
        y = "Área predicha (cm²)"
    ) +
    theme_classic() +
    theme(legend.position = "top",
          legend.title = element_blank())



# mortalidad
# removes ceros porque el offset da error con ellos
df2 <- df |> filter(num_frag>0) |> droplevels()


# modelo binomial negativo (poisson presenta sobredispersión)
# debido a que sd >> mean
mm1 <- glmmTMB(
    mort ~ gira + site + offset(log(num_frag)) + (1 | str_code),
    family = nbinom2,
    data = df2
)
performance::check_overdispersion(mm1)
summary(mm1)
DHARMa::simulateResiduals(fittedModel = mm1, plot = TRUE)

# grafico
new_data <- expand.grid(site=levels(df2$site),
                        gira = seq(as.Date("2020-12-28"),
                                   as.Date("2026-02-28"),
                                   by = "month"),
                        num_frag = seq(1, max(df2$num_frag)),
                        str_code=levels(df2$str_code))

new_data$preds <- predict(mm1, newdata = new_data, type = "response")

# Plot with ggplot2
p2 <- ggplot(new_data, aes(x = gira, y = preds, col=site)) +
    stat_summary(geom = "line", fun = "mean", linewidth = 1) +
    labs(y = "Fragmentos muertos", x = "Fecha")+
    scale_color_manual(values = paleta)+
    theme_classic()+
    theme(legend.position = "top",
          legend.title = element_blank())










#################################################################
# estructuras pequeñas
#################################################################

# cargar datos
# usar el archivo procesado correcto para estructuras pequeñas
# y mantener la estructura de observaciones por fotografía

df <- readRDS("data/processed/final_small.rds")

df <- df |>
    clean_names() |>
    filter(!is.na(area_cm2), !is.na(gira), !is.na(site), !is.na(str_code)) |>
    # usar tiempo relativo, primera fecha observada = 0, segunda = 1, etc.
    mutate(gira_rel = as.numeric(gira - min(gira), units = "days") / 30) |>
    droplevels()

# modelo mixto simple y estable para datos de área en estructuras pequeñas
# usa tiempo relativo en meses para evitar valores numéricos grandes en polinomios
m_small <- glmmTMB(
    area_cm2 ~ poly(gira_rel, 2) + site + (1 | str_code),
    data = df,
    family = tweedie(link = "log")
)

# resumen y supuestos
summary(m_small)
res_small <- DHARMa::simulateResiduals(m_small)
plot(res_small)
DHARMa::testUniformity(res_small)
DHARMa::testDispersion(res_small)
DHARMa::testZeroInflation(res_small)
plot_model(m_small, type = "est", transform = NULL)
plot_model(m_small, type = "re", transform = NULL)

# predicciones para visualizar el efecto temporal por sitio
pred_df <- expand_grid(
    gira_rel = seq(min(df$gira_rel), max(df$gira_rel), length.out = 100),
    site = levels(df$site),
    str_code = NA
)

pred_df$pred <- predict(
    m_small,
    newdata = pred_df,
    type = "response",
    re.form = NA
)

pred_df <- pred_df |>
    mutate(
        gira = min(df$gira) + round(gira_rel * 30),
        site = factor(site)
    )

p3 <- ggplot(pred_df, aes(x = gira, y = pred, color = site, group = site)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = paleta) +
    labs(
        x = "Fecha",
        y = "Área predicha (cm²)"
    ) +
    theme_classic() +
    theme(
        legend.position = "top",
        legend.title = element_blank())



# mortalidad
# mortalidad
df <- readRDS("data/processed/final_small.rds")
mort_df <- df |>
    clean_names() |>
    filter(!is.na(mort), !is.na(num_frag), num_frag > 0) |>
    mutate(gira_rel = as.numeric(gira - min(gira), units = "days") / 30) |>
    droplevels()

mm0 <- glmmTMB(mort ~ gira_rel + site + offset(log(num_frag)) + (1 | str_code),
               data = mort_df,
               family = nbinom2)

mm1 <- glmmTMB(
    mort ~ poly(gira_rel, 2) + site + offset(log(num_frag)) + (1 | str_code),
    data = mort_df,
    family = nbinom2)

mm2 <- glmmTMB(
    mort ~ gira_rel * site + offset(log(num_frag)) + (1 | str_code),
    data = mort_df,
    family = nbinom2)

# se probó pero hay problemas de convergencia
mm3 <- glmmTMB(
    mort ~ poly(gira_rel, 2) * site + offset(log(num_frag)) + (1 | str_code),
    data = mort_df,
    family = nbinom2)

AIC(mm0, mm1, mm2, mm3)
BIC(mm0, mm1, mm2, mm3)

# modelo recomendado para mortalidad en estructuras pequeñas
m_mort_small <- mm1

summary(m_mort_small)
res_mort <- DHARMa::simulateResiduals(m_mort_small)
plot(res_mort)
DHARMa::testUniformity(res_mort)
DHARMa::testDispersion(res_mort)
DHARMa::testZeroInflation(res_mort)
plot_model(m_mort_small, type = "est", transform = NULL)
plot_model(m_mort_small, type = "re", transform = NULL)

pred_mort <- expand_grid(
    gira_rel = seq(min(mort_df$gira_rel), max(mort_df$gira_rel), length.out = 100),
    site = levels(mort_df$site),
    num_frag = round(mean(mort_df$num_frag, na.rm = TRUE)),
    str_code = NA)

pred_mort$pred <- predict(
    m_mort_small,
    newdata = pred_mort,
    type = "response",
    re.form = NA)

pred_mort <- pred_mort |>
    mutate(gira = min(mort_df$gira) + round(gira_rel * 30))

p_mort_small <- ggplot(pred_mort, aes(x = gira, y = pred, color = site, group = site)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = paleta) +
    labs(x = "Fecha",
         y = "Mortandad predicha (fragmentos)") +
    theme_classic() +
    theme(legend.position = "top",
          legend.title = element_blank())

p_mort_small


