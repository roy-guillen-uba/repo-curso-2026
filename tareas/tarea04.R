# ECON-520 - Tarea 04
# Resuelvo 31 ejercicios de transformaciones y uniones.
# Fuentes:
# https://r4ds.hadley.nz/data-transform.html
# https://r4ds.hadley.nz/joins.html

# PREPARO LOS PAQUETES
dir.create(tempdir(), recursive = TRUE, showWarnings = FALSE)
for (paquete in c("tidyverse", "nycflights13", "maps")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete, repos = "https://cloud.r-project.org")
  }
}
library(tidyverse)
library(nycflights13)
# Uso maps para dibujar los estados.
dir.create("resultados_tarea04", showWarnings = FALSE)

# Conservo NA si no tengo valores para calcular la media.
media_valida <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# 3.2.5 - FILAS -----------------------------------------------
cat("\n3.2.5 - Filas\n")

# 1.a. Selecciono llegadas con demora de al menos 120 minutos.
llegadas_2h <- flights |> filter(arr_delay >= 120)
print(llegadas_2h)
# b. Selecciono vuelos a Houston.
houston <- flights |> filter(dest %in% c("IAH", "HOU"))
print(houston)
# c. Selecciono United, American y Delta.
tres_aerolineas <- flights |> filter(carrier %in% c("UA", "AA", "DL"))
print(tres_aerolineas)
# d. Selecciono julio, agosto y septiembre.
verano <- flights |> filter(month %in% c(7, 8, 9))
print(verano)
# e. Busco llegadas tardias sin demora de salida.
llegada_tardia <- flights |> filter(arr_delay > 120, dep_delay <= 0)
print(llegada_tardia)
# f. Busco salidas con demora >= 60 y recuperacion > 30 minutos.
recuperacion <- flights |>
  filter(dep_delay >= 60, dep_delay - arr_delay > 30)
print(recuperacion)

# 2. Ordeno por demora y por hora de salida.
print(flights |> arrange(desc(dep_delay)))
print(flights |> arrange(dep_time))
# Interpreto 2400 como medianoche al final del dia.

# 3. Calculo la velocidad media en millas por hora.
rapidos <- flights |>
  filter(air_time > 0) |>
  arrange(desc(distance / air_time * 60)) |>
  mutate(velocidad_mph = distance / air_time * 60)
print(rapidos |> select(year:day, carrier, flight, origin, dest,
                       distance, air_time, velocidad_mph))
# Reviso velocidades extremas; calculo promedios, no velocidades instantaneas.

# 4. Compruebo que hubo salidas registradas en los 365 dias.
dias_con_salida <- flights |>
  filter(!is.na(dep_time)) |>
  distinct(year, month, day) |>
  mutate(fecha = make_date(year, month, day))
calendario <- tibble(fecha = seq(as.Date("2013-01-01"),
                                as.Date("2013-12-31"), by = "day"))
print(nrow(dias_con_salida))
print(anti_join(calendario, dias_con_salida, by = "fecha"))

# 5. Conservo todos los empates en las distancias extremas.
print(flights |> slice_max(distance, n = 1, with_ties = TRUE))
print(flights |> slice_min(distance, n = 1, with_ties = TRUE))

# 6. Comparo filtrar antes y despues de ordenar.
orden_a <- flights |> filter(dest == "IAH") |> arrange(dep_delay)
orden_b <- flights |> arrange(dep_delay) |> filter(dest == "IAH")
print(identical(orden_a, orden_b))
# Obtengo el mismo resultado. Al filtrar primero, ordeno menos filas.
# Limito esta equivalencia a condiciones independientes del orden.

# 3.3.5 - COLUMNAS --------------------------------------------
cat("\n3.3.5 - Columnas\n")

# 1. Convierto HHMM a minutos: de 550 a 600 transcurren 10 minutos.
horarios <- flights |>
  mutate(
    salida_min = (dep_time %/% 100) * 60 + dep_time %% 100,
    programada_min = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100,
    diferencia_reloj = salida_min - programada_min,
    diferencia_dias = dep_delay - diferencia_reloj
  )
print(horarios |> select(dep_time, sched_dep_time, dep_delay,
                         diferencia_reloj, diferencia_dias))
# Ajusto multiplos de 1440 minutos si cambia el dia.
print(horarios |> filter(!is.na(diferencia_dias)) |>
        count(diferencia_dias, sort = TRUE))

# 2. Selecciono las mismas cuatro columnas de varias maneras.
columnas <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
print(flights |> select(dep_time, dep_delay, arr_time, arr_delay))
print(flights |> select(all_of(columnas)))
print(flights |> select(any_of(columnas)))
print(flights |> select(matches("^(dep|arr)_(time|delay)$")))
# Tambien selecciono por posicion, segun el orden actual.
print(flights |> select(4, 6, 7, 9))

# 3. Al repetir un nombre sin renombrarlo no duplico la columna.
print(flights |> select(dep_time, dep_time, dep_delay))

# 4. Uso any_of para ignorar nombres ausentes.
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
print(flights |> select(any_of(variables)))
print(flights |> select(any_of(c(variables, "columna_inexistente"))))
# Uso all_of si necesito exigir todos los nombres.

# 5. Comparo contains con y sin distincion de mayusculas.
print(flights |> select(contains("TIME")))
print(flights |> select(contains("TIME", ignore.case = FALSE)))
# Obtengo cero columnas al buscar TIME con ignore.case = FALSE.

# 6. Renombro la duracion y la llevo al principio.
print(flights |> rename(air_time_min = air_time) |> relocate(air_time_min))

# 7. Ordeno antes de seleccionar para no perder arr_delay.
print(flights |> arrange(arr_delay) |> select(tailnum))

# 3.5.7 - GRUPOS ----------------------------------------------
cat("\n3.5.7 - Grupos\n")

# 1. Comparo demoras medias de llegada y salida por aerolinea.
demoras_carrier <- flights |>
  group_by(carrier) |>
  summarise(n_total = n(), n_llegadas = sum(!is.na(arr_delay)),
            llegada_media = media_valida(arr_delay),
            salida_media = media_valida(dep_delay), .groups = "drop") |>
  arrange(desc(llegada_media))
print(demoras_carrier)
# Identifico F9 con mayor demora media de llegada: 21.92 minutos.
composicion <- flights |> count(carrier, dest, sort = TRUE)
print(composicion)
# Resto la media de cada ruta como ajuste descriptivo.
media_ruta <- flights |>
  group_by(origin, dest) |>
  summarise(media_ruta = media_valida(arr_delay), .groups = "drop")
ajuste_ruta <- flights |>
  left_join(media_ruta, by = join_by(origin, dest)) |>
  group_by(carrier) |>
  summarise(exceso_medio = media_valida(arr_delay - media_ruta),
            n = sum(!is.na(arr_delay)), .groups = "drop") |>
  arrange(desc(exceso_medio))
print(ajuste_ruta)
# No separo efectos causales: las rutas, horarios y condiciones difieren.

# 2. Busco la mayor demora por destino.
maximos_destino <- flights |>
  filter(!is.na(dep_delay)) |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1, with_ties = TRUE) |>
  ungroup()
print(maximos_destino)

# 3. Agrupo por hora programada para evitar el desplazamiento por demora.
demoras_hora <- flights |>
  group_by(hour) |>
  summarise(demora_media = media_valida(dep_delay),
            n_validos = sum(!is.na(dep_delay)), .groups = "drop")
print(demoras_hora)
p_hora <- ggplot(demoras_hora, aes(x = hour, y = demora_media)) +
  geom_line() + geom_point() +
  labs(x = "Hora programada de salida", y = "Demora media (minutos)",
       title = "Demoras de salida a lo largo del dia") + theme_minimal()
print(p_hora)
# Encuentro mas demora hacia la tarde, con variaciones entre horas.

# 4. Pruebo n negativo con cinco filas.
ejemplo_slice <- tibble(valor = c(5, 1, 4, 2, 3))
print(ejemplo_slice |> slice_min(valor, n = -2))
print(ejemplo_slice |> slice_max(valor, n = -2))
# Con n = -2 obtengo tres filas: 1,2,3 para min y 5,4,3 para max.
# Conservo mas filas si hay empates y uso with_ties = TRUE.

# 5. Expreso count con agrupacion y resumen.
print(flights |> count(carrier, sort = TRUE))
print(flights |> group_by(carrier) |>
        summarise(n = n(), .groups = "drop") |> arrange(desc(n)))
# Uso sort = TRUE para ordenar por frecuencia descendente.

# 6. Comparo agrupacion, ordenamiento y resumen.
df <- tibble(x = 1:5, y = c("a", "b", "a", "a", "b"),
             z = c("K", "K", "L", "L", "K"))
# a. Agrupo sin cambiar las cinco filas ni su orden.
print(df |> group_by(y))
# b. Ordeno por y: x queda 1,3,4,2,5.
print(df |> arrange(y))
# c. Obtengo a = 8/3 y b = 7/2, una fila por grupo.
print(df |> group_by(y) |> summarise(mean_x = mean(x)))
# d. Obtengo (a,K)=1, (a,L)=3.5 y (b,K)=3.5; conservo grupos por y.
print(df |> group_by(y, z) |> summarise(mean_x = mean(x)))
# e. Obtengo los mismos valores sin agrupacion.
print(df |> group_by(y, z) |>
        summarise(mean_x = mean(x), .groups = "drop"))
# f. Con summarise obtengo tres filas; con mutate conservo cinco
# y agrego medias 1,3.5,3.5,3.5,3.5.
print(df |> group_by(y, z) |> summarise(mean_x = mean(x)))
print(df |> group_by(y, z) |> mutate(mean_x = mean(x)))

# 19.2.4 - CLAVES ---------------------------------------------
cat("\n19.2.4 - Claves\n")

# 1. Uno weather$origin (foranea) con airports$faa (primaria).
# Represento la relacion como muchos registros de clima por aeropuerto.
clima_aeropuertos <- weather |>
  left_join(airports |> select(faa, name), by = join_by(origin == faa))
print(clima_aeropuertos |> select(origin, name, time_hour, temp))

# 2. Uniria dest con el aeropuerto del clima a la hora de llegada.
# Ajustaria fecha y zona horaria; no usaria la hora de salida.

# 3. Busco horas locales repetidas.
horas_repetidas <- weather |>
  count(year, month, day, hour, origin) |> filter(n > 1)
print(horas_repetidas)
print(weather |> semi_join(horas_repetidas,
                           by = join_by(year, month, day, hour, origin)) |>
        transmute(origin, year, month, day, hour,
                  instante_utc = format(time_hour, tz = "UTC", usetz = TRUE)))
# Identifico la 1:00 del 3 de noviembre, repetida por el cambio horario.
# Distingo los instantes con UTC y uso origin + time_hour como clave.
print(weather |> count(origin, time_hour) |> filter(n > 1))

# 4. Creo fechas especiales con clave year + month + day.
fechas_especiales <- tribble(
  ~year, ~month, ~day, ~evento,
  2013L, 11L, 28L, "Accion de Gracias",
  2013L, 12L, 24L, "Nochebuena",
  2013L, 12L, 25L, "Navidad"
)
vuelos_fechas <- flights |>
  left_join(fechas_especiales, by = join_by(year, month, day))
print(vuelos_fechas |> count(evento))
# Tambien uniria weather por fecha. No deduzco menor demanda solo por el evento.

# 19.3.4 - UNIONES BASICAS -------------------------------------
cat("\n19.3.4 - Uniones basicas\n")
flights2 <- flights |>
  select(year:day, hour, origin, dest, tailnum, carrier,
         dep_time, dep_delay, arr_delay, time_hour)

# 1. Elijo las 48 horas con mayor demora media de salida.
# Agrupo los tres aeropuertos por hora programada del anio.
demora_instante <- flights |>
  group_by(time_hour) |>
  summarise(demora_media = media_valida(dep_delay),
            n_validos = sum(!is.na(dep_delay)), .groups = "drop") |>
  filter(n_validos > 0)
peores_48 <- demora_instante |>
  arrange(desc(demora_media), time_hour) |> slice_head(n = 48)
print(peores_48, n = 48)
# Consulto el clima por aeropuerto en cada hora seleccionada.
clima_peores <- weather |>
  inner_join(peores_48, by = join_by(time_hour))
print(clima_peores |> select(origin, time_hour, demora_media,
                             precip, visib, wind_speed))

# Comparo las horas seleccionadas con el resto de horas con vuelos.
comparacion_clima <- weather |>
  semi_join(demora_instante, by = join_by(time_hour)) |>
  mutate(grupo = if_else(time_hour %in% peores_48$time_hour,
                         "Peores 48", "Resto")) |>
  group_by(grupo) |>
  summarise(n_registros = n(),
            lluvia_media = media_valida(precip),
            visibilidad_media = media_valida(visib),
            viento_medio = media_valida(wind_speed), .groups = "drop")
print(comparacion_clima)
# Encuentro mas lluvia y viento y menor visibilidad en las peores horas.
# No deduzco causalidad. Reviso n_validos; excluyo demoras faltantes.
p_clima <- ggplot(clima_peores,
                   aes(x = precip, y = demora_media, color = origin)) +
  geom_point(na.rm = TRUE) +
  labs(x = "Precipitacion (pulgadas)", y = "Demora media de la hora (min)",
       color = "Origen", title = "Clima en las 48 horas con mayor demora") +
  theme_minimal()
print(p_clima)

# 2. Selecciono vuelos a los diez destinos mas frecuentes.
top_dest <- flights2 |> count(dest, sort = TRUE) |> head(10)
vuelos_top10 <- flights2 |> semi_join(top_dest, by = join_by(dest))
print(top_dest)
print(vuelos_top10)
# Uso semi_join para filtrar sin duplicar filas ni agregar columnas.

# 3. Busco vuelos sin clima para su origen y hora programada.
sin_clima <- flights2 |>
  anti_join(weather, by = join_by(origin, time_hour))
print(nrow(sin_clima))
print(sin_clima |> count(origin))
# Cuento tambien las salidas registradas.
print(sin_clima |> filter(!is.na(dep_time)) |> count(origin))
# Encuentro vuelos sin coincidencia meteorologica.

# 4. Busco matriculas sin registro en planes.
sin_avion <- flights2 |> anti_join(planes, by = join_by(tailnum))
faltantes_carrier <- sin_avion |>
  count(carrier, sort = TRUE) |>
  mutate(porcentaje_vuelos = 100 * n / sum(n))
print(faltantes_carrier)
print(sin_avion |> distinct(tailnum, carrier) |> count(carrier, sort = TRUE))
print(sin_avion |> summarise(n_tailnum_na = sum(is.na(tailnum))))
# Identifico AA y MQ en cerca del 91% de los vuelos sin coincidencia.
# Distingo vuelos de matriculas y no deduzco la causa del faltante.

# 5. Agrego las aerolineas de cada avion.
carriers_avion <- flights |>
  filter(!is.na(tailnum)) |>
  distinct(tailnum, carrier) |>
  group_by(tailnum) |>
  summarise(carriers = paste(sort(carrier), collapse = ", "),
            n_carriers = n(), .groups = "drop")
planes_carriers <- planes |>
  left_join(carriers_avion, by = join_by(tailnum))
print(planes_carriers)
print(carriers_avion |> filter(n_carriers > 1))
# Rechazo la relacion unica: encuentro matriculas con varias aerolineas.

# 6. Renombro antes de unir para distinguir origen y destino.
coordenadas_origen <- airports |>
  transmute(origin = faa, lat_origen = lat, lon_origen = lon)
coordenadas_destino <- airports |>
  transmute(dest = faa, lat_destino = lat, lon_destino = lon)
vuelos_coordenadas <- flights |>
  left_join(coordenadas_origen, by = join_by(origin)) |>
  left_join(coordenadas_destino, by = join_by(dest))
print(vuelos_coordenadas |> select(origin, dest, lat_origen, lon_origen,
                                   lat_destino, lon_destino))
stopifnot(nrow(vuelos_coordenadas) == nrow(flights))
# Conservo NA si falta el aeropuerto.

# 7. Calculo demoras por destino y agrego coordenadas.
demora_destino <- flights |>
  group_by(dest) |>
  summarise(demora_media = media_valida(arr_delay),
            n_validos = sum(!is.na(arr_delay)), .groups = "drop") |>
  left_join(airports |> select(faa, name, lat, lon), by = join_by(dest == faa))
print(demora_destino |> arrange(desc(demora_media)))
print(demora_destino |> filter(is.na(lat) | is.na(lon)))
# Muestro EE.UU. continental; conservo los demas destinos en la tabla.
estados <- map_data("state")
mapa_anual <- ggplot() +
  geom_polygon(data = estados, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "white", linewidth = 0.2) +
  geom_point(data = demora_destino,
             aes(x = lon, y = lat, color = demora_media), size = 2.5,
             na.rm = TRUE) +
  scale_color_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                         midpoint = 0, na.value = "grey50") +
  coord_quickmap(xlim = c(-125, -66), ylim = c(24, 50)) +
  labs(title = "Demora media de llegada por destino - 2013",
       color = "Minutos", x = NULL, y = NULL) + theme_minimal()
print(mapa_anual)

# 8. Comparo el 13 de junio con el anio.
vuelos_13jun <- flights |> filter(month == 6, day == 13)
print(vuelos_13jun |>
        summarise(n = n(), sin_salida = sum(is.na(dep_time)),
                  demora_salida = media_valida(dep_delay),
                  demora_llegada = media_valida(arr_delay)))
# Obtengo 45.79 minutos de demora de salida y 63.75 de llegada.
demora_13jun <- vuelos_13jun |>
  group_by(dest) |>
  summarise(demora_media = media_valida(arr_delay),
            n_validos = sum(!is.na(arr_delay)), .groups = "drop") |>
  left_join(airports |> select(faa, name, lat, lon), by = join_by(dest == faa))
print(demora_13jun |> arrange(desc(demora_media)))
mapa_13jun <- ggplot() +
  geom_polygon(data = estados, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "white", linewidth = 0.2) +
  geom_point(data = demora_13jun,
             aes(x = lon, y = lat, color = demora_media), size = 2.5,
             na.rm = TRUE) +
  scale_color_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                         midpoint = 0, na.value = "grey50") +
  coord_quickmap(xlim = c(-125, -66), ylim = c(24, 50)) +
  labs(title = "Demora media por destino - 13 de junio de 2013",
       subtitle = "Vuelos desde Nueva York; vista continental",
       color = "Minutos", x = NULL, y = NULL) + theme_minimal()
print(mapa_13jun)
# Fijo la misma escala de colores para comparar mapas.
limites_color <- range(c(demora_destino$demora_media,
                         demora_13jun$demora_media), na.rm = TRUE)
escala_comun <- scale_color_gradient2(
  low = "steelblue", mid = "white", high = "firebrick",
  midpoint = 0, limits = limites_color, na.value = "grey50")
# Reemplazo la escala anterior; puedo recibir un aviso de ggplot.
mapa_anual <- mapa_anual + escala_comun
mapa_13jun <- mapa_13jun + escala_comun
print(mapa_anual)
print(mapa_13jun)
# Relaciono las demoras con las tormentas severas de esos dias.
# No atribuyo cada demora al clima: el mapa muestra destinos.
# Consulto estas fuentes:
# https://en.wikipedia.org/wiki/June_12%E2%80%9313%2C_2013_derecho_series
# https://www.spc.noaa.gov/climo/reports/130613_rpts.html

# GUARDO MIS RESULTADOS
write_csv(demoras_carrier, "resultados_tarea04/demoras_carrier.csv")
write_csv(demoras_hora, "resultados_tarea04/demoras_hora.csv")
write_csv(peores_48, "resultados_tarea04/peores_48_horas.csv")
write_csv(comparacion_clima, "resultados_tarea04/comparacion_clima.csv")
write_csv(demora_destino, "resultados_tarea04/demora_destino.csv")
write_csv(demora_13jun, "resultados_tarea04/demora_13jun.csv")
ggsave("resultados_tarea04/demoras_hora.png", p_hora,
       width = 8, height = 5, dpi = 300)
ggsave("resultados_tarea04/clima_peores_horas.png", p_clima,
       width = 8, height = 5, dpi = 300)
ggsave("resultados_tarea04/mapa_anual.png", mapa_anual,
       width = 10, height = 6, dpi = 300)
ggsave("resultados_tarea04/mapa_13jun.png", mapa_13jun,
       width = 10, height = 6, dpi = 300)
cat("\nFin del script tarea04.R\n")
cat("Carpeta de salida:", normalizePath("resultados_tarea04"), "\n")
