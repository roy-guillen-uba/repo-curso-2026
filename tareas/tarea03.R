# ECON-520 - Tarea 03: visualizacion con tidyverse
# Fuente: https://r4ds.hadley.nz/data-visualize.html
# Resuelvo los 23 ejercicios de 1.2.5, 1.4.3, 1.5.5 y 1.6.1.
# Ejecuto el archivo desde el principio con Source en RStudio.
# Consulto los graficos en Plots y uso sus flechas para recorrerlos.

# PREPARACION -------------------------------------------------
# Recreo la carpeta temporal si falta.
dir.create(tempdir(), recursive = TRUE, showWarnings = FALSE)

# Instalo los paquetes que me falten y luego los cargo.
for (paquete in c("tidyverse", "palmerpenguins")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete, repos = "https://cloud.r-project.org")
  }
}
library(tidyverse)
library(palmerpenguins)

# Guardo las salidas del ultimo bloque en esta carpeta.
dir.create("resultados_tarea03", showWarnings = FALSE)

# 1.2.5 - PRIMEROS PASOS --------------------------------------
cat("\nSeccion 1.2.5\n")

# 1. Consulto las dimensiones: 344 filas y 8 columnas.
print(dim(penguins))

# 2. Identifico bill_depth_mm como la profundidad del pico en mm.
# Puedo abrir la documentacion ejecutando: ?penguins
print(summary(penguins$bill_depth_mm))

# 3. Grafico las dimensiones del pico.
# Distingo agrupamientos; una sola tendencia global no resume
# adecuadamente la relacion. Mas adelante separo por especie.
p_pico <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(na.rm = TRUE)
print(p_pico)

# 4. Coloco species en y y bill_depth_mm en x.
# Los puntos se superponen en tres bandas. Prefiero un boxplot
# para comparar distribuciones de una medida entre especies.
print(ggplot(penguins, aes(x = bill_depth_mm, y = species)) +
        geom_point(na.rm = TRUE))
print(ggplot(penguins, aes(x = bill_depth_mm, y = species)) +
        geom_boxplot(na.rm = TRUE))

# 5. Reconozco que geom_point necesita x e y.
# La instruccion incompleta seria: ggplot(penguins) + geom_point()
# La corrijo agregando el mapeo de ambas coordenadas.
print(ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
        geom_point(na.rm = TRUE))

# 6. Uso na.rm = TRUE para omitir puntos con coordenadas faltantes
# sin emitir el aviso. El valor predeterminado es FALSE.
# No modifico ni borro filas de la base original.
print(sum(is.na(penguins$flipper_length_mm) | is.na(penguins$body_mass_g)))
p_aletas <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE)
print(p_aletas)

# 7. Agrego el pie solicitado a mi grafico.
p_pie <- p_aletas +
  labs(caption = "Data come from the palmerpenguins package.")
print(p_pie)

# 8. Mapeo la profundidad del pico al color solo de los puntos.
# Mantengo una curva general sin asignarle ese color.
print(ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
        geom_point(aes(color = bill_depth_mm), na.rm = TRUE) +
        geom_smooth(na.rm = TRUE))

# 9. Predigo puntos y curvas separados por isla, sin banda de
# confianza. El mapeo global del color se hereda en ambas capas.
print(ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g,
                          color = island)) +
        geom_point(na.rm = TRUE) +
        geom_smooth(se = FALSE, na.rm = TRUE))

# 10. Comparo el mapeo global con el mapeo repetido por capa.
# Obtengo el mismo grafico porque uso iguales datos y coordenadas.
p_global <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(na.rm = TRUE)
p_capas <- ggplot() +
  geom_point(data = penguins,
             aes(x = flipper_length_mm, y = body_mass_g), na.rm = TRUE) +
  geom_smooth(data = penguins,
              aes(x = flipper_length_mm, y = body_mass_g), na.rm = TRUE)
print(p_global)
print(p_capas)

# 1.4.3 - DISTRIBUCIONES --------------------------------------
cat("\nSeccion 1.4.3\n")

# 1. Al asignar species a y obtengo barras horizontales.
print(ggplot(penguins, aes(y = species)) + geom_bar())

# 2. Comparo contorno y relleno: color modifica los bordes;
# fill modifica el interior y hace mas visible el color de la barra.
print(ggplot(penguins, aes(x = species)) + geom_bar(color = "red"))
print(ggplot(penguins, aes(x = species)) + geom_bar(fill = "red"))

# 3. Con bins elijo la cantidad de intervalos del histograma.
# Comparo 10 y 50; el valor predeterminado es 30.
print(ggplot(penguins, aes(x = body_mass_g)) +
        geom_histogram(bins = 10, na.rm = TRUE))
print(ggplot(penguins, aes(x = body_mass_g)) +
        geom_histogram(bins = 50, na.rm = TRUE))

# 4. Comparo anchos de intervalo de carat en diamonds.
# Elijo 0.01 para explorar concentraciones cerca de pesos comerciales;
# 0.1 permite una vista general y 0.5 oculta detalles.
# La eleccion depende de la pregunta, no hay un unico ancho correcto.
for (ancho in c(0.01, 0.1, 0.5)) {
  print(ggplot(diamonds, aes(x = carat)) +
          geom_histogram(binwidth = ancho, boundary = 0,
                         fill = "steelblue", color = "white") +
          labs(title = paste("Ancho de intervalo:", ancho)))
}

# 1.5.5 - RELACIONES ------------------------------------------
cat("\nSeccion 1.5.5\n")

# 1. Reviso clases y valores de mpg.
# Identifico como categoricas manufacturer, model, trans, drv, fl,
# class; como numericas displ, year, cyl, cty, hwy.
# Interpreto year como anio y cyl como un conteo discreto: tambien
# puedo tratarlos como categorias si quiero comparar grupos.
# En la impresion veo <chr>, <int> y <dbl> bajo los nombres.
print(mpg)
glimpse(mpg)
# Puedo consultar los significados ejecutando: ?mpg

# 2. Uso cty como tercera variable numerica.
p_mpg <- ggplot(mpg, aes(x = displ, y = hwy))
print(p_mpg + geom_point())
print(p_mpg + geom_point(aes(color = cty)))
print(p_mpg + geom_point(aes(size = cty)))
print(p_mpg + geom_point(aes(color = cty, size = cty)))

# Obtengo una escala gradual de color y tamanos para numeros.
# Para categorias, uso colores o formas discretas; el tamanio puede
# sugerir un orden que las categorias nominales no tienen.
# No puedo asignar una variable continua directamente a shape.
# Capturo este error deliberado para continuar con los ejercicios.
tryCatch(
  print(p_mpg + geom_point(aes(shape = cty))),
  error = function(e) {
    message("Ejercicio 1.5.5.2 - error esperado: ", conditionMessage(e))
  }
)
# Para mostrar formas categoricas, uso el tipo de traccion.
print(p_mpg + geom_point(aes(shape = drv)))

# 3. Pruebo linewidth. No cambia el tamanio de los puntos:
# geom_point utiliza size; linewidth corresponde a geometrias de linea.
# Segun la version puede aparecer un aviso de estetica ignorada.
print(p_mpg + geom_point(aes(linewidth = cty)))

# 4. Represento una misma variable con color y forma.
# Refuerzo la identificacion de grupos sin agregar otra variable.
print(p_mpg + geom_point(aes(color = drv, shape = drv)))

# 5. Separo las dimensiones del pico por especie.
# Reconozco una asociacion positiva dentro de cada especie, que
# resulta poco clara al mezclar especies con medidas diferentes.
print(p_pico + aes(color = species))
print(p_pico + aes(color = species) + facet_wrap(~species))

# 6. Reproduzco dos leyendas con titulos distintos: Species y species.
p_leyendas <- ggplot(penguins,
                     aes(x = bill_length_mm, y = bill_depth_mm,
                         color = species, shape = species)) +
  geom_point(na.rm = TRUE)
print(p_leyendas + labs(color = "Species"))
# Las uno asignando el mismo titulo a color y shape.
print(p_leyendas + labs(color = "Species", shape = "Species"))

# 7. En el primer grafico respondo que proporcion de cada isla
# corresponde a cada especie. Mi denominador es el total de la isla.
print(ggplot(penguins, aes(x = island, fill = species)) +
        geom_bar(position = "fill") +
        labs(y = "Proporcion dentro de cada isla"))
# En el segundo respondo como se distribuye cada especie entre
# islas. Mi denominador es el total de esa especie.
print(ggplot(penguins, aes(x = species, fill = island)) +
        geom_bar(position = "fill") +
        labs(y = "Proporcion dentro de cada especie"))

# 1.6.1 - EXPORTACION -----------------------------------------
cat("\nSeccion 1.6.1\n")

# 1. Muestro primero las barras y despues la dispersion.
# Al no indicar plot, ggsave guarda el ultimo grafico: cty vs hwy.
print(ggplot(mpg, aes(x = class)) + geom_bar())
print(ggplot(mpg, aes(x = cty, y = hwy)) + geom_point())
ggsave("resultados_tarea03/mpg-plot.png", width = 7, height = 5, dpi = 300)

# 2. Cambio la extension a .pdf para guardar en ese formato.
# Consulto ?ggsave para ver los dispositivos y formatos disponibles.
# Entre ellos encuentro png, pdf, jpeg, tiff, bmp y svg;
# algunos dispositivos requieren paquetes o soporte adicional.
ggsave("resultados_tarea03/mpg-plot.pdf", width = 7, height = 5)

cat("\nFin del script tarea03.R\n")
cat("Carpeta de salida:", normalizePath("resultados_tarea03"), "\n")
