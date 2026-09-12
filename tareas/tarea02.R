# ECON-520 - Tarea 02: R base
# Parte A: 26 ejercicios de W3Schools de los temas indicados en el PPT.
# Parte B: ejemplos de las diapositivas 25-34 para completar esos temas.
# El PPT remite a ejercicios dados en clase sin enumerarlos individualmente.
# Este archivo identifica los ejercicios incluidos; no presupone esa seleccion.
# Solo utiliza R base. Ejecutar desde el principio con Source en RStudio.

# PARTE A. EJERCICIOS DE W3SCHOOLS

cat("\n1. Tipos de datos\n")
# Data Types 1: identificar el tipo de una variable numerica.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_data_types1
myVar <- 30
print(class(myVar))  # numeric

# Data Types 2: consultar la clase con class().
# https://www.w3schools.com/r/exercise.asp?filename=exercise_data_types2
x <- 10.5
print(class(x))

cat("\n2. Cadenas de texto\n")
# Strings 1: asignar una cadena.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_strings1
str <- "Hello"
print(str)

# Strings 2: contar caracteres, incluidos espacio y signo de exclamacion.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_strings2
str <- "Hello World!"
print(nchar(str))  # 12

# Strings 3: buscar un caracter dentro de una cadena.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_strings3
str <- "Hello World!"
print(grepl("H", str))  # TRUE

# Strings 4: unir dos cadenas con un espacio.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_strings4
str1 <- "Hello"
str2 <- "World"
print(paste(str1, str2))

cat("\n3. Booleanos\n")
# Booleans 1: evaluar una desigualdad.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_booleans1
print(10 < 9)  # FALSE

# Booleans 2: comparar dos variables.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_booleans2
a <- 10
b <- 9
print(a > b)  # TRUE

# Booleans 3: elegir un operador que devuelva TRUE.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_booleans3
a <- 10
b <- 9
print(a > b)

cat("\n4. Operadores\n")
# Operators 1: multiplicacion.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_operators1
print(10 * 5)  # 50

# Operators 2: division.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_operators2
print(10 / 5)  # 2

# Operators 3: comparar igualdad; == no es asignacion.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_operators3
print(5 == 5)  # TRUE

cat("\n5. Condicionales\n")
# If Else 1: ejecutar una instruccion si a es mayor que b.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_if_else1
a <- 50
b <- 10
if (a > b) {
  print("Hello World")
}

# If Else 2: ejecutar una instruccion si a y b son iguales.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_if_else2
a <- 50
b <- 50
if (a == b) {
  print("Hello World")
}

# If Else 3: elegir entre dos resultados.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_if_else3
a <- 50
b <- 50
if (b == a) {
  print("Yes")
} else {
  print("No")
}

cat("\n6. Bucles while y for\n")
# Loops 1: mostrar del 1 al 5; actualizar i evita un bucle infinito.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_while_loop1
i <- 1
while (i < 6) {
  print(i)
  i <- i + 1
}

# Loops 2: salir al alcanzar 4, antes de imprimirlo (salida: 1, 2, 3).
# https://www.w3schools.com/r/exercise.asp?filename=exercise_while_loop2
i <- 1
while (i < 6) {
  print(i)
  i <- i + 1
  if (i == 4) {
    break
  }
}

# Loops 3: saltar el 3 (salida: 1, 2, 4, 5, 6).
# https://www.w3schools.com/r/exercise.asp?filename=exercise_while_loop3
i <- 0
while (i < 6) {
  i <- i + 1
  if (i == 3) {
    next
  }
  print(i)
}

# Loops 4: recorrer una secuencia con for.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_while_loop4
for (x in 1:10) {
  print(x)
}

cat("\n7. Estructuras de datos\n")
# Data Structures 1: crear un vector de texto.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_vectors1
fruits <- c("banana", "apple", "orange")
print(fruits)

# Data Structures 2: contar los elementos del vector.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_vectors2
fruits <- c("banana", "apple", "orange")
print(length(fruits))  # 3

# Data Structures 3: crear una lista.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_vectors3
thislist <- list("apple", "banana", "cherry")
print(thislist)

# Data Structures 4: crear una matriz; R llena por columnas por defecto.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_vectors4
thismatrix <- matrix(c("apple", "banana", "cherry", "orange"),
                     nrow = 2, ncol = 2)
print(thismatrix)

# Data Structures 5: array de 4 filas, 3 columnas y 2 capas.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_vectors5
# Aclaracion: dim=c(4,3,2) tiene TRES dimensiones, aunque el enunciado diga dos.
thisarray <- c(1:24)
multiarray <- array(thisarray, dim = c(4, 3, 2))
print(multiarray)
print(dim(multiarray))

# Data Structures 6: columnas con distintos tipos en un data frame.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_vectors6
Data_Frame <- data.frame(
  Training = c("Strength", "Stamina", "Other"),
  Pulse = c(100, 150, 120),
  Duration = c(60, 30, 45)
)
print(Data_Frame)

# Data Structures 7: representar categorias mediante un factor.
# https://www.w3schools.com/r/exercise.asp?filename=exercise_vectors7
music_genre <- factor(c("Jazz", "Rock", "Classic", "Classic",
                        "Pop", "Jazz", "Rock", "Jazz"))
print(music_genre)
print(levels(music_genre))

# PARTE B. EJEMPLOS DEL PPT: R BASE APLICADO A DATOS ECONOMICOS
# Fuente: E520-2026C2-Clases-03-04.pptx, diapositivas 25-34.
# Los importes y umbrales siguientes son ejemplos didacticos del PPT.

cat("\nB1. Asignacion y tipos (diapositiva 25)\n")
encuestado_id <- 1045
ingreso <- 350000.50
miembros_hogar <- 4L
estado <- "Ocupado"
busca_trabajo <- FALSE
print(class(ingreso))
print(class(miembros_hogar))
print(class(estado))
print(class(busca_trabajo))

cat("\nB2. Numeros y texto (diapositiva 26)\n")
# https://www.w3schools.com/r/r_numbers.asp
horas_trabajadas <- 40.5
edad_anios <- 28L
print(class(horas_trabajadas))
print(class(edad_anios))
print(as.integer(horas_trabajadas))  # Trunca: 40; no redondea.
print(as.numeric(edad_anios))
sector_actividad <- "Comercio"
categoria_ocupacional <- "Cuentapropista"
print(nchar(sector_actividad))
print(paste("Sector:", sector_actividad, "-", categoria_ocupacional))
print(grepl("propia", "Cuenta propia con local"))

cat("\nB3. Operadores (diapositiva 27)\n")
salario_mensual <- 450000
salario_anual <- salario_mensual * 13
print(salario_anual)  # 5850000 en el ejemplo simplificado.
es_mayor_edad <- edad_anios >= 18
es_desocupado <- estado == "Desocupado"
es_pea <- (estado == "Ocupado" | estado == "Desocupado") & edad_anios >= 16
print(es_mayor_edad)
print(es_desocupado)
print(es_pea)
print(!busca_trabajo)

cat("\nB4. if, else if y else (diapositiva 28)\n")
# Se llama categoria_ingreso: los tres grupos no constituyen deciles.
if (salario_mensual > 800000) {
  categoria_ingreso <- "Alto"
} else if (salario_mensual >= 300000) {
  categoria_ingreso <- "Medio"
} else {
  categoria_ingreso <- "Bajo"
}
print(categoria_ingreso)

cat("\nB5. while y break (diapositiva 29)\n")
meses_busqueda <- 0
while (meses_busqueda < 3) {
  print(paste("Mes", meses_busqueda, ": Buscando empleo..."))
  meses_busqueda <- meses_busqueda + 1
}
meses_busqueda <- 0
while (TRUE) {
  meses_busqueda <- meses_busqueda + 1
  if (meses_busqueda == 2) {
    print("Empleo encontrado")
    break
  }
}

cat("\nB6. for e indices (diapositiva 30)\n")
salarios_hora <- c(1500, 2200, 1800, 3100)
for (salario in salarios_hora) {
  print(salario * 8)
}
# seq_along tambien funciona si el vector esta vacio.
for (i in seq_along(salarios_hora)) {
  salarios_hora[i] <- salarios_hora[i] * 1.10
}
print(salarios_hora)

cat("\nB7. Vectores y listas (diapositiva 31)\n")
edades_hogar <- c(45, 42, 16, 12)
promedio_edad <- mean(edades_hogar)
print(promedio_edad)  # 28.75
print(edades_hogar[1])
print(edades_hogar[edades_hogar >= 18])
jefe_hogar <- list(id = 101, nombre = "Carlos",
                   edades_familia = edades_hogar, es_propietario = TRUE)
print(jefe_hogar)
print(jefe_hogar$nombre)

cat("\nB8. Matrices y arrays (diapositiva 32)\n")
datos_transicion <- c(80, 20, 15, 85)
matriz_transicion <- matrix(datos_transicion, nrow = 2, byrow = TRUE)
print(matriz_transicion)
print(matriz_transicion[1, 2])
panel_laboral <- array(1:12, dim = c(2, 2, 3))
print(panel_laboral)
print(panel_laboral[, , 2])

cat("\nB9. Data frames (diapositiva 33)\n")
microdatos <- data.frame(
  id_persona = c(1, 2, 3), edad = c(34, 19, 52),
  ingreso = c(450000, 0, 780000),
  trabajo_semana_pasada = c(TRUE, FALSE, TRUE)
)
print(microdatos)
str(microdatos)
print(summary(microdatos))
print(microdatos$ingreso)

cat("\nB10. Factores y niveles (diapositiva 34)\n")
vector_estados <- c("Ocupado", "Desocupado", "Inactivo", "Ocupado")
estado_factor <- factor(vector_estados)
print(estado_factor)
print(levels(estado_factor))
nivel_edu <- factor(c("Secundario", "Universitario", "Primario"),
                    levels = c("Primario", "Secundario", "Universitario"),
                    ordered = TRUE)
print(nivel_edu)
print(is.ordered(nivel_edu))
