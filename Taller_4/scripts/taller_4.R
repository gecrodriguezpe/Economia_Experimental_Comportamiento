#### Script Taller 4 ####

library(tidyverse)
library(readxl)
library(reticulate)

# Importación base de datos
setwd("~/Documents/GitHub/semestre6_git/economia_experimental_personal/Taller_4/bases_datos")
tabla_1 = read_excel("taller4_datos_modificada.xlsx", sheet = "tabla1")
tabla_2 = read_excel("taller4_datos_modificada.xlsx", sheet = "tabla2")
completa = read_excel("taller4_datos_modificada.xlsx", sheet = "datos")

# Variables globales ----

# Media de una loteria con 80 % de probabilidad de obtener un pago de 4000 y 20 % de probabilidad de obtener un pago de 0 
lottery_mean = 3200 
lottery_mean2 = 800


# Funciones complementarias ----

# Función para agrupar por una variable y contar el número de elementos 
# en cada grupo 
funcion_conteo = function(df, ...){
  df2 = df %>% 
    group_by(...) %>% 
    summarise(n = n())
  return(df2)
}

# Función para crear el histograma del punto 3 y del punto 7
histograma = function(df, titulo, lottery_mean, x_lab, bin_width, x_var, fill_var){
  grafica = df %>% 
    ggplot(aes(x = {{x_var}})) +
    geom_histogram(aes(fill = {{fill_var}}), binwidth = bin_width, color = "black") +
    geom_text(aes(label = scales::percent(..prop..)), 
              position = position_dodge(width = 1), 
              stat = "count", vjust = -0.8, 
              check_overlap = TRUE) +
    geom_vline(xintercept = lottery_mean, color = "black") +
    theme_light() +
    ggtitle(titulo) + 
    ylab("Número de individuos") +
    xlab(x_lab)
  return(grafica)
}

# Función para crear el histograma del punto 6
histograma2 = function(df, titulo, lottery_mean, x_lab, bin_width, x_var, color_llenado){
  grafica = df %>% 
    ggplot(aes(x = {{x_var}})) +
    geom_histogram(binwidth = bin_width, color = "black", fill = color_llenado) + 
    geom_text(aes(label = scales::percent(..prop..)), 
              position = position_dodge(width = 1), 
              stat = "count", vjust = -0.8, check_overlap = TRUE) +
    geom_vline(xintercept = lottery_mean, color = "black") +
    theme_light() +
    ggtitle(titulo) + 
    ylab("Número de individuos") +
    xlab(x_lab)
  return(grafica)
}

# 1. Punto 2 ----

# Función que determina el numero de aversos, neutrales y propensos al riesgo para la tarea 1
punto2 = function(tabla){
  # Nota: Se considerará el renglón que cambió como el punto de indiferencia 
  df = tabla[-1] # df es el mismo dataframe que se provee a la función sin la primera columna
  # variables contadoras para actitudes/preferencias frente al riesgo 
  averso = 0; propenso = 0; neutral = 0
  # Variable contadora para ver cuántas personas hacen multiple switching
  multiple_switching = 0
  # line denota cada fila
  for (line in 1:(nrow(df))){
    cont = 0
    # letter denota cada columna 
    for (letter in 1:(ncol(df))){
      if ((as.character(df[line,][letter]) == "D") && cont == 0){
        # En la columna 3 es donde se encuentra la media de la loteria
        if (letter < 3){
          propenso = propenso + 1
          cont = 1
        }else if (letter == 3){
          neutral = neutral + 1
          cont = 1
        }else if (letter > 3){
          averso = averso + 1
          cont = 1
        }
      }
      if ((as.character(df[line,][letter]) == "I") && cont == 1){
        multiple_switching = multiple_switching + 1
      }
      if (cont == 0 && letter == (ncol(df))){
        averso = averso + 1
      }
    }
  }
  resultados = tibble(averso = averso, neutral = neutral, propenso = propenso, multiple_switching = multiple_switching)
  return(resultados)
}

punto_2 = punto2(tabla = tabla_1) # Base de datos 1

# 2. Punto 3 ---- 

# Función que genera un histograma que contiene el equivalente de certeza y su actitud frente al riesgo para cada individuo
punto3 = function(tabla){
  # Creo un diccionario de python para almacenar los diferentes valores de equivalente de certeza
  equi_dict = dict("1" = 4000, "2" = 3600, "3" = 3200, "4" = 2800, "5" = 2400, "6" = 2000, 
                   "7" = 1600, "8" = 1200, "9" = 800, "10" = 400, "11" = 0, convert = TRUE)
  # Nota: Se considerará el renglón que cambió como el punto de indiferencia 
  df = tabla[-1] # df es el mismo dataframe que se provee a la función sin la primera columna
  # vector que me almancena la actitud al riesgo de cada uno de los individuos
  risk_actitude = c()
  # vector que me almacena el equivalente de certeza de cada uno de los individuos
  equi_certeza = c()
  # line denota cada fila
  for (line in 1:(nrow(df))){
    cont = 0
    # letter denota cada columna 
    for (letter in 1:(ncol(df))){
      if ((as.character(df[line,][letter]) == "D") && cont == 0){
        # Uso el diccionario para colocar el equivalente de certeza de cada individuo en un vector
        equi_certeza = append(equi_certeza, equi_dict[letter])
        # En la columna 3 es donde se encuentra la media de la loteria
        if (letter < 3){
          risk_actitude = append(risk_actitude, "propenso")
          cont = 1
        }else if (letter == 3){
          risk_actitude = append(risk_actitude, "neutral")
          cont = 1
        }else if (letter > 3){
          risk_actitude = append(risk_actitude, "averso")
          cont = 1
        }
      }
      if (cont == 0 && letter == (ncol(df))){
        equi_certeza = append(equi_certeza, 0)
        risk_actitude = append(risk_actitude, "averso")
      }
    }
  }
  # Resultados es un data frame que me almacena la info. sobre el equivalente de certeza y la actitud frente al riesgo
  resultados = tibble(iid = 1:220, risk_actitude = risk_actitude, equi_certeza = equi_certeza)
  return(resultados)
}

# Df con la información de conteo 
punto_3 = punto3(tabla = tabla_1) # Base de datos 1

# Clasificación respecto a la actitud frente al riesgo 
conteo1 = punto_3 %>% 
  funcion_conteo(risk_actitude)

# Clasificación respecto a su equivalente de certeza 
conteo2 = punto_3 %>% 
  funcion_conteo(equi_certeza) 

# Histograma distribución de equivalentes de certeza y actitud frente al riesgo
x11()
hist_punto3 = histograma(punto_3, "Distribución de equivalentes de certeza y actitud frente al riesgo",
                         lottery_mean, "Equivalentes de certeza", 400, equi_certeza, risk_actitude); hist_punto3

# 3. Punto 4 ----

# Función que determina el numero de personas que hacen multiple switching en la tabla 2 
punto4_multiple = function(tabla){
  # Nota: Se considerará el renglón que cambió como el punto de indiferencia 
  df = tabla[-1] # df es el mismo dataframe que se provee a la función sin la primera columna
  # line denota cada fila
  ind_switching = c()
  for (line in 1:(nrow(df))){
    # Defino las variables contador: cuantas veces el individuo ha hecho switch  y
    #                       switch: si el individuo en la iteración anterior se encontraba en "I" o en "D"
    cont = 0
    switch = "I"
    # letter denota cada columna 
    for (letter in 1:(ncol(df))){
      if (letter == 1){
        switch = as.character(df[line,][letter])
      }else{
        if (switch != as.character(df[line,][letter])){
          cont = cont + 1
          switch = as.character(df[line,][letter])
        }
        if (cont > 1){
          ind_switching = append(ind_switching, "multiple switch")
          break
        }
      }
    }
    if (cont <= 1){
      ind_switching = append(ind_switching, "zero or one switch")
    }
  }
  # switch_df es un data frame que me almacena si hay ocurrencia de multiple switching
  iid = 1:220
  switch_df = tibble(iid = iid, ind_switching = ind_switching)
  return(switch_df)
}

# DF con la información de quién hace multiple switching y quién no
mult_switch_4 = punto4_multiple(tabla = tabla_2)

# Conteo de cuantos individuos hacen multiple switching
conteo_multiple_switch = mult_switch_4 %>%
  funcion_conteo(ind_switching)

# Función que determina el numero de personas que empiezan en D
punto4_first_D = function(tabla){
  df = tabla[-1] # df es el mismo dataframe que se provee a la función sin la primera columna
  # Creo que un vector que almacena si la primera decisión fue en D o en I 
  begin_letter = c()
  # line denota cada fila
  for (line in 1:(nrow(df))){
    # Si la primera letra es D, entonces no maximiza utilidad
    if (as.character(df[line,][1] == "D")){
      begin_letter = append(begin_letter, "Primero es D")
    }else{
      begin_letter = append(begin_letter, "Primero es I")
    }
  }
  # primera_letra es un data frame que me almacena si la primera letra fue D o fue I
  iid = 1:220
  primera_letra = tibble(iid = iid, begin_letter = begin_letter)
  return(primera_letra)
}

first_D_4 = punto4_first_D(tabla_2)

# Conteo de cuantos individuos escogen D en su primera decisión
conteo_begin_D = first_D_4 %>% 
  funcion_conteo(begin_letter)

# 4. Punto 5 ----

# Función que determina el numero de personas que no cumplen IIA 
# por razones diferentes a: mutiple switching o a primero es D
punto_5_no_IIA_cambio_renglon = function(completa, mult_switch, begin_D){
  # Utilizo la función inner_join para unir los data frames
  first = inner_join(completa, mult_switch, by = "iid")
  todas = inner_join(first, begin_D, by = "iid")
  # filtro la base de datos de tal forma que solo considere las observaciones 
  # que no hagan multiple switching o cuya primera opción no sea D
  filtrada = todas %>%  
    filter(ind_switching != "multiple switch" & begin_letter != "Primero es D")
  # df es el mismo dataframe que se provee a la función sin la primera columna  
  df = filtrada[-1] 
  # vector que me almacena la información de si el individuo satisface IIA o no
  IIA = c()
  # line denota cada fila
  for (line in 1:(nrow(df))){
    # flag es una variable que cambia de valor si se viola IIA
    flag = TRUE
    # letter denota cada columna 
    for (letter in 1:11){  
      letter2 = letter + 11
      if (as.character(df[line,][letter]) != as.character(df[line,][letter2])){
          IIA = append(IIA, "Viola IIA")
          flag = FALSE
          break
      }
    }
    if (flag == TRUE){
      IIA = append(IIA, "Satisface IIA")
    }
  }
  # ultima es una base de datos para saber si el invidividuo satisface IIA o no
  ultima = filtrada %>%
    mutate(IIA = IIA) %>%
    select(iid, IIA)
  return(ultima)
}

violax_IIA_punto5 = punto_5_no_IIA_cambio_renglon(completa, mult_switch_4, first_D_4)

# Conteo de cuantos individuos violan IIA diferentes 
# a los que hacen multiple switching o escogen D en su primera elección
conteo_violax_IIA = violax_IIA_punto5 %>% 
  funcion_conteo(IIA)

# 5. Punto 6 ----

# Definiciones de diccionarios: 


# Diccionario para x definido como el pago en caso de extracción favorable para lo lotería de la izquierda tarea 2
dict_extrax_favorable = dict("1" = 4000, "2" = 3600, "3" = 3200, "4" = 2800, "5" = 2400, "6" = 2000, 
                                   "7" = 1600, "8" = 1200, "9" = 800, "10" = 400, "11" = 0, convert = TRUE)
  
# Función que genera un histograma que contiene el equivalente de certeza y su actitud frente al riesgo para cada individuo
punto6 = function(tabla, mult_switch, begin_D, violax_IIA, dyct){
  # Utilizo la función inner_join para unir los data frames
  first = inner_join(tabla, mult_switch, by = "iid")
  todas = inner_join(first, begin_D, by = "iid")
  # filtro la base de datos de tal forma que solo considere las observaciones 
  # que no hagan multiple switching o cuya primera opción no sea D
  filtrada = todas %>%  
    filter(ind_switching != "multiple switch" & begin_letter != "Primero es D")
  # df es el mismo dataframe que se provee a la función sin la primera columna  
  df = filtrada[-1] 
  # Creo un diccionario de python para almacenar los diferentes valores 
  # del pago en caso de extracción favorable para la loteria de la izquierda
  equi_dict = dyct
  # vector que me almancena la actitud al riesgo de cada uno de los individuos
  risk_actitude = c()
  # vector que me almacena el equivalente de certeza de cada uno de los individuos
  equi_certeza = c()
  # line denota cada fila
  for (line in 1:(nrow(df))){
    cont = 0
    # letter denota cada columna
    for (letter in 1:(ncol(df))){
      if ((as.character(df[line,][letter]) == "D") && cont == 0){
        # Uso el diccionario para colocar el equivalente de certeza de cada individuo en un vector
        equi_certeza = append(equi_certeza, equi_dict[letter])
        # En la columna 3 es donde se encuentra la media de la loteria
        if (letter < 3){
          risk_actitude = append(risk_actitude, "propenso")
          cont = 1
        }else if (letter == 3){
          risk_actitude = append(risk_actitude, "neutral")
          cont = 1
        }else if (letter > 3){
          risk_actitude = append(risk_actitude, "averso")
          cont = 1
        }
      }
      if (cont == 0 && letter == (ncol(df))){
        equi_certeza = append(equi_certeza, 0)
        risk_actitude = append(risk_actitude, "averso")
      }
    }
  }
  # Resultados es un data frame que me almacena la info. sobre el equivalente de certeza y la actitud frente al riesgo
  resultados = violax_IIA %>% 
    mutate(pago_esperado2 = equi_certeza)
  return(resultados)
}

punto_6 = punto6(tabla_2, mult_switch_4, 
                 first_D_4, violax_IIA_punto5, dict_extrax_favorable)

# Clasificación respecto al pago esperado de la lotería de la izquierda
conteo_punto6 = punto_6 %>% 
  funcion_conteo(pago_esperado2)

# Histograma de los pagos de las loterias de la izquierda en caso de extracción favorable
x11()
hist_punto6 = histograma2(punto_6, "Distribución de pagos (x) en caso de extracción favorable para\n lo lotería de la izquierda",
                         lottery_mean, "Pagos esperados para lo lotería de la izquierda", 
                         400, pago_esperado2, "green1"); hist_punto6

# 6. Punto 7 ----

# 6.1 Usando x como pago esperado de la lotería de la izquierda tarea 2 ----

# Genero la base de datos que contiene la diferencia entre el pago en caso de extracción favorable
# de la loteria de la izquierda da la tabla 2 y el equivalente de certeza de la tabla 1
punto7 = function(df1, df2){
  df = df1 %>% 
    inner_join(df2) %>% 
    mutate(diferencia = pago_esperado2 - equi_certeza) %>% 
    relocate(pago_esperado2, .before = equi_certeza) %>% 
    relocate(diferencia, .after = equi_certeza)
  return(df)
}

punto_7 = punto7(punto_3, punto_6)

punto_7_final = punto_7 %>% 
  mutate(comportamiento = ifelse(diferencia == 0, "Satisface IIA",
         "No satisface IIA"))

x11()
hist_punto7_escalado = histograma(punto_7_final, 
                                  "Distribución de la diferencia de los de pagos (x) en caso de extracción favorable \n con el certero equivalente",
                                  0, "Diferencia de los de pagos (x) en caso de extracción favorable \n con el certero equivalente", 
                                  400, diferencia, comportamiento); hist_punto7_escalado


