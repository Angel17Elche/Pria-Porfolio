title: "Practica RPS V3"
author: "Jose García, Adrian Lozano, Javier Ortega, Ángel García"
format: html
editor: visual

obtener_eleccion_usuario_v3 <- function() {
  eleccion <- tolower(readline("Introduzca piedra (R), papel (P) o tijera (S): "))
  return(eleccion)
}

obtener_eleccion_bot_v3 <- function(jugador_identificador, eleccion_usuario, num_jugadas) {
  opciones <- c("r", "p", "s")
  
  if (file.exists(paste0(jugador_identificador, "_historial.csv"))) {
    historial <- read.csv(paste0(jugador_identificador, "_historial.csv"), header = TRUE)
    
    if (num_jugadas <= 3) {
      ultimas_jugadas <- head(historial$Eleccion, num_jugadas)
      eleccion_bot <- obtener_mejor_contra_juego(ultimas_jugadas)
    } else {
      perfil <- table(historial$Eleccion)
      eleccion_bot <- obtener_mejor_contra_juego_perfil(perfil)
    }
  } else {
    eleccion_bot <- sample(opciones, 1)
  }
  
  return(eleccion_bot)
}

obtener_mejor_contra_juego <- function(ultimas_jugadas) {
  opciones <- c("r", "p", "s")
  return(sample(opciones, 1))
}

obtener_mejor_contra_juego_perfil <- function(perfil) {
  eleccion_contrajuego <- switch(
    names(which.max(perfil)),
    r = "p",  
    p = "s",  
    s = "r",  
    "r"  
  )
  return(eleccion_contrajuego)
}

determinar_resultado_v3 <- function(usuario, bot) {
  if (usuario == bot) {
    return("Empate")
  } else if (
    (usuario == "r" && bot == "s") ||
    (usuario == "p" && bot == "r") ||
    (usuario == "s" && bot == "p")
  ) {
    return("¡Ganaste!")
  } else {
    return("¡El bot gana!")
  }
}

obtener_identificador_jugador <- function() {
  identificador <- readline("Introduzca su identificador de jugador: ")
  return(identificador)
}

ver_historial_v3 <- function(jugador_identificador) {
  if (!file.exists("historial_detalle.csv")) {
    cat("No hay historial disponible.\n")
    return()
  }
  
  historial_detalle <- read.csv("historial_detalle.csv", header = TRUE)
  historial_jugador <- subset(historial_detalle, Jugador == jugador_identificador)
  
  if (nrow(historial_jugador) == 0) {
    cat("No se encontraron registros para el jugador.\n")
    return()
  }
  
  victorias <- sum(historial_jugador$Resultado == "¡Ganaste!")
  derrotas <- sum(historial_jugador$Resultado == "¡El bot gana!")
  empates <- sum(historial_jugador$Resultado == "Empate")
  total_juegos <- nrow(historial_jugador)
  
  cat("Estadísticas para ", jugador_identificador, ":\n")
  cat("Victorias: ", victorias, "\n")
  cat("Derrotas: ", derrotas, "\n")
  cat("Empates: ", empates, "\n")
  cat("Proporción victorias/total: ", victorias/total_juegos, "\n")
  cat("Proporción derrotas/total: ", derrotas/total_juegos, "\n")
  cat("Proporción empates/total: ", empates/total_juegos, "\n")
}


actualizar_historial_v3 <- function(jugador_identificador, eleccion_usuario, eleccion_bot, resultado) {
  if (!file.exists("historial_detalle.csv")) {
    # Crear un data frame vacío con las mismas columnas que 'nueva_entrada'
    historial_detalle <- data.frame(Jugador = character(), Eleccion = character(), EleccionBot = character(), Resultado = character(), stringsAsFactors = FALSE)
  } else {
    historial_detalle <- read.csv("historial_detalle.csv", header = TRUE)
  }
  
  nueva_entrada <- data.frame(Jugador = jugador_identificador, Eleccion = eleccion_usuario, EleccionBot = eleccion_bot, Resultado = resultado, stringsAsFactors = FALSE)
  historial_detalle <- rbind(historial_detalle, nueva_entrada)
  
  write.csv(historial_detalle, "historial_detalle.csv", row.names = FALSE)
}

jugar_piedra_papel_tijeras_v3 <- function() {
  cat("Bienvenido a Piedra, Papel, Tijeras (Versión 3)\n")
  cat("Introduzca piedra (R), papel (P) o tijera (S)\n")
  cat("Instrucciones: q (salir), h (histórico de la partida), c (créditos)\n")
  
  usuario_gana <- 0
  bot_gana <- 0
  jugador_identificador <- obtener_identificador_jugador()
  num_jugadas <- 0
  
  while (TRUE) {
    eleccion_usuario <- obtener_eleccion_usuario_v3()
    
    if (eleccion_usuario == "q") {
      cat("Gracias por jugar.\n")
      break
    }
    
    if (eleccion_usuario == "h") {
      ver_historial_v3(jugador_identificador)
    } else if (eleccion_usuario == "c") {
      # Agregar función de créditos si es necesario
      cat("Créditos: TuNombre\n")
    } else if (eleccion_usuario %in% c("r", "p", "s")) {
      num_jugadas <- num_jugadas + 1
      eleccion_bot <- obtener_eleccion_bot_v3(jugador_identificador, eleccion_usuario, num_jugadas)
      resultado <- determinar_resultado_v3(eleccion_usuario, eleccion_bot)
      cat("Elegiste:", eleccion_usuario, "\n")
      cat("El bot eligió:", eleccion_bot, "\n")
      cat("Resultado: ", resultado, "\n\n")
      if (resultado == "¡Ganaste!") {
        usuario_gana <- usuario_gana + 1
      } else if (resultado == "¡El bot gana!") {
        bot_gana <- bot_gana + 1
      }
      
      actualizar_historial_v3(jugador_identificador, eleccion_usuario, eleccion_bot, resultado)
    } else {
      cat("Error, tecla no válida. Inténtalo de nuevo.\n")
    }
  }

}

jugar_piedra_papel_tijeras_v3()



