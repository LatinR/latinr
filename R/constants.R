
latinr_service <- function() {
  "latinr_easychair"
}

.latinr_url <- function(what) {
  switch(what,
         verify = "https://easychair.org/account/verify",
         login = "https://easychair.org/account/signin",
         latinr = "https://easychair.org/conferences/?conf=latinr2021",
         singup = "https://easychair.org/account/signup",
         submit = "https://easychair.org/conferences/submission_new_z.cgi",
         stop("Unrecognised what")
  )
}

.topics <- c("Aplicaciones de R en distintas disciplinas de la academia y la industria", 
             "Uso de R en conjunto con otros lenguajes de programaci\u00F3n y plataformas", 
             "Desarrollo de nuevos paquetes R",
             "Uso innovador de paquetes R existentes", 
             "Uso de R en la ense\u00F1anza", 
             "Iniciativas innovadoras para el aprendizaje de R",              
             "Investigaci\u00F3n reproducible usando R",              
             "An\u00E1lisis de grandes datos con R",
             "Aprendizaje autom\u00E1tico con R", 
             "Visualizaci\u00F3n de datos con R",              
             "An\u00E1lisis de redes con R", 
             "An\u00E1lisis de datos espaciales con R",
             "Uso de R para an\u00E1lisis de datos abiertos")
             
             

.submission_track <- "257342"

.topics_number <- seq(323629, 323640)

.types <- c("oral", "poster", "table", "lightning")

.types_number <- c(171277, 171278, 171277, 171279)
