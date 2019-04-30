
latinr_service <- function() {
  "latinr_easychair"
}

.latinr_url <- function(what) {
  switch(what,
         verify = "https://easychair.org/account/verify",
         login = "https://easychair.org/account/signin",
         latinr = "https://easychair.org/conferences/?conf=latinr2019",
         submit = "https://easychair.org/conferences/submission_new_z.cgi",
         stop("Unrecognised what")
  )
}

.topics <- c("Aplicaciones de R en distintas disciplinas de la academia y la industria", 
             "Desarrollo de nuevos paquetes R", "Uso de R en la ense\032anza", 
             "Investigaci\032n reproducible usando R", "Aprendizaje autom\032tico con R", 
             "An\032lisis de redes con R", "Uso de R en conjunto con otros lenguajes de programaci\032n y plataformas", 
             "Uso innovador de paquetes R existentes", "Iniciativas innovadoras para el aprendizaje de R", 
             "An\032lisis de grandes datos con R", "Visualizaci\032n de datos con R", 
             "Uso de R para an\032lisis de datos abiertos")


.submission_track <- "240492"

.topics_number <- seq(281909, 281920)

.types <- c("oral", "poster", "table")