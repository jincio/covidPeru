#' load positive COVID cases
#'
#' Cargal la base de datos de casos confirmados desde la página de datos abiertos
#'
#' @return data_frame
#' @export
#'
#' @examples
da_positivos<-function (){
  file="https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download"
  data=readr::read_csv(file, progress = TRUE,col_names = TRUE,
                       readr::locale(encoding = "latin1"),
                       col_types = NULL)
  data1 = dplyr::mutate(data,year = substr(FECHA_RESULTADO,1,4),
                          month = substr(FECHA_RESULTADO,5,6),
                          day = substr(FECHA_RESULTADO,7,8),
                        fecha=as.Date(paste0(year,"-",month,"-",day)),
                        EDAD_n = as.numeric(EDAD),
                        semana = lubridate::epiweek(fecha))
  return(data1)
}
#' load fallecidos Covid
#'
#'Carga la base de datos de Fallecidos desde la página de datos abiertos
#'
#' @return
#' Data.frame con la información de "fallecidos" (datos abiertos). Agregando la variable fecha y semana.
#' @export
#'
#' @examples
#'
da_fallecidos<-function (){
  file="https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download"
  fallecidos=readr::read_csv(file, progress = TRUE,col_names = TRUE,
                       readr::locale(encoding = "latin1"),
                       col_types = NULL)
  fallecidos=mutate(fallecidos,year = substr(FECHA_FALLECIMIENTO,1,4),
                    month = substr(FECHA_FALLECIMIENTO,5,6),
                    day = substr(FECHA_FALLECIMIENTO,7,8),
                    fecha = as.Date(paste0(year,"-",month,"-",day)),
                    semana = lubridate::epiweek(fecha))
  return(fallecidos)
}

#' da_sinadef
#'
#' @return
#' @export
#'
#' @examples
da_sinadef<-function (){
  file="https://cloud.minsa.gob.pe/s/nqF2irNbFomCLaa/download"
  data<- readr::read_csv2(file, progress = TRUE,col_names = FALSE,
                          readr::locale(encoding = "latin1"),
                          col_types = NULL)
  cat("si lees esto es que el archivo bajo bien :)")
  cat("...limpiando el archivo")
  espacio_columna <- which(data[,1]=="Nº")[1]
  col_names <- data[espacio_columna,]
  colnames(data) <- col_names
  colnames(data)[14] <-"Year"
  inicio <- espacio_columna+1
  cat("...Eliminamos informacion vacia")
  data1 <- data[inicio:nrow(data),]
  data1 <- data1[,colSums(is.na(data1))<nrow(data1)]
  cat("...Creando variables standards")
  data1 <- data1 %>% filter(`DEPARTAMENTO DOMICILIO` != "EXTRANJERO",
                            `MUERTE VIOLENTA` %in% c("SIN REGISTRO","NO SE CONOCE")) %>%
    mutate(fecha = as.Date(FECHA),semana = lubridate::epiweek(fecha), mes = as.numeric(MES),
           year = as.numeric(Year),dia = weekdays(fecha)) %>%
    select(fecha,semana,year,dia,`DEPARTAMENTO DOMICILIO`,`PROVINCIA DOMICILIO`)
  return(data1)
}
