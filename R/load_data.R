#' Load positive COVID cases
#'
#' Cargal la base de datos de casos confirmados desde la página de datos abiertos
#'
#' @return data_frame
#' @export
#'
#' @examples
da_positivos<-function (){
  file="https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download"
  data=data.table::fread(file,encoding="Latin-1")
  data1 = dplyr::mutate(data,year = substr(FECHA_RESULTADO,1,4),
                          month = substr(FECHA_RESULTADO,5,6),
                          day = substr(FECHA_RESULTADO,7,8),
                        fecha=as.Date(paste0(year,"-",month,"-",day)),
                        EDAD_n = as.numeric(EDAD),
                        semana = lubridate::epiweek(fecha))
  return(data1)
}
#' Load fallecidos Covid
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
  data=data.table::fread(file,encoding="Latin-1")
  fallecidos=dplyr::mutate(data,year = substr(FECHA_FALLECIMIENTO,1,4),
                           month = substr(FECHA_FALLECIMIENTO,5,6),
                           day = substr(FECHA_FALLECIMIENTO,7,8),
                           fecha = as.Date(paste0(year,"-",month,"-",day)),
                           semana = lubridate::epiweek(fecha))
  return(fallecidos)
}

#' da_sinadef
#' Load death data from SINADEF
#'
#' @return
#' @export
#'
#' @examples
da_sinadef<-function (){
  file="https://cloud.minsa.gob.pe/s/nqF2irNbFomCLaa/download"
  data=data.table::fread(file,encoding="Latin-1")
  cat("si lees esto es que el archivo bajo bien :)")
  cat("...limpiando el archivo")
  espacio_columna <- which(data[,1]=="Nº")[1]
  col_names <- data[espacio_columna,]
  colnames(data) <- dplyr::as_tibble(t(col_names))$V1
  colnames(data)[14] <-"Year"
  inicio <- espacio_columna+1
  cat("...Eliminamos informacion vacia")
  data1 <- data[inicio:nrow(data),]
  data1 <- data1 %>% dplyr::select_if(~sum(!is.na(.)) > 0)
  cat("...Creando variables standards")
  data1 <- data1 %>% dplyr::filter(`DEPARTAMENTO DOMICILIO` != "EXTRANJERO",
                            `MUERTE VIOLENTA` %in% c("SIN REGISTRO","NO SE CONOCE")) %>%
    dplyr::mutate(fecha = as.Date(FECHA),semana = lubridate::epiweek(fecha), mes = as.numeric(MES),
           year = as.numeric(Year),dia = weekdays(fecha)) %>%
    dplyr::select(fecha,semana,year,dia,`DEPARTAMENTO DOMICILIO`,`PROVINCIA DOMICILIO`)
  return(data1)
}



#' Load Vaccinated people in Peru
#'
#' Cargal la base de datos de vacunas aplicadas desde la  página de datos abiertos
#'
#' @return
#' Data.frame con la información  de "vacunados" (datos abiertos). Agregando la variable fecha y semana.
#' @export
#'
#' @examples
da_vacunados<-function (){
  file = "https://cloud.minsa.gob.pe/s/ZgXoXqK2KLjRLxD/download"
  data = data.table::fread(file,encoding="Latin-1")
  data1= dplyr::mutate(data,year = substr(FECHA_VACUNACION,1,4),
                       month = substr(FECHA_VACUNACION,5,6),
                       day = substr(FECHA_VACUNACION,7,8),
                       fecha=as.Date(paste0(year,"-",month,"-",day)),
                       EDAD_n = as.numeric(EDAD),
                       semana = lubridate::epiweek(fecha))
  return( data1 )
}
