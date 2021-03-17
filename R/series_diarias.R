#' fallecidos_diarios
#'
#' @param data Data frame creado con la función da_fallecidos()
#' @param DEPARTAMENTO Departamento del Perú. Callao está listado como departamento
#' @param mediamovil Integer para calcular o graficar la media móvil. Eje: 1,2,3.
#'
#' @return
#' @export
#'
#' @examples
fallecidos_diarios<-function(data,DEPARTAMENTO=NULL,mediamovil=NA){
  if ("METODODX"%in%colnames(data)){
    stop("La base de datos es incorrecta, use la función da_fallecidos()")
  }

  if(is.null(DEPARTAMENTO)){
    titulo=paste0("Fallecidos por dia a nivel Nacional")
    data=data

    if (is.na(mediamovil))
    {
      f_dia_depa=data%>%
        dplyr::select("fecha")%>%
        group_by(fecha)%>%
        summarize(count=n())
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = count)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos diarios",title = titulo)#+
        #ggplot2::geom_line(size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    else{
      k=mediamovil
      f_dia_depa=data%>%
        dplyr::select("fecha")%>%
        group_by(fecha)%>%
        summarize(count=n()) %>%
        mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = media.movil)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos diarios",title = titulo)+
        ggplot2::geom_line(aes(x = fecha, y = count),size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)### Grafico aquí de barras con media movil?
      #return(f_dia_depa)
    }

  } else{
    DEPARTAMENTO=toupper(DEPARTAMENTO)
    titulo=paste0("Fallecidos por dia en ",DEPARTAMENTO)
    data=data
    departamento_select=DEPARTAMENTO

    if (is.na(mediamovil))
    {
      f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
        dplyr::select("DEPARTAMENTO","fecha")%>%
        group_by(DEPARTAMENTO,fecha)%>%
        summarize(count=n())
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = count)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos diarios",title = titulo)#+
        #ggplot2::geom_line(size = 1.2,colour = "darkblue")

      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    else{
      k=mediamovil
      f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
        dplyr::select("DEPARTAMENTO","fecha")%>%
        group_by(DEPARTAMENTO,fecha)%>%
        summarize(count=n()) %>% ungroup() %>%
        mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = media.movil)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos diarios",title = titulo)+
        ggplot2::geom_line(aes(x = fecha, y = count),size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    return(resultado)
  }
  return(resultado)
}

#' casos_diarios
#'
#' @param data Data frame creado con la función da_positivos()
#' @param DEPARTAMENTO Departamento del Perú. Callao está listado como departamento
#' @param mediamovil Integer para calcular o graficar la media móvil. Eje: 1,2,3.
#'
#' @return
#' @export
#'
#' @examples
casos_diarios<-function(data,DEPARTAMENTO=NULL,mediamovil=NA){
  if (!"METODODX" %in% colnames(data)){
    stop("La base de datos es incorrecta, use la función da_positivos()")
  }
  if(is.null(DEPARTAMENTO)){
    titulo=paste0("Casos nuevos por dia a nivel Nacional")
    data=data

    if (is.na(mediamovil))
    {
      f_dia_depa=data%>%
        dplyr::select("fecha")%>%
        group_by(fecha)%>%
        summarize(count=n())
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = count)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Casos diarios",title = titulo)#+
        #ggplot2::geom_line(size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    else{
      k=mediamovil
      f_dia_depa=data%>%
        dplyr::select("fecha")%>%
        group_by(fecha)%>%
        summarize(count=n()) %>%
        mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = media.movil)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Casos diarios",title = titulo)+
        ggplot2::geom_line(aes(x = fecha, y = count),size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)### Grafico aquí de barras con media movil?
      #return(f_dia_depa)
    }

  } else{
    DEPARTAMENTO=toupper(DEPARTAMENTO)
    titulo=paste0("Casos nuevos por dia en ",DEPARTAMENTO)
    data=data
    departamento_select=DEPARTAMENTO

    if (is.na(mediamovil))
    {
      f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
        dplyr::select("DEPARTAMENTO","fecha")%>%
        group_by(DEPARTAMENTO,fecha)%>%
        summarize(count=n())
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = count)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Casos diarios",title = titulo)#+
        #ggplot2::geom_line(size = 1.2,colour = "darkblue")

      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    else{
      k=mediamovil
      f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
        dplyr::select("DEPARTAMENTO","fecha")%>%
        group_by(DEPARTAMENTO,fecha)%>%
        summarize(count=n()) %>% ungroup() %>%
        mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = media.movil)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Casos diarios",title = titulo)+
        ggplot2::geom_line(aes(x = fecha, y = count),size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    return(resultado)
  }
  return(resultado)
}

#' sinadef_diarios
#'
#' @param data Data frame creado con la función da_sinadef()
#' @param DEPARTAMENTO Departamento del Perú. Callao está listado como departamento
#' @param mediamovil Integer para calcular o graficar la media móvil. Eje: 1,2,3.
#'
#' @return
#' @export
#'
#' @examples
sinadef_diarios<-function(data,DEPARTAMENTO=NULL,mediamovil=NA){
  if ("METODODX"%in%colnames(data)){
    stop("La base de datos es incorrecta, use la función da_sinadef()")
  }

  if(is.null(DEPARTAMENTO)){
    titulo=paste0("Fallecidos por dia del 2020")
    data=data

    if (is.na(mediamovil))
    {
      f_dia_depa=data%>%
        dplyr::select("fecha")%>% filter(fecha >"2020-01-01") %>%
        group_by(fecha)%>%
        summarize(count=n())%>%
        ungroup()
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = count)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos diarios",title = titulo)#+
      #ggplot2::geom_line(size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    else{
      k=mediamovil
      f_dia_depa=data%>%
        dplyr::select("fecha")%>%
        group_by(fecha)%>% filter(fecha >"2020-01-01") %>%
        summarize(count=n()) %>%
        mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))%>%
        ungroup()
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = media.movil)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos diarios",title = titulo)+
        ggplot2::geom_line(aes(x = fecha, y = count),size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)### Grafico aquí de barras con media movil?
      #return(f_dia_depa)
    }

  } else{
    DEPARTAMENTO=toupper(DEPARTAMENTO)
    titulo=paste0("Fallecidos Sinadef por dia en ",DEPARTAMENTO)
    data=data
    departamento_select=DEPARTAMENTO

    if (is.na(mediamovil))
    {
      f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
        dplyr::select("DEPARTAMENTO","fecha")%>% filter(fecha >"2020-01-01") %>%
        group_by(DEPARTAMENTO,fecha)%>%
        summarize(count=n())%>%ungroup()
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = count)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos Sinadef diarios",title = titulo)#+
      #ggplot2::geom_line(size = 1.2,colour = "darkblue")

      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    else{
      k=mediamovil
      f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
        dplyr::select("DEPARTAMENTO","fecha")%>%  filter(fecha >"2020-01-01") %>%
        group_by(DEPARTAMENTO,fecha)%>%
        summarize(count=n()) %>% ungroup() %>%
        mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = media.movil)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos Sinadef diarios",title = titulo)+
        ggplot2::geom_line(aes(x = fecha, y = count),size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    return(resultado)
  }
  return(resultado)
}


#' Fallecidos_ratio
#'
#' @param data Data frame creado con la función da_fallecidos()
#' @param DEPARTAMENTO Departamento del Perú. Callao está listado como departamento
#' @param mediamovil Integer para calcular o graficar la media móvil. Eje: 1,2,3.
#' @param semanal Boleano que permite obtener la información en frecuencia semanal
#'
#' @return
#' @export
#'
#' @examples
fallecidos_crecimiento<-function(data,DEPARTAMENTO=NULL,mediamovil=NA,semanal = FALSE){
  if ("METODODX" %in% colnames(data)){
    stop("La base de datos es incorrecta, use la funcion da_fallecidos()")
  }
  if(is.null(DEPARTAMENTO)){
    titulo=paste0("Variacion % de Fallecidos por dia a nivel Nacional")
    data=data
    if (is.na(mediamovil))
    {
      if (isFALSE(semanal)){
        f_dia_depa=data%>%
          dplyr::select("fecha")%>%
          group_by(fecha)%>%
          summarize(count=n()) %>% mutate(growth = (count-lag(count))/lag(count))
      } else if(isTRUE(semanal)){
        titulo=paste0("Variacion % de Fallecidos por semana a nivel Nacional")
        f_dia_depa=data%>% mutate(semana = lubridate::week(fecha)) %>%
          dplyr::select("semana")%>%
          group_by(semana)%>%
          summarize(count=n()) %>% mutate(growth = (count-lag(count))/lag(count),
                                          year= 2020) %>%
          mutate(fecha_ultima = MMWRweek::MMWRweek2Date(year,semana,07),fecha = fecha_ultima)
      }
      
      #return(f_dia_depa)
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = growth)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos variacion",title = titulo, x = NULL)+
        scale_x_date(date_breaks = "1 month",date_labels = "%b-%Y")
        #ggplot2::geom_line(size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)
    }
    else{
      k=mediamovil
      if (isFALSE(semanal)){
        f_dia_depa=data%>%
          dplyr::select("fecha")%>%
          group_by(fecha)%>%
          summarize(count=n()) %>%
          mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))%>%
          mutate(growth = (media.movil-lag(media.movil))/lag(media.movil))
      } else if (isTRUE(semanal)){
        titulo=paste0("Variacion % de Fallecidos por semana a nivel Nacional")
        f_dia_depa=data%>%
          mutate(semana = lubridate::week(fecha)) %>%
          dplyr::select("semana")%>%
          group_by(semana)%>%
          summarize(count=n()) %>%
          mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))%>%
          mutate(growth = (media.movil-lag(media.movil))/lag(media.movil),year = 2020) %>%
          mutate(fecha_ultima = MMWRweek::MMWRweek2Date(year,semana,07),fecha = fecha_ultima)
      }
      
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = growth)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos variacion",title = titulo,x=NULL)+
        ggplot2::geom_line(aes(x = fecha, y = growth),size = 1.2,colour = "darkblue")+
        scale_x_date(date_breaks ="1 month",date_labels = "%b-%Y")
      resultado=list(f_dia_depa, grafico)### Grafico aquí de barras con media movil?
      #return(f_dia_depa)
    }
  } else{
    DEPARTAMENTO=toupper(DEPARTAMENTO)
    titulo=paste0("Variacion % de Fallecidos por dia en ",DEPARTAMENTO)
    data=data
    departamento_select=DEPARTAMENTO

    if (is.na(mediamovil))
    {
      if (isFALSE(semanal)){
        f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
          dplyr::select("DEPARTAMENTO","fecha")%>%
          group_by(DEPARTAMENTO,fecha)%>%
          summarize(count=n()) %>% mutate(growth = (count-lag(count))/lag(count))
      } else if(isTRUE(semanal)){
        titulo=paste0("Variacion % de Fallecidos por semana en ",DEPARTAMENTO)
        f_dia_depa=data%>%
          mutate(semana = lubridate::week(fecha)) %>%
          filter(DEPARTAMENTO==departamento_select)%>%
          dplyr::select("DEPARTAMENTO","semana")%>%
          group_by(DEPARTAMENTO,semana)%>%
          summarize(count=n()) %>% mutate(growth = (count-lag(count))/lag(count),year =2020)%>%
          mutate(fecha_ultima = MMWRweek::MMWRweek2Date(year,semana,07),fecha = fecha_ultima)
      }
      
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = growth)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos variacion",title = titulo,x=NULL)+
        scale_x_date(date_breaks ="1 month",date_labels = "%b-%Y")
        #ggplot2::geom_line(size = 1.2,colour = "darkblue")

      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    else{
      k=mediamovil
      if(isFALSE(semanal)){
        f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
          dplyr::select("DEPARTAMENTO","fecha")%>%
          group_by(DEPARTAMENTO,fecha)%>%
          summarize(count=n()) %>% ungroup() %>%
          mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right")) %>%
          mutate(growth = (media.movil-lag(media.movil))/lag(media.movil))
      } else if(isTRUE(semanal)){
        titulo=paste0("Variacion % de Fallecidos por semana en ",DEPARTAMENTO)
        f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
          mutate(semana= lubridate::week(fecha))%>%
          dplyr::select("DEPARTAMENTO","semana")%>%
          group_by(DEPARTAMENTO,semana)%>%
          summarize(count=n()) %>% ungroup() %>%
          mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right")) %>%
          mutate(growth = (media.movil-lag(media.movil))/lag(media.movil),year=2020)%>%
          mutate(fecha_ultima = MMWRweek::MMWRweek2Date(year,semana,07),fecha = fecha_ultima)
      }
      
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = growth)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="fallecidos variacion",title = titulo,x=NULL)+
        ggplot2::geom_line(aes(x = fecha, y = growth),size = 1.2,colour = "darkblue")+
        scale_x_date(date_breaks ="1 month",date_labels = "%b-%Y")
      resultado=list(f_dia_depa, grafico)### Grafico aquí de barras con media movil?
      #return(f_dia_depa)
    }
    return(resultado)
  }
  return(resultado)
}

#' Casos_crecimiento
#'
#' @param data Data frame creado con la función da_positivos()
#' @param DEPARTAMENTO Departamento del Perú. Callao está listado como departamento
#' @param mediamovil Integer para calcular o graficar la media móvil. Eje: 1,2,3.
#' @param semanal Boleano True permite obtener la informacion en frecuencia semanal
#'
#' @return
#' @export
#'
#' @examples
casos_crecimiento<-function(data,DEPARTAMENTO=NULL, mediamovil = NA,semanal = FALSE){
  if (!"METODODX" %in% colnames(data)){
    stop("La base de datos es incorrecta, use la funcion da_positivos()")
  }
  if(is.null(DEPARTAMENTO)){
    titulo=paste0("Variacion % de Casos por dia a nivel Nacional")
    data=data

    if (is.na(mediamovil))
    {
      if(isFALSE(semanal)){
        f_dia_depa=data%>%
          dplyr::select("fecha")%>%
          group_by(fecha)%>%
          summarize(count=n()) %>% mutate(growth = (count - lag(count))/lag(count))
      } else if(isTRUE(semanal)){
        titulo=paste0("Variacion % de Casos por semana a nivel Nacional")
        f_dia_depa=data%>%
          mutate(semana = lubridate::week(fecha)) %>% dplyr::select("semana")%>%
          group_by(semana)%>%
          summarize(count=n()) %>% mutate(growth = (count - lag(count))/lag(count),
                                          year = 2020)%>%
          mutate(fecha_ultima = MMWRweek::MMWRweek2Date(year,semana,07),fecha = fecha_ultima)
      }
      
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = growth)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Casos variacion",title = titulo,x=NULL)+
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
        #ggplot2::geom_line(size = 1.2,colour = "darkblue")

      resultado=list(f_dia_depa, grafico)
    }
    else{
      k=mediamovil
      if (isFALSE(semanal)){
        f_dia_depa=data%>%
          dplyr::select("fecha")%>%
          group_by(fecha)%>%
          summarize(count=n()) %>%
          mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))%>%
          mutate(growth = (media.movil - lag(media.movil))/lag(media.movil))
      } else if(isTRUE(semanal)){
        titulo=paste0("Variacion % de Casos por semana a nivel Nacional")
        f_dia_depa=data%>% mutate(semana = lubridate::week(fecha)) %>%
          dplyr::select("semana")%>%
          group_by(semana)%>%
          summarize(count=n()) %>%
          mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))%>%
          mutate(growth = (media.movil - lag(media.movil))/lag(media.movil),year = 2020,
                 fecha_ultima = MMWRweek::MMWRweek2Date(year,semana,07),fecha = fecha_ultima)
      }
      
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = growth)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Casos variacion",title = titulo, x=NULL)+
        ggplot2::geom_line(aes(x = fecha, y = growth),size = 1.2,colour = "darkblue") +
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
      
      resultado=list(f_dia_depa, grafico)
      ### Grafico aquí de barras con media movil?
      #return(f_dia_depa)
    }
  } else{
    DEPARTAMENTO=toupper(DEPARTAMENTO)
    titulo=paste0("Variacion % de Casos por dia en ",DEPARTAMENTO)
    data=data
    departamento_select=DEPARTAMENTO
    if(is.na(mediamovil)){
      if(isFALSE(semanal)){
        f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
          dplyr::select("DEPARTAMENTO","fecha")%>%
          group_by(DEPARTAMENTO,fecha)%>%
          summarize(count=n()) %>% ungroup() %>% mutate(growth = (count-lag(count))/lag(count))
      } else if(isTRUE(semanal)){
        titulo=paste0("Variacion % de Casos por semana en ",DEPARTAMENTO)
        f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
          mutate(semana = lubridate::week(fecha)) %>%
          dplyr::select("DEPARTAMENTO","semana")%>%
          group_by(DEPARTAMENTO,semana)%>%
          summarize(count=n()) %>% ungroup() %>%
          mutate(growth = (count-lag(count))/lag(count),year = 2020,
                 fecha_ultima =MMWRweek::MMWRweek2Date(year,semana,07),fecha = fecha_ultima)
      }
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = growth)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Casos Variacion",title = titulo, x=NULL)+
        scale_x_date(date_breaks = "1 month",date_labels = "%b-%Y")
      
        #ggplot2::geom_line(size = 1.2,colour = "darkblue")

      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    } else {
      k=mediamovil
      if (isFALSE(semanal)){
        f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
          dplyr::select("DEPARTAMENTO","fecha")%>%
          group_by(DEPARTAMENTO,fecha)%>%
          summarize(count=n()) %>% ungroup() %>%
          mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right")) %>%
          mutate(growth = (media.movil - lag(media.movil))/lag(media.movil)) 
      } else if(isTRUE(semanal)){
        titulo=paste0("Variacion % de Casos por semana en ",DEPARTAMENTO)
        f_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
          mutate(semana = lubridate::week(fecha)) %>%
           dplyr::select("DEPARTAMENTO","semana")%>%
          group_by(DEPARTAMENTO,semana)%>%
          summarize(count=n()) %>% ungroup() %>%
          mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right")) %>%
          mutate(growth = (media.movil - lag(media.movil))/lag(media.movil),year =2020) %>%
          mutate(fecha_ultima = MMWRweek::MMWRweek2Date(year,semana,07),fecha = fecha_ultima)
      }
      
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = growth)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Casos Variacion",title = titulo, x=NULL)+
        ggplot2::geom_line(aes(x = fecha, y = growth),size = 1.2,colour = "darkblue")+
        scale_x_date(date_breaks = "1 month",date_labels = "%b-%Y")
      resultado=list(f_dia_depa, grafico)### Grafico aquí de barras con media movil?
      #return(f_dia_depa)
    }
  }
  return(resultado)
}

#' vacunados_diarios
#'
#' @param data Data frame creado con la función da_vacunados()
#' @param DEPARTAMENTO Departamento del Perú. Callao está listado como departamento
#' @param mediamovil Integer para calcular o graficar la media móvil. Eje: 1,2,3.
#'
#' @return
#' @export
#'
#' @examples
vacunados_diarios <- function(data,DEPARTAMENTO=NULL,mediamovil=NA){
  if ("METODODX"%in%colnames(data)){
    stop("La base de datos es incorrecta, use la función da_vacunados()")
  }
  
  if(is.null(DEPARTAMENTO)){
    titulo=paste0("Vacunados por dia a nivel Nacional")
    data=data
    
    if (is.na(mediamovil))
    {
      v_dia_depa=data%>%
        dplyr::select("fecha")%>%
        group_by(fecha)%>%
        summarize(count=n())
      grafico <- v_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = count)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Vacunados diarios",title = titulo)#+
      #ggplot2::geom_line(size = 1.2,colour = "darkblue")
      resultado=list(v_dia_depa, grafico)
      #return(v_dia_depa)
    }
    else{
      k=mediamovil
      v_dia_depa=data%>%
        dplyr::select("fecha")%>%
        group_by(fecha)%>%
        summarize(count=n()) %>%
        mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))
      grafico <- v_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = media.movil)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Vacunados diarios",title = titulo)+
        ggplot2::geom_line(aes(x = fecha, y = count),size = 1.2,colour = "darkblue")
      resultado=list(v_dia_depa, grafico)### Grafico aquí de barras con media movil?
      #return(v_dia_depa)
    }
    
  } else{
    DEPARTAMENTO=toupper(DEPARTAMENTO)
    titulo=paste0("Vacunados por dia en ",DEPARTAMENTO)
    data=data
    departamento_select=DEPARTAMENTO
    
    if (is.na(mediamovil))
    {
      v_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
        dplyr::select("DEPARTAMENTO","fecha")%>%
        group_by(DEPARTAMENTO,fecha)%>%
        summarize(count=n())
      grafico <- v_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = count)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Vacunados diarios",title = titulo)#+
      #ggplot2::geom_line(size = 1.2,colour = "darkblue")
      
      resultado=list(v_dia_depa, grafico)
      #return(v_dia_depa)
    }
    else{
      k=mediamovil
      v_dia_depa=data%>%filter(DEPARTAMENTO==departamento_select)%>%
        dplyr::select("DEPARTAMENTO","fecha")%>%
        group_by(DEPARTAMENTO,fecha)%>%
        summarize(count=n()) %>% ungroup() %>%
        mutate(media.movil = zoo::rollmean(count,k=k, fill=NA,align = "right"))
      grafico <- v_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = media.movil)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Vacunados diarios",title = titulo)+
        ggplot2::geom_line(aes(x = fecha, y = count),size = 1.2,colour = "darkblue")
      resultado=list(v_dia_depa, grafico)
      #return(v_dia_depa)
    }
    return(resultado)
  }
  return(resultado)
}
