da_vacunados<-function (){
  file = "https://cloud.minsa.gob.pe/s/ZgXoXqK2KLjRLxD/download"
  data = data.table::fread(file,encoding="Latin-1")
  library(dplyr)
  library(lubridate)
  
  data1= dplyr::mutate(data,year = substr(FECHA_VACUNACION,1,4),
                        month = substr(FECHA_VACUNACION,5,6),
                        day = substr(FECHA_VACUNACION,7,8),
                        fecha=as.Date(paste0(year,"-",month,"-",day)),
                        EDAD_n = as.numeric(EDAD),
                        semana = lubridate::epiweek(fecha))
  return( data1 )
}



library(ggplot2)
vacunados_diarios <- function(data,DEPARTAMENTO=NULL,mediamovil=NA){
  if ("METODODX"%in%colnames(data)){
    stop("La base de datos es incorrecta, use la función da_vacunados()")
  }
  
  if(is.null(DEPARTAMENTO)){
    titulo=paste0("Vacunados por dia a nivel Nacional")
    data=data
    
    if (is.na(mediamovil))
    {
      f_dia_depa=data%>%
        dplyr::select("fecha")%>%
        group_by(fecha)%>%
        summarize(count=n())
      grafico <- f_dia_depa %>% ggplot2::ggplot(aes(x = fecha, y = count)) +
        ggplot2::geom_bar(stat="identity")+
        labs(y="Vacunados diarios",title = titulo)#+
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
        labs(y="Vacunados diarios",title = titulo)+
        ggplot2::geom_line(aes(x = fecha, y = count),size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)### Grafico aquí de barras con media movil?
      #return(f_dia_depa)
    }
    
  } else{
    DEPARTAMENTO=toupper(DEPARTAMENTO)
    titulo=paste0("Vacunados por dia en ",DEPARTAMENTO)
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
        labs(y="Vacunados diarios",title = titulo)#+
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
        labs(y="Vacunados diarios",title = titulo)+
        ggplot2::geom_line(aes(x = fecha, y = count),size = 1.2,colour = "darkblue")
      resultado=list(f_dia_depa, grafico)
      #return(f_dia_depa)
    }
    return(resultado)
  }
  return(resultado)
}

vacunados_data = da_vacunados()

vacunados_diarios( vacunados_data , DEPARTAMENTO = "Lima", mediamovil = 2)