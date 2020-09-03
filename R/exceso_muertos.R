#' exceso_muertes
#'
#' @param data
#' @param DEPARTAMENTO
#' @param metodo Default=TRUE. El default toma las 11 primeras semanas del 2020. Alternativo toma años anteriores.
#'
#' @return
#' @export
#'
#' @examples
exceso_muertes<-function(data,DEPARTAMENTO=NULL,metodo=TRUE){
  if (!"DEPARTAMENTO DOMICILIO"%in%colnames(data)){
    stop("La base de datos es incorrecta, use la función da_sinadef()")
  }
  #att <- assertthat::assert_that
  if(is.null(DEPARTAMENTO)){
    titulo=paste0("Exceso de fallecidos a nivel nacional")
    data=data
    ult_semana=lubridate::epiweek(max(data$fecha))
    ult_dia=weekdays(max(data$fecha))
    actualizacion=lubridate::date(max(data$fecha))
    semana_tomada=ifelse(ult_dia=="Saturday",ult_semana,ult_semana-1)
    print(paste0("Ojo: archivo actualizado al ",actualizacion,
      " tomamos referencia hasta la semana ",semana_tomada))
    if (isTRUE(metodo))
    {
      f_semana_depa=data%>%
        dplyr::select("semana","year")%>%
        group_by(semana, year)%>%
        summarize(numero_fallecidos=n()) %>% arrange(year) %>% filter(year==2020) %>%
        ungroup() %>%
        dplyr::mutate(Esperado =
                        ifelse(semana>10,mean(numero_fallecidos[semana<11],na.rm = T),
                               numero_fallecidos)) %>%
        dplyr::mutate(Exceso = ifelse(numero_fallecidos-Esperado<0,0,
                                      numero_fallecidos-Esperado))%>%
        filter(semana<=semana_tomada)  %>%
        mutate(fecha_ultima = MMWRweek::MMWRweek2Date(year,semana,07))

      grafico <- f_semana_depa %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x =fecha_ultima , y = Esperado,colour = "Esperado"),size = 1.2)+
      ggplot2::geom_line(aes(x =fecha_ultima, y = numero_fallecidos,colour = "Observado"),size = 1.2)+
        theme(legend.position = "bottom",axis.text.x=element_text(angle=90, hjust=1))+
        scale_color_manual("Leyenda",values = c("Esperado" = "#386cb0","Observado" ="red3"))+
        labs(x = "Mes",y = "Número de fallecidos",title = titulo,
             caption = "Metodo 2020")+
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
      resultado=list(f_semana_depa, grafico)
    }
    else if (isFALSE(metodo)){
      f_semana_depa=data%>%
        dplyr::select("semana","year")%>%
        group_by(semana, year)%>%
        summarize(numero_fallecidos=n()) %>% arrange(year)
      f_semana_dep1 <- tidyr::spread(f_semana_depa,year,numero_fallecidos)
      row_mean = rowMeans(f_semana_dep1[,c("2017","2018","2019")])
      row_mean = round(row_mean,0)

      f_semana_dep1$esperado = row_mean
      f_semana_dep1 <- f_semana_dep1[!is.na(f_semana_dep1$`2020`),]
      f_semana_dep1=f_semana_dep1%>%filter(semana<=semana_tomada) %>%
        mutate(fecha_ultima = MMWRweek::MMWRweek2Date(2020,semana,07))
      

      grafico <- f_semana_dep1 %>% ggplot2::ggplot() +
        geom_line(aes(x = fecha_ultima, y = esperado,colour = "Esperado"),size = 1.2)+
        geom_line(aes(x = fecha_ultima, y = `2020`,colour = "Observado"),size = 1.2)+
        theme(legend.position = "bottom",axis.text.x=element_text(angle=90, hjust=1))+
        scale_color_manual("Leyenda",values = c("Esperado" = "#386cb0","Observado" ="red3"))+
        labs(x = "Mes",y = "Número de fallecidos",title = titulo,
             caption = "Metodo historico")+
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
      resultado=list(f_semana_dep1, grafico)
    }
  }
  else{
    DEPARTAMENTO=toupper(DEPARTAMENTO)
    titulo=paste0("Exceso de fallecidos por semana en ",DEPARTAMENTO)
    data=data
    departamento_select=DEPARTAMENTO
    ult_semana=lubridate::epiweek(max(data$fecha))
    ult_dia=weekdays(max(data$fecha))
    actualizacion=lubridate::date(max(data$fecha))
    semana_tomada=ifelse(ult_dia=="Saturday",ult_semana,ult_semana-1)
    print(paste0("Ojo: archivo actualizado al ",actualizacion,
                 " tomamos referencia hasta la semana ",semana_tomada))
    if (isTRUE(metodo))
    {
      f_semana_depa=data%>%filter(`DEPARTAMENTO DOMICILIO`==departamento_select)%>%
        dplyr::select("year","semana")%>%
        group_by(year, semana)%>%
        summarize(numero_fallecidos=n()) %>% arrange(year) %>% filter(year==2020) %>%
        dplyr::mutate(Esperado =
                        ifelse(semana>10,mean(numero_fallecidos[semana<11]),
                               numero_fallecidos)) %>%
        dplyr::mutate(Exceso = ifelse(numero_fallecidos-Esperado<0,0,
                                      numero_fallecidos-Esperado))%>%
        filter(semana<=semana_tomada) %>%
        mutate(fecha_ultima = MMWRweek::MMWRweek2Date(year,semana,07))
        #MMWRweek::MMWRweek2Date(year,semana,07)

      grafico <- f_semana_depa %>% ggplot2::ggplot() +
        geom_line(aes(x = fecha_ultima, y = Esperado,colour = "Esperado"),size = 1.2)+
        geom_line(aes(x = fecha_ultima, y = numero_fallecidos,colour = "Observado"),size = 1.2)+
        theme(legend.position = "bottom",axis.text.x=element_text(angle=90, hjust=1))+
        scale_color_manual("Leyenda",values = c("Esperado" = "#386cb0","Observado" ="red3"))+
        labs(y="Fallecidos semanales",title = titulo, caption = "Metodo 2020",
             x = "Mes")+
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
        
      resultado=list(f_semana_depa, grafico)
      #return(f_dia_depa)
    }
    else if (isFALSE(metodo)){
      f_semana_depa=data%>% filter(`DEPARTAMENTO DOMICILIO`==departamento_select) %>%
        dplyr::select("semana","year")%>%
        group_by(semana, year)%>%
        summarize(numero_fallecidos=n()) %>% arrange(year)
      f_semana_dep1 <- tidyr::spread(f_semana_depa,year,numero_fallecidos)
      row_mean = rowMeans(f_semana_dep1[,c("2017","2018","2019")])
      row_mean = round(row_mean,0)

      f_semana_dep1$esperado = row_mean
      f_semana_dep1 <- f_semana_dep1[!is.na(f_semana_dep1$`2020`),]
      f_semana_dep1=f_semana_dep1%>%filter(semana<=semana_tomada)%>%
        mutate(fecha_ultima = MMWRweek::MMWRweek2Date(2020,semana,07))
      
      grafico <- f_semana_dep1 %>%ggplot2::ggplot() +
        geom_line(aes(x = fecha_ultima, y = esperado,colour = "Esperado"),size = 1.2)+
        geom_line(aes(x = fecha_ultima, y = `2020`,colour = "Observado"),size = 1.2)+
        theme(legend.position = "bottom",axis.text.x=element_text(angle=90, hjust=1))+
        scale_color_manual("Leyenda",values = c("Esperado" = "#386cb0","Observado" ="red3"))+
        labs(x = "Mes",y = "Número de fallecidos",title = titulo,
             caption = "Metodo historico")+
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
      resultado=list(f_semana_dep1, grafico)
    }
  }
  return(resultado)
  }
