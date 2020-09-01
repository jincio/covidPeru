#' exceso_muertes
#'
#' @param data
#' @param DEPARTAMENTO
#' @param metodo default="Mactual". Mactual considera el promedio de las primeras 11 semanas del 2020. "Mhistorico" compara la semana del 2020 con las el promedio de fallecidos en la misma semana en en el 2017, 2018,2019.
#'
#' @return
#' @export
#'
#' @examples
exceso_muertes<-function(data,DEPARTAMENTO=NULL,metodo="M2020"){
  if (!"DEPARTAMENTO DOMICILIO"%in%colnames(data)){
    stop("La base de datos es incorrecta, use la función da_sinadef()")
  }
  if(is.null(DEPARTAMENTO)){
    titulo=paste0("Exceso de fallecidos nacional")
    data=data
    ult_semana=lubridate::epiweek(max(data$fecha))
    ult_dia=weekdays(max(data$fecha))
    semana_tomada=ifelse(ult_dia=="Saturday",ult_semana,ult_semana-1)
    cat("Ojo: archivo actualizado al",lubridate::date(max(sinadef$fecha)),
      "tomamos referencia hasta la semana",semana_tomada)
    if (metodo == "M2020")
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
        filter(semana<=semana_tomada)

      #n <- tail(f_semana_depa$semana,1)
      #semana_eliminada <- semana_eliminada-1
      #n <- ifelse(is.na(semana_eliminada),n,semana_eliminada-1)
      grafico <- f_semana_depa %>% filter(semana<=semana_tomada)%>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = as.numeric(semana), y = Esperado,colour = "Esperado"),size = 1.2)+
      ggplot2::geom_line(aes(x = as.numeric(semana), y = numero_fallecidos,colour = "Observado"),size = 1.2)+
        theme(legend.position = "bottom")+
        scale_color_manual("Leyenda",values = c("Esperado" = "#386cb0","Observado" ="red3"))+
        labs(x = "N° semana",y = "Número de fallecidos",title = titulo)+
        scale_x_continuous(breaks = seq(1,semana_tomada,2), labels = seq(1,semana_tomada,2))
      resultado=list(f_semana_depa, grafico)
      #return(f_dia_depa)
    }
    else if (metodo =="Mhistorico"){
      f_semana_depa=data%>%
        dplyr::select("semana","year")%>%
        group_by(semana, year)%>%
        summarize(numero_fallecidos=n()) %>% arrange(year)
      f_semana_dep1 <- spread(f_semana_depa,year,numero_fallecidos)
      row_mean = rowMeans(f_semana_dep1[,c("2017","2018","2019")])
      row_mean = round(row_mean,0)

      f_semana_dep1$esperado = row_mean
      f_semana_dep1 <- f_semana_dep1[!is.na(f_semana_dep1$`2020`),]
      f_semana_dep1=f_semana_dep1%>%filter(semana<=semana_tomada)
      #f_semana_dep1$`2020`[is.na(f_semana_dep1$`2020`)] = f_semana_dep1$esperado[is.na(f_semana_dep1$`2020`)]
      #n <- tail(f_semana_dep1$semana,1)
      #n <- ifelse(is.na(semana_eliminada),n,semana_eliminada-1)

      grafico <- f_semana_dep1 %>% filter(semana <= semana_tomada)%>% ggplot2::ggplot() +
        geom_line(aes(x = as.numeric(semana), y = esperado,colour = "Esperado"),size = 1.2)+
        geom_line(aes(x = as.numeric(semana), y = `2020`,colour = "Observado"),size = 1.2)+
        theme(legend.position = "bottom")+
        scale_color_manual("Leyenda",values = c("Esperado" = "#386cb0","Observado" ="red3"))+
        labs(x = "N° semana",y = "Número de fallecidos",title = titulo)+
        scale_x_continuous(breaks = seq(1,semana_tomada,2), labels = seq(1,semana_tomada,2))
      resultado=list(f_semana_dep1, grafico)
    }

  } else{
    DEPARTAMENTO=toupper(DEPARTAMENTO)
    titulo=paste0("Exceso de fallecidos por semana en ",DEPARTAMENTO)
    data=data
    departamento_select=DEPARTAMENTO
    ult_semana=lubridate::epiweek(max(data$fecha))
    ult_dia=weekdays(max(data$fecha))
    semana_tomada=ifelse(ult_dia=="Saturday",ult_semana,ult_semana-1)
    cat("Ojo: archivo actualizado al",max(sinadef$fecha),
      "tomamos referencia hasta la semana",semana_tomada)
    if (metodo == "Mactual")
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
        filter(semana<=semana_tomada)

      grafico <- f_semana_depa %>% filter(semana <=semana_tomada) %>% ggplot2::ggplot() +
        geom_line(aes(x = as.numeric(semana), y = Esperado,colour = "Esperado"),size = 1.2)+
        geom_line(aes(x = as.numeric(semana), y = numero_fallecidos,colour = "Observado"),size = 1.2)+
        theme(legend.position = "bottom")+
        scale_color_manual("Leyenda",values = c("Esperado" = "#386cb0","Observado" ="red3"))+
        scale_x_continuous(breaks = seq(1,semana_tomada,2), labels = seq(1,semana_tomada,2))+
        labs(y="Fallecidos semanales",title = titulo)
      resultado=list(f_semana_depa, grafico)
      #return(f_dia_depa)
    }
    else if (metodo == "Mhistorico"){
      f_semana_depa=data%>% filter(`DEPARTAMENTO DOMICILIO`==departamento_select) %>%
        dplyr::select("semana","year")%>%
        group_by(semana, year)%>%
        summarize(numero_fallecidos=n()) %>% arrange(year)
      f_semana_dep1 <- spread(f_semana_depa,year,numero_fallecidos)
      row_mean = rowMeans(f_semana_dep1[,c("2017","2018","2019")])
      row_mean = round(row_mean,0)

      f_semana_dep1$esperado = row_mean
      f_semana_dep1 <- f_semana_dep1[!is.na(f_semana_dep1$`2020`),]
      f_semana_dep1=f_semana_dep1%>%filter(semana<=semana_tomada)
      #f_semana_dep1$`2020`[is.na(f_semana_dep1$`2020`)] = f_semana_dep1$esperado[is.na(f_semana_dep1$`2020`)]
      grafico <- f_semana_dep1 %>%filter(semana<=semana_tomada) %>%ggplot2::ggplot() +
        geom_line(aes(x = as.numeric(semana), y = esperado,colour = "Esperado"),size = 1.2)+
        geom_line(aes(x = as.numeric(semana), y = `2020`,colour = "Observado"),size = 1.2)+
        theme(legend.position = "bottom")+
        scale_color_manual("Leyenda",values = c("Esperado" = "#386cb0","Observado" ="red3"))+
        labs(x = "N° semana",y = "Número de fallecidos",title = titulo)+
        scale_x_continuous(breaks = seq(1,semana_tomada,2), labels = seq(1,semana_tomada,2))
      resultado=list(f_semana_dep1, grafico)

    }
    return(resultado)
  }
  return(resultado)
}
