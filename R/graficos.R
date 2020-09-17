#' piramide_fcovid
#'
#'\code{piramide_fcovid} es una funcion creada por Jesus Gamboa (twitter:jgamboa)
#' @param data un data frame con \code{da_fallecidos()}
#' @param DEPARTAMENTO
#'
#' @return
#' @export
#'
#' @examples
piramide_fcovid<- function(data,DEPARTAMENTO=NULL){
  if ("METODODX"%in%colnames(data)){
    stop("La base de datos es incorrecta, use la función da_fallecidos()")
  }

  if(is.null(DEPARTAMENTO)){

    data.piramide = data %>%
      dplyr::mutate(EDAD_CAT = cut(EDAD_DECLARADA,
                                   #breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,120),
                                   breaks = seq(0,120,5),
                                   include.lowest = TRUE)) %>%
      dplyr::filter(!is.na(EDAD_CAT)) %>%
      dplyr::group_by(SEXO,EDAD_CAT) %>%
      count()

    grafico.piramide = data.piramide %>%
      dplyr::mutate(n = ifelse(SEXO=="FEMENINO",
                        -n, n)) %>%
      ggplot2::ggplot(aes(x = EDAD_CAT,
                 y = n,
                 fill = SEXO,
                 label = abs(n))) +
      ggplot2::geom_col(position = "stack", alpha = 0.6) +
      ggplot2::geom_text(size = 3, check_overlap = TRUE)+
      ggplot2::scale_y_continuous(labels = abs) +
      ggplot2::labs(x = "Edad", y = "Número de fallecidos", title = "Distribución nacional de fallecimientos COVID-19 \n por edad y sexo") +
      ggplot2::scale_fill_manual(values = c("orange", "darkblue")) +
      ggplot2::coord_flip()
  }else{

    DEPA = toupper(DEPARTAMENTO)
    data.piramide = data %>%
      dplyr::filter(DEPARTAMENTO==DEPA) %>%
      dplyr::mutate(EDAD_CAT = cut(EDAD_DECLARADA,
                                   breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,120),
                                   include.lowest = TRUE)) %>%
      dplyr::filter(!is.na(EDAD_CAT)) %>%
      dplyr::group_by(SEXO,EDAD_CAT) %>%
      count()

    grafico.piramide = data.piramide %>%
      dplyr::mutate(n = ifelse(SEXO=="FEMENINO",
                               -n, n)) %>%
      ggplot2::ggplot(aes(x = EDAD_CAT,
                 y = n,
                 fill = SEXO,
                 label = abs(n))) +
      ggplot2::geom_col(position = "stack", alpha = 0.6) +
      ggplot2::geom_text(size = 3, check_overlap = TRUE)+
      ggplot2::scale_y_continuous(labels = abs) +
      ggplot2::labs(x = "Edad", y = "Número de fallecidos",
           title = paste0("Distribución de fallecimientos COVID-19  \n por edad y sexo en ",
                          paste(toupper(substr(DEPA, 1, 1)), tolower(substr(DEPA, 2, nchar(DEPA))), sep=""))) +
      ggplot2::scale_fill_manual(values = c("orange", "darkblue")) +
      ggplot2::coord_flip()
  }
  return(list(data.piramide,grafico.piramide))
}


#' piramide positivis
#'
#'\code{piramide_fcovid} es una funcion creada por Jesus Gamboa (twitter:jgamboa)
#' @param data a data frame creado por da_positivos()
#' @param DEPARTAMENTO
#'
#' @return
#' @export
#'
#' @examples
piramide_pcovid <- function(data,DEPARTAMENTO=NULL){
  if (!"METODODX"%in%colnames(data)){
    stop("La base de datos es incorrecta, use la función da_fallecidos()")
  }

  if(is.null(DEPARTAMENTO)){

    data.piramide = data %>%
      dplyr::mutate(EDAD_CAT = cut(EDAD,
                                   #breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,120),
                                   breaks = seq(0,120,5),
                                   include.lowest = TRUE)) %>%
      dplyr::filter(!is.na(EDAD_CAT)) %>%
      dplyr::group_by(SEXO,EDAD_CAT) %>%
      count()

    grafico.piramide = data.piramide %>%
      dplyr::mutate(n = ifelse(SEXO=="FEMENINO",
                               -n, n)) %>%
      ggplot2::ggplot(aes(x = EDAD_CAT,
                 y = n,
                 fill = SEXO,
                 label = abs(n))) +
      ggplot2::geom_col(position = "stack", alpha = 0.6) +
      ggplot2::geom_text(size = 3, check_overlap = TRUE)+
      ggplot2::scale_y_continuous(labels = abs) +
      ggplot2::labs(x = "Edad", y = "Número de casos", title = "Distribución nacional de casos positivos COVID-19 \n por edad y sexo") +
      ggplot2::scale_fill_manual(values = c("orange", "darkblue")) +
      ggplot2::coord_flip()
  }else{

    DEPA = toupper(DEPARTAMENTO)
    data.piramide = data %>%
      dplyr::filter(DEPARTAMENTO==DEPA) %>%
      dplyr::mutate(EDAD_CAT = cut(EDAD,
                                   #breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,120),
                                   breaks = seq(0,120,5),
                                   include.lowest = TRUE)) %>%
      dplyr::filter(!is.na(EDAD_CAT)) %>%
      dplyr::group_by(SEXO,EDAD_CAT) %>%
      count()

    grafico.piramide = data.piramide %>%
      dplyr::mutate(n = ifelse(SEXO=="FEMENINO",
                               -n, n)) %>%
      ggplot2::ggplot(aes(x = EDAD_CAT,
                 y = n,
                 fill = SEXO,
                 label = abs(n))) +
      ggplot2::geom_col(position = "stack", alpha = 0.6) +
      ggplot2::geom_text(size = 3, check_overlap = TRUE)+
      ggplot2::scale_y_continuous(labels = abs) +
      ggplot2::labs(x = "Edad", y = "Número de casos",
           title = paste0("Distribución de casos positivos COVID-19 \n por edad y sexo en ",
                          paste(toupper(substr(DEPA, 1, 1)), tolower(substr(DEPA, 2, nchar(DEPA))), sep=""))) +
      ggplot2::scale_fill_manual(values = c("orange", "darkblue")) +
      ggplot2::coord_flip()
  }
  return(list(data.piramide,grafico.piramide))
}
