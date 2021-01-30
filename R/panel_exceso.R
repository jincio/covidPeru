#' panel exceso
#'
#'\code{panel_exceso} es una funcion creada por Gabriel Carrasco (twitter:Gabc91)
#' @param data a un data frame creado con da_sinadef()
#'
#' @return a ggplot object
#' @export
#'
#' @examples

panel_exceso <- function(data) {
  if (!"DEPARTAMENTO DOMICILIO"%in%colnames(data)){
    stop("La base de datos es incorrecta, use la función da_sinadef()")
  }
  a<-data.frame(dep = unique(data$`DEPARTAMENTO DOMICILIO`))%>%
    dplyr::mutate(exceso = purrr::map(.x = dep, .f = ~exceso_muertes(data, .x)[1] %>%
                                 as.data.frame())) %>%
    tidyr::unnest(cols = c(exceso))

  fig <- a %>%
    mutate(Exceso_p = (numero_fallecidos/Esperado)-1)%>% 
    #filter(semana>10) %>%
    ggplot2::ggplot(aes(x=semana, y = dep, fill = Exceso_p)) +
    ggplot2::geom_tile() +
    ggplot2::ggtitle("Exceso de muerte (%) por semana y Region") +
    scale_fill_viridis_c(option = "A", trans = "pseudo_log", direction = -1) +
    labs(fill = "Exceso (%)") +
    scale_x_continuous(expand = c(0,0)) +
    theme(legend.position = "top")
  return(fig)
}


