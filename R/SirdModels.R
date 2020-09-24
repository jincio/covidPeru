#' sird_basico
#'
#'\code{panel_exceso} es una funcion creada por Cesar Urquizo (twitter:CUrquizoU)
#' @param delta
#' @param gamma
#' @param theta
#' @param N
#' @param d
#'
#' @return
#' @export
#'
#' @examples
sird_basico<- function(delta, gamma, theta, N, d) {

  tf = nrow(d)
  tfp = tf-3
  t0 = 74  #Dia del inicio de la pandemia
  t0p = t0+1

  beta = matrix(0,tfp,1)
  S = matrix(0,tfp,1)
  I = matrix(0,tfp,1)
  R = matrix(0,tfp,1)
  delta_d =  matrix(0,tf,1)
  delta2_d = matrix(0,tf,1)
  D = matrix(0,tf,1)

  #Construyendo total de muertes, la doble y triple diferencia
  D[t0]=d[t0]
  for (i in t0+1:tf) {
    D[i] =D[i-1] + d[i]
  }


  for (i in 2:tf) {
    delta_d[i,1] =d[i,1]-d[i-1,1]
  }

  for (i in 3:tf) {
    delta2_d[i,1] =delta_d[i,1]-delta_d[i-1,1]

  }

  #Esta es la parte mas importante. Despejando el sistema de ecuaciones en diferencia, todo se resume a dos ecuaciones:
  #beta y S. Determino beta y S iterativamente (a partir de D), y luego con eso puedo recuperar I,R.
  temp1 = (1/theta)*delta2_d[t0+3,1] + delta_d[t0+2,1]
  temp2 = (1/theta)*delta_d[t0+2,1] + d[t0+1,1]
  beta[t0,1] = gamma + temp1/temp2
  S[t0,1] = N


  for (i in t0p:tfp) {
    temp1 = (1/theta)*delta2_d[i+3] + delta_d[i+2]
    temp2 = (1/theta)*delta_d[i+2] + d[i+1]
    temp3 = (1/theta)*delta_d[i+1] + d[i]
    S[i] =min(S[i-1]*(1-beta[i-1]*(1/(delta*gamma*N))*(temp3)),S[i-1])
    beta[i] = (N/S[i])*(gamma + (temp1)/(temp2))
  }


  for (i in t0p:tfp) {
    I[i,1] = ((1/theta)*delta_d[i+2,1] + d[i+1,1])*(1/(delta*gamma))
  }



  for (i in t0p:tfp) {
    R[i,1] = d[i+1,1]/(delta*theta)
  }

  s = S/N*100
  i = I/N*100


  total_i = 100-s

  total_i_N = total_i*N/100

  #Aqui obtengo la variacion en infecciones por dia
  casos_diarios = matrix(0,tfp,1)
  for (i in t0p:tfp) {
    casos_diarios[i] =total_i_N[i]-total_i_N[i-1]
  }

  i = I/N*100


  #---------
  # R0 y Rt
  #---------
  R0 = matrix(0,tfp,1)
  Ret = matrix(0,tfp,1)

  R0 = beta/gamma
  Ret = R0*s/100

  # Se elimina informacion sobrante, las fechas iniciales

  n <- length(R0)
  #-------------------------------PASO 3--------------------------
  #---------------------------
  # Resultados en una lista
  #---------------------------

  resultados <- list(S = S[74:n], I = I[74:n] , R = R[74:n] , D = D[74:n],
                     R0=R0[74:n], Ret=Ret[74:n],casos_diarios=casos_diarios[74:n])
  return(resultados)
}

#' Sird_Villaverde
#'
#'\code{panel_exceso} es una funcion creada por Cesar Urquizo (twitter:CUrquizoU)
#'
#' @param data
#' @param DEPARTAMENTO
#' @param delta
#' @param gamma
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
sird_villaverde <- function(data,DEPARTAMENTO = NULL,delta =0.01 ,gamma =0.1, theta =0.2){
  if (!"DEPARTAMENTO DOMICILIO"%in%colnames(data)){
    stop("La base de datos es incorrecta, use la función da_sinadef()")
  }
  #-------------------------------PASO 1--------------------------
  if(is.null(DEPARTAMENTO)){
    # Base de datos fallecidos sinadef
    bases <- data %>%
      mutate(dia = yday(fecha), mes = as.numeric(substr(fecha,6,7)),
             anho = as.numeric(substr(fecha,1,4))) %>%
      select(fecha,dia,mes,anho) %>%
      group_by(fecha) %>%
      summarize(dia = mean(dia), mes = mean(mes), anho = mean(anho),cont = n()) %>%
      ungroup()%>%
      mutate(dia_mes = paste0(dia,"-",mes))

    # Poblacion
    base_reg <- base_poblacion %>% summarise(total = sum(Poblacion))
  } else{
    # Bases fallecidos poblacion a nivel regional
    departamento_select = toupper(DEPARTAMENTO)
    bases <- data %>%
      filter(`DEPARTAMENTO DOMICILIO` == departamento_select)%>%
      mutate(dia = yday(fecha), mes = as.numeric(substr(fecha,6,7)),
             anho = as.numeric(substr(fecha,1,4))) %>%
      select(fecha,dia,mes,anho,`DEPARTAMENTO DOMICILIO`,`PROVINCIA DOMICILIO`) %>%
      group_by(fecha, `DEPARTAMENTO DOMICILIO`) %>%
      summarize(dia = mean(dia), mes = mean(mes), anho = mean(anho),cont = n()) %>%
      ungroup()%>%
      mutate(dia_mes = paste0(dia,"-",mes))

    # Poblacion del departamento
    departamento_select = tolower(DEPARTAMENTO)
    base_reg <- base_poblacion %>% filter(Departamento==departamento_select) %>%
      summarise(total = sum(Poblacion))
  }

  #-------------------------------PASO 2--------------------------

  # Este procedimiento es el mismo para ambos tipos
  keeps <- c("dia_mes","dia", "mes","anho","cont")

  # Exceso de fallecidos
  bases <- bases[,keeps]
  bases <- bases %>% filter(anho %in% c(2019,2020))

  # Filter
  bases1 <- bases %>% tidyr::spread(key = anho,value = cont, fill = 0)
  colnames(bases1)[c(4,5)] <-c("anho2019","anho2020")
  bases1 <- bases1[order(bases1$dia,bases1$mes),]
  bases1 <- bases1 %>% mutate(excess_death = ifelse(anho2020-anho2019< 0,
                                                    0,anho2020-anho2019))


  # Detectar ultimo dia de registro y retrocedemos 7 dias
  dia_limite <- lubridate::yday(Sys.Date())-7

  x = mFilter::hpfilter(bases1$excess_death[1:dia_limite],freq=5000,type="lambda",drift=FALSE)
  d = x$trend

  # Inputs
  N <- base_reg[["total"]]

  #-------------------------------PASO 3--------------------------
  # Estimacion se realiza hasta 7 dias atras
  resultados <- sird_basico(delta, gamma, theta, N, d)

  return(resultados)
}

