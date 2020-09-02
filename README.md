
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covidPeru

<!-- badges: start -->

<!-- badges: end -->

El objetivo de este paquete es facilitar el uso de la información que
tenemos disponible en el portal de datos abiertos del gobierno sobre
COVID.

Empezó como algo de uso personal que espero sirva para tomadores de
decisiones, periodistas, o cualquier persona que quiera trabajar con
datos sobre COVID.

## Installation

Puedes instalar el paquete desde este

``` r
install.packages("devtools")
devtools::install_github("jincio/covidPeru")
```

## Ejemplo

Primero tenemos que llamar los data frames y luego podemos proceder con
la limpieza y generar unos gráficos.

``` r
library(dplyr) ## Necesario!
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(readr) ## Necesario!
library(ggplot2) ## Necesario!
library(lubridate)## Necesario!
#> Warning: package 'lubridate' was built under R version 4.0.2
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:dplyr':
#> 
#>     intersect, setdiff, union
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
library(covidPeru) 
```

``` r
library(covidPeru)
```

    #> Loading covidPeru

Cargamos el último set disponible en el portal de datos abiertos de
pruebas analizadas con resultado positivo.

``` r
positivos=da_positivos() # Crea un dataframe con la información positivos
```

``` r
head(positivos)
#> # A tibble: 6 x 14
#>   UUID  DEPARTAMENTO PROVINCIA DISTRITO METODODX  EDAD SEXO  FECHA_RESULTADO
#>   <chr> <chr>        <chr>     <chr>    <chr>    <dbl> <chr>           <dbl>
#> 1 7320… LIMA         LIMA      SAN BOR… PCR         34 FEME…        20200325
#> 2 e816… LIMA         LIMA      SAN BOR… PCR         30 FEME…        20200406
#> 3 71ec… LIMA         LIMA      SAN BOR… PCR         34 FEME…        20200410
#> 4 566a… LIMA         LIMA      BREÑA    PCR         32 FEME…        20200403
#> 5 0275… LIMA         LIMA      LIMA     PCR         31 FEME…        20200409
#> 6 f016… LIMA         LIMA      LIMA     PCR         30 FEME…        20200409
#> # … with 6 more variables: year <chr>, month <chr>, day <chr>, fecha <date>,
#> #   EDAD_n <dbl>, semana <dbl>
```

Cargamos el último set disponible en el portal de datos abiertos de
fallecidos por COVID-19.

``` r
fallecidos=da_fallecidos() # Crea un dataframe con la información de fallecidos.
```

``` r
head(fallecidos)
#> # A tibble: 6 x 13
#>   UUID  FECHA_FALLECIMI… EDAD_DECLARADA SEXO  FECHA_NAC DEPARTAMENTO PROVINCIA
#>   <chr>            <dbl>          <dbl> <chr>     <dbl> <chr>        <chr>    
#> 1 6b9e…         20200701             63 MASC…  19561228 LIMA         LIMA     
#> 2 beca…         20200701             63 MASC…  19570629 LIMA         LIMA     
#> 3 e5c2…         20200701             67 MASC…  19530524 LA LIBERTAD  ASCOPE   
#> 4 e844…         20200701             59 MASC…  19610210 LA LIBERTAD  TRUJILLO 
#> 5 dfe8…         20200701             61 FEME…  19581113 LA LIBERTAD  TRUJILLO 
#> 6 d780…         20200701             54 FEME…  19650921 LA LIBERTAD  VIRU     
#> # … with 6 more variables: DISTRITO <chr>, year <chr>, month <chr>, day <chr>,
#> #   fecha <date>, semana <dbl>
```

Cargamos el último set de SINADEF disponible en el portal de datos
abiertos. Este demora un poco más, tengan paciencia\!.

``` r
sinadef=da_sinadef()
```

``` r
head(sinadef)
#> # A tibble: 6 x 6
#>   fecha      semana  year dia      `DEPARTAMENTO DOMICILIO` `PROVINCIA DOMICILI…
#>   <date>      <dbl> <dbl> <chr>    <chr>                    <chr>               
#> 1 2020-01-24      4  2020 Friday   CUSCO                    CUSCO               
#> 2 2020-01-30      5  2020 Thursday CALLAO                   CALLAO              
#> 3 2020-01-19      4  2020 Sunday   CAJAMARCA                CAJAMARCA           
#> 4 2020-01-13      3  2020 Monday   LA LIBERTAD              TRUJILLO            
#> 5 2020-01-25      4  2020 Saturday CAJAMARCA                CAJAMARCA           
#> 6 2020-01-12      3  2020 Sunday   JUNIN                    YAULI
```

Una vez con las bases podemos usar las otras funciones para generar
series diarias y algunos gráficos de estas series.

Las funcion entrega una lista con dos objetos: una data y un gráfico.

``` r
fdiarios=fallecidos_diarios(fallecidos)[[1]]
head(fdiarios)
#> # A tibble: 6 x 2
#>   fecha      count
#>   <date>     <int>
#> 1 2020-03-18     1
#> 2 2020-03-19     3
#> 3 2020-03-20     2
#> 4 2020-03-21     1
#> 5 2020-03-22     2
#> 6 2020-03-23     1
```

``` r
grafico=fallecidos_diarios(fallecidos)[[2]]
print(grafico)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Podemos identificar un departamento en específico.

``` r
fdiarios=fallecidos_diarios(fallecidos, "lima")[[1]]
head(fdiarios)
#> # A tibble: 6 x 3
#> # Groups:   DEPARTAMENTO [1]
#>   DEPARTAMENTO fecha      count
#>   <chr>        <date>     <int>
#> 1 LIMA         2020-03-18     1
#> 2 LIMA         2020-03-19     3
#> 3 LIMA         2020-03-20     2
#> 4 LIMA         2020-03-23     1
#> 5 LIMA         2020-03-24     2
#> 6 LIMA         2020-03-26     3
```

``` r
grafico=fallecidos_diarios(fallecidos, "lima")[[2]]
print(grafico)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

  - Podemos agregar una media móvil también.

<!-- end list -->

``` r
grafico=fallecidos_diarios(fallecidos, "lima",mediamovil = 7)[[2]]
print(grafico)
#> Warning: Removed 6 rows containing missing values (position_stack).
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

### Exceso de muertos

En el paquete también está disponible la función para calcular el exceso
de muertos en Perú y por departamentos por semana.

En este caso la información se calcula por semana. Hay dos “métodos”
disponibles. El método por default toma como referencia las primeras 11
semanas del 2020 (“M2020”), y el “alternativo” que usa el promedio de
muertos de los años 2017,2018, 2019 con la semana de comparación.

``` r
base=exceso_muertes(sinadef)[1]
#> [1] "Ojo: archivo actualizado al 2020-08-31 tomamos referencia hasta la semana 35"
head(base, 30)
#> [[1]]
#> # A tibble: 35 x 5
#>    semana  year numero_fallecidos Esperado Exceso
#>     <dbl> <dbl>             <int>    <dbl>  <dbl>
#>  1      1  2020              1224     1224      0
#>  2      2  2020              2167     2167      0
#>  3      3  2020              2120     2120      0
#>  4      4  2020              2107     2107      0
#>  5      5  2020              1995     1995      0
#>  6      6  2020              2120     2120      0
#>  7      7  2020              2099     2099      0
#>  8      8  2020              2084     2084      0
#>  9      9  2020              2190     2190      0
#> 10     10  2020              2178     2178      0
#> # … with 25 more rows
```

``` r
grafico=exceso_muertes(sinadef)[2]
print(grafico)
```

``` r
grafico=exceso_muertes(sinadef,metodo = FALSE)[2]
#> [1] "Ojo: archivo actualizado al 2020-08-31 tomamos referencia hasta la semana 35"
print(grafico)
#> [[1]]
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

**Lima**

``` r
Lima=exceso_muertes(sinadef,"Lima")[2]
#> [1] "Ojo: archivo actualizado al 2020-08-31 tomamos referencia hasta la semana 35"
print(Lima)
#> [[1]]
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />
