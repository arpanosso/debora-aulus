
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Análise Doutorado Débora

## Biorigin

### Carregando pacotes

``` r
library(tidyverse)
library(ExpDes.pt)
library(lme4)
library(agricolae)
library(readxl)
library(janitor)
library(MASS)
library(car)
```

### Entrada de dados

``` r
## Lendo os dados
dados <- read_xlsx("data/dados_01.xlsx",
          sheet = "Análises Fezes") %>% 
  clean_names() %>% 
  mutate(
    dose = case_when(
      tr == 1 ~ 0,
      tr == 2 | tr == 5 ~ 0.2,
      tr == 3 | tr == 6 ~ 0.4,
      tr == 4 | tr == 7 ~ 0.8
    ),
    ingrediente = ifelse(tr==1,"PCL",
                         ifelse(tr > 1 & tr<=4 ,"PCL","PCLs"))
  )

#extraindo o controle
da <- dados %>% filter(dose==0) %>% 
  mutate(
    ingrediente="PCLs"
  )

#duplicando o controle
dados <- rbind(dados,da)
```

### Vislumbre

``` r
glimpse(dados)
#> Rows: 64
#> Columns: 31
#> $ cao         <chr> "Zulu", "Lilás", "Amêndoa", "Maia", "Major", "Aurora", "Ne~
#> $ tr          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3~
#> $ bl          <dbl> 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1~
#> $ mm          <dbl> 29.69652, 30.10801, 28.71755, 28.65898, 28.64619, 27.68278~
#> $ mo          <dbl> 70.30348, 69.89199, 71.28245, 71.34102, 71.35381, 72.31722~
#> $ ee          <dbl> 8.144940, 7.690260, 8.065103, 7.818093, 7.822882, 8.007406~
#> $ pb          <dbl> 25.74069, 26.28684, 26.16885, 26.58215, 25.75865, 28.95122~
#> $ am          <dbl> 0.22309865, 0.33276908, 0.24086184, 0.31584216, 0.28490313~
#> $ fb          <dbl> 13.68760, 13.67271, 12.80728, 12.53250, 13.60551, 12.91738~
#> $ eb          <dbl> 3717.751, 3758.580, 3735.262, 3776.294, 3774.567, 3865.065~
#> $ d_ms        <dbl> 84.38131, 85.87391, 82.77157, 84.03706, 83.41780, 82.09041~
#> $ d_mm        <dbl> 24.893497, 31.129777, 19.883795, 25.920002, 23.080513, 19.~
#> $ d_mo        <dbl> 88.29678, 89.47715, 86.91083, 87.86231, 87.38918, 86.19581~
#> $ d_ee        <dbl> 92.30688, 93.43049, 91.59717, 92.45284, 92.15526, 91.32744~
#> $ d_pb        <dbl> 84.68726, 85.85677, 82.82809, 83.83815, 83.73127, 80.25120~
#> $ d_am        <dbl> 99.90784, 99.87567, 99.89025, 99.86665, 99.87505, 99.96496~
#> $ d_fb        <dbl> 38.91930, 44.81660, 36.95733, 42.84126, 35.54022, 33.90144~
#> $ d_eb        <dbl> 87.96825, 88.99856, 86.66569, 87.50943, 87.03080, 85.65680~
#> $ ms_in       <dbl> 167.3840, 161.9562, 150.1691, 184.0309, 185.6728, 174.6911~
#> $ mo_in       <dbl> 157.0472, 151.9546, 140.8954, 172.6661, 174.2065, 163.9031~
#> $ pb_in       <dbl> 43.94665, 42.52158, 39.42689, 48.31730, 48.74837, 45.86513~
#> $ fibra_in    <dbl> 5.858441, 5.668467, 5.255920, 6.441082, 6.498547, 6.114189~
#> $ ee_in       <dbl> 27.67860, 26.78106, 24.83195, 30.43133, 30.70283, 28.88690~
#> $ amido_in    <dbl> 63.285703, 61.233515, 56.776980, 69.579679, 70.200446, 66.~
#> $ fezesmn     <dbl> 60.730, 50.342, 59.544, 62.802, 70.714, 69.870, 47.096, 46~
#> $ fezesms     <dbl> 26.14319, 22.87808, 25.87178, 29.37674, 30.78864, 31.28646~
#> $ msfezes     <dbl> 43.04824, 45.44532, 43.44986, 46.77676, 43.53966, 44.77810~
#> $ score       <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4~
#> $ ph          <dbl> 6.500000, 6.753333, 6.580000, 7.256667, 6.650000, 6.766667~
#> $ dose        <dbl> 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.2, 0.2, 0.2~
#> $ ingrediente <chr> "PCL", "PCL", "PCL", "PCL", "PCL", "PCL", "PCL", "PCL", "P~
```

## Normalidade de Homocedasticidade

``` r
variaveis <- dados %>% 
  dplyr::select(mm:ph,-score)

for(i in seq_along(variaveis)){
  nome <- names(variaveis[i])
  print("====================================")
  print(str_to_upper(nome))
  print("====================================")
  
  daux <- variaveis[i]
  names(daux) <- "y" ### <-
  y <- daux$y ### <- 
  bl <- dados$bl %>% as_factor()
  tr <- dados$tr %>% as_factor()
  
  mod <- aov(y ~ tr + bl)
  rs <- mod %>% rstudent()
  hist(rs, main = paste("Resíduo ", str_to_upper(nome)) )
  
  print(shapiro.test(rs))
  
  plot(y ~ tr)
  
  boxcox(mod,seq(-10,10,.5))
  cat("\n")

}
#> [1] "===================================="
#> [1] "MM"
#> [1] "===================================="
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.95277, p-value = 0.0157

![](README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "MO"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.95277, p-value = 0.0157

![](README_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "EE"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-7.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.96971, p-value = 0.1169

![](README_files/figure-gfm/unnamed-chunk-5-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-9.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "PB"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-10.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.95028, p-value = 0.01181

![](README_files/figure-gfm/unnamed-chunk-5-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-12.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "AM"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-13.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.85449, p-value = 2.254e-06

![](README_files/figure-gfm/unnamed-chunk-5-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-15.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "FB"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-16.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.96898, p-value = 0.107

![](README_files/figure-gfm/unnamed-chunk-5-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-18.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "EB"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-19.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.96403, p-value = 0.05902

![](README_files/figure-gfm/unnamed-chunk-5-20.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-21.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "D_MS"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-22.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.96972, p-value = 0.1169

![](README_files/figure-gfm/unnamed-chunk-5-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-24.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "D_MM"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-25.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.94957, p-value = 0.0109

![](README_files/figure-gfm/unnamed-chunk-5-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-27.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "D_MO"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-28.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.97563, p-value = 0.2356

![](README_files/figure-gfm/unnamed-chunk-5-29.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-30.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "D_EE"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-31.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.968, p-value = 0.09512

![](README_files/figure-gfm/unnamed-chunk-5-32.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-33.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "D_PB"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-34.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.97742, p-value = 0.2892

![](README_files/figure-gfm/unnamed-chunk-5-35.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-36.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "D_AM"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-37.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.87891, p-value = 1.401e-05

![](README_files/figure-gfm/unnamed-chunk-5-38.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-39.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "D_FB"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-40.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.9782, p-value = 0.3157

![](README_files/figure-gfm/unnamed-chunk-5-41.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-42.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "D_EB"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-43.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.97609, p-value = 0.2484

![](README_files/figure-gfm/unnamed-chunk-5-44.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-45.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "MS_IN"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-46.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.96103, p-value = 0.04127

![](README_files/figure-gfm/unnamed-chunk-5-47.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-48.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "MO_IN"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-49.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.96099, p-value = 0.04106

![](README_files/figure-gfm/unnamed-chunk-5-50.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-51.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "PB_IN"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-52.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.9785, p-value = 0.3266

![](README_files/figure-gfm/unnamed-chunk-5-53.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-54.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "FIBRA_IN"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-55.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.94707, p-value = 0.008231

![](README_files/figure-gfm/unnamed-chunk-5-56.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-57.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "EE_IN"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-58.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.9621, p-value = 0.04687

![](README_files/figure-gfm/unnamed-chunk-5-59.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-60.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "AMIDO_IN"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-61.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.76412, p-value = 8.929e-09

![](README_files/figure-gfm/unnamed-chunk-5-62.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-63.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "FEZESMN"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-64.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.95491, p-value = 0.02011

![](README_files/figure-gfm/unnamed-chunk-5-65.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-66.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "FEZESMS"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-67.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.97881, p-value = 0.3381

![](README_files/figure-gfm/unnamed-chunk-5-68.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-69.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "MSFEZES"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-70.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.96968, p-value = 0.1164

![](README_files/figure-gfm/unnamed-chunk-5-71.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-72.png)<!-- -->

    #> 
    #> [1] "===================================="
    #> [1] "PH"
    #> [1] "===================================="

![](README_files/figure-gfm/unnamed-chunk-5-73.png)<!-- -->

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  rs
    #> W = 0.96124, p-value = 0.04232

![](README_files/figure-gfm/unnamed-chunk-5-74.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-75.png)<!-- -->
