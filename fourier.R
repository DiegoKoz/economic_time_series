#### Librerias ####
library(tidyverse)
library(lubridate)

set.seed(1)

#### Datos ####
gold          = read_csv("data/GOLD_1791-2018.csv",skip = 3)
interest_rate = read_csv("data/INTERESTRATE_1857-2018.csv",skip = 1)
cpi           = read_csv("data/USCPI_1774-2018.csv", skip=4)
gdp           = read_csv("data/USGDP_1790-2018.csv", skip=2)
wage          = read_csv("data/USWAGE_1774-2018.csv", skip=3)

#### Funciones ####
# Corrijo los encabezados de los csv, que no quedan bien en R. Paso los nombres de los encabezados originales a labels de las columnas (metadata)
change_header = function(tabla, nuevos_enc) {
  if (is.data.frame(tabla)) {
    for (i in names(tabla)) {
      attr(tabla[[i]], "label") = i
    }
    names(tabla) = nuevos_enc
  } else {
    stop("Se tiene que pasar un data frame o similar objeto al campo tabla")
  }
  return(tabla)
}

calculo_r2 = function(datos, prediccion) {
    1 - (sum((datos - prediccion) ^ 2, na.rm = TRUE) / sum((datos - mean(datos)) ^ 2, na.rm = TRUE))
}

# Programa
cpi           = change_header(cpi, c("year", "cpi"))
gold          = change_header(gold, c("year", "value"))
interest_rate = change_header(interest_rate, c("year", "short_term_ord", "short_term_surp", "long_term"))
gdp           = change_header(gdp, c("year", "nominal", "real_2012_base", "gdp_deflator", "pop", "nominal_per_cap", "real_per_cap_2012_base"))
wage          = change_header(wage, c("year", "cost_unsk", "prod_work_hourly_comp"))

#### Tasa de interes de largo plazo de los EE UU ####
# Visualizacion de la serie original
plot(interest_rate$long_term, type = "l", ylim = c(-3, 14), ylab = "Tasa de interes")
abline(h = mean(interest_rate$long_term), lty = 3)

interest_rate$long_term_cntr = interest_rate$long_term - mean(interest_rate$long_term)

fft_ir = fft(interest_rate$long_term_cntr)

# Visualizacion de la transformada de Fourier de la serie original
plot(Mod(fft_ir), type = "l", ylab = "Modulo", xlab = "Frecuencias")
plot(interest_rate$long_term_cntr, type = "l")
abline(h = 0, lty = 3)

# Frecuencias explicativas de la tasa de interes de largo plazo
ir_fft_freqs_explicativas6 = fft_ir * c(0, rep(1, 6), rep(0, length(fft_ir) - 13), rep(1, 6))
ir_antifft6 = Re(fft(ir_fft_freqs_explicativas6, inverse = TRUE) / nrow(interest_rate))
lines(ir_antifft6, col = "blue", type = "l") # En el último extremo parece que ajusta raro.

ir_rsq_antifft6 = calculo_r2(na.omit(interest_rate$long_term_cntr), ir_antifft6)
ir_rsq_antifft6

# Con las primeras 6 frecuencias logro un coeficiente de determinacion en el orden del 91.14%
ir_fft_freqs_explicativas4 = fft_ir * c(0, rep(1, 4), rep(0, length(fft_ir) - 9), rep(1, 4))
ir_antifft4 = Re(fft(ir_fft_freqs_explicativas4, inverse = TRUE) / nrow(interest_rate))
lines(ir_antifft4, col = "red", type = "l") # En el último extremo parece que ajusta raro.

# Con las primeras 4 frecuencias tambien se encuentra un R cuadrado aceptable.
ir_rsq_antifft4 = calculo_r2(na.omit(interest_rate$long_term_cntr), ir_antifft4)
ir_rsq_antifft4

# Residuos explicativos usando las 4 primeras frecuencias
ir_resid = 
















