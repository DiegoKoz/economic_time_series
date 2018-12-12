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

antifft = function(fouriert, cant_freq) {
    # cant_freq tiene que llevar las primeras frecuencias que se desean
    largo_fourier = length(fouriert)
    total_posiciones_freq = 2 * cant_freq + 1
    vector_multiplicador = c(1, rep(1, cant_freq), rep(0, largo_fourier - total_posiciones_freq), rep(1, cant_freq))
    Re(fft(fouriert * vector_multiplicador, inverse = TRUE) / largo_fourier)
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
ir_resid_4 = interest_rate$long_term_cntr - ir_antifft4

plot(y = interest_rate$long_term_cntr, interest_rate$year, type = "l")
abline(h = 0, lty = 3)
lines(y = ir_resid_4, interest_rate$year, type = "l", col = "red")

#### Indice de precios al consumidor de los EEUU ####
# Visualización de los datos
plot(y = cpi[["cpi"]], x = cpi[["year"]], type = "l", ylab = "Nivel de precios", xlab = "Tiempo")

cpi[["cpi_log"]] = log10(cpi[["cpi"]])

# Armando las dos regresiones lineales por periodo
cpi_log_lm_pre  = lm(cpi_log ~ year, data = cpi[cpi$year <= 1900,])
cpi_log_lm_post = lm(cpi_log ~ year, data = cpi[cpi$year > 1900,])

plot(cpi[["cpi_log"]], x = cpi[["year"]], type = "l", xlab = "Año", ylab = "Log10(CPI)")
abline(v = c(1971, 1945, 1929), col = "red", lty = 3)
text(x = 1910, y = 2.0, labels = "Crisis '30")
text(x = 1945, y = 2.2, labels = "Fin SGM")
text(x = 1971, y = 2.3, labels = "Fin patrón\noro")
abline(reg = cpi_log_lm_pre, col = "blue")
abline(reg = cpi_log_lm_post, col = "blue")

# Residuos de tendencias lineales del IPC centrados antes y despues del 1900 
cpi_log_cntr_pre = cpi$cpi_log[cpi$year <= 1900] - predict(cpi_log_lm_pre,
                                                           newdata = cpi[cpi$year <= 1900, "year"])
cpi_log_cntr_post = cpi$cpi_log[cpi$year > 1900] - predict(cpi_log_lm_post,
                                                           newdata = cpi[cpi$year > 1900, "year"])

plot(y = cpi_log_cntr_pre, x = cpi[["cpi_log"]][cpi[["year"]] <= 1900], xlab = "Año",
     ylab = "Log10(CPI) centrado", type = "l")

fft_cpi_log_cntr_pre  = fft(cpi_log_cntr_pre)
fft_cpi_log_cntr_post = fft(cpi_log_cntr_post)

plot(Mod(fft_cpi_log_cntr_pre), type = "l", xlab = "Frecuencias", ylab = "Modulo", main = "Pre-1900")
plot(Mod(fft_cpi_log_cntr_post), type = "l", xlab = "Frecuencias", ylab = "Modulo", main = "Post-1900")

cpi_antifft_9_pre = antifft(fft_cpi_log_cntr_pre, 9)
cpi_antifft_9_post = antifft(fft_cpi_log_cntr_post, 9)

plot(y=c(cpi_log_cntr_pre, cpi_log_cntr_post), x=cpi[["year"]], type="l")
lines(y=c(cpi_antifft_9_pre, cpi_antifft_9_post), x=cpi[["year"]], col="red")
abline(v=1900,col="blue",lty=3)

# R cuadrado
cpi_rsq_antifft_9 = calculo_r2(c(cpi_log_cntr_pre, cpi_log_cntr_post), c(cpi_antifft_9_pre, cpi_antifft_9_post))
cpi_rsq_antifft_9 
cpi_rsq_antifft_9_pre = calculo_r2(cpi_log_cntr_pre, cpi_antifft_9_pre)
cpi_rsq_antifft_9_pre 
cpi_rsq_antifft_9_post = calculo_r2(cpi_log_cntr_post, cpi_antifft_9_post)
cpi_rsq_antifft_9_post 

#### PBI Real de los EEUU ####
# Centrando los datos
real_gdp_lm   = lm(log10(real_2012_base) ~ year, data = gdp)
real_gdp_cntr = log10(gdp[["real_2012_base"]]) - predict(object = real_gdp_lm, newdata = gdp[,"year"])

plot(real_gdp_cntr, type = "l")
abline(h=0, col="blue", lty=3)

# Transformada de Fourier
fft_real_gdp_cntr = fft(real_gdp_cntr)
plot(Mod(fft_real_gdp_cntr), type = "l")

# Antifft
real_gdp_antifft_9 = antifft(fft_real_gdp_cntr, 9)
plot(y=real_gdp_cntr, x=gdp[["year"]], type = "l", xlab="Año", ylab="Log10(PBI) centrado")
lines(y=real_gdp_antifft_9, x=gdp[["year"]], col="red")
abline(h=0,lty=3)

real_gdp_antifft_10 = antifft(fft_real_gdp_cntr, 10)
plot(y=real_gdp_cntr, x=gdp[["year"]], type = "l", xlab="Año", ylab="Log10(PBI) centrado")
lines(y=real_gdp_antifft_10, x=gdp[["year"]], col="red")
abline(h=0,lty=3)

# R cuadrado
real_gdp_r2_9 = calculo_r2(real_gdp_cntr, real_gdp_antifft_9)
real_gdp_r2_10 = calculo_r2(real_gdp_cntr, real_gdp_antifft_10)

# No cambia mucho al usar mas frecuencias 
























