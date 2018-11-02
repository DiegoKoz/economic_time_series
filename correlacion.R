# Librerias
library(tidyverse)
library(lubridate)
library(forecast)
library(ggrepel)
library(rvest)
library(plotly)

# Datasets
dja = read_csv('data/DJA.csv',skip=4)
interestrate = read_csv('data/INTERESTRATE_1857-2018.csv',skip=1)
sap = read_csv('data/SAP_1871-2018.csv',skip=1)
cpi = read_csv('data/USCPI_1774-2018.csv',skip=4)
gdp = read_csv('data/USGDP_1790-2018.csv',skip=2)
wg  = read_csv('data/USWAGE_1774-2018.csv',skip=3)
gold= read_csv('data/GOLD_1791-2018.csv',skip=3)

# Exploracion
# Consumer Price Index
names(cpi)=c('year', 'cpi')
glimpse(cpi)

# Producto Bruto Interno
for (i in names(gdp)) attr(gdp[[i]], "label") = i
names(gdp) = c("year", "nom_gdp", "real_gdp", "gdp_deflator", "pop", "nom_gdp_cap", "real_gdp_cap")
glimpse(gdp)

# Oro
for (i in names(gold)) attr(gold[[i]], "label") = i
names(gold)=c("year","price")

# Visualizando el problema
cpi_ts = ts(cpi[['cpi']], start=min(cpi[['year']]))
plot(cpi_ts, type='l')

real_gdp_cap_ts = ts(gdp[["real_gdp_cap"]], start=min(gdp[["year"]]))
plot(real_gdp_cap_ts, type='l')

# Trabajando la serie de indice de precios al consumidor
plot(cpi_ts)
cpi_ma10 = ma(cpi_ts, order = 10)
lines(cpi_ma10, col="red")

cpi_filtrada=cpi_ts-cpi_ma10
plot(cpi_filtrada)
plot(diff(cpi_ts))
abline(a=0,b=0)

# Es interesante la frecuencia que había antes de deflaciones, era un fenómeno mucho más común de lo que es en el último tiempo, luego de 1930. ¿Será la forma en que está medida la inflación la que influya en este fenómeno? También pudo haber influido en la ortodoxia económica, influyendo fuertemente en el pensamiento de que los precios pueden bajar, aunque esto casi ya no se ve. También es un mundo con la moneda respaldada por el stock de oro.
plot(Mod(fft(na.omit(cpi_filtrada))))

# No parece que en la observacion de la inflacion haya aliasing.
# Aparecen varias frecuencias, aún no sé cómo se debería interpretar este hallazgo.

plot(Mod(fft(diff(cpi_ts))))
# En los cambios en el nivel de precios se puede ver que hay una frecuencia dominante aún. Quizá todavía se pueda descomponer aún un poco más.

# Oro
# Claramente se rompe todo en los 70's porque se sale del patrón oro durante el gobierno de Nixon
plot(gold, type="l")
gold_ma10=ma(ts(gold$price, start=min(gold$year)),order=10)
plot(gold_ma10)
plot(gold$price-gold_ma10)
sum(!is.na(gold_ma10))
gold_ts=ts(gold$price, start=min(gold$year))

# Supongo que hay algún correlato entre esto y la inflación, ya que se puede emitir moneda sin respaldo alguno en un metal fuerte y de precio estable como fue el oro. Precio determinado por su referencia.
ccf(gold_ts, window(cpi_ts,1791,2017))

# Super correlacionada! Vemos que la correlación más alta es cerca del lag cero, por lo que el levantamiento del patrón oro causó inflación sostenida, en lugar de una estabilidad de precios.
# Diferenciando

gold_ts_diff=diff(gold_ts)
cpi_ts_diff=diff(cpi_ts)

ccf(gold_ts_diff, window(cpi_ts_diff,1791,2017))

# Nuevamente se ve la correlación alta cerca del lag 0 entre cambios del precio del oro y cambios en el nivel de precios. Busco ahora lo mismo a nivel de porcentaje.

