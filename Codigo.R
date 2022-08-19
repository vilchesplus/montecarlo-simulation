library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)
library(xts)

ticker <- c("FB", "AMZN", "AAPL", "NFLX","GOOG")
#precios <- read.csv("~/Dropbox/Master Análisis de datos/TFM/Datos históricos FAANG.csv", sep=";", quote="", dec = ",",stringsAsFactors=FALSE)

precios <- read.delim2("~/Dropbox/Master Análisis de datos/TFM/Datos históricos FAANG.csv")
precios <- as_tibble(precios)
precios <- precios %>%
  mutate(Fecha = dmy(Fecha))  #lubridate library
str(precios)
head(precios)
#as.Date(precios$Fecha,format='%d-%m-%Y')
precios <- tk_xts(precios, date_var = Fecha) #timetk library
str(precios)

head(precios, 4)

precios_mensuales <- to.monthly(precios, indexAt = "last", OHLC = FALSE) #xts package
head(precios_mensuales,4)

rendimientos_activo <- na.omit(Return.calculate(precios_mensuales, method = "log")) #PerformanceAnalytics package
head(rendimientos_activo,4)



highchart(type = "stock") %>% 
  hc_title(text = "Monthly Log Returns") %>%
  hc_add_series(rendimientos_activo[, ticker[1]], 
                name = ticker[1]) %>%
  hc_add_series(rendimientos_activo[, ticker[2]], 
                name = ticker[2]) %>%
  hc_add_series(rendimientos_activo[, ticker[3]], 
                name = ticker[3]) %>%
  hc_add_series(rendimientos_activo[, ticker[4]], 
                name = ticker[4]) %>%
  hc_add_series(rendimientos_activo[, ticker[5]], 
                name = ticker[5]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)




  

hc_hist_fun <- function(n = 1, object, color){
  hc_hist <- hist(object[, ticker[n]],
                  breaks = 50,
                  plot = FALSE)
  hchart(hc_hist, color = color) %>%
    hc_title(text =
               paste(ticker[n],"Log Returns Distribution",
                     sep = " ")) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
}


hc_hist_fun(1, rendimientos_activo, "cornflowerblue")
hc_hist_fun(2, rendimientos_activo, "green")
hc_hist_fun(3, rendimientos_activo, "pink")
hc_hist_fun(4, rendimientos_activo, "purple")
hc_hist_fun(5, rendimientos_activo, "yellow")

#map(1:5, hc_hist_fun, rendimientos_activo, "blue")


boxplot(as.tibble(rendimientos_activo), col = rainbow(ncol(as.tibble(rendimientos_activo)))) #Boxplot para comprobar como los valores de la mediana están siempre por encima de 0.0
abline(0,0, col="thistle",lty=2)
w <- c(0.20,
       0.20,
       0.20,
       0.20,
       0.20)
tibble(w,ticker)




rendimientos_cartera_xts_rebalanceo_anual <- 
  Return.portfolio(rendimientos_activo, weights = w, rebalance_on = "years") %>%
  `colnames<-`("rendimientos")
rendimientos_cartera_xts_sin_rebalanceo_anual <- 
  Return.portfolio(rendimientos_activo, weights = w) %>%
  `colnames<-`("rendimientos")
head(rendimientos_cartera_xts_rebalanceo_anual,4)
tail(rendimientos_cartera_xts_rebalanceo_anual, 10)

highchart(type = "stock") %>% 
  hc_title(text = "Rendimientos mensuales de la cartera") %>%
  hc_add_series(rendimientos_cartera_xts_rebalanceo_anual$rendimientos, 
                name = "Rebalanceo anual", color = "cornflowerblue") %>%
  hc_add_series(rendimientos_cartera_xts_sin_rebalanceo_anual$rendimientos, 
                name = "Sin Rebalanceo anual", color = "brown") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)  


hc_cartera <- hist(rendimientos_cartera_xts_rebalanceo_anual$rendimientos, breaks = 50, plot = FALSE)

hchart(hc_cartera, color = "blue", name = 'Cartera') %>% 
  hc_title(text = "Distribucion rendimientos de la cartera") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_add_theme(hc_theme_flat())

boxplot(as.tibble(rendimientos_cartera_xts_rebalanceo_anual$rendimientos)) #Boxplot para comprobar como los valores de la mediana están siempre por encima de 0.0.
abline(0,0, col="red",lty=2)

crecimiento_cartera_dolar <- 
  Return.portfolio(rendimientos_activo, 
                   wealth.index = 1, weights = w, rebalance_on = "years") %>%
  `colnames<-`("Crecimiento") 

head(crecimiento_cartera_dolar,4)
tail(crecimiento_cartera_dolar,4)

highchart(type = "stock") %>% 
  hc_add_series(crecimiento_cartera_dolar, 
                name = "cartera", color = "cornflowerblue", lineWidth = 1) %>%
  hc_title(text = "Crecimiento de un dolar") %>%
  hc_yAxis(title = list(text = "Crecimiento del dolar"),
           opposite = FALSE,
           labels = list(format = "${value}")) %>% 
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE)

matriz_cov <- cov(rendimientos_activo)
cartera_sd <- StdDev(rendimientos_activo, weights = w)
cartera_sd_porcentaje <- round(cartera_sd * 100, 2)

sd_plot <- sd(rendimientos_cartera_xts_rebalanceo_anual$rendimientos)
mean_plot <- mean(rendimientos_cartera_xts_rebalanceo_anual$rendimientos)

rendimientos_activo_tidy <- 
  precios %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "fecha") %>%
  gather(activo, rendimientos, -fecha) %>% 
  group_by(activo) %>%  
  mutate(rendimientos = (log(rendimientos) - log(lag(rendimientos)))) %>%
  spread(activo, rendimientos) %>% 
  select(fecha, ticker) %>%
  na.omit()

rendimientos_activo_alargado <- 
  rendimientos_activo_tidy %>% 
  gather(activo, rendimientos, -fecha)



ggplot(rendimientos_cartera_xts_rebalanceo_anual, aes(x = Index, y = rendimientos)) +  geom_point(color = "cornflowerblue") +   ggtitle("Dispersión de los rendimientos por fecha")+ theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Fecha")




  


rendimientos_activo_alargado %>%
  group_by(activo) %>%
  summarize(sd = 100 *sd(rendimientos)) %>%
  add_row(activo = "Cartera",
          sd = sd(rendimientos_cartera_xts_rebalanceo_anual$rendimientos)) %>%
  ggplot(aes(x = activo,
             y = sd,
             colour = activo)) +
  geom_point() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(
    aes(x = "Cartera",
        y =
          sd(rendimientos_cartera_xts_rebalanceo_anual$rendimientos) + .2),
    label = "Cartera",
    color = "cornflowerblue") +
  labs(y = "Desviación Típica", x = "Activo") + 
  ggtitle("Desviación típica  de cada activo y cartera en porcentaje")


rendimientos_activo_alargado %>%
  group_by(activo) %>%
  summarise(Rendimiento_esperado = mean(rendimientos),
            Desviación_típica = sd(rendimientos)) %>%
  add_row(activo = "Cartera",
          Desviación_típica =
            sd(rendimientos_cartera_xts_rebalanceo_anual$rendimientos),
          Rendimiento_esperado =
            mean(rendimientos_cartera_xts_rebalanceo_anual$rendimientos)) %>%

  ggplot(aes(x = Desviación_típica,
             y = Rendimiento_esperado,
             color = activo)) +
  geom_point(size = 2) +
  geom_text(
    aes(x =
          sd(rendimientos_cartera_xts_rebalanceo_anual$rendimientos) * 1.11,
        y =
          mean(rendimientos_cartera_xts_rebalanceo_anual$rendimientos),
        label = "Cartera")) +
  ylab("Rendimiento esperado") +
  xlab("Desviación típica") +
  ggtitle("Rendimientos mensuales esperados versus Riesgo") +
  scale_y_continuous(labels = function(x){ paste0(x, "%")}) +
  # The next line centers the title
  theme_update(plot.title = element_text(hjust = 0.5))


window <- 12
rolling_sd_xts <- rollapply(rendimientos_cartera_xts_rebalanceo_anual,
                                 FUN = sd, 
                                 width = window) %>% na.omit()
head(rolling_sd_xts)

rolling_sd_xts_hc <- round(rolling_sd_xts, 4) * 100

highchart(type = "stock") %>% 
  hc_title(text = "Volatilidad de la Cartera") %>%
  hc_add_series(rolling_sd_xts_hc) %>% 
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(
    labels = list(format = "{value}%"), 
    opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE)


sd(rendimientos_cartera_xts_rebalanceo_anual)




StdDev(rendimientos_activo, weights = w, portfolio_method
       = "component") ### StdDev - portfolio standard deviation
### contribution -  individual asset's contribution to a whole portfolio risk (standard deviation). The sum of all individuals contributions gives the portfolio StdDev
### pct_contrib_stdDev - contribution in %. Basically, pct_contrib_stdDev = contribution / StdDev 

 a <- StdDev(rendimientos_activo, weights = w, portfolio_method
       = "component")
names(a$pct_contrib_StdDev)
as.tibble(a$pct_contrib_StdDev) %>%
  add_column(activos = names(a$pct_contrib_StdDev))

hist(rendimientos_cartera_xts_rebalanceo_anual, main = "Histograma de los rendimientos de la cartera", xlab = "Rendimientos cartera", ylab = "Frecuencia")

as.tibble(a$pct_contrib_StdDev) %>%
  ggplot(aes(x = names(a$pct_contrib_StdDev), y = a$pct_contrib_StdDev)) +
  geom_col(fill = 'cornflowerblue',
           colour = 'pink',
           width = .6) +
  scale_y_continuous(labels = percent,
                     breaks = pretty_breaks(n = 20)) +
  ggtitle("Contribucion en porcentaje al riesgo de la cartera") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Activo") +
  ylab("Porcentaje de contribucion al Riesgo")



#Rolling contribucion riesgo activos:

my_interval_sd <- function(returns_df, start = 1, window = 12, weights){
  
  # Fecha de inicio
  start_date <- returns_df$fecha[start]
  
  # Fecha final que depende de la fecha inicial y del valor de window.
  end_date <-  returns_df$fecha[c(start + window)]
  
  # Filtramos entre las fechas de inicio y final.
  interval_to_use <- returns_df %>% filter(fecha >= start_date & fecha < end_date)
  
  # Convertimos en objeto xts para poder utilizar la función StdDev.
  returns_xts <- interval_to_use %>% tk_xts(date_var = fecha) 
  
  # Pesos de la cartera.
  w <- weights
  
  
  results_as_xts <- StdDev(returns_xts, weights = w, portfolio_method = "component")
  
  # Convertimos los resultados a formato tibble.
  results_to_tibble <- tk_tbl(t(results_as_xts$pct_contrib_StdDev)) %>% 
    mutate(fecha = ymd(end_date)) %>%
    mutate_if(is.numeric, function(x) x * 100) %>% 
    select(fecha, everything())  
}

test_my_function_1 <- my_interval_sd(rendimientos_activo_tidy, start = 3, window = 12, weights = w)
test_my_function_1$fecha
rendimientos_activo_tidy$fecha[3]
end_date <-  rendimientos_activo_tidy$fecha[c(3 + window)]
interval_to_use <- rendimientos_activo_tidy %>% filter(fecha >= rendimientos_activo_tidy$fecha[3] & fecha < end_date)
returns_xts <- interval_to_use %>% tk_xts(date_var = fecha) 
results_as_xts <- StdDev(returns_xts, weights = w, portfolio_method = "component")
results_as_xts$contribution
results_to_tibble <- tk_tbl(t(results_as_xts$pct_contrib_StdDev)) %>% 
  mutate(fecha = ymd(end_date)) %>%
  mutate_if(is.numeric, function(x) x * 100) %>% 
  select(fecha, everything()) 

# Marginal contribution of each asset. 
marginal_contribution <- w %*% cov(returns_xts) / sd(returns_xts)
component_contribution <- marginal_contribution * w 
components_summed <- rowSums(component_contribution)
component_percentages <- component_contribution / sd(returns_xts)

covariance_matrix <- 
  cov(rendimientos_activo)

sd_portfolio <- 
  sqrt(t(w) %*% covariance_matrix %*% w)

marginal_contribution <- w %*% cov(rendimientos_activo) / sd_portfolio
component_contribution <- marginal_contribution * w 
components_summed <- rowSums(component_contribution)
component_percentages <- component_contribution / cartera_sd[1,1]


portfolio_vol_components_tidy <-
  map_df(1:(nrow(rendimientos_activo_tidy) - window),
         my_interval_sd,
         returns_df = rendimientos_activo_tidy,
         weights = w,
         window = window)

window <- 12

tail(portfolio_vol_components_tidy)
head(portfolio_vol_components_tidy,20)

portfolio_vol_components_tidy_xts <-
  portfolio_vol_components_tidy %>%
  tk_xts(date_var = fecha,
         silent = TRUE)

highchart(type = "stock") %>%
  hc_title(text = "Contribucion a la volatilidad") %>%
  hc_add_series(portfolio_vol_components_tidy_xts[, 1],
                name = ticker[1]) %>%
  hc_add_series(portfolio_vol_components_tidy_xts[, 2],
                name = ticker[2]) %>%
  hc_add_series(portfolio_vol_components_tidy_xts[, 3],
                name = ticker[3]) %>%
  hc_add_series(portfolio_vol_components_tidy_xts[, 4],
                name = ticker[4]) %>%
  hc_add_series(portfolio_vol_components_tidy_xts[, 5],
                name = ticker[5]) %>%
  hc_yAxis(labels = list(format = "{value}%"),
           max = max(portfolio_vol_components_tidy_xts) + 5,
           min = min(portfolio_vol_components_tidy_xts) - 5,
           opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)


highchart() %>%
  hc_chart(type = "area") %>%
  hc_title(text = "Contribucion a la volatilidad") %>%
  hc_plotOptions(area = list(
    stacking = "percent",
    lineColor = "#ffffff",
    lineWidth = 1,
    marker = list(
      lineWidth = 1,
      lineColor = "#ffffff"
    ))
  ) %>%
  hc_add_series(portfolio_vol_components_tidy_xts[, 1],
                name = ticker[1]) %>%
  hc_add_series(portfolio_vol_components_tidy_xts[, 2],
                name = ticker[2]) %>%
  hc_add_series(portfolio_vol_components_tidy_xts[, 3],
                name = ticker[3]) %>%
  hc_add_series(portfolio_vol_components_tidy_xts[, 4],
                name = ticker[4]) %>%
  hc_add_series(portfolio_vol_components_tidy_xts[, 5],
                name = ticker[5]) %>%
  hc_yAxis(labels = list(format = "{value}%"),
           opposite = FALSE) %>%
  hc_xAxis(type = "datetime") %>%
  hc_tooltip(pointFormat =
               "<span style=\"color:{series.color}\">
             {series.name}</span>:<b>{point.percentage:.1f}%</b><br/>",
             shared = TRUE) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

media_rendimientos <- mean(rendimientos_cartera_xts_rebalanceo_anual$rendimientos)
desv_rendimientos <- sd(rendimientos_cartera_xts_rebalanceo_anual$rendimientos)

sim_mensuales <- rnorm(120, media_rendimientos, desv_rendimientos)
head(sim_mensuales)
tail(sim_mensuales)

sim_rend <- 
  tibble(c(1, 1 + sim_mensuales)) %>% 
  `colnames<-`("rendimientos")
head(sim_rend)
tail(sim_rend)


sim_crecimiento <- 
  sim_rend %>%
    mutate(crecimiento = cumprod(rendimientos)) %>% 
  select(-rendimientos)
head(sim_crecimiento)
tail(sim_crecimiento)


TCAC <- ((sim_crecimiento$crecimiento[nrow(sim_crecimiento)] ^ (1/10)) -1) * 100
TCAC

TCAC <- ((sim_crecimiento$crecimiento[nrow(sim_crecimiento)] / sim_crecimiento$crecimiento[1]) ^ (1/(10)) - 1 ) *100
TCAC

simulacion_cumprod <- function(inicial, N, mean, stdev) {
  tibble(c(inicial, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("rendimientos") %>%
    mutate(crecimiento = cumprod(rendimientos)) %>% 
    select(crecimiento)
}




head(simulacion_cumprod(1, 120, media_rendimientos, desv_rendimientos))
tail(simulacion_cumprod(1, 120, media_rendimientos, desv_rendimientos))


sims <- 1000
starts <-
  rep(1, sims) %>%
  set_names(paste("sim", 1:sims, sep = ""))

monte_carlo_sim_1000 <- 
  map_dfc(starts, simulacion_cumprod,
          N = 120, mean = media_rendimientos, 
          stdev = desv_rendimientos) %>% 
  mutate(mes = seq(1:nrow(.))) %>% 
  select(mes, everything()) %>% 
  `colnames<-`(c("mes", names(starts)))


tail(monte_carlo_sim_1000 %>% select(mes, sim1, sim2, sim999,sim1000))


mc_gathered <-
  monte_carlo_sim_1000 %>%
  gather(sim, crecimiento, -mes) %>%
  group_by(sim)
head(mc_gathered)
tail(mc_gathered)


hchart(mc_gathered,
       type = 'line',
       hcaes(y = crecimiento,
             x = mes,
             group = sim)) %>%
  hc_title(text = "1000 Simulaciones") %>%
  hc_xAxis(title = list(text = "meses")) %>%
  hc_yAxis(title = list(text = "crecimiento dólar"),
           labels = list(format = "${value}")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)

sim_summary <-
  monte_carlo_sim_1000 %>%
  gather(sim, crecimiento, -mes) %>%
  group_by(sim) %>%
  summarise(final = last(crecimiento)) %>%
  summarise(
    max = max(final),
    min = min(final),
    mediana = median(final))
sim_summary

mc_max_med_min <-
  mc_gathered %>%
  filter(
    last(crecimiento) == sim_summary$max ||
      last(crecimiento) == sim_summary$median ||
      last(crecimiento) == sim_summary$min) %>%
  group_by(sim)

hchart(mc_max_med_min,
       type = 'line',
       hcaes(y = crecimiento,
             x = mes,
             group = sim)) %>%
  hc_title(text = "Mínimo y máximo") %>%
  hc_xAxis(title = list(text = "meses")) %>%
  hc_yAxis(title = list(text = "crecimiento dólar"),
           labels = list(format = "${value}")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)
