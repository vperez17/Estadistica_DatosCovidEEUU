  #Estudio del impacto del Covid19 en los diferentes estados de EEUU


library(EEUU_Covid19)
library(dplyr)
library(ggplot2)
library(e1071)
library(ppcor)

#Estados con mayor calidad de los datos
EEUU_Covid19_A <- EEUU_Covid19 %>%
  filter(dataQualityGrade == "A+")
EEUU_Covid19_A

#Estados con datos del 2020-12-05
EEUU_Covid19_Hoy <- EEUU_Covid19 %>%
  filter(date > "2020-12-05")
EEUU_Covid19_Hoy


#Recuperados el 5 de diciembre (diagrama de barras)
ggplot(EEUU_Covid19_Hoy, aes(x=state, y=recovered))+geom_col()+ ggtitle("Recuperados el dia 5 de Diciembre")+labs(x="Estado", y="Numero de recuperados")


  #Incremento de muertes (histograma)
    ggplot(EEUU_Covid19_A, aes(x=deathIncrease))+scale_x_log10()+geom_histogram()+ggtitle("Incremento de muertes")+labs(x="Nº de muertes", y="Cantidad")

  #Nº de muertes a lo largo del tiempo (diagrama de cajas)
    ggplot(EEUU_Covid19_A, aes(x=date,y=deathConfirmed, color=state))+scale_y_log10()+geom_boxplot()+ggtitle("Total de muertes confirmadas")+labs(x="Tiempo", y="Numero de muertes confirmadas")

  #Nº Positivos - Nº Hospitalizados (diagrama de lineas)
    ggplot(EEUU_Covid19_A, aes(x=positive,y=hospitalized, color=state))+geom_line()+ggtitle("Gravedad")+labs(x="Numero de positivos", y="Positivos hospitalizados")
  
  #Positivos a lo largo del tiempo (diagrama de lineas)
    ggplot(EEUU_Covid19_A, aes(x=date,y=positive))+geom_line()+ facet_wrap(~state)+ggtitle("Positivos")+labs(x="Tiempo", y="Numero de positivos")

  #Recuperados el 5 de diciembre (diagrama de barras)
    ggplot(EEUU_Covid19_Hoy, aes(x=state, y=recovered))+geom_col()+ ggtitle("Recuperados el dia 5 de Diciembre")+labs(x="Estado", y="Numero de recuperados")

    
    
#Variables de estudio

  #Número de fallecidos
    #media
    summarize (EEUU_Covid19_A, mediaFallecidos = mean(deathConfirmed, na.rm = TRUE))
    #mediana
    summarize (EEUU_Covid19_A, medianaFallecidos = median(deathConfirmed, na.rm = TRUE))
    #quantiles
    summarize (EEUU_Covid19_A, quantilesFallecidos = quantile(deathConfirmed, na.rm = TRUE))
    #desviacion tipica
    summarize (EEUU_Covid19_A, desviacionTipicaFallecidos = sd(deathConfirmed, na.rm = TRUE))
    #kurtosis
    kurtosis(EEUU_Covid19_A$deathConfirmed, na.rm = TRUE)
    #asimetria
    skewness(EEUU_Covid19_A$deathConfirmed, na.rm = TRUE)
    #histograma
    ggplot(EEUU_Covid19_A, aes(x=deathConfirmed))+scale_x_log10()+geom_histogram()+ggtitle("Histograma muertes confirmadas") +labs(x="Muertes confirmadas", y="Cantidad")

  #Incremento de fallecidos
    #media
    summarize (EEUU_Covid19_A, mediaIncrementoMuertos = mean(deathIncrease, na.rm = TRUE)) 
    #mediana
    summarize (EEUU_Covid19_A, medianaIncrementoMuertos = median(deathIncrease, na.rm = TRUE))
    #quantiles
    summarize (EEUU_Covid19_A, quantilesIncrementoMuertos = quantile(deathIncrease, na.rm = TRUE))
    #desviacion tipica
    summarize (EEUU_Covid19_A, desviacionTipicaIncrementoMuertos = sd(deathIncrease, na.rm = TRUE))
    #kurtosis
    kurtosis(EEUU_Covid19_A$deathIncrease, na.rm = TRUE)
    #asimetria
    skewness(EEUU_Covid19_A$deathIncrease, na.rm = TRUE)
    #histograma
    ggplot(EEUU_Covid19_A, aes(x=deathIncrease))+scale_x_log10()+geom_histogram()+ggtitle("Histograma incremento de muertes")+labs(x="Nº de muertes", y="Cantidad")

  #Número de positivos
    #media
    summarize (EEUU_Covid19_A, mediaPositivos = mean(positive, na.rm = TRUE)) 
    #mediana
    summarize (EEUU_Covid19_A, medianaPositivos = median(positive, na.rm = TRUE))
    #quantiles
    summarize (EEUU_Covid19_A, quantilesPositivos = quantile(positive, na.rm = TRUE))
    #desviacion tipica
    summarize (EEUU_Covid19_A, desviacionTipicaPositivos = sd(positive, na.rm = TRUE))
    #kurtosis
    kurtosis(EEUU_Covid19_A$positive, na.rm = TRUE)
    #asimetria
    skewness(EEUU_Covid19_A$positive, na.rm = TRUE)
    #histograma
    ggplot(EEUU_Covid19_A, aes(x=positive))+scale_x_log10()+geom_histogram()+ggtitle("Histograma de casos positivos")+labs(x="Nº de positivos", y="Cantidad")

  #Número de hospitalizados
    #media
    summarize (EEUU_Covid19_A, mediaHospitalizados = mean(hospitalized, na.rm = TRUE))
    #mediana
    summarize (EEUU_Covid19_A, mediaHospitalizados = median(hospitalized, na.rm = TRUE))
    #quantiles
    summarize (EEUU_Covid19_A, quantilesHospitalizados = quantile(hospitalized, na.rm = TRUE))
    #desviacion tipica
    summarize (EEUU_Covid19_A, desviacionTipicaHospitalizados = sd(hospitalized, na.rm = TRUE))
    #kurtosis
    kurtosis(EEUU_Covid19_A$hospitalized, na.rm = TRUE)
    #asimetria
    skewness(EEUU_Covid19_A$hospitalized, na.rm = TRUE)
    #histograma
    ggplot(EEUU_Covid19_A, aes(x=hospitalized))+scale_x_log10()+geom_histogram()+ggtitle("Histograma hospitalizados")+labs(x="Nº de hospitalizados", y="Cantidad")

  #Número de recuperados
    #media
    summarize (EEUU_Covid19_Hoy, mediaRecuperados = mean(recovered, na.rm = TRUE))
    #mediana
    summarize (EEUU_Covid19_Hoy, medianaRecuperados = median(recovered, na.rm = TRUE))
    #quantiles
    summarize (EEUU_Covid19_Hoy, quantilesRecuperados = quantile(recovered, na.rm = TRUE))
    #desviacion tipica
    summarize (EEUU_Covid19_Hoy, desviacionTipicaRecuperados = sd(recovered, na.rm = TRUE))
    #kurtosis
    kurtosis(EEUU_Covid19_Hoy$recovered, na.rm = TRUE)
    #asimetria
    skewness(EEUU_Covid19_Hoy$recovered, na.rm = TRUE)
    #histograma
    ggplot(EEUU_Covid19_Hoy, aes(x=recovered))+scale_x_log10()+geom_histogram()+ggtitle("Histograma recuperados")+labs(x="Nº de recuperados", y="Cantidad")

    
#Para dos variables
    #Diagrama de dispersion
    plot(x=EEUU_Covid19_A$positive, y=EEUU_Covid19_A$hospitalized,main = "Diagrama de dispersion", xlab = "Positivos", ylab = "Hospitalizados")
    
    #Recta de regresion (nº positivos - nº hopitalizados)
    regresion <- lm(EEUU_Covid19_A$hospitalized ~ EEUU_Covid19_A$positive, data =EEUU_Covid19_A)
    plot(x=EEUU_Covid19_A$positive, y=EEUU_Covid19_A$hospitalized,main = "Recta de regresion", xlab = "Positivos", ylab = "Hospitalizados")
    abline(regresion)
    
    #correlacion dos variables
    cor.test(x=EEUU_Covid19_A$positive, y=EEUU_Covid19_A$hospitalized, method = "pearson")
