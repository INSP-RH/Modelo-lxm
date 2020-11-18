rm(list=ls())
library(readxl)
library(stringi)
library(survey)
library(lubridate)
library(tidyverse)

#Load database from the Mexican surveillance system of COVID-19 
load("Data/200906COVID19MEXICO.rda")

#Vector to complete missing dates in the database 
days.v <- seq.Date(as.Date("2020-02-27"),
                   as.Date("2020-02-27") + 90, "day") #91 max days simulated


#Get reported covid cases in Mexico City the first 91 days of the pandemic
REPORTADOS <- data_060920_small %>%
    filter(RESULTADO == 1, ENTIDAD_UM == 9)  %>%
    mutate(FECINGRE= as.Date(FECHA_INGRESO, format = "%Y-%m-%d"),
           POSITIVO = 1) %>%
    mutate(FECINGRE = if_else(FECINGRE < "2020-02-27", 
                              as.Date("2020-02-27", format = "%Y-%m-%d"), 
                              FECINGRE))  %>%
    group_by(FECINGRE)  %>%
    summarise(CASOS = sum(POSITIVO)) %>%
    mutate(CASOS.ACUM = cumsum(CASOS)) %>%
    mutate(DAY = 1) %>%
    complete(FECINGRE = days.v,
             fill = list(CASOS = 0, CASOS.ACUM = 0, DAY = 1)) %>%
    mutate(DAY = cumsum(DAY)) 


days <- 91 #max days simulated

#Load results from the model simulations
load("Results/raw.4x3.rda")
load("Results/raw.5x2.rda")
load("Results/raw.4x10.rda")
load("Results/raw.6x1.rda")
load("Results/raw.3x4.rda")



mat_list <- list(raw43, raw410, raw52, raw61, raw34)

df_list <- lapply(mat_list, as.data.frame)
names(df_list) <- c("Resultados.43", "Resultados.410", "Resultados.52", "ctf", "Resultados.34")


#===================================================================================
#                          Get model summary of each lxm scheme
#===================================================================================

qest = c(0.40, 0.5, 0.60)         
casos_list <- list()

for (i in 1:length(df_list)) {
    
    casos <- df_list[[i]] 
    casos <- (t(apply(casos, 1, function (x) quantile(x, na.rm = TRUE, qest))))          
    casos_list[[i]] <- as.data.frame(casos)
    names(casos_list)[i] <- names(df_list)[i]
    rm(casos)
    
}


#===================================================================================
#                                Figure 3 in appendix
#===================================================================================

#pdf("validation.pdf", width = 15)
ggplot() +
    
    theme_classic() +
    
    
    geom_ribbon(data = casos_list$ctf, aes(x = 1:days, ymin = `40%`, ymax = `60%`), 
                fill = "#00BFC4", alpha = 0.2) +
    
    
    geom_line(data = REPORTADOS[1:days, ], aes(x = 1:days, y = CASOS.ACUM, 
                                               color = "Casos reportados en \n Ciudad de México"), 
              linetype = "solid", group = 1, size = 0.75) +
    
    
    
    geom_line(data = casos_list$ctf, aes(x = 1:days, y = `50%`, 
                                         color = "Modelo ajustado"), group = 1, size = 0.75) +
    
    
    labs(color = "") +
    scale_x_continuous(limits = c(1, days), breaks = seq (0, days, 10)) +
    scale_y_continuous(labels = scales::comma) +
    xlab("Días desde el inicio del brote") +
    ylab("Casos acumulados") +
    
    theme(legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          title = element_text(size = 22))
#dev.off()


#===================================================================================
#                                Figure 4 in appendix
#===================================================================================

#pdf("lxm_sims.pdf", width = 15)
ggplot() +
    
    theme_classic() +
    
    
    geom_line(data = REPORTADOS[1:days, ], aes(x = 1:days, y = CASOS.ACUM, 
                                               color = "Casos reportados en \n Ciudad de México"), 
              linetype = "solid", group = 1, size = 0.75) +
    
    
    geom_vline(xintercept = 56, linetype = "dashed") +
    
    
    geom_line(data = casos_list$Resultados.43, aes(x = 1:days, y = `50%`,
                                                   color = "4x3"), group = 1, size = 0.75) +
    
    
    geom_line(data = casos_list$Resultados.34, aes(x = 1:days, y = `50%`,
                                                   color = "3x4"), group = 1, size = 0.75) +
    
    
    
    geom_line(data = casos_list$Resultados.410, aes(x = 1:days, y = `50%`,
                                                    color = "4x10"), group = 1, size = 0.75) +
    
    
    geom_line(data = casos_list$Resultados.52, aes(x = 1:days, y = `50%`,
                                                   color = "5x2"), group = 1, size = 0.75) +
    
    geom_line(data = casos_list$ctf, aes(x = 1:days, y = `50%`, 
                                         color = "Modelo ajustado"), group = 1, size = 0.75) +
    
    
    
    
    labs(color = "Esquema de cuarentena") +
    
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(limits = c(1, days), breaks = seq (0, days, 10)) +
    xlab("Días desde el inicio del brote") +
    ylab("Casos acumulados") +
    
    theme(legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          title = element_text(size = 22))
#dev.off()

