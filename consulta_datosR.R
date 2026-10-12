
library(RPresto)
library("RPostgreSQL")
library(ggplot2)
library(Rcpp)
library(grid)
library(sp)
#library(mapview)
#library(rgdal)
library(raster)
#library(ggmap)
library(dplyr)
library(lubridate)
library(patchwork)
library(stringr)
library(tidyr)
library(purrr)

conteo_f<-data.frame(1,1,1,1,1,1,1,1,1,1,1,1,1)

names(conteo_f)<-c("Estacion","Precipitación-10min", "Vel. viento-10 min","Dir. viento-10 min","Presión Atmosferica-Hr","Temperatura-Hr", "Humedad Relativa-Hr", "Nivel de Agua-Hr","Bateria-Hr","Rad. visible-Hr","Rad. global-Hr","Temp. máxima-Hr","Temp. minima-Hr")
names(conteo_f)<-c("Estacion","1","2","3","4","5","6","7","8","11","12","85","45")

conteo_f <- conteo_f %>%
  mutate(Estacion = as.character(Estacion))

cat_pol<-openxlsx::read.xlsx("F:/proyectos/contraloria_2026_419est/ESTADO_RED_CTR.xlsx",sheet="ajust")

#cat_pol <- cat_pol %>% 
#  filter(str_detect(Propietario, "^AO"))

drivPostgre <- dbDriver("PostgreSQL")
##esta linea consulta la replica de polaris, no la operativa
con_scada <- dbConnect(drivPostgre, dbname = "ideam", host = "172.16.1.193", port = 5432,
                       user = "fda_ideam", password = "Fd4_1d34m201902")

conPres <- dbConnect(
  RPresto::Presto(),
  host='172.16.50.20',
  port='8080',
  user='<jorge>',
  schema='<raw>',
  catalog='<cassandra>'
)

#1:lluvia,2:vel viento, 3:dirviento, 4: presion, 5: temperatura, 6: humedad, 7: nivel, 11: rad visible, 12: rad global

cat_variables <- data.frame(
  id_measure = c(1, 2, 3,4,5,6,7,8,11,12,85,45),
  nombre_var = c("Precipitación-10min", "Vel. viento-10 min","Dir. viento-10 min","Presión Atmosferica-Hr","Temperatura-Hr", "Humedad Relativa-Hr", "Nivel de Agua-Hr","Bateria-Hr","Rad. visible-Hr","Rad. global-Hr","Temp. máxima-Hr","Temp. minima-Hr"),
  # Límite máximo esperado de datos al mes para ajustar la escala de color
  max_esperado = c(4500,4500,4500,744,744,744,744,744,744,744,744,744) 
)
######################
### este for crea los graficos de numero de datos por variable
### consulta datos de cassandra y de polaris
######################

for (i in 1:400) {
  
  id = cat_pol[i, 2]
  
  ## solo definir las variables de busqueda.
  if (cat_pol[i, 4] == "HIDROLÓGICA") {
    medidas = c(1, 7, 8)
  } else {
    medidas = c(1, 2, 3, 4, 5, 6, 8, 11, 12, 85, 45)
  }
  
  medidas_sql <- paste(medidas, collapse = ", ")
  
  prec_datos <- dbGetQuery(con_scada, paste0(
    "SELECT id_stz, id_measure, date_record, date_ins, raw_data FROM data_radio.archive_data ",
    "WHERE id_measure IN (", medidas_sql, ") AND date_record >= CAST('2023-01-02 00:00:00' AS TIMESTAMP)",
    "AND id_stz = ", id
  ))
  
  ## CONSULTA ULTIMOS DATOS CASSANDRA COPIA DE POLARIS
  prec_datos2 <- dbGetQuery(conPres, paste0(
    "SELECT id_stz, id_measure, date_record, date_ins, raw_data FROM cassandra.polaris.archive_data ",
    "WHERE id_measure IN (", medidas_sql, ") AND date_record >= CAST('2023-01-02 00:00:00' AS TIMESTAMP) ",
    "AND id_stz = ", id
  ))
  
  if (dim(prec_datos)[1] > 0 | dim(prec_datos2)[1] > 0) {
    UNION <- bind_rows(prec_datos, prec_datos2)
  } else {
    UNION = 0
  }
  
  ## ESTE CONDICIONAL BUSCA LOS CODIGOS ALTERNOS ESTACIONES CON c_
  if (!is.na(cat_pol[i, 5])) {
    id = cat_pol[i, 5]
    
    prec_datos <- dbGetQuery(con_scada, paste0(
      "SELECT id_stz, id_measure, date_record, date_ins, raw_data FROM data_radio.archive_data ",
      "WHERE id_measure IN (", medidas_sql, ") AND date_record >= CAST('2023-01-02 00:00:00' AS TIMESTAMP)",
      "AND id_stz = ", id
    ))
    
    prec_datos2 <- dbGetQuery(conPres, paste0(
      "SELECT id_stz, id_measure, date_record, date_ins, raw_data FROM cassandra.polaris.archive_data ",
      "WHERE id_measure IN (", medidas_sql, ") AND date_record >= CAST('2023-01-02 00:00:00' AS TIMESTAMP) ",
      "AND id_stz = ", id
    ))
    
    if (dim(prec_datos)[1] > 0 | dim(prec_datos2)[1] > 0) {
      UNION2 <- list(prec_datos, prec_datos2) %>%
        keep(~ !is.null(.x) && nrow(.x) > 0) %>%
        map_dfr(~ .x %>% mutate(date_ins = as.POSIXct(date_ins)))
    } else {
      UNION2 = 0
    }
    
    if (exists("UNION2") && is.data.frame(UNION2) && nrow(UNION2) > 0) {
      UNION <- bind_rows(UNION, UNION2)
    }
  }
  
  ## Se ordenan los datos y se hacen las gráficas y conteos
  if (is.data.frame(UNION)) {
    prec_datos <- UNION %>% filter(date_record >= "2023-01-02")
    
    conteo_mensual <- prec_datos %>%
      mutate(
        date_record = as.POSIXct(date_record),
        ano = year(date_record),
        mes = month(date_record)
      ) %>%
      group_by(ano, mes, id_measure) %>%
      summarise(total_registros = n(), .groups = "drop") %>%
      arrange(ano, mes)
    
    conteo_anual <- prec_datos %>%
      mutate(
        date_record = as.POSIXct(date_record),
        ano = year(date_record)
      ) %>%
      group_by(ano, id_measure) %>%
      summarise(total_registros = n(), .groups = "drop") %>%
      arrange(ano)
    
    conteo_anual_total <- prec_datos %>%
      mutate(
        date_record = as.POSIXct(date_record),
        ano = year(date_record)
      ) %>%
      group_by(ano) %>%
      summarise(total_registros = n(), .groups = "drop") %>%
      arrange(ano)
    
    conteo_periodo_variable <- prec_datos %>%
      group_by(id_measure) %>%
      summarise(
        total_registros = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(total_registros))
    
    fila_actual <- conteo_periodo_variable %>%
      mutate(id_measure = as.character(id_measure)) %>%
      pivot_wider(
        names_from = id_measure,
        values_from = total_registros,
        values_fill = 0
      ) %>%
      bind_rows(as.data.frame(matrix(0, nrow = 0, ncol = length(names(conteo_f)), 
                                     dimnames = list(NULL, names(conteo_f))))) %>%
      select(all_of(names(conteo_f)))
    
    fila_actual <- fila_actual %>%
      mutate(Estacion = as.character(Estacion))
    
    fila_actual[1] <- cat_pol[i, 7]
    
    fila_actual[setdiff(names(conteo_f), names(fila_actual))] <- 0
    fila_actual <- fila_actual[names(conteo_f)]
    
    if (i == 1) {
      conteo_f[i, ] <- fila_actual
    } else {
      conteo_f <- bind_rows(conteo_f, fila_actual)
    }
    
    # 3. Validar si la consulta procesada devolvió datos para graficar
    if (nrow(prec_datos) == 0) {
      message(paste("La estación", id, "no tiene datos registrados para las variables indicadas."))
    } else {
      
      # 4. Cruzar datos con el catálogo para obtener los nombres de las variables
      df_procesado <- prec_datos %>%
        inner_join(cat_variables, by = "id_measure") %>%
        mutate(
          date_record = as.POSIXct(date_record),
          ano = as.character(year(date_record)),
          mes = month(date_record),
          mes_nombre = factor(
            mes, 
            levels = 1:12, 
            labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                       "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
          )
        ) %>%
        group_by(id_measure, nombre_var, max_esperado, ano, mes_nombre) %>%
        summarise(total_registros = n(), .groups = "drop") %>%
        complete(
          nesting(id_measure, nombre_var, max_esperado),
          ano = as.character(2023:2026),
          mes_nombre = factor(c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                              levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                         "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
          fill = list(total_registros = 0)
        ) %>%
        mutate(ano = as.factor(ano))
      
      # 5. Generar lista de gráficos individuales
      lista_graficos <- list()
      vars_presentes <- unique(df_procesado$id_measure)
      
      for (var_id in vars_presentes) {
        df_var <- df_procesado %>% filter(id_measure == var_id)
        nombre_actual <- unique(df_var$nombre_var)
        max_val <- unique(df_var$max_esperado)
        
        p <- ggplot(df_var, aes(x = mes_nombre, y = ano, fill = total_registros)) +
          geom_tile(color = "white", linewidth = 0.4) +
          geom_text(aes(label = ifelse(total_registros == 0, "", total_registros)), color = "black", size = 2.5) +
          scale_fill_gradientn(
            colors = c("#D73027", "#FEE08B", "#1A9850"),
            values = c(0, 0.5, 1),
            limits = c(0, max_val),
            oob = scales::squish,
            name = "Datos"
          ) +
          labs(
            title = nombre_actual,
            x = "Mes",
            y = "Año"
          ) +
          theme_minimal() +
          theme(
            panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 11, color = "#1A237E"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.text.y = element_text(size = 8)
          )
        
        lista_graficos[[as.character(var_id)]] <- p
      }
      
      # 6. Unir gráficas y guardar en la carpeta destino
      panel_final <- wrap_plots(lista_graficos, ncol = 2) + 
        plot_annotation(
          title = paste("Disponibilidad de Datos - Estación", cat_pol[i,7],"-",cat_pol[i,4],"Estado: ",cat_pol[i,9]," Tipo TX: ",cat_pol[i,10]),
          subtitle = "Conteo mensual por variable con escalas adaptadas al total esperado",
          theme = theme(plot.title = element_text(size = 14, face = "bold"))
        )
      
      ggsave(
        filename = paste0("F:/proyectos/contr_imagenes3/", cat_pol[i, 7], "_.pdf"),
        plot = panel_final,
        width = 20,
        height = 12,
        units = "in",
        dpi = 500
      )
    }
    
  } else {
    message(paste("La estación con ID", id, "no tiene datos en UNION."))
  }
  
  print(i)
  if (exists("UNION")) rm(UNION)
}

# Desconexión de bases de datos al terminar
dbDisconnect(con_scada)
dbDisconnect(conPres)

#writexl::write_xlsx(conteo_f,"F:/proyectos/contraloria_2026_419est/datos_conteo.xlsx")






library(scales)

fil1<-prec_datos[prec_datos$date_record>"2020-01-01 00:00:00",]

conteos <- fil1 %>%
  mutate(
    date_record = as.POSIXct(date_record), # Asegura formato fecha/hora
    ano = year(date_record),
    mes = month(date_record)
  ) %>%
  group_by(ano,id_measure) %>%
  summarise(total_registros = n(), .groups = "drop") %>%
  arrange(ano)

# 2. Generar el gráfico de barras agrupadas

conteos$ano <- factor(conteos$ano)

p<-ggplot(conteos, aes(x = ano, y =total_registros , fill = factor(id_measure))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = comma(total_registros)), 
    position = position_dodge(width = 0.8), 
    angle=90,
    hjust = -0.2, 
    size = 2.8,
    fontface = "plain"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_viridis_d(
    option = "turbo",
    labels = c(
      "1" = "Lluvia 10 min",
      "2" = "Viento - vel 10 min",
      "3" = "Viento - dir 10 min",
      "4" = "Presión atmosferica - Horaria",
      "5" = "Temperatura - Horaria",
      "6" = "Humedad - Horaria",
      "8" = "Bateria - Horaria",
      "12" = "Radiación global",
      "45" = "Temperatura minima",
      "85" = "Temperatura máxima"
    )
  ) +
  labs(
    title = "Comparativo Año a Año por Categoría",
    subtitle = "Evolución de valores totales (2020 - 2022)",
    x = "Año",
    y = "Valor Total",
    fill = "Categoría"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    plot.title = element_text(face = "bold", size = 8),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

ggsave(
  filename = paste0("F:/proyectos/contr_imagenes3/",id,"_.pdf"),
  plot = p,
  width = 8,
  height = 6,
  units = "in",     # Unidades: "in" (pulgadas), "cm", "mm", o "px"
  dpi = 300
)

}