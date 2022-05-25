create_crime_counts <- function(){
  library(tabulizer)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  pdfs <- list.files("~/Dropbox/IRL/cost_of_crime_2021/GP/",full.names = T)
  xlsx <- list.files("~/Dropbox/IRL/cost_of_crime_2021/4EGS/",full.names = T)
  
  extract_table_from_gp  <- function(pdf_path) {
    
    year <- substr(pdf_path, nchar(pdf_path)-7, nchar(pdf_path)-4)
    page <- 40
    area_list = list(c(85, 5, 569, 300))
    if (year == 2020) {
      page <- 36
    }
    print(year)
    tabel <- extract_tables(pdf_path,pages = page, method = 'lattice', guess = F, area = area_list)[[1]]
    #tabel <- tabel[[1]][,1:2]
    tabel <- as.data.frame(tabel)
    tabel <- setNames(tabel, c("label","value"))
    tabel$year <- year
    return(tabel)
  }
  
  pdf_tables <- lapply(pdfs, extract_table_from_gp)
  
  gp_table <- rbindlist(pdf_tables)
  
  gp_table$value <- as.numeric(gsub(" ",'',gp_table$value, fixed = T))
  
  gp_table <- gp_table %>% 
    filter(!is.na(value)) %>% 
    filter( label %in% c("кража","разбой","грабеж",
                         "мошенничество","мошенничество ст. 159-159.6 УК РФ")) 
  
  gp_table$label[gp_table$label == 'мошенничество ст. 159-159.6 УК РФ'] <- "мошенничество"
  
  gp_table <- gp_table %>% arrange(desc(year))
  
  gp_result <- pivot_wider(gp_table, names_from = year, names_sort = F)
  
  ### Working with 4-EGS data sheets
  
  xlsx <- list.files("~/Dropbox/IRL/cost_of_crime_2021/4EGS/",full.names = T)
  
  extract_from_sheet <- function(xls_file,sheet_number){
    tab <- suppressMessages(read_excel(xls_file,sheet = sheet_number))
    
    year <- substr(xls_file, nchar(xls_file)-7, nchar(xls_file)-4)
    label <- tab[[2]][4]
    value <- tab[[2]][12]
    
    return(data.frame(year = year,
                      crime_label = label,
                      value = as.numeric(value),
                      sheet_number = sheet_number,
                      xls_file = basename(xls_file)))
  }
  
  extract_crime_stats_from_excel_file <- function(xls_file, crime_labels){
    print(basename(xls_file))
    tab <- rbindlist(lapply(1:124,
                            function(x) extract_from_sheet(xls_file = xls_file,
                                                           sheet_number = x)))
    
    tab <- tab %>% filter(str_detect(string = crime_label,
                                     pattern = paste(crime_labels, collapse = "|")))
    
    tab$crime_type <- str_extract(tab$crime_label, paste(crime_labels, collapse="|"))
    return(tab)
    
  }
  
  # All types of crime/sheet numbers:
  temp <- rbindlist(lapply(1:145, function(x) extract_from_sheet(xls_file = xlsx[[1]], sheet_number = x)))
  
  crime_labels <- c("умышленное причинение тяжкого вреда здоровью ст. 111 УК РФ",
                    "ч. 4 ст. 111",
                    "средней тяжести",
                    #"здоровью                         ст. 112 УК РФ",
                    "умышленное причинение легкого вреда здоровью ст. 115 УК РФ",
                    "побои ст. 116 УК РФ",
                    "путем кражи ст. 158 УК РФ",
                    "путем мошенничества ст. 159 УК РФ",
                    "путем грабежа ст. 161 УК РФ",
                    "разбоя"
  )
  
  tab <- rbindlist(lapply(xlsx,
                          function(x) extract_crime_stats_from_excel_file(x,
                                                                          crime_labels)))
  tab <- tab %>% filter(str_detect(crime_label,"по мотивам",negate = T))
  
  crime_type_recode <- c(`умышленное причинение тяжкого вреда здоровью ст. 111 УК РФ` = 'Тяжкий вред здоровью',
                         `ч. 4 ст. 111` = "Тяжкий вред и смерть",
                         `средней тяжести`="Средняя тяжесть здоровью",
                         `умышленное причинение легкого вреда здоровью ст. 115 УК РФ`="Лёгкий вред здоровью",
                         `побои ст. 116 УК РФ` = "Побои",
                         `путем кражи ст. 158 УК РФ` = "Кража",
                         `путем мошенничества ст. 159 УК РФ` = "Мошенничество",
                         `путем грабежа ст. 161 УК РФ` = "Грабеж",
                         `разбоя` = "Разбой")
  
  tab$crime_type <- recode(tab$crime_type, !!!crime_type_recode)
  
  tab_wide <- tab %>% select(year, crime_type, value) %>% 
    pivot_wider(names_from = crime_type, names_sort = T) %>% 
    mutate(`Тяжкий вред здоровью` = `Тяжкий вред здоровью` - `Тяжкий вред и смерть`,
           `Грабеж и разбой` = Грабеж + Разбой,
           `Нападение` = `Тяжкий вред здоровью` + `Лёгкий вред здоровью` + `Средняя тяжесть здоровью` + Побои) %>% 
    pivot_longer(!year) %>%
    arrange(desc(year)) %>% 
    pivot_wider(names_from = year) %>% 
    filter(!(name %in% c("Тяжкий вред и смерть","Лёгкий вред здоровью", "Средняя тяжесть здоровью",
                         "Тяжкий вред здоровью","Разбой","Грабеж","Побои")))
  
  tab_wide[5,] <- list("Удалённое преступление",510400,294409,174674, NA, NA)
  
  tab_long <- tab_wide %>% pivot_longer(!name, names_to = "year", values_to = "total_count")
  
  ###
  
  gam_estimates <- data.frame(
    stringsAsFactors = FALSE,
    name = c("Нападение","Грабеж и разбой",
             "Кража","Мошенничество","Удалённое преступление"),
    point_est = c(242218L, 300418L, 205322L, 151018L, 61804L),
    lower_ci = c(66840L, 30189L, 26451L, 0L, 0L),
    upper_ci = c(402589L, 554212L, 376168L, 289789L, 133379L)) %>% 
    pivot_longer(!name, names_to = "estimate_type", values_to = "single_cost")
  
  estimates <- left_join(tab_long, gam_estimates, by = 'name') %>% 
    mutate(total_cost = round(total_count*single_cost / 1000000000,2)) %>% 
    pivot_wider(id_cols = c("name",'year'),names_from = estimate_type, values_from = total_cost)
  
  estimates$name <- factor(estimates$name, levels = c("Кража","Мошенничество","Грабеж и разбой","Нападение","Удалённое преступление"))
  
  saveRDS(estimates, "yearly_total_crime_cost.rds")
}

create_crime_counts()


### Plot

estimates <- readRDS("yearly_total_crime_cost.rds")

library(ggplot2)
library(scales)
#library(ggrepel)
official_crime_costs_plot <- ggplot(data = estimates, aes(x = as.Date(paste0("01-01-",year), format = "%d-%m-%Y"), y = point_est,
                                  group = name)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = .3, colour = NA, fill = "grey70") +
  geom_line(aes(y = point_est),linetype = "solid", lwd = 1)+
  geom_point(size=3)+
  geom_text(aes(label=gsub("\\.",",",as.character(format(round(point_est,digits = 1),nsmall = 1))),
                y =point_est),
            hjust=0.5, vjust=-1)+
  theme_minimal() + theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 year",minor_breaks = NULL,expand = c(0,0),
               labels=date_format("%Y"),
               limits = as.Date(c('2015-12-01','2020-02-01')))+
  labs(x = NULL,y = "Совокупная стоимость преступлений в год, млрд руб.")+#, color = "Тип преступлений")+
  #scale_color_discrete(breaks = levels(with(estimates, reorder(name, point_est))))+
  theme(legend.position = "none",panel.grid.minor = element_blank()) +
  expand_limits(y = 0) +
  facet_wrap(name~.,ncol = 1,strip.position = "top",scales = 'free_y')

cairo_pdf("official_crime_costs.pdf", height = 10, width = 15)
official_crime_costs_plot
dev.off()
