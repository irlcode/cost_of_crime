library(ggplot2)
library(scales)

estimates <- readRDS("data/yearly_total_crime_cost.rds")

official_crime_costs_plot <- ggplot(data = estimates,
                                    aes(x = as.Date(paste0("01-01-",year),
                                                    format = "%d-%m-%Y"),
                                        y = point_est,
                                  group = name)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = .3, colour = NA, fill = "grey70") +
  geom_line(aes(y = point_est),linetype = "solid", lwd = 1)+
  geom_point(size=3)+
  geom_text(aes(label=gsub("\\.",",",
                           as.character(format(round(point_est,digits = 1),
                                               nsmall = 1))),
                y = point_est),
            hjust=0.5, vjust=-1)+
  theme_minimal() + 
  theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 year",minor_breaks = NULL,expand = c(0,0),
               labels=date_format("%Y"),
               limits = as.Date(c('2015-12-01','2020-02-01')))+
  labs(x = NULL,y = "Совокупная стоимость преступлений в год, млрд руб.")+
  theme(legend.position = "none",panel.grid.minor = element_blank()) +
  expand_limits(y = 0) +
  facet_wrap(name~.,ncol = 1,strip.position = "top",scales = 'free_y')

cairo_pdf("media/official_crime_costs.pdf", height = 10, width = 15)
official_crime_costs_plot
dev.off()

# EN version
official_crime_costs_plot_en <- ggplot(data = estimates,
                                    aes(x = as.Date(paste0("01-01-",year),
                                                    format = "%d-%m-%Y"),
                                        y = point_est,
                                        group = name_en)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = .3, colour = NA, fill = "grey70") +
  geom_line(aes(y = point_est),linetype = "solid", lwd = 1)+
  geom_point(size=3)+
  geom_text(aes(label=as.character(format(round(point_est,digits = 1),
                                               nsmall = 1)),
                y = point_est),
            hjust=0.5, vjust=-1)+
  theme_minimal() + 
  theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 year",minor_breaks = NULL,expand = c(0,0),
               labels=date_format("%Y"),
               limits = as.Date(c('2015-12-01','2020-02-01')))+
  labs(x = NULL,y = "Yearly total cost of crime, bln rubles")+
  theme(legend.position = "none",panel.grid.minor = element_blank()) +
  expand_limits(y = 0) +
  facet_wrap(name_en~.,ncol = 1,strip.position = "top",scales = 'free_y')

cairo_pdf("media/official_crime_costs_en.pdf", height = 10, width = 15)
official_crime_costs_plot_en
dev.off()
