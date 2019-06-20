library(stringr)
library(ggplot2)

#змінюємо назви предметів
values_subjects$subject <- values_subjects$subject  %>% 
   str_replace_all('Ball100', '') %>% 
   str_replace_all('bio', 'Біологія') %>% 
   str_replace_all('chem', 'Хімія') %>%
   str_replace_all('eng', 'Англійська мова') %>%
   str_replace_all('deu', 'Німецька мова') %>%
   str_replace_all('fra', 'Французька мова') %>% 
   str_replace_all('geo', 'Географія') %>% 
   str_replace_all('hist', 'Історія України') %>% 
   str_replace_all('math', 'Математика') %>% 
   str_replace_all('phys', 'Фізика') %>%
   str_replace_all('spa', 'Іспанська мова') %>%
   str_replace_all('Ukr', 'Українська мова та література')

#сортуємо у відповідності до середнього балу
values_subjects <- values_subjects %>% 
   mutate(subject = forcats::fct_reorder(subject, mean))

#визначаємо середнє значення
mean <- rivne %>% 
   filter(value != "NA", value != 0) %>% 
   summarise(mean = mean(value))

#Візуалізуємо
png("vis_rivne_subjects.png", width = 2900, height = 2400)

ggplot(values_subjects)+
   geom_linerange(aes(x = subject, ymin = 0, ymax = mean), size = 70, color = "#00609d", alpha = 1)+
   geom_hline(yintercept = 0, size = 3, alpha = 1, color = "#333629", linetype = "solid")+
   geom_hline(yintercept = mean$mean, linetype = "dashed", color = "#f73149", size = 3)+
   geom_label(aes(x = subject, y = -1.5, label = stringr::str_wrap(subject, 17), family = "Ubuntu Mono Bold"), 
              hjust = 1, size = 15.5, label.size = 0, fontface = "bold", label.r = unit(0, "lines"), fill = "#ffffff", alpha = 0.9, color = "black")+
   geom_text(aes(x = subject, label = mean, y = 2, family = "Ubuntu Medium"),
             size = 26, alpha = 0.9, colour = "white", hjust = 0, vjust = 0.5)+
   geom_label(aes(label = "Середній бал", x = 3.2, y = 156), label.padding = unit(0.0, "lines"), label.size = 0, fill = "#ffffff", size = 18, colour = "black", family = "Ubuntu Mono", vjust = 0)+
   geom_curve(aes(x = 3, y = 153, xend = 2.5, yend = 139.5), 
              color = 'black', curvature = -0.3, size = 2, arrow = arrow(length = unit(15, 'pt')))+
   geom_label(aes(label = str_wrap("Лише один учасник тестування", 20), x = 9.3, y = 183), label.padding = unit(0.0, "lines"), label.size = 0, fill = "#ffffff", size = 18, colour = "black", family = "Ubuntu Mono", vjust = 0)+
   geom_curve(aes(x = 10, y = 175, xend = 11, yend = 164), 
              color = 'black', curvature = 0.3, size = 2.5, arrow = arrow(length = unit(15, 'pt')))+
   
   coord_flip()+
   scale_y_continuous(limits = c(-26.5, 200))+
   
   labs(title = "Як здавали ЗНО на Рівненщині | 2018", 
        subtitle = "Історія України найважче дається вступникам, хоча це 2-ий за популярністю предмет")+
   
   theme_minimal(base_family = "Ubuntu Medium")+
   theme(
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      text = element_text(family = "Ubuntu Medium"),
      plot.title = element_text(family = "Ubuntu Bold", 
                                face = "bold",
                                size = 100, 
                                margin = margin(t = 40, b = 10),
                                colour = "#000000",
                                hjust = 0),
      plot.subtitle = element_text(family = "Ubuntu Light",
                                   size = 60, 
                                   face = "bold",
                                   margin = margin(t = 40, b = 60),
                                   colour = "#222830",
                                   hjust = 0),
      legend.position = 'top',
      plot.background = element_rect(fill = "#ffffff", color = "#ffffff", size = 10, linetype='solid'),
      plot.margin = unit(c(2, 1, 3, 2.5), "cm"))

dev.off()  
