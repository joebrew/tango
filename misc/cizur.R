library(dplyr)
library(ggplot2)
library(tango)
x = census %>% filter(grepl('Cizur', municipio)) %>%
  mutate(total = ifelse(sexo == 'Hombres', total * -1, total))

ggplot(data = x,
       aes(x = edad,
           y = total,
           fill = sexo,
           group = sexo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(breaks = seq(-50, 50, by = 25),
                     labels = abs(seq(-50, 50, by = 25)))

y = census %>% group_by(sexo, edad) %>%
  summarise(total = sum(total, na.rm = T)) %>%
  mutate(total = ifelse(sexo == 'Hombres', total * -1, total))

ggplot(data = y,
       aes(x = edad,
           y = total,
           fill = sexo,
           group = sexo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(breaks = seq(-50, 50, by = 25),
                     labels = abs(seq(-50, 50, by = 25)))

z = municipios[municipios@data$id == x$id[1],]
library(sp)
plot(z)
