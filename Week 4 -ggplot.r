# install.packages("tidyverse")

library(tidyverse)

data()

BOD

?BOD

# 0-6.35
ggplot(data = BOD, mapping = aes(x = Time, y = demand)) +
  geom_point(size = 5) +
  geom_line(color = "red")



ggplot(BOD, aes(Time, demand)) +
  geom_point(size = 3) +
  geom_line(color = "red")


data()

view(CO2)


# 14.45
CO2 %>%
  ggplot(aes(conc, uptake, colour = Treatment)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Type) +
  labs(title = "Concentration of co2") +
  theme_bw()


CO2 %>%
  ggplot(aes(Treatment, uptake)) +
  geom_boxplot() +
  geom_point(
    alpha = 0.5,
    aes(size = conc, colour = Plant)
  ) +
  facet_wrap(~Type) +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Chilled vs Non-chilled",
  )

# 19.49
View(mpg)
mpg %>%
  filter(cty < 25) %>%
  ggplot(aes(displ, cty)) +
  geom_point(aes(colour = drv, size = trans), alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~year, nrow = 1) +
  labs(title = "Displacement vs Highway MPG") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    x = "Engine Size",
    y = "MPG in the City ",
    title = "Fuel efficiency",
  ) +
  theme_bw()
