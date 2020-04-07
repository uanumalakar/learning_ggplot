library(tidyverse)

gapminder <- read_csv("data/gapminder_data.csv")

gapminder_1977 <- filter(gapminder, year==1977)

ggplot(data=gapminder_1977,
       mapping=aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop))+
  geom_point()+
  scale_x_log10()

ggplot(data=gapminder_1977,
       mapping=aes(x=lifeExp, y=gdpPercap, colour=continent, size=pop))+
  geom_point()+
  scale_x_log10()

ggplot(data=gapminder_1977,
       mapping=aes(x=pop, y=gdpPercap, colour=continent, size=lifeExp))+
  geom_point()+
  scale_x_log10()

gapminder_1977 %>% 
ggplot()+
  geom_point(mapping=aes(x=gdpPercap, y=lifeExp, shape=continent, size=pop))+
  geom_line(mapping=aes(x=gdpPercap, y=lifeExp))+
  scale_x_log10()

gapminder_1977 %>% 
  ggplot(mapping=aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop))+
  geom_point()+
  scale_x_log10()

gapminder_1977 %>% 
  ggplot(mapping=aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop))+
  geom_point(shape=21, colour="black", fill="white", size=5, stroke=5)+
  scale_x_log10()

gapminder_1977 %>% 
  ggplot(mapping=aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop))+
  geom_point()+
  scale_x_log10()


gapminder%>% 
  ggplot(mapping=aes(x=year, y=lifeExp))+
  geom_point()


gapminder%>% 
  ggplot(mapping=aes(x=year, y=lifeExp, colour=continent))+
  geom_point()+
  geom_line()+
  geom_point()

gapminder%>% 
  ggplot(mapping=aes(x=year, y=lifeExp, colour=continent, group=country))+
  geom_point(colour="black")+
  geom_line()

gapminder%>% 
  ggplot(mapping=aes(x=year, y=lifeExp, group=country))+
  geom_point(colour="black")+
  geom_line(aes(colour=continent))

gapminder %>% 
  group_by(continent, year) %>% 
  summarise(mean_lifeExp=mean(lifeExp)) %>% 
  ggplot(aes(x=year, y=mean_lifeExp, colour=continent))+ 
  geom_point()

gapminder %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp))+
  geom_point(alpha=0.5)+
  scale_x_log10()+
  geom_smooth(method = "lm")


gapminder %>% 
  ggplot(aes(x=lifeExp, y=gdpPercap))+
  geom_point(aes(colour=continent, shape= continent, alpha=0.5))+
  scale_x_log10()+
  geom_smooth(method = "lm", size=2, colour="red")+
  scale_colour_manual(values=c("tomato", "steelblue", "orange", "yellow", "dodgerblue", "green", "purple"))


a_countries <- filter(gapminder, str_starts(country, "A"))

ggplot(a_countries, aes(x=year, y=lifeExp, colour=continent, group=country))+
  geom_line()+
  facet_wrap(~country)

ggplot(
  data = gapminder, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
) +
  geom_point(alpha=0.5) +
  facet_wrap(~year)+
  scale_x_log10()+
  geom_smooth(colour="red", method="lm")
