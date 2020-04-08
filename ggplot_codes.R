library(tidyverse)
library(knitr)

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

rough_plot <- ggplot(data=a_countries, aes(x=year, y=lifeExp, colour=continent))+
  geom_line()+
  facet_wrap(~country)

rough_plot+
  labs(title="Figure 1",
       x="Year",
       y="Life Expectancy",
       colour="Continent")

rough_plot+
  labs(title="Life expectancy by 'A' countries from 1950 to 2009",
       x="Year",
       y="Life Expectancy",
       colour="Continent",
       caption = "Data source: Gapminder")+
  theme_bw()+#theme_bw() + theme_minimal() + theme_linedraw are some inbuilt options
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face="bold")
  )


lifeExp_plot <- rough_plot+
  labs(title="Life expectancy by 'A' countries from 1950 to 2009",
       x="Year",
       y="Life Expectancy",
       colour="Continent",
       caption = "Data source: Gapminder")+
  theme_bw()+#theme_bw() + theme_minimal() + theme_linedraw are some inbuilt options
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face="bold"),
    strip.background = element_blank(),
    panel.grid.major=element_line(size=1),
    axis.title = element_text(size=10, color="blue"),
    legend.position = "bottom"
  )

ggsave(filename="outputs/lifeExp.png", plot=lifeExp_plot, width=12, height=10, dpi=300,
       units="cm")

library(cowplot)

plot1 <- ggplot(gapminder, aes(x=gdpPercap, y=lifeExp))+geom_point()
plot2 <- ggplot(gapminder, aes(x=continent, y=lifeExp))+geom_boxplot()
plot3 <- ggplot(gapminder, aes(x=gdpPercap, y=pop))+geom_point()
plot4 <- ggplot(gapminder, aes(x=lifeExp, y=pop))+geom_point()

plot_grid(plot1, plot2, plot3, plot4)

plot_grid(plot1, plot2, plot3, plot4, rel_heights=c(1,3)) #(first row, second row) - we are saying increase second row 3 times more than the first

plot_grid(plot1, plot2, plot3, plot4, rel_heights=c(1,3),
          rel_widths = c(2,1)) #(first row, second row) - we are saying increase second row 3 times more than the first

combined_plot <- plot_grid(plot1, plot2, plot3, plot4, labels ="AUTO")

ggsave(filename="outputs/combined plot.png", plot=combined_plot, width=16, height=10, dpi=300,
       units="cm")

aus_gapminder <- filter(gapminder, country=="Australia")
ggplot(aus_gapminder)+
  aes(x=year, y=pop)+
  geom_line()+
  scale_y_log10()
