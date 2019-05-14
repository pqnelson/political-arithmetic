library("ggplot2")
library("ggthemes")

xs <- 98.0
n <- 14.0
fun.1 <- function(x) (n/xs)*((xs/(xs + x))**(n+1))

expected <- 98.0/13.0

my_breaks <- c(0,10,20,30,40)

p <- ggplot(data = data.frame(x = c(0,40)), aes(x = x)) +
  stat_function(fun = fun.1, geom="line") + 
  scale_x_continuous(breaks = c(pretty(my_breaks), expected), labels = c(pretty(my_breaks), '7.5')) +
  geom_vline(xintercept = expected, color = "#FF2700") +
  labs(caption="http://political-arithmetic.blogspot.com",
       title= "Days until next Democratic announcement", 
       subtitle="after April 8th, 2019") +
  theme_fivethirtyeight(base_size = 8) +
  theme(axis.title = element_text()) + ylab("Probability") + xlab("Days")
ggsave("img/2018-04-primary.png", plot=p, width=3, height=3)

# fivethirtyeight_pal()(3)
# lists 3 colors for use