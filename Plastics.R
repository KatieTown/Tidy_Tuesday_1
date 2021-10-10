library(tidyverse)
library(reshape2)
library(scales)
install.packages("ggthemes")
library(ggthemes)
vignette("pivot")

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

plastics$year<-as.factor(plastics$year)

Data<-data.frame(plastics[c(2,5,6,8,9,10,11,7,12)])
colnames(Data)<-c("Year", "High Density Polyethylene",
                      "Low Density Polyethylene",
                      "Polyester",
                      "Polypropylene",
                      "Polystyrene",
                      "PVC",
                      "Other", "grand_total")
Data<-Data %>%
  group_by(Year) %>%
  summarise(across(`High Density Polyethylene`:grand_total, sum, na.rm=T))

Data<-pivot_longer(Data, `High Density Polyethylene`:Other)

ggplot(data=Data, aes(fct_rev(fct_reorder(name, value)), value, fill=Year))+
  geom_col(position="dodge2", alpha=.9)+
  scale_fill_manual(values=c("#00688B", "#BD2B48"))+
    labs(y="Worldwide Plastic Count\n", x="", 
    title="Worldwide Plastic Count of Plastic Types between 2019-2020",
    subtitle="T = Thousand",
    caption= "Data from: https://www.breakfreefromplastic.org/")+
  theme(legend.position=c(0.85,0.885),
  legend.title = element_blank(),
  legend.background =  element_rect(fill = "#FFFEF2"),
  plot.background = element_rect(fill = '#FFFEF2', colour = '#FFFEF2'),
  panel.background = element_rect(fill = '#FFFEF2', colour = '#FFFEF2'),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle=element_text(hjust=0.5, size=8))+
  scale_y_continuous(limits=c(0,600000),labels = label_number(suffix = " T", scale = 1e-3))+
  annotate(geom="text", x=6.8, y=12500, label="954",
           color="black", size=3)+
  annotate(geom="text", x=7.2, y=12500, label="2,215",
           color="black", size=3)+
  annotate(geom="text", x=0.76, y=530000, label="518,138",
           color="black", size=3)+
  annotate(geom="text", x=1.2, y=145000, label="132,445",
           color="black", size=3)


ggsave("Graph2.jpeg", dpi=300)

