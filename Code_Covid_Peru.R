#.............................................
#              Covid-19 Perú
#.............................................
#
# Universidade Federal do Rio de Janeiro 
# Departamento de Métodos estatísticos
#
# Author : Pamela Massiel Chiroque Solano
# Date   : 04/04/2020
# Email  : pchiroque@gmail.com
#        : pamela@dme.ufrj.br
# City   : Rio de Janeiro, Friburgo 
#.............................................


##### Artigo: Figure #####

require(dplyr)
require(tidyr)
require(ggplot2)

rm(list = ls())
setwd("~/Documents/COVID_19/CodePeru")

A.J <- read.csv(file = "Artigo_COVID-19 deaths by pop.csv",header = TRUE,sep = ";",dec = ".")%>%as.data.frame()
A.J$country <- as.factor( c("Brazil","Chile", "Colombia","Ecuador", "Peru",        
                            "Japan", "Korea", "China",                    
                            "Spain", "United States"))

WeekFirst.Death <- A.J$WeekFirst.Death

WeekFirst.Death <- c( 1.8, 2., 2.2, 2.4, 0.8, 3, 3.9, 1.1, 4.1, 5)

dat <- as.data.frame(cbind(WeekFirst.Death,-20000000))

n.week <- 16

tirar.name.week <- c(paste("week_",1:n.week,sep = ""))

A.J <- A.J[setdiff(A.J%>%colnames(), tirar.name.week)]

A.J <-A.J%>%`colnames<-`(c("region","country","pop",            
                           "debut","X1st.Death","X1stD_elapse",   
                           "WeekFirst.Death",1:n.week))%>%
  gather(Week,numberDeath,paste(1:n.week,sep=""))


A.J <- A.J%>%
  mutate(Mortality=(log(numberDeath/pop)*1000000) )

Mortality2=((A.J$numberDeath/A.J$pop)*1000000)

A.J$Week <- as.numeric(A.J$Week)
A.J$WeekFirst.Death <- as.numeric(A.J$WeekFirst.Death )

n.country <- A.J%>%dplyr::select(country)%>%table%>%names

dat$country <- as.factor( c("Brazil","Chile", "Colombia","Ecuador", "Peru",        
                            "Japan", "Korea", "China",                    
                            "Spain", "United States"))


custom.Esc <- c(
  "brown", # "#CC79A7",# Brazil
  rgb(255,194,54, maxColorValue = 255), #Chile
  "green",#rgb(88,187,132, maxColorValue = 255), # China
  rgb(0,233,248, maxColorValue = 255), #Colom
  rgb(150,44,230, maxColorValue = 255), #ecuador
  "#000000", #Japan
  "violet" , # Korea
  rgb(255,62,44, maxColorValue = 255), #"#C4961A", # Peru
  "#00AFBB",#"#D55E00",  # Spain
  "blue" # #United states
) 

bb <- seq(min(A.J$Mortality,na.rm = TRUE),
          max(A.J$Mortality,na.rm = TRUE),length=7)

MM <- as.character(c(round(seq(min(Mortality2,na.rm = TRUE),
                               max(Mortality2,na.rm = TRUE),length=7),3)[1],round(seq(min(Mortality2,na.rm = TRUE),
                                                                                      max(Mortality2,na.rm = TRUE),length=7),0)[-1] ))

MM <- c("0.003","", "", "1",   "10",   "100",   "490"  )
p <- A.J%>%
  ggplot()+
  #  ylim(c(0,75))+
  geom_line(aes(x=Week,y=Mortality,color=country,size=country),show.legend = TRUE)+
  geom_point(data = dat, aes(x=WeekFirst.Death,y=V2,color=country,size=country),shape=8,show.legend = FALSE)+
  scale_size_manual(values=c(rep(1.5,7),2.7, rep(1.5,2) ))+
  scale_color_manual(values=custom.Esc)+
  theme_bw()+
  theme(axis.text.x = element_text(size=20,angle=0,hjust = 0.5),
        axis.text.y = element_text(size=20),
        axis.title = element_text(size=20),
        strip.text =  element_text(size=20),
        plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18))+
  theme(legend.position = "bottom",
        legend.box.spacing =unit(1, "cm"),
        legend.title = element_text(size=20),
        legend.text = element_text(size = 18))+
  scale_x_continuous(breaks = c(1:n.week),
                     labels = c(1:n.week)) +
  #guides(size =FALSE)+
  scale_y_continuous( breaks = bb,
                      labels = MM) +
  labs(color="Country",size="Country",
       title = "Death per one million population in selected countries",
       subtitle = "(Logarithmic scale - weekly updated until April 26th, 2020)" )+
  xlab("Timeline in weeks since the first death") +
  ylab("Mortality")+ theme(axis.text=element_text(size=20),
                           axis.title=element_text(size=20))+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )+
  annotate(geom="text", x=15, y=-20000000, label=  " * 1st Death - week", 
           size=4) 
p
