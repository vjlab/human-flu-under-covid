library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggstream)
library(xlsx)
library("rjson")
library(ggpubr)
library(lubridate)

# color
ggsci_simpsons = c("254 212 57","112 154 225","138 145 151","210 175 129","253 116 70","213 228 162","25 126 192","240 92 59","70 115 46","113 208 245","55 3 53","7 81 73","200 8 19","145 51 31","26 153 147","253 140 193")
colors.16 <- sapply(strsplit(ggsci_simpsons, " "), function(x)
  rgb(x[1], x[2], x[3], maxColorValue=255))

colors.16 <- rev(colors.16)

x <- colors.16[13]
colors.16[13] <- colors.16[4]
colors.16[4] <- x

x <- colors.16[8]
colors.16[8] <- colors.16[11]
colors.16[11] <- x

############# vic ################
vic.meta.input.m <- read_tsv("../data/streamgraph/vic_input.tsv")

mycolors <- colorRampPalette(c("#d7191c","#abd9e9","#fdae61","#2c7bb6"))(4)

vic.stream <- ggplot(vic.meta.input.m, aes(x = date, y = count_clade, fill=clade)) +
  guides(fill=guide_legend(title="Clade",ncol=2))+
  theme(legend.position="none")+
  ylim(-500, 500)+
  scale_x_date(limits = as.Date(c('2018-12-01','2021-08-01')))+
  scale_fill_manual(values=mycolors[c(1,3,2,4)],limits=c('V1A','V1A.3/150K','V1A.3/133R',"V1A/165N"))+
  geom_stream(colour = "black",size=0.1)
vic.stream

############# yam ################
yam.meta.input <- read_tsv("../data/streamgraph/yam_input.tsv")

yam.stream <- ggplot(yam.meta.input, aes(x = date, y = count_clade, fill=clade)) +
  guides(fill=guide_legend(title="Clade",ncol=2))+
  theme(legend.position="none")+
  ylim(-500, 500)+
  scale_x_date(limits = as.Date(c('2018-12-01','2021-08-01')))+
  #scale_fill_manual(values=mycolors[c(1,3,2,4)],breaks=c('V1A','V1A.3/150K','V1A.3/133R',"V1A/165N"))+
  geom_stream(colour = "black",size=0.1)
yam.stream

############# h1n1pdm ################

h1n1pdm.meta.input.m <- read_tsv("../data/streamgraph/h1n1pdm_input.tsv")

h1n1pdm.stream <- ggplot(h1n1pdm.meta.input.m, aes(x = date, y = count_clade, fill=clade)) +
  guides(fill=guide_legend(title="Clade",ncol=2))+
  theme(legend.position="none")+
  ylim(-500, 500)+
  scale_x_date(limits = as.Date(c('2018-12-01','2021-08-01')))+
  scale_fill_manual(values=c(colors.16[1:13],"#F0FFFF","#F0F8FF"),limits=c("6b","6b1","6b1.A","6b1.A/156K","6b1.A/183P-1","6b1.A/183P-2","6b1.A/183P-3","6b1.A/183P-5","6b1.A/183P-5a","6b1.A/183P-5b","6b1.A/183P-6","6b1.A/183P-7","6b1.A/187A","6c","6b1.A/183P-4"))+
  
  geom_stream(colour = "black",size=0.1)
h1n1pdm.stream

############# h3n2 ################

h3n2.meta.input.m <- read_tsv("../data/streamgraph/h3n2_input.tsv")

h3n2.stream <- ggplot(h3n2.meta.input.m, aes(x = date, y = count_clade, fill=clade)) +
  guides(fill=guide_legend(title="Clade",ncol=2))+
  theme(legend.position="none")+
  ylim(-500, 500)+
  scale_x_date(limits = as.Date(c('2018-12-01','2021-08-01')))+
  scale_fill_manual(values=c(colors.16[1:14],"#F0FFFF","#F0F8FF"),limits=c("3c2.A","3c3.A","A1","A1b","A1b/131K","A1b/135K","A1b/135N","A1b/137F","A1b/186D","A1b/197R","A1b/94N","A2","A2/re","A3","3c","A4"))+
  geom_stream(colour = "black",size=0.1)
h3n2.stream

all.stream <- ggarrange(h3n2.stream,h1n1pdm.stream,vic.stream,yam.stream,ncol=1,nrow=4)

ggsave("../results/flu_stream_graph_2018_12_to_2021_07.pdf",all.stream,width=18,height=28,units="cm")
