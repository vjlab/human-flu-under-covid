library(mapproj)
library(ggplot2)
library(rgdal)
library(plyr)
library(readxl)
library(scatterpie)
library(ggnewscale)
library(ggtree)
library(ape)
library(tidyverse)
library(RColorBrewer)
library(treeio)
library(dplyr)
library("rjson")
library(ggmap)
library(rworldmap)
library(ggpubr)

#color
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

#load gps
country.gps <- read_tsv("../data/countries_gps.txt")

############# vic ################

tree <- read.nexus("../results/iqtree/vic.afa.timetree.nex")

vic.meta.all <- read_tsv("../data/vic_HA_meta.tsv")

clade <- tree_subset(tree,node=1522, levels_back=0)

(p <- ggtree(clade, mrsd = "2021-06-25", alpha = 0.1) + 
    theme_tree2()+
    #geom_text2(aes(subset = !isTip, label=label),size = 1)+  #support value
    #geom_text(aes(label=node), hjust=-.3)+
    scale_x_continuous(limits = c(2007.5,2021.6),breaks = c(2010,2012,2014,2016,2018,2020))+
    scale_y_continuous(expand = c(0.02,0))
  #geom_hilight(node=1253, fill="steelblue", alpha=0.5) +
  #geom_hilight(node=1215, fill="#8852b8", alpha=0.5)
)

## tree
head(p$data$label)
p$data$clade <- sapply(p$data$label, function(x){
  tmp <- vic.meta.all %>% filter(Taxa == x) %>% .$clade
  if(length(tmp)==0){return(NA)}else{return(tmp)}
})

sort(table(p$data$clade))
num_of_colors <- length(unique(p$data$clade))-1
#mycolors <- colorRampPalette(c("#CC5252","#5b52cc","#cc529b","#52cc56"))(num_of_colors)
mycolors <- colorRampPalette(c("#d7191c","#abd9e9","#fdae61","#2c7bb6"))(num_of_colors)
#mycolors[2] <- "#BABABA"

vic.tree <- p + geom_tippoint(aes(fill = factor(clade)), size = 1.5, alpha=0.8, shape = 21, stroke = 0.2,color="black") + 
  #aes(color=factor(clade))+
  scale_fill_manual(values=mycolors)+
  #scale_alpha_manual(values=c(1,1,1,1,0.7))+
  coord_cartesian(clip="off")+
  theme(text = element_text(size=8))+
  theme(legend.position=c(0.35,0.61),
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"))+
  guides(fill=guide_legend(title="Clade"))+
  theme(legend.position="none")+
  annotate("text",label="B/Victoria",x=2010,y=1000,size=3)
vic.tree

#new map
vic.meta <- vic.meta.all %>%
  filter(date > "2020-04")

vic.meta.input <- vic.meta[c('country','clade')]

vic.meta.input$country <- gsub(" \\(SAR\\)|, .*|n Federation","",vic.meta.input$country)
vic.meta.input$country <- gsub("South Province of the Western Cape|South Gauteng|South Province of KwaZulu-Natal|South Province of North-West","South Africa",vic.meta.input$country)

#vic.meta.input <- as.data.frame(table(vic.meta.input))

#vic.meta.input <- vic.meta.input%>% 
#  pivot_wider(names_from = clade, values_from = Freq)

#vic.meta.input$Cases <- apply(vic.meta.input[,-1],1,sum)

#country clades... lat log
vic.meta.input.gps <- merge(vic.meta.input,country.gps,by="country",all.x = TRUE)
#check some countries can't match
unique(vic.meta.input.gps[which(is.na(vic.meta.input.gps$code)),1])

#world map
worldMap <- fortify(map_data("world"), region = "subregion")%>%
  filter(region != "Antarctica" & long > -130.0141)
#table(worldMap$subregion)
#table(worldMap$region)
#colnames(worldMap)

#add random position

random_num <- length(vic.meta.input.gps[which(vic.meta.input.gps$country == 'China'),c("latitude")])
vic.meta.input.gps[which(vic.meta.input.gps$country == 'China'),c("latitude")] <- vic.meta.input.gps[which(vic.meta.input.gps$country == 'China'),c("latitude")]+as.vector(runif(random_num,-2,2))
vic.meta.input.gps[which(vic.meta.input.gps$country == 'China'),c("longitude")] <- vic.meta.input.gps[which(vic.meta.input.gps$country == 'China'),c("longitude")]+as.vector(runif(random_num,-5,5))

vic.meta.input.gps$clade <- factor(vic.meta.input.gps$clade, levels = c("V1A.3/150K","V1A.3/133R", "V1A","V1A/165N"))
vic.meta.input.gps<- vic.meta.input.gps[order(vic.meta.input.gps$clade),]

vic.map.new <- ggplot()+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),
               fill="#E6E6E6",color="black",size=0.1)+
  guides(fill=FALSE)+
  theme_bw()+
  theme(text = element_text(size=8))+
  new_scale("fill") + #otherwise, there is a bug: Error in new_scale("fill") : could not find function "new_scale"
  geom_jitter(aes(x=longitude, y=latitude, fill=clade),data=vic.meta.input.gps, alpha = 0.8,size = 1.5,width=4,height=2,shape=21,stroke=0.2)+ #add pie chart
  scale_fill_manual(values=mycolors[c(1,3,2,4)],breaks=c('V1A','V1A.3/150K','V1A.3/133R',"V1A/165N"))+
  #theme(panel.grid =element_blank()) +   
  theme(axis.text = element_blank()) +   
  theme(axis.ticks = element_blank()) +   
  theme(panel.border = element_blank())+   
  theme(plot.margin = margin(t = -0.4,  
                             r = -0.4,  
                             b = -0.4,
                             l = -0.4,  
                             unit = "cm"))+
  labs(x = "", y = "", title = "")

vic.map.new

#old map
vic.meta <- vic.meta.all %>%
  filter(date < "2020-04" & date > "2020-01")

vic.meta.input <- vic.meta[c('country','clade')]

pie_clade_group <- names(sort(table(vic.meta.input$clade)))

vic.meta.input$country <- gsub(" \\(SAR\\)|, .*|n Federation","",vic.meta.input$country)
vic.meta.input$country <- gsub("South Province of the Western Cape|South Gauteng|South Province of KwaZulu-Natal|South Province of North-West","South Africa",vic.meta.input$country)
vic.meta.input$country <- gsub("Korea","South Korea",vic.meta.input$country)

#vic.meta.input <- as.data.frame(table(vic.meta.input))

#vic.meta.input <- vic.meta.input%>% 
#  pivot_wider(names_from = clade, values_from = Freq)

#vic.meta.input$Cases <- apply(vic.meta.input[,-1],1,sum)

#country clades... lat log
vic.meta.input.gps <- merge(vic.meta.input,country.gps,by="country",all.x = TRUE)
#check some countries can't match
unique(vic.meta.input.gps[which(is.na(vic.meta.input.gps$code)),1])

#world map
worldMap <- fortify(map_data("world"), region = "subregion")%>%
  filter(region != "Antarctica" & long > -130.0141)
#table(worldMap$subregion)
#table(worldMap$region)
#colnames(worldMap)

#add random position

random_num <- length(vic.meta.input.gps[which(vic.meta.input.gps$country == 'China'),c("latitude")])
vic.meta.input.gps[which(vic.meta.input.gps$country == 'China'),c("latitude")] <- vic.meta.input.gps[which(vic.meta.input.gps$country == 'China'),c("latitude")]+as.vector(runif(random_num,-2,2))
vic.meta.input.gps[which(vic.meta.input.gps$country == 'China'),c("longitude")] <- vic.meta.input.gps[which(vic.meta.input.gps$country == 'China'),c("longitude")]+as.vector(runif(random_num,-5,5))

vic.meta.input.gps$clade <- factor(vic.meta.input.gps$clade, levels = c("V1A.3/133R", "V1A","V1A.3/150K"))
vic.meta.input.gps<- vic.meta.input.gps[order(vic.meta.input.gps$clade),]

vic.map.old <- ggplot()+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),
               fill="#E6E6E6",color="black",size=0.1)+
  guides(fill=FALSE)+
  theme_bw()+
  theme(text = element_text(size=8))+
  new_scale("fill") + #otherwise, there is a bug: Error in new_scale("fill") : could not find function "new_scale"
  geom_jitter(aes(x=longitude, y=latitude, fill=clade),data=vic.meta.input.gps, alpha = 0.8,size = 1.5,width=4,height=2,shape=21,stroke=0.2)+ #add pie chart
  scale_fill_manual(values=mycolors[c(1,3,2)],breaks=c('V1A','V1A.3/150K','V1A.3/133R'))+
  #theme(panel.grid =element_blank()) +   
  theme(axis.text = element_blank()) +   
  theme(axis.ticks = element_blank()) +   
  theme(panel.border = element_blank())+   
  theme(plot.margin = margin(t = -0.4,  
                             r = -0.4,  
                             b = -0.4,
                             l = -0.4,  
                             unit = "cm"))+
  labs(x = "", y = "", title = "")+
  annotate("text",label="B/Victoria",x=-110,y=-25,size=3)

vic.map.old

vic.composite <- ggarrange(vic.tree,vic.map.old,vic.map.new,ncol=3,nrow=1,widths=c(2,3,3))
vic.composite
############# yam ################

tree <- read.nexus("../results/iqtree/yam.afa.timetree.nex")

yam.meta.all <- read_tsv("../data/yam_HA_meta.tsv")

(p <- ggtree(tree, mrsd = "2020-03-24", alpha = 0.1) + 
    theme_tree2()+
    #geom_text2(aes(subset = !isTip, label=label),size = 1)+  #support value
    #geom_text(aes(label=node), hjust=-.3)+
    scale_x_continuous(limits = c(2007.5,2021.6),breaks = c(2020,2012,2014,2016,2018,2020))+
    #scale_x_continuous(limits = c(2011,2021.5),breaks = c(2012,2014,2016,2018,2020))+
    scale_y_continuous(expand = c(0.02,0))
  #geom_hilight(node=1253, fill="steelblue", alpha=0.5) +
  #geom_hilight(node=1215, fill="#8852b8", alpha=0.5)
)

## tree
head(p$data$label)
p$data$clade <- sapply(p$data$label, function(x){
  tmp <- yam.meta.all %>% filter(Taxa == x) %>% .$clade
  if(length(tmp)==0){return(NA)}else{return(tmp)}
})

sort(table(p$data$clade))
num_of_colors <- length(unique(p$data$clade))-1
#mycolors <- colorRampPalette(c("#CC5252","#5b52cc","#cc529b","#52cc56"))(num_of_colors)
mycolors <- colorRampPalette(c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))(num_of_colors)
#mycolors[2] <- "#BABABA"

yam.tree <- p + geom_tippoint(aes(fill = factor(clade)), size = 1.5, alpha=0.8, shape = 21, stroke = 0.2,color="black") + 
  scale_fill_manual(values="#d7191c")+
  #scale_alpha_manual(values=c(1,1,1,1,0.7))+
  coord_cartesian(clip="off")+
  theme(text = element_text(size=8))+
  theme(legend.position=c(0.33,0.61),
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"))+
  guides(fill=guide_legend(title="Clade"))+
  theme(legend.position="none")+
  annotate("text",label="B/Yamagata",x=2012,y=250,size=3)

yam.tree

#old map
yam.meta <- yam.meta.all %>%
  filter(date < "2020-04" & date > "2020-01")

yam.meta.input <- yam.meta[c('country','clade')]

pie_clade_group <- names(sort(table(yam.meta.input$clade)))

yam.meta.input$country <- gsub(" \\(SAR\\)|, .*|n Federation","",yam.meta.input$country)
yam.meta.input$country <- gsub("South Province of the Western Cape","South Africa",yam.meta.input$country)

#yam.meta.input <- as.data.frame(table(yam.meta.input))

#yam.meta.input <- yam.meta.input%>% 
#  pivot_wider(names_from = clade, values_from = Freq)

#yam.meta.input$Cases <- apply(yam.meta.input[,-1],1,sum)

#country clades... lat log
yam.meta.input.gps <- merge(yam.meta.input,country.gps,by="country",all.x = TRUE)
#check some countries can't match
unique(yam.meta.input.gps[which(is.na(yam.meta.input.gps$code)),1])

#world map
worldMap <- fortify(map_data("world"), region = "subregion")%>%
  filter(region != "Antarctica" & long > -130.0141)
#table(worldMap$subregion)
#table(worldMap$region)
#colnames(worldMap)

#Error in rowSums(data[, cols]) :'x' must be an array of at least two dimensions
#yam.meta.input.gps$zero <- 0
yam.map.new <- ggplot()+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),
               fill="#E6E6E6",color="black",size=0.1)+
  guides(fill=FALSE)+
  theme_bw()+
  theme(text = element_text(size=8))+
  new_scale("fill") + #otherwise, there is a bug: Error in new_scale("fill") : could not find function "new_scale"
  #geom_jitter(aes(x=longitude, y=latitude, fill=clade),data=yam.meta.input.gps, alpha = 0.8,size = 1.5,width=4,height=2,shape=21,stroke=0.2)+ #add pie chart
  #scale_fill_manual(values=c("#CC5252","#FFFFFF"),breaks=c("172Q","zero"))+
  #theme(panel.grid =element_blank()) +   
  theme(axis.text = element_blank()) +   
  theme(axis.ticks = element_blank()) +   
  theme(panel.border = element_blank())+   
  theme(plot.margin = margin(t = -0.4,  
                             r = -0.4,  
                             b = -0.4,
                             l = -0.4,  
                             unit = "cm"))+
  labs(x = "", y = "", title = "")
yam.map.new

yam.map.old <- ggplot()+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),
               fill="#E6E6E6",color="black",size=0.1)+
  guides(fill=FALSE)+
  theme_bw()+
  theme(text = element_text(size=8))+
  new_scale("fill") + #otherwise, there is a bug: Error in new_scale("fill") : could not find function "new_scale"
  geom_jitter(aes(x=longitude, y=latitude, fill=clade),data=yam.meta.input.gps, alpha = 0.8,size = 1.5,width=4,height=2,shape=21,stroke=0.2)+ #add pie chart
  scale_fill_manual(values=c("#d7191c","#FFFFFF"),breaks=c("172Q","zero"))+
  #theme(panel.grid =element_blank()) +   
  theme(axis.text = element_blank()) +   
  theme(axis.ticks = element_blank()) +   
  theme(panel.border = element_blank())+   
  theme(plot.margin = margin(t = -0.4,  
                             r = -0.4,  
                             b = -0.4,
                             l = -0.4,  
                             unit = "cm"))+
  labs(x = "", y = "", title = "")+
  annotate("text",label="B/Yamagata",x=-110,y=-25,size=3)
yam.map.old


yam.composite <- ggarrange(yam.tree,yam.map.old,yam.map.new,ncol=3,nrow=1,widths=c(2,3,3))
yam.composite
############# h3n2 ################

tree <- read.nexus("../results/iqtree/h3n2.afa.timetree.nex")

h3n2.meta.all <- read_tsv("../data/h3n2_HA_meta.tsv")

clade <- tree_subset(tree,node=1269, levels_back=0)

(p <- ggtree(clade, mrsd = "2021-08-02", alpha = 0.1) + 
    theme_tree2()+
    #geom_text2(aes(subset = !isTip, label=label),size = 1)+  #support value
    #geom_text(aes(label=node), hjust=-.3)+
    scale_x_continuous(limits = c(2007.5,2021.6),breaks = c(2010,2012,2014,2016,2018,2020))+
    scale_y_continuous(expand = c(0.02,0))
  #geom_hilight(node=1253, fill="steelblue", alpha=0.5) +
  #geom_hilight(node=1215, fill="#8852b8", alpha=0.5)
)

## tree
head(p$data$label)
p$data$clade <- sapply(p$data$label, function(x){
  tmp <- h3n2.meta.all %>% filter(Taxa == x) %>% .$clade
  if(length(tmp)==0){return(NA)}else{return(tmp)}
})

sort(table(p$data$clade))
num_of_colors <- length(unique(p$data$clade))-1
#mycolors <- colorRampPalette(c("#CC5252","#5b52cc","#cc529b","#52cc56"))(num_of_colors)
mycolors <- colorRampPalette(c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))(num_of_colors)
#mycolors[2] <- "#BABABA"

h3n2.tree <- p + geom_tippoint(aes(fill = factor(clade)), size = 1.5, alpha=0.8, shape = 21, stroke = 0.2,color="black") + 
  scale_fill_manual(values=colors.16)+
  #scale_alpha_manual(values=c(1,1,1,1,0.7))+
  coord_cartesian(clip="off")+
  theme(text = element_text(size=8))+
  theme(legend.position=c(0.4,0.58),
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"))+
  guides(fill=guide_legend(title="Clade",ncol=2))+
  theme(legend.position="none")+
  annotate("text",label="A(H3N2)",x=2010,y=1000,size=3)
h3n2.tree

#new map
h3n2.meta <- h3n2.meta.all %>%
  filter(date > "2020-04")

h3n2.meta.input <- h3n2.meta[c('country','clade')]

pie_clade_group <- names(sort(table(h3n2.meta.input$clade)))

h3n2.meta.input$country <- gsub(" \\(SAR\\)|, .*|n Federation","",h3n2.meta.input$country)
h3n2.meta.input$country <- gsub("South Province of the Western Cape","South Africa",h3n2.meta.input$country)

#h3n2.meta.input <- as.data.frame(table(h3n2.meta.input))

#h3n2.meta.input <- h3n2.meta.input%>% 
#  pivot_wider(names_from = clade, values_from = Freq)

#h3n2.meta.input$Cases <- apply(h3n2.meta.input[,-1],1,sum)

#country clades... lat log
h3n2.meta.input.gps <- merge(h3n2.meta.input,country.gps,by="country",all.x = TRUE)
#check some countries can't match
unique(h3n2.meta.input.gps[which(is.na(h3n2.meta.input.gps$code)),1])

#world map
worldMap <- fortify(map_data("world"), region = "subregion")%>%
  filter(region != "Antarctica" & long > -130.0141)
#table(worldMap$subregion)
#table(worldMap$region)
#colnames(worldMap)

h3n2.meta.input.gps$clade <- factor(h3n2.meta.input.gps$clade, levels = c("A1b/94N","A1b/186D", "A1b/137F","A1b/131K",'unassigned','3c3.A','A1b','A2/re'))
h3n2.meta.input.gps<- h3n2.meta.input.gps[order(h3n2.meta.input.gps$clade),]

h3n2.map.new <- ggplot()+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),
               fill="#E6E6E6",color="black",size=0.1)+
  guides(fill=FALSE)+
  theme_bw()+
  theme(text = element_text(size=8))+
  new_scale("fill") + #otherwise, there is a bug: Error in new_scale("fill") : could not find function "new_scale"
  geom_jitter(aes(x=longitude, y=latitude, fill=clade),data=h3n2.meta.input.gps, alpha = 0.8,size = 1.5,width=4,height=2,shape=21,stroke=0.2)+ #add pie chart
  scale_fill_manual(values=c(colors.16[c(2,4,13,5,8,9,11)],"white"),breaks=c('3c3.A','A1b','A2/re','A1b/131K','A1b/137F','A1b/186D','A1b/94N','unassigned'))+
  #theme(panel.grid =element_blank()) +   
  theme(axis.text = element_blank()) +   
  theme(axis.ticks = element_blank()) +   
  theme(panel.border = element_blank())+   
  theme(plot.margin = margin(t = -0.4,  
                             r = -0.4,  
                             b = -0.4,
                             l = -0.4,  
                             unit = "cm"))+
  labs(x = "", y = "", title = "")
h3n2.map.new

#old map
h3n2.meta <- h3n2.meta.all %>%
  filter(date < "2020-04" & date > "2020-01")

h3n2.meta$Taxa <- h3n2.meta$strain

#h3n2.meta[which(h3n2.meta$Isolate_Id %in% c("EPI_ISL_430450","EPI_ISL_430458")),5] <- "Russia"

h3n2.meta.input <- merge(h3n2.meta,data,by="Taxa",all=FALSE)

h3n2.meta.input <- h3n2.meta.input[c('country','clade')]

pie_clade_group <- names(sort(table(h3n2.meta.input$clade)))

h3n2.meta.input$country <- gsub(" \\(SAR\\)|, .*|n Federation","",h3n2.meta.input$country)
h3n2.meta.input$country <- gsub("South Province of the Western Cape","South Africa",h3n2.meta.input$country)
h3n2.meta.input$country <- gsub("Korea","South Korea",h3n2.meta.input$country)
h3n2.meta.input$country <- gsub("South KwaZulu Natal","South Africa",h3n2.meta.input$country)

#h3n2.meta.input <- as.data.frame(table(h3n2.meta.input))

#h3n2.meta.input <- h3n2.meta.input%>% 
#  pivot_wider(names_from = clade, values_from = Freq)

#h3n2.meta.input$Cases <- apply(h3n2.meta.input[,-1],1,sum)

#country clades... lat log
h3n2.meta.input.gps <- merge(h3n2.meta.input,country.gps,by="country",all.x = TRUE)
#check some countries can't match
unique(h3n2.meta.input.gps[which(is.na(h3n2.meta.input.gps$code)),1])

#world map
worldMap <- fortify(map_data("world"), region = "subregion")%>%
  filter(region != "Antarctica" & long > -130.0141)
#table(worldMap$subregion)
#table(worldMap$region)
#colnames(worldMap)

h3n2.map.old <- ggplot()+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),
               fill="#E6E6E6",color="black",size=0.1)+
  guides(fill=FALSE)+
  theme_bw()+
  theme(text = element_text(size=8))+
  new_scale("fill") + #otherwise, there is a bug: Error in new_scale("fill") : could not find function "new_scale"
  geom_jitter(aes(x=longitude, y=latitude, fill=clade),data=h3n2.meta.input.gps, alpha = 0.8,size = 1.5,width=4,height=2,shape=21,stroke=0.2)+ #add pie chart
  scale_fill_manual(values=colors.16[c(2,5,6,8,9,10,11,14)],breaks=c("3c3.A","A1b/131K","A1b/135K","A1b/137F","A1b/186D","A1b/197R","A1b/94N","A3"))+
  #theme(panel.grid =element_blank()) +   
  theme(axis.text = element_blank()) +   
  theme(axis.ticks = element_blank()) +   
  theme(panel.border = element_blank())+   
  theme(plot.margin = margin(t = -0.4,  
                             r = -0.4,  
                             b = -0.4,
                             l = -0.4,  
                             unit = "cm"))+
  labs(x = "", y = "", title = "")+
  annotate("text",label="A(H3N2)",x=-110,y=-25,size=3)
h3n2.map.old

h3n2.composite <- ggarrange(h3n2.tree,h3n2.map.old,h3n2.map.new,ncol=3,nrow=1,widths=c(2,3,3))
h3n2.composite

############# h1n1pdm ################

tree <- read.nexus("../results/iqtree/h1n1pdm.afa.timetree.nex")

h1n1pdm.meta.all <- read_tsv("../data/h1n1pdm_HA_meta.tsv")

(p <- ggtree(tree, mrsd = "2021-07-09", alpha = 0.1) + 
    theme_tree2()+
    #geom_text2(aes(subset = !isTip, label=label),size = 1)+  #support value
    #geom_text(aes(label=node), hjust=-.3)+
    #scale_x_continuous(limits = c(2011,2021.5),breaks = c(2012,2014,2016,2018,2020))+
    scale_x_continuous(limits = c(2007.5,2021.6),breaks = c(2010,2012,2014,2016,2018,2020))+
    scale_y_continuous(expand = c(0.02,0))
  #geom_hilight(node=1253, fill="steelblue", alpha=0.5) +
  #geom_hilight(node=1215, fill="#8852b8", alpha=0.5)
)

## tree
head(p$data$label)
p$data$clade <- sapply(p$data$label, function(x){
  tmp <- h1n1pdm.meta.all %>% filter(Taxa == x) %>% .$clade
  if(length(tmp)==0){return(NA)}else{return(tmp)}
})

sort(table(p$data$clade))
num_of_colors <- length(unique(p$data$clade))-1
#mycolors <- colorRampPalette(c("#CC5252","#5b52cc","#cc529b","#52cc56"))(num_of_colors)
mycolors <- colorRampPalette(c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))(num_of_colors)
#mycolors[2] <- "#BABABA"

h1n1pdm.tree <- p + geom_tippoint(aes(fill = factor(clade)), size = 1.5, alpha=0.8, shape = 21, stroke = 0.2,color="black") + 
  scale_fill_manual(values=colors.16)+
  #scale_alpha_manual(values=c(1,1,1,1,0.7))+
  coord_cartesian(clip="off")+
  theme(text = element_text(size=8))+
  theme(legend.position=c(0.45,0.5),
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"))+
  guides(fill=guide_legend(title="Clade",ncol=2))+
  theme(legend.position="none")+
  annotate("text",label="A(H1N1)pdm",x=2012,y=800,size=3)
h1n1pdm.tree

#new map
h1n1pdm.meta <- h1n1pdm.meta.all %>%
  filter(date > "2020-04")

h1n1pdm.meta.input <- h1n1pdm.meta[c('country','clade')]

pie_clade_group <- names(sort(table(h1n1pdm.meta.input$clade)))

h1n1pdm.meta.input$country <- gsub(" \\(SAR\\)|, .*|n Federation","",h1n1pdm.meta.input$country)
h1n1pdm.meta.input$country <- gsub("South Province of the Western Cape","South Africa",h1n1pdm.meta.input$country)

#h1n1pdm.meta.input <- as.data.frame(table(h1n1pdm.meta.input))

#h1n1pdm.meta.input <- h1n1pdm.meta.input%>% 
#  pivot_wider(names_from = clade, values_from = Freq)

#h1n1pdm.meta.input$Cases <- apply(h1n1pdm.meta.input[,-1],1,sum)

#country clades... lat log
h1n1pdm.meta.input.gps <- merge(h1n1pdm.meta.input,country.gps,by="country",all.x = TRUE)
#check some countries can't match
unique(h1n1pdm.meta.input.gps[which(is.na(h1n1pdm.meta.input.gps$code)),1])

#world map
worldMap <- fortify(map_data("world"), region = "subregion")%>%
  filter(region != "Antarctica" & long > -130.0141)
#table(worldMap$subregion)
#table(worldMap$region)
#colnames(worldMap)

h1n1pdm.meta.input.gps$clade <- factor(h1n1pdm.meta.input.gps$clade, levels = c('6b1.A/187A','6b1.A/183P-5b','6b1.A/183P-5a','6b1.A/156K','6b1.A/183P-7','6b1'))
h1n1pdm.meta.input.gps<- h1n1pdm.meta.input.gps[order(h1n1pdm.meta.input.gps$clade),]

h1n1pdm.map.new <- ggplot()+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),
               fill="#E6E6E6",color="black",size=0.1)+
  guides(fill=FALSE)+
  theme_bw()+
  theme(text = element_text(size=8))+
  new_scale("fill") + #otherwise, there is a bug: Error in new_scale("fill") : could not find function "new_scale"
  geom_jitter(aes(x=longitude, y=latitude, fill=clade),data=h1n1pdm.meta.input.gps, alpha = 0.8,size = 1.5,width=4,height=2,shape=21,stroke=0.2)+ #add pie chart
  scale_fill_manual(values=colors.16[c(10,9,4,13,12,2)],breaks=c('6b1.A/183P-5b','6b1.A/183P-5a','6b1.A/156K','6b1.A/187A','6b1.A/183P-7','6b1'))+
  #theme(panel.grid =element_blank()) +   
  theme(axis.text = element_blank()) +   
  theme(axis.ticks = element_blank()) +   
  theme(panel.border = element_blank())+   
  theme(plot.margin = margin(t = -0.4,  
                             r = -0.4,  
                             b = -0.4,
                             l = -0.4,  
                             unit = "cm"))+
  labs(x = "", y = "", title = "")
h1n1pdm.map.new

#old map
h1n1pdm.meta <- h1n1pdm.meta.all %>%
  filter(date < "2020-04" & date > "2020-01")

h1n1pdm.meta$Taxa <- h1n1pdm.meta$strain

#h1n1pdm.meta[which(h1n1pdm.meta$Isolate_Id %in% c("EPI_ISL_430450","EPI_ISL_430458")),5] <- "Russia"

h1n1pdm.meta.input <- merge(h1n1pdm.meta,data,by="Taxa",all=FALSE)

h1n1pdm.meta.input <- h1n1pdm.meta.input[c('country','clade')]

pie_clade_group <- names(sort(table(h1n1pdm.meta.input$clade)))

h1n1pdm.meta.input$country <- gsub(" \\(SAR\\)|, .*|n Federation","",h1n1pdm.meta.input$country)
h1n1pdm.meta.input$country <- gsub("South Province of the Western Cape","South Africa",h1n1pdm.meta.input$country)
h1n1pdm.meta.input$country <- gsub("Korea","South Korea",h1n1pdm.meta.input$country)
#h1n1pdm.meta.input <- as.data.frame(table(h1n1pdm.meta.input))

#h1n1pdm.meta.input <- h1n1pdm.meta.input%>% 
#  pivot_wider(names_from = clade, values_from = Freq)

#h1n1pdm.meta.input$Cases <- apply(h1n1pdm.meta.input[,-1],1,sum)

#country clades... lat log
h1n1pdm.meta.input.gps <- merge(h1n1pdm.meta.input,country.gps,by="country",all.x = TRUE)
#check some countries can't match
unique(h1n1pdm.meta.input.gps[which(is.na(h1n1pdm.meta.input.gps$code)),1])

#world map
worldMap <- fortify(map_data("world"), region = "subregion")%>%
  filter(region != "Antarctica" & long > -130.0141)
#table(worldMap$subregion)
#table(worldMap$region)
#colnames(worldMap)

#h1n1pdm.meta.input.gps$clade <- factor(h1n1pdm.meta.input.gps$clade, levels = c('6b1.A/187A','6b1.A/183P-5b','6b1.A/183P-5a','6b1.A/156K','6b1'))
#h1n1pdm.meta.input.gps<- h1n1pdm.meta.input.gps[order(h1n1pdm.meta.input.gps$clade),]

h1n1pdm.map.old <- ggplot()+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),
               fill="#E6E6E6",color="black",size=0.1)+
  guides(fill=FALSE)+
  theme_bw()+
  theme(text = element_text(size=8))+
  new_scale("fill") + #otherwise, there is a bug: Error in new_scale("fill") : could not find function "new_scale"
  geom_jitter(aes(x=longitude, y=latitude, fill=clade),data=h1n1pdm.meta.input.gps, alpha = 0.8,size = 1.5,width=4,height=2,shape=21,stroke=0.2)+ #add pie chart
  scale_fill_manual(values=colors.16[c(3,4,5,9,10,12,13)],breaks=c("6b1.A","6b1.A/156K","6b1.A/183P-1","6b1.A/183P-5a","6b1.A/183P-5b","6b1.A/183P-7","6b1.A/187A"))+
  #theme(panel.grid =element_blank()) +   
  theme(axis.text = element_blank()) +   
  theme(axis.ticks = element_blank()) +   
  theme(panel.border = element_blank())+   
  theme(plot.margin = margin(t = -0.4,  
                             r = -0.4,  
                             b = -0.4,
                             l = -0.4,  
                             unit = "cm"))+
  labs(x = "", y = "", title = "")+
  annotate("text",label="A(H1N1)pdm",x=-110,y=-25,size=3)
h1n1pdm.map.old

h1n1.composite <- ggarrange(h1n1pdm.tree,h1n1pdm.map.old,h1n1pdm.map.new,ncol=3,nrow=1,widths=c(2,3,3))
h1n1.composite

final.tree <- ggarrange(h3n2.tree,h1n1pdm.tree,vic.tree,yam.tree,ncol=2,nrow=2)
ggsave("../results/composite_tree_2_2.pdf",final.tree,width=19,height=12,units="cm")

final.map <- ggarrange(h3n2.map.old,h3n2.map.new,h1n1pdm.map.old,h1n1pdm.map.new,vic.map.old,vic.map.new,yam.map.old,yam.map.new,ncol=2,nrow=4)
ggsave("../results/composite_map.pdf",final.map,width=18,height=16,units="cm")
