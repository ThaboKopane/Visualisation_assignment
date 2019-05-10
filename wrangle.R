library('ggplot2')
library('plyr')
library('reshape2')
library('readxl')
library('vcd')
library('ggpubr')
library('tibble')

#packages = ('ggplot2', 'plyr', 'reshape2', 'readxl', 'vcd', 'ggpubr', 'tibble')

sexData = read_excel('SexData1.xlsx')
contigency = read_excel('magic.xlsx')
julian = contigency[2:5]
rownames(julian) = contigency$factor

##frequency of shiiiiiiiiiiit
judas = count(sexData, c('Gender', 'Verification', 'Sexual_polarity', 'Sexual_orientation', 'Looking_for',
                 'Risk'))
judas = na.omit(judas)

library(FactoMineR)
library(factoextra)
######judas
res.ca <- CA(julian, graph = FALSE)
fviz_ca_biplot(res.ca, repel = TRUE, col)












ggballoonplot(julian)+
  scale_fill_viridis_c(option = "C")

ggballoonplot(as.data.frame(judas), x = "Gender", y = "Sexual_orientation", size = "freq",
              fill = "freq", facet.by = "Risk",
              ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "C")


judasTable = table(judas$Gender, judas$Verification, judas$Risk,judas$Sexual_orientation,judas$freq)
mosaic(julian, shade = TRUE, legend = TRUE)

