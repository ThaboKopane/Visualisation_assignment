
packages <- c('ggplot2', 'reshape2', 'plyr', 'plotly', 'ggthemes', 'extrafont',
              'scales', 'png', 'viridis', 'ggraph', 'igraph')

##function to load packaes
check.packages <- function(pkg) {
  new.pkg <- pkg[! (pkg %in% installed.packages()[, 'Package'])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}

check.packages('readxl')
sexData <- read_excel('SexData1.xlsx')


hist(sexData$Last_login_days, breaks=20)
hist(sexData$member_length, breaks=20)
hist(sexData$Age, breaks=20)





###add stuff
check.packages('plyr')
nuAge = as.data.frame(round(sexData$Age))
nuSex = cbind.data.frame(nuAge, sexData)
names(nuSex)[names(nuSex) == 'round(sexData$Age)'] <- 'nuAge'
judas = count(nuSex, c('Gender',
                         'Risk', 'member_length'))
judas = na.omit(judas)
#nuSex = na.omit(nu)
check.packages('ggplot2')
ggplot(nuSex, aes(x=member_length, y=nuAge, group=Risk)) +
  geom_point(aes(shape=Gender, color=Risk))+
  theme(panel.background=element_rect(fill="black"), plot.background=element_rect(fill="black"))



check.packages('plotly')
ggplot(nuSex, aes(x=Age, y=Last_login_days, group=Risk)) +
  geom_point(aes(shape=Verification, color=Risk)) +
  theme(panel.background=element_rect(fill="black"), plot.background=element_rect(fill="black"))



"An example of my humanoid plot"
###Risk and sexual orientation
risk_gen = count(nuSex, c('Risk', 'Gender'))
ori_gen = count(nuSex, c('Sexual_orientation', 'Gender'))
risk_gen = na.omit(risk_gen)
ori_gen = na.omit(ori_gen)

##frequency to proportions
freqRisk = as.data.frame(risk_gen$freq/sum(risk_gen$freq))
freqRisk = cumsum(freqRisk)
risk_final = cbind(risk_gen, freqRisk)
names(risk_final)[names(risk_final) == 'risk_gen$freq/sum(risk_gen$freq)'] <- 'proportion'
freqOri  = as.data.frame(ori_gen$freq/sum(ori_gen$freq))
freqOri = cumsum(freqOri)
ori_final = cbind(ori_gen, freqOri)
names(ori_final)[names(ori_final) == 'ori_gen$freq/sum(ori_gen$freq)'] <- 'proportion'



###multiple charts
julian = count(nuSex, c('Risk', 'Last_login_days', 'member_length',
                        'Sexual_polarity', 'nuAge', 'Gender'))
julian = na.omit(julian)

ggplot(data = julian, aes(x=Last_login_days, y= member_length)) +
  geom_point(aes(text=paste('Polarity:', Sexual_polarity)), colour='#ffff99', size=3) +
  geom_smooth(aes(colour=Risk, fill = Risk)) + facet_wrap(~ Sexual_polarity) +
  theme(panel.background=element_rect(fill="black"),  plot.background = element_rect('black'),
        axis.text = element_text(size=15, colour = 'white'),
        axis.title = element_text(size = 15, face = 'bold', colour = 'white'),
        legend.text = element_text(size=15, face='bold'))


treedata = count(nuSex, c('Looking_for', 'nuAge', 'Last_login_days', 'Risk', 'Sexual_polarity', 'member_length','Gender'))
treedata = na.omit(treedata)

##density plot for a stacked graph
# plot 3: Stacked density plot:

verified = count(nuSex, c('nuAge', 'Gender', 'Sexual_orientation'))
verified = na.omit(verified)




##distribution plot
ggplot(verified, aes(Sexual_orientation, nuAge)) +
  geom_violin(aes(fill=Gender)) + theme(plot.background = element_rect(fill = 'black'),
                                        panel.background=element_rect(fill="black"),
                                        axis.text = element_text(size=15, colour = 'white'),
                                        axis.title = element_text(size = 15, face = 'bold', colour = 'white'),
                                        legend.text = element_text(size=15, face='bold'))



#3rdige
check.packages('ggridges')
ggplot(julian, aes(x=Last_login_days, y = Risk, fill=Gender)) +
  geom_density_ridges() + theme_ridges()  +
  scale_fill_manual(values=c("#49ADEA", "#F19FCA")) +
  theme(plot.background=element_rect(fill="black"),
        axis.text = element_text(size=15, colour = 'white'),
        axis.title = element_text(size = 15, face = 'bold', colour = 'white'),
        legend.text = element_text(size=15, face='bold', colour = 'white'))
  



###bpxxxxxx plot



# grouped boxplot
# scale_fill_manual(values=c("#49ADEA", "#F19FCA")) +
ggplot(treedata, aes(x=Looking_for, y=nuAge, fill=Gender)) + 
  geom_boxplot() + scale_fill_manual(breaks = c("2", "1"), values=c("#49ADEA", "#F19FCA")) +
  theme(plot.background=element_rect(fill="black"), panel.background = element_rect(fill = 'black'),
        axis.text = element_text(size=15, colour = 'white'),
        axis.title = element_text(size = 15, face = 'bold', colour = 'white'),
        legend.text = element_text(size=15, face='bold'))


##newer box
names=c(rep("A", 80) , rep("B", 50) , rep("C", 70))
value=c( sample(2:5, 80 , replace=T) , sample(4:10, 50 , replace=T), 
         sample(1:7, 70 , replace=T) )
data=data.frame(names,value)

#Graph
qplot( x=Risk , y=freq , data=verified , geom=c("boxplot","jitter") , fill=Risk)





###newVerified
nuVer = count(nuSex, c('Verification', 'Gender', 'Risk'))
nuVer = na.omit(nuVer)
##density myman
# plot 3: Stacked density plot:
ggplot(nuVer,aes(x=freq, group=Verification, fill=Verification)) + 
  geom_density(adjust=1.5, position="fill")



###stacked
# plot 3: Stacked density plot:
ggplot(treedata,aes(x=nuAge, group=Looking_for, fill=Sexual_polarity)) +
  geom_density(adjust=1.5, position="fill") + 
  theme(plot.background=element_rect(fill="black"), panel.background = element_rect(fill = 'black'),
        axis.text = element_text(size=15, colour = 'white'),
        axis.title = element_text(size = 15, face = 'bold', colour = 'white'),
        legend.text = element_text(size=15, face='bold'))

