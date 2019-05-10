library('ggplot2')
library('reshape2')
library('plyr')
library('readxl')
library('gridExtra')
library('plotly')
sexData <- read_excel('SexData1.xlsx')


l1 <- ggplot(sexData, aes(x=Number_of_Comments_in_public_forum, y=time_spent_chatting_days, group=Risk)) +
  geom_point(aes(shape=Verification, color=Risk))

ggplotly(l1)


###add stuff
nuAge = as.data.frame(round(sexData$Age))
nuSex = cbind.data.frame(nuAge, sexData)
names(nuSex)[names(nuSex) == 'round(sexData$Age)'] <- 'nuAge'
judas = count(nuSex, c('Gender',
                         'Risk', 'nuAge'))
judas = na.omit(judas)

ggplot(judas, aes(x=nuAge, y=freq, group=Risk)) +
  geom_point(aes(shape=Gender, color=Risk))



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

#risk_ori_fem = count(nuSex, c('Se'))

g1<-ggplot(risk_final, aes(Gender, proportion, color=factor(Risk)))+geom_point()
g2<-ggplot(ori_final, aes(Gender, proportion, color=factor(Sexual_orientation)))+geom_point()

##plot both
p=subplot( ggplotly(g1), ggplotly(g2))
p
