#Working Directory on R:
getwd()

#Extraction of the data:
EDA<-read.csv(file="C:\\Users\\Lenovo\\Documents\\EDA_1.csv",sep=",",header=T);EDA

#Library extraction:
library(ggplot2)
library(dplyr)

as.data.frame((EDA))



#par(mfrow=c(1,2))

#i>
#Scatterplot 1.1:
plot(EDA$Citations.per.document,EDA$H.index,xlab = 'Number of Citations per documents',ylab='H Index value',main='Scatterplot of H index vs Citations per Documents',sub='Scatterplot 1.2',type='p')

#Conclusion:From the given Scatterplot 1.2 , we can see a positive association between H Index and Citations per Documents.

#ii>
#Scatterplot 1.2:
plot(EDA$Citations-EDA$Self.citations,EDA$H.index,xlab = 'Number of Non-self Citations',ylab = 'H Index value',main='Scatterplot of H index vs Non-Self Citations ',sub='Scatterplot 1.3',type='p')

#Conclusion:From the given Scatterplot 1.3 , we can see a positive association between H Index and Non-Self Citations per Documents.


#2.Among the top 10 Ranked Countries of AI studies, how can we compare the culture of AI studies and the utility of documents published?

#Total Documents Published by top 10 Ranked Countries of AI studies:

#Vertical Bar Diagram 1.1:

#par(mfrow=c(2,2))
barplot(EDA$Documents[1:10],width=1,names.arg=c('China','USA','India','Japan','UK','Germany','France','Italy','Spain','Canada'),horiz=F,col=c('grey','white','grey','grey','black','black','black','black','black','white'),sub='Vertical Bar Diagram 1.1',main='Documents Published of top 10 AI rank Countries',xlab='Countries',ylab='Number of Documents',ylim=c(0,250000),cex.names=0.8)
legend('topright',c('Asia','Northern America','Western Europe'),fill=c('grey','white','black'),title='Regions',cex=0.6)
box()


#Total Citations by top 10 Ranked Countries of AI studies:

#Vertical Bar Diagram 1.2:

barplot(EDA$Citations[1:10],width=1,names.arg=c('China','USA','India','Japan','UK','Germany','France','Italy','Spain','Canada'),horiz=F,col=c('lawn green','dark green','lawn green','lawn green','spring green','spring green','spring green','spring green','spring green','dark green'),sub='Vertical Bar Diagram 1.2',main='Citations of top 10 AI rank Countries',xlab='Countries',ylab='Citations',ylim=c(0,3800000),cex.names=1)
legend('topright',c('Asia','Northern America','Western Europe'),fill=c('lawn green','dark green','spring green'),cex=0.6,title = 'Regions')
box()

#Citations per Documents Published by top 10 Ranked Countries of AI studies:

#Vertical Bar Diagram 1.3:

barplot(EDA$Citations.per.document[1:10],width=20,space=0.3,names.arg=c('China','USA','India','Japan','UK','Germany','France','Italy','Spain','Canada'),horiz=F,col=c('blue','dark blue','blue','blue','deep sky blue','deep sky blue','deep sky blue','deep sky blue','deep sky blue','dark blue'),sub='Vertical Bar Diagram 1.3',main='Citations per Documents of top 10 AI rank Countries',xlab='Countries',ylab='Citations per Docs',ylim=c(0,25),cex.names=1)
legend(x=100,y=25,c('Asia','America','WEurope'),fill=c('blue','dark blue','deep sky blue'),cex=0.6)
box()

#Non Self-Citations of top 10 ranked Countries of AI studies:

EDA_self_Citation<-EDA$Citations[1:10]-EDA$Self.citations[1:10]

#Vertical Bar Diagram 1.4:
barplot(EDA_self_Citation,names.arg=EDA$Country[1:10],cex.names = 0.6,ylim=c(0,2500000),main='Non-Self Citations of top 10 AI Countries',sub='Vertical Bar Diagram 1.4',ylab = 'Non-Self Citations',xlab = 'Countries',col=c('maroon','dark red','maroon','maroon','red','red','red','red','red','dark red'))
legend('topright',c('Asia','Northern America','Western Europe'),fill=c('maroon','dark red','red'),cex=0.6,title = 'Regions')
box()



#Conclusion: 

#Canada and USA have higher Citations per Documents,so the utility of documents in Northern America is higher as these are highly cited.

#USA,UK,China 's published documents are highly cited.

#3.How can we compare the culture of study of AI in different continents(1996-2021):
  
#a.Average Number of Documents published by countries of a particular continent(1996-2021)

#b.Number of citations mentioned to the countries of a particular continent(1996-2021)

#c.Highest H index of countries of a particular continent.(1996-2021)


#a>

as.data.frame(EDA)
Africa_documents<-EDA%>%filter(Region=='Africa')%>%select('Documents')
Africa_citations<-EDA%>%filter(Region=='Africa')%>%select('Citations')
Asia_documents<-EDA%>%filter(Region=='Asiatic Region')%>%select('Documents')
Asia_citations<-EDA%>%filter(Region=='Asiatic Region')%>%select('Citations')
Egypt_documents<-EDA%>%filter(Region=='Africa/Middle East')%>%select('Documents')
Egypt_citations<-EDA%>%filter(Region=='Africa/Middle East')%>%select('Citations')
Easteurope_documents<-EDA%>%filter(Region=='Eastern Europe')%>%select('Documents')
Easteurope_citations<-EDA%>%filter(Region=='Eastern Europe')%>%select('Citations')
LatinAmerica_documents<-EDA%>%filter(Region=='Latin America')%>%select('Documents')
LatinAmerica_citations<-EDA%>%filter(Region=='Latin America')%>%select('Citations')
MiddleEast_documents<-EDA%>%filter(Region=='Middle East')%>%select('Documents') 
MiddleEast_citations<-EDA%>%filter(Region=='Middle East')%>%select('Citations')
NorthAmerica_documents<-EDA%>%filter(Region=='Northern America')%>%select('Documents')
NorthAmerica_citations<-EDA%>%filter(Region=='Northern America')%>%select('Citations')
Pacific_documents<-EDA%>%filter(Region=='Pacific Region')%>%select('Documents')
Pacific_citations<-EDA%>%filter(Region=='Pacific Region')%>%select('Citations')
Westeurope_documents<-EDA%>%filter(Region=='Western Europe')%>%select('Documents')
Westeurope_citations<-EDA%>%filter(Region=='Western Europe')%>%select('Citations')



Africa<-mean(Africa_documents$Documents)
Asia<-mean(Asia_documents$Documents)
Egypt<-mean(Egypt_documents$Documents)
Easteurope<-mean(Easteurope_documents$Documents)
Latin<-mean(LatinAmerica_documents$Documents)
MiddleEast<-mean(MiddleEast_documents$Documents)
America<-mean(NorthAmerica_documents$Documents)
Westeurope<-mean(Westeurope_documents$Documents)
Pacific<-mean(Pacific_documents$Documents)

Docs<-data.frame(Africa,Asia,Egypt,Easteurope,Latin,MiddleEast,America,Westeurope,Pacific);Docs




#Horizontal Bar Diagram 1.1:
barplot.default(as.matrix(Docs),names.arg = colnames(Docs),xlab='Average published documents of Regions',ylab='Regions',xlim=c(0,150000),horiz = T,sub='Horizontal Bar Diagram 1.1',main='Average number of Published Documents across Regions',col='dark magenta',space = 1,cex.names=0.6)
legend('topright',c('Average number of Documents'),  fill = 'dark magenta')
box()

#Conclusion:

#From Horizontal Bar Diagram 1.1,we notice Northern America has higher average number of published documents not only it has only 2 Countries but also those two are in the top 10 ranked AI contries.

#Again, Asia and West Europe being the top contributers of published documents, due to the lack of encouragement of AI studies in all countries, they have less average number of published documents.




#b>

Africa<-mean(Africa_citations$Citations)
Asia<-mean(Asia_citations$Citations)
Egypt<-mean(Egypt_citations$Citations)
Easteurope<-mean(Easteurope_citations$Citations)
Latin<-mean(LatinAmerica_citations$Citations)
MiddleEast<-mean(MiddleEast_citations$Citations)
America<-mean(NorthAmerica_citations$Citations)
Westeurope<-mean(Westeurope_citations$Citations)
Pacific<-mean(Pacific_citations$Citations)

Cits<-data.frame(Africa,Asia,Egypt,Easteurope,Latin,MiddleEast,America,Westeurope,Pacific);Cits



#Horizontal Bar Diagram 1.2:
barplot.default(as.matrix(Cits),names.arg = c(colnames(Cits)),xlab='Average Number of citations of Regions',sub='Horizontal Bar Diagram 1.2',main='Average Citations across Regions',horiz=T,xlim=c(0,2500000),col='midnight blue',space=0.8,cex.names=0.6)
legend('topright',c('Average number of Citations'),  fill = 'midnight blue')
box()

#From Horizontal Bar Diagram 1.2,we notice Northern America has higher average number of Citations not only it has only 2 Countries but also those two are in the top 10 ranked AI contries with very high Citations and Citations per documents.

#Again, Asia and West Europe producing documents having high citations , due to the lack of encouragement of AI studies in all countries, they have less average number of Citations.

 
#c>

Africa_hindex<-EDA%>%filter(Region=='Africa')%>%select('H.index')
Asia_hindex<-EDA%>%filter(Region=='Asiatic Region')%>%select('H.index')
Egypt_hindex<-EDA%>%filter(Region=='Africa/Middle East')%>%select('H.index')
Easteurope_hindex<-EDA%>%filter(Region=='Eastern Europe')%>%select('H.index')
LatinAmerica_hindex<-EDA%>%filter(Region=='Latin America')%>%select('H.index')
MiddleEast_hindex<-EDA%>%filter(Region=='Middle East')%>%select('H.index')
NorthAmerica_hindex<-EDA%>%filter(Region=='Northern America')%>%select('H.index')
Westeurope_hindex<-EDA%>%filter(Region=='Western Europe')%>%select('H.index')
Pacific_hindex<-EDA%>%filter(Region=='Pacific Region')%>%select('H.index')



Africa<-max(Africa_hindex)
Asia<-max(Asia_hindex)
Egypt<-max(Egypt_hindex)
Easteurope<-max(Easteurope_hindex)
Latin<-max(LatinAmerica_hindex)
MiddleEast<-max(MiddleEast_hindex)
America<-max(NorthAmerica_hindex)
Westeurope<-max(Westeurope_hindex)
Pacific<-max(Pacific_hindex)



hindex<-data.frame(Africa,Asia,Egypt,Easteurope,Latin,MiddleEast,America,Westeurope,Pacific);hindex

#Vertical Bar Diagram 2.3:

barplot.default(c(62,324,83,110,116,168,572,309,206),names.arg = c(colnames(Cits)),xlab='Region',ylab='Highest H-Index of Regions',main='Highest H index among Regions',sub='Vertical Bar Diagram 2.3',ylim=c(0,600),col=c('white','yellow','green','brown','red','pink','violet','blue','grey'),space=1)
legend("topright",c('South Africa','China','Egypt','Poland','Brazil','Iran','USA','UK','Australia'),fill=c('white','yellow','green','brown','red','pink','violet','blue','grey'),cex=0.7,title='Countries')
box()

#Conclusion:Vertical Bar Diagram 2.3 shows us the Country of a Region having the highest H Index.

#4.Contribution of Regions in the field of AI study in terms of :
#a.Total Documents published(1996-2021)

#b.Total Citations Received(1996-2021)

#a>
Africa<-sum(Africa_documents$Documents)
Asia<-sum(Asia_documents$Documents)
Egypt<-sum(Egypt_documents$Documents)
Easteurope<-sum(Easteurope_documents$Documents)
Latin<-sum(LatinAmerica_documents$Documents)
MiddleEast<-sum(MiddleEast_documents$Documents)
America<-sum(NorthAmerica_documents$Documents)
Westeurope<-sum(Westeurope_documents$Documents)
Pacific<-sum(Pacific_documents$Documents)

SumsDoc<-data.frame(Africa,Asia,Egypt,Easteurope,Latin,MiddleEast,America,Westeurope,Pacific);SumsDoc
colnames(SumsDoc)
a<-SumsDoc/sum(EDA$Documents)*100;a


#Piechart 1.1:

pie((as.matrix(SumsDoc)),labels=c('Africa(1.639637%)','Asia(41.12199%)','Egypt(0.4335113%)','Easteurope(5.430245%)','Latin(3.308362%)','MiddleEast(4.418974%)','Northern America(17.02193%)','Westeurope(23.97359%)','Pacific(2.651759%)'),col =c('light yellow','saddle brown','light golden rod yellow','dark orange','golden rod','gold','orange','chocolate','yellow'),main='Total AI Document Contributions of Continents(1996-2021)',sub='Piechart 1.1',radius=1,cex=1.0,clockwise = 0)




#Conclusion:From Piechart 1.1, Asia,Northern America,and Westeurope have contributed to about 82.12% of total documents published on AI(1996-2021)

#b>
Africa<-sum(Africa_citations$Citations)
Asia<-sum(Asia_citations$Citations)
Egypt<-sum(Egypt_citations$Citations)
Easteurope<-sum(Easteurope_citations$Citations)
Latin<-sum(LatinAmerica_citations$Citations)
MiddleEast<-sum(MiddleEast_citations$Citations)
America<-sum(NorthAmerica_citations$Citations)
Westeurope<-sum(Westeurope_citations$Citations)
Pacific<-sum(Pacific_citations$Citations)


SumsCits<-data.frame(Africa,Asia,Egypt,Easteurope,Latin,MiddleEast,America,Westeurope,Pacific);SumsCits
SumsCits/sum(EDA$Citations)*100


#Piechart 1.2:

pie((as.matrix(SumsCits)),labels=c('Africa(0.64%)','Asia(28.48%)','Egypt(0.30%)','Easteurope(2.97%)','Latin(1.87%)','MiddleEast(4.52%)','America(28.86%)','Westeurope(29.21%)','Pacific(3.15%)'),col =c('ivory','blue','white','light sky blue','cyan','dodger blue','medium blue','navy','deep sky blue'),main='Total Citations of Continents(1996-2021)',sub='Piechart 1.2',radius=1,cex=1.0,clockwise = 1)




#Conclusion:From Piechart 1.2, Asia,Northern America,and Westeurope have about 86.6% of Total Citations across 9 Regions(1996-2021)

#Piechart 1.3:

#Non-Self Citations:


EDA$Nonself_Citations<-EDA$Citations-EDA$Self.citations

Africa_self<-EDA%>%filter(Region=='Africa')%>%select('Nonself_Citations')
Egypt_self<-EDA%>%filter(Region=='Africa/Middle East')%>%select('Nonself_Citations')
Asia_self<-EDA%>%filter(Region=='Asiatic Region')%>%select('Nonself_Citations')
Easteurope_self<-EDA%>%filter(Region=='Eastern Europe')%>%select('Nonself_Citations')
LatinAmerica_self<-EDA%>%filter(Region=='Latin America')%>%select('Nonself_Citations')
MiddleEast_self<-EDA%>%filter(Region=='Middle East')%>%select('Nonself_Citations')
NorthAmerica_self<-EDA%>%filter(Region=='Northern America')%>%select('Nonself_Citations')
Pacific_self<-EDA%>%filter(Region=='Pacific Region')%>%select('Nonself_Citations')
Westeurope_self<-EDA%>%filter(Region=='Western Europe')%>%select('Nonself_Citations')



Africa<-sum(Africa_self$Nonself_Citations)
Egypt<-sum(Egypt_self$Nonself_Citations)
Asia<-sum(Asia_self$Nonself_Citations)
Easteurope<-sum(Easteurope_self$Nonself_Citations)
LatinAmerica<-sum(LatinAmerica_self$Nonself_Citations)
MiddleEast<-sum(MiddleEast_self$Nonself_Citations)
NorthAmerica<-sum(NorthAmerica_self$Nonself_Citations)
Pacific<-sum(Pacific_self$Nonself_Citations)
Westeurope<-sum(Westeurope_self$Nonself_Citations)


NonselfCits<-data.frame(Africa,Asia,Egypt,Easteurope,Latin,MiddleEast,America,Westeurope,Pacific);NonselfCits
NonselfCits/sum(NonselfCits$Africa+NonselfCits$Asia+NonselfCits$Egypt+NonselfCits$Easteurope+NonselfCits$Latin+NonselfCits$MiddleEast+NonselfCits$America+NonselfCits$Westeurope+NonselfCits$Pacific)*100

pie((as.matrix(NonselfCits)),labels=c('Africa(0.62%)','Asia(21.27%)','Egypt(0.30%)','Easteurope(2.65%)','Latin(2.30%)','MiddleEast(4.66%)','NorthernAmerica(35.58%)','Westeurope(29.31%)','Pacific(3.30%)'),col =c('green yellow','dark green','yellow','lime green','chartreuse','sea green','dark slate gray','dark cyan','yellow green'),main='Non-Self Citations of Continents(1996-2021)',sub='Piechart 1.3',radius=1,cex=1.0,clockwise = 1)

#Conclusion:NorthernAmerica,Westeurope,Asia have about 86.2% of Non-Self Citations

  

#Scatterplot across Different Regions:
ggplot(data=EDA,aes(x=Citations,y=H.index))+geom_point(aes(x=Citations,y=H.index),color='black')+theme_light()+labs(title='Scatterplot',subtitle='H index vs Citations',x='Citations',y='H index')+facet_wrap(~Region)+ylim(0,600)+xlim(0,3600000)

#Scatterplot across Different Regions:
ggplot(data=EDA,aes(x=Documents,y=H.index))+geom_point(aes(x=Documents,y=H.index),color='green')+theme_dark()+labs(title='Scatterplot',subtitle='H index vs Documents',x='Documents',y='H index')+facet_wrap(~Region)+ylim(0,600)+xlim(0,240000)

#Trend that rank given based on Documents Published:
barplot(EDA$Documents,main='Rank Given based on Documents',space=2,sub='Verical Bar Diagram',xlab='Countries',ylab='Documents')


#Scatter plot of non self Citations:
ggplot(data=EDA,aes(x=(EDA$Citations-EDA$Self.citations),y=H.index))+geom_point(aes(x=(EDA$Citations-EDA$Self.citations),y=H.index),color='black')+theme_get()+labs(title='Scatterplot',subtitle='H index vs Non-Self Citations',x='Non-self Citations',y='H index')+facet_wrap(~Region)+ylim(0,600)+xlim(0,2500000)



