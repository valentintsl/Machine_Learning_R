setwd("D:\\Apprentissage avec R")
getwd()

library(data.table)

#MANIPULATION OF DATA.TABLE
#Load the data with fread
DT <- fread("data\\train.csv")

#Print the first 5 lines of each column
print(head(DT))

#Print summary of all columns
print(summary(DT))

#Alternative to summary
str(DT)

#Dimension of the dataframe
dim(DT)

#All variable names
names(DT)

#Print all the values of the column 'Id'
DT$Id

#Print all the values of the column SalesPrice
DT$SalePrice

#How many values "LotConfig" in the data
table(DT$LotConfig)
#We observe 5 class the most are Inside
#Number of class
unique(DT$LotConfig)

#Print only the DT lines for which LotConfig == 'Inside', and add the condition PoolQC is NA
DT[LotConfig=='Inside']

DT[LotConfig=='Inside' & is.na(PoolQC)]

#Print only WoodDeskSF column and Fence
DT[,.(WoodDeckSF, Fence)]#Don't forget the comma

#Define the vector variable feature containing WoodDeskSF and Fence
feature <- c('WoodDeckSF','Fence')
DT[,feature, with=FALSE]

#Create a variable DT2 extracted from DT with Loconfig = Inside and only with WoodDeckSF and Fence columns
DT2 <- DT[LotConfig =='Inside',.(WoodDeckSF, Fence)]

#Let's save DT before transformation
saveDT <- DT

#Create a new feature that indicates NA on PoolQC column
DT[,Pool:=is.na(PoolQC)]


#MANIPULATION OF REGEX
#Identify column names beginning by a digit
nom ="ESILV"
c<-grepl("^B", nom)
print(c)

#Identify name beginning by a digit
Nom2<-"10_avenue"
cc <- grepl("^[0-9]", Nom2)
print(cc)

s<-grepl("^[0-9]", names(DT))
print(s)

names(DT[s])

FeatDebNum<-names(DT)[s]
print(FeatDebNum)

paste0("a","b")

#Add F_ before each name

for (f in FeatDebNum) setnames(DT, f, paste0("F_",f))

print(names(DT))
print(FeatDebNum)


#Which are the columns containing numeric values ? 

#Which are the variables types in each column ?

sapply(DT, class)

#Which are the variables ?

non_char_DT<-DT[,names(which(sapply(DT,class)!="character")), with=FALSE]

#In each columns, how many differents elements are inside
number_different<-sapply(non_char_DT, function(x) length(unique(x)))

print(DT$SalePrice)


#Print histogram with parameter bins=50 and white background
ggplot(DT,aes(x=SalePrice))+geom_histogram(bins=50, fill="blue")+theme_classic()

#Les valeurs sont assez grandes -> passez � l'chelle log en abcisse

DT[,SalePrice:=log(SalePrice)]
ggplot(DT,aes(x=SalePrice))+geom_histogram(bins=50, fill="blue")+theme_classic()

#Find the data types in each columns
all_types=sapply(DT, class)

indices_char_

# Identify columns names beginning by a digit and add a letter in front
# identifier les noms de colonne commençant par un chiffre et ajouter une lettre devant
FeatDebNum<-names(DT)[grepl("^[[:digit:]]",names(DT))]

for (f in FeatDebNum) setnames(DT,f,paste0("F_",f)) # faire en sorte qu'aucun nom de variable ne commence par un chiffre _ interdit ..

#GGplot
library(ggplot2)
library(ggthemes)
library(corrplot)
#GGally::ggpairs(DT[sample(nrow(DT),1000),int[1:10],with=FALSE]) # attention temps de calcul !

#Corrplot : Correlation matrix
corrplot(cor(DT[,names(which(sapply(DT,class)!="character")),with=FALSE]), method = "ellipse")

#GEOM HISTOGRAM
#target - log effect
ggplot(DT,aes(x=SalePrice))+geom_histogram(bins=50,fill="blue")+theme_tufte()
ggplot(DT,aes(x=log(SalePrice)))+geom_histogram(bins=50,fill="blue")+theme_tufte() 
DT[,SalePrice:=log(SalePrice)]

#The logarithmic approach make comparable errors indepedently of absolute values and also 
#has the effect of bringing the distribution closer to a Gaussian distribution, which is important for the linear regression approach

#NA Processing
 
# type des features
int<-names(DT)[which(sapply(DT, class)%in% c("integer","numeric"))]
char<-names(DT)[which(sapply(DT, class)=="character")]
level<-sort(sapply(DT[,char,with=FALSE], function(x) length(unique(x))))# identifier le nombre de valeur différentes pour les colonnes string

# cartographier les NA
isna<-sort(sapply(DT, function(x) sum(is.na(x))/length(x)))
isna<-isna[isna>0]
isnaDT<-data.table(var=names(isna),txna=isna)
isnaDT[,type:="integer"] ; isnaDT[var %in% char,type:="string"] ; 
isnaDT[,var:=factor(var,levels=names(isna))] # pour ordonner l'affichage
isnaDT[var %in% char,type:="string"] # pour différencier la couleur en fonction du type
ggplot(isnaDT[txna>0],aes(x=var,y=txna))+geom_bar(stat="identity",aes(fill=type))+theme_tufte()
# ou avec un package (voir aussi package DataExplorer)
gg_miss_var(DT) # avec package naniar
gg_miss_upset(DT)

# observation des correlations
temp<-copy(DT[,c(char,"SalePrice"),with=FALSE]) 
temp<-melt.data.table(temp,id.vars = "SalePrice")
ggplot(temp,aes(x=value,y=SalePrice))+geom_violin(fill="blue")+facet_wrap(~variable,scales="free_x")+theme_tufte()
temp<-copy(DT[,int,with=FALSE]) 
temp<-melt.data.table(temp,id.vars = "SalePrice")
ggplot(temp,aes(x=value,y=SalePrice))+geom_point(col="blue")+facet_wrap(~variable,scales="free_x")+theme_tufte()

#traitement NA
DTfull<-copy(DT)
for (c in intersect(names(isna),char)) DTfull[(is.na(get(c))),(c):="ex.na"]
for (c in intersect(names(isna),int)) DTfull[is.na(get(c)),(c):=median(DTfull[[c]],na.rm=TRUE)]

#traitement des facteurs peu fréquents - expliquer le traitement suivant
for (c in char) for (v in names(which(table(DTfull[[c]])<15))) DTfull[get(c)==v,(c):="Autre"]
for (c in char) if(min(table(DT[[c]]))<40) {temp<-names(head(sort(table(DTfull[[c]])),2)) ; for (t in temp) DTfull[get(c)==t,(c):=paste(temp,collapse="_")]}

# préparation des bases ####
valid<-sample(nrow(DTfull),floor(nrow(DTfull)/3)) ;# test<-sample(nrow(DT[-valid]),floor(nrow(DT)/10))
DTTrain<-DTfull[-valid] ; DTValid<-DTfull[valid]
supp<-names(which(sapply(DTTrain[,char,with=FALSE],function(x) length(unique(x)))==1)) ; for (c in supp) {DTTrain[,(c):=NULL] ; DTValid[,(c):=NULL]}
char<-names(DTTrain)[which(sapply(DTTrain, class)=="character")]

DTFactor<-rbind(DTTrain,DTValid)
for (c in char) DTFactor[,(c):=as.factor(get(c))] 
DTTrainRF<-DTFactor[1:nrow(DTTrain)] ; DTValidRF<-DTFactor[(nrow(DTTrain)+1):nrow(DTFactor)] ;

require(dummies)
DTMatrice<-rbind(DTTrain,DTValid)
DTMatrice<-dummy.data.frame(DTMatrice)
DTTrainMat<-as.matrix(DTMatrice[1:nrow(DTTrain),]) ; DTValidMat<-as.matrix(DTMatrice[(nrow(DTTrain)+1):nrow(DTMatrice),]) 

