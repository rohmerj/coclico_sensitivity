library(sensitivity)

rm(list=ls())

##  population
load("./data/population.RData")
dens = df.pop[,c("GISCO_ID","POP_DENS_2")]

##  results
## CHOOSE COUNTRY
#for (country in c("ES","FR","IT","UK","FI")){
for (country in c("ES")){

if (country != "FI"){
	doe = read.csv("./data/doe5.csv",sep=";")
	result=read.csv("./data/result5.csv",sep=";",header=T)
}else{
	doe = read.csv("./data/finlandes_doe.csv",sep=";")
	result=read.csv("./data/finlandes_results.csv",sep=";",header=T)
}

resu = result[,-1]
LAU = result[,1]
PAYS = NULL
for (i in 1:length(LAU)){
	PAYS[i] = strsplit(LAU[i],"_")[[1]][1]
}

### filtre country
f = which(PAYS == country)
resu = resu[f,]
LAU = LAU[f]
PAYS = PAYS[f]

### data format
resu = t(resu)
resu = apply(resu,1,as.numeric)
dat = data.frame(doe[,-7],t(resu))

### filtre SSP
for(CASE in c("SSPonly","HE")){

if (CASE == "SSPonly") f = which(dat$ssp != "HE")
if (CASE != "SSPonly") f = which(dat$ssp == "HE" | dat$ssp == "SSP585")
dat = dat[f,]

ouX = 1:6
ouY = (6+1):(6+length(LAU))#7:357

Names= c("curve","price","area","ssp","defence","water-depth")

cc = 0
BUG = NULL 
FO = TO = FOlw = FOup = TOlw = TOup = matrix(0,6,length(ouY))
QQ =  matrix(0,length(ouY),5)

#### run
#j = 131##132
DEBUG = NULL
for (j in 1:length(ouY)){
#for (j in 1:1){

	#print(LAU[j])
	print(paste0(round(j/length(ouY),2)))	

	### DENSITY
	#agricultural (<50 hab/km²) => undefended
	#urban (>50 & <1000) => low-defended
	#dense urban (> 1000)  => high-defended

	dens.val = dens[which(dens[,1] == LAU[j]),2]
	if (is.na(dens.val)==T) dens.val = 100	

	if (dens.val <= 50){prob_defence = c(0.1,0.2,0.7);kernel_defence="categ_proba1_anova"}
	if (dens.val <= 1000 & dens.val > 50){prob_defence = c(0.1,0.7,0.2);kernel_defence="categ_proba2_anova"}
	if (dens.val > 1000){prob_defence = c(0.7,0.2,0.1);kernel_defence="categ_proba3_anova"}

	### data for HSIC
	datX = dat[,ouX]
	for (jj in 1:6) datX[,jj] = as.factor(datX[,jj])
	datY = dat[,ouY[j]]
	ffna = which(is.na(datY))
	datY[ffna] = 0

	if (max(datY)>0){
	DEBUG[j] = 1
	
	### sampel / prob.
	nmc = nrow(datX)*5
	rr.courbe = sample(unique(datX[,1]),nmc,replace=T)
	rr.price = sample(unique(datX[,2]),nmc,replace=T)
	rr.area = sample(unique(datX[,3]),nmc,replace=T)
	rr.ssp = sample(unique(datX[,4]),nmc,replace=T)
	rr.defence = sample(unique(datX[,5]),nmc,replace=T,prob=prob_defence)
	rr.hauteur = sample(unique(datX[,6]),nmc,replace=T)
	datX2 = data.frame(
			curve=rr.courbe,
			price=rr.price,
			area=rr.area,
			ssp=rr.ssp,
			defence=rr.defence,
			height=rr.hauteur)

	datY2 = NULL
	for (ii in 1:nmc){
		f = which(datX2[ii,1] == datX[,1]  & 
			datX2[ii,2] == datX[,2] &
			datX2[ii,3] == datX[,3] &
			datX2[ii,4] == datX[,4] &
			datX2[ii,5] == datX[,5] &
			datX2[ii,6] == datX[,6]
			)
		if (length(f) > 0){
			datY2[ii] = datY[f]
		}else{
			datY2[ii] = -9999			
		}	
	}
	f = which(datY2 >= 0)
	datX2 = datX2[f,]
	datY2 = datY2[f]
	
	###
	sensi2 <- sensiHSIC(model=NULL, X = datX2[,c(1:6)],
			 kernelX=c("categ_anova","categ_anova","categ_anova","categ_anova","categ_anova","categ_anova"), 
					paramX = 1,
					kernelY="rbf", paramY=NA,
					nboot = 0, conf = 0.95,
					anova = list(obj = "both", is.uniform = FALSE),
					)
	ss.temp2 = tell(sensi2, y=data.frame(datY2))

	FO[,j] = ss.temp2$FO[,1]
	TO[,j] = ss.temp2$TO[,1]

	### STATISTICS of DAMAGE
	QQ[j,] = quantile(datY,c(0.05,0.25,0.5,0.75,0.95))

	###

	}

}##j

save(DEBUG,LAU,TO,FO,QQ,file=paste0("./resu/sensitivity_country_",country,"_",CASE,".RData"))

}##country

}## case
################
