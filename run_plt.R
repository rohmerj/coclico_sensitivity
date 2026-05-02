library(ggplot2)
library(gridExtra)

rm(list=ls())

get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
    }

tt <- theme(
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),#, face="bold"
		axis.text.y= element_text(size=14),
		axis.text.x= element_text(size=14),
		axis.ticks = element_line(size = 0.75),
		legend.title=element_text("Category of LAU"),
		legend.position="none"
		)

#############################################################
######## BOXPLOT per LAU type
#############################################################

## CHOOSE RESULT
NOM_RESU = paste0("./resu/All/sensitivity_country_",country,"_SSPonly.RData")

##  population
load(".././data/population.RData")
dens = df.pop[,c("GISCO_ID","POP_DENS_2")]

## CHOOSE COUNTRY
p = list()
Country = c("Italy","Spain","France","the UK","Finland")
c = 0

for (country in c("IT","ES","FR","UK","FI")){

load(NOM_RESU)

### filtre country
type_urban = NULL
for (j in 1:length(LAU)){
	dens.val = dens[which(dens[,1] == LAU[j]),2]
	if (is.na(dens.val)==T) dens.val = 100
	if (dens.val <= 50){type_urban[j] = "rural"}
	if (dens.val <= 1000 & dens.val > 50){type_urban[j] = "semirural"}
	if (dens.val > 1000){type_urban[j] = "urban"}
}

###
Names= c("Damage curve","Repair cost","Area","SSP-RCP","Protection level","Water-depth")
f = which(DEBUG==1)
table(type_urban[f])

df = data.frame(
	LAU = rep(LAU[f],6),
	LAU_category=rep(type_urban[f],6),
	var = c(
		rep(Names[1],length(f)),
		rep(Names[2],length(f)),
		rep(Names[3],length(f)),
		rep(Names[4],length(f)),
		rep(Names[5],length(f)),
		rep(Names[6],length(f))
		),
	FO=c(FO[1,f],FO[2,f],FO[3,f],FO[4,f],FO[5,f],FO[6,f])
)
c = c + 1
p[[country]] = ggplot(df,aes(x=as.factor(var),y=FO,fill=LAU_category))+coord_flip()+#
	geom_boxplot(outlier.shape=NA)+ylab("Sensitivity measure")+xlab("")+ggtitle(Country[c])+theme_bw()+tt
	#geom_point(position=position_jitterdodge(),size=1.5)+theme_bw()

}## country

#############################################################
######## BOXPLOT for ALL 
#############################################################
df = rbind(p[[1]]$data, p[[2]]$data, p[[3]]$data, p[[4]]$data, p[[5]]$data)
df$country = c(
	rep("Italy",nrow(p[[1]]$data)),
	rep("Spain",nrow(p[[2]]$data)),
	rep("France",nrow(p[[3]]$data)),
	rep("the UK",nrow(p[[4]]$data)),
	rep("Finland",nrow(p[[5]]$data))
		)
tt <- theme(
		axis.title.x = element_text(size=16),
		axis.title.y = element_text(size=16),#, face="bold"
		axis.text.y= element_text(size=16),
		axis.text.x= element_text(size=16),
		axis.ticks = element_line(size = 1),
		legend.title=element_blank(),
		legend.position="none",
		legend.text = element_text(size=14)
		)
col = RColorBrewer::brewer.pal(6, "Dark2")[c(3,1,6,2,4)]
df$var[(df$var=="SSP-RCP")] = "SSP scenario"

PLT0 = ggplot(df,aes(x=as.factor(var),y=FO,fill=country))+
	geom_boxplot(outlier.shape=NA,size=.5)+
	ylab("Sensitivity measure")+xlab("")+
	theme_bw()+tt+theme(legend.position="top")+
	scale_fill_manual(values = col)

print(PLT0)
