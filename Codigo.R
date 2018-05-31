dat <- read.csv2( file = "Base de Datos Tiendas.csv")
## convertir la variable tiempo de servicio en numeros
dat$Tiempo.de.servicio <- as.numeric(substr(x = dat$Tiempo.de.servicio,start = 0,stop = 2))
## Sacar la variables con la que vamos a trabajar
dat <- data.frame(dat[,c(1,2,3,4,5,7,10,11,14,15,16,17)])
colnames(dat)<- c("I","M","R","U","S","D","N","C","E","TM","MS","TS")
#--------------------------------------------------------------------------
#/////////////Nombres de la variables /////////////////////////////////////
#--------------------------------------------------------------------------
## R: numero de registrso
# id : id
# M: mes
# U : Ubicacion
# S : sector
# D : Domicilio 
# N : numero de personas
# C : categoria 
# E : estrato
# TM: tamaño medio
# MS : municipio 
# Ts: tiempo de cervicio
#---------------------------------------------------------------------------
dat$M <- as.factor(dat$M)
dat$R <- as.integer(dat$R)
dat$TS <- as.numeric(dat$TS)
dat$I <- as.factor(dat$I)
dat <- na.omit(dat)
#----------------------------------------------------------------------------
#//////////////////Mejor distribuciones para la variable respuesta    ///////
#----------------------------------------------------------------------------
require(gamlss)
four.hist <- function(k, f, p,datos) {
  dt <- datos
  mod <- fitDist(dt[, p], type=f, k=k)
  par(mfrow=c(2, 2), bg='gray98')
  for(i in 1:4){
    denst <- density(dt[, p])
    res <- histDist(dt[, p], family=names(mod$fits)[i],
                    main='', 
                    ylab='Density',
                    xlab=p, las=1,
                    line.wd=3,
                    line.ty=1,
                    line.col='dodgerblue2',
                    ylim=c(0, (2 * max(denst$y))))
    gaic <- round(-2 * logLik(res) + k * length(res$parameters), 2)
    title(main=paste(i, ')', names(mod$fits)[i], 
                     'distribution with GAIC=', gaic),
          col.main='blue4')
    param <- c('mu', 'sigma', 'nu', 'tau') 
    np <- length(res$parameters)
    fun1 <- function(x) eval(parse(text=x))
    hat.param <- sapply(as.list(paste('res$', param[1:np], sep='')), fun1)
    hat.param <- round(hat.param, digits=2)
    txt <- paste('hat(', param[1:np], ')==', hat.param, sep='')
    txt <- paste(txt, collapse=', ')
    legend('topright', bty='n',
           legend=eval(parse(text=paste('expression(', txt, ')'))))
  }
}
#para mirar la distribucion para cada uno de los meses 
d1 <- subset(x = dat,subset = M== 1, select = c(I,R))
d2 <- subset(x = dat,subset = M == 2, select = c(I,R))
d3<-subset(x = dat,subset = M == 3, select = c(I,R))
d4<-subset(x = dat,subset = M == 4, select = c(I,R))
d5<- subset(x = dat,subset = M == 5, select = c(I,R))
d6 <- subset(x = dat,subset = M == 6, select = c(I,R))
d7 <- subset(x = dat,subset = M == 7, select = c(I,R))
d8 <- subset(x = dat,subset = M == 8, select = c(I,R))
## funcion para busca la variable 
require(gamlss)
fitDist(y = dat$R,k = log(nrow(dat)),type = "realplus")
four.hist(k = 2,f = "realplus",p = 2,datos = d2)
four.hist(k = 2,f = "realplus",p = 2,datos = d3)
four.hist(k = 2,f = "realplus",p = 2,datos = d6)
##descripcion: "No","WEI3","BCCGo"
#--------------------------------------------------------------------------
#/////////////////Mejores Modelos //////////////////////////////////////////
#----------------------------------------------------------------------------
library(gamlss)
mod <- gamlss(formula = R ~ re(random = ~1|M)+S+D+N+C+E+TM+MS+TS,data = dat)
summary(mod)
Rsq(mod)
summary(mod)
p <- predict(mod)
cor(dat$R,p)
sum(abs(p-dat$R))/nrow(dat)
data.frame(p,dat$R)
GAIC(mod)
stepGAIC(object = mod,scope = ~(re(random = ~1|M)+S+D+N+C+E+TM+MS+TS)^2,direction = "both")


mod.1 <-gamlss(formula = R ~ (re(random = ~1 | M)) + S + D + N + C + E + TM + MS + TS +  
                 C:MS + E:MS + N:MS + D:MS + E:TM + TM:MS + C:TM + C:TS +  
                 D:TS + TM:TS + MS:TS + N:TM + S:E + N:C + N:TS + D:TM + D:C +  
                 C:E + E:TS + S:TM + S:D + S:N + S:MS + N:E,data = dat,
               sigma.formula = ~(re(random = ~1 | M)) + S + D + N + C + E + TM + MS + TS )

Rsq(mod.1)
plot(mod.1)
p <- predict(mod.1)
cor(dat$R,p)
names(mod.1)
GAIC(mod.1)


#----------------------------------------------------------


mod1 <-gamlss(formula = R ~ (re(random = ~1 | M)) + S + D + N + C + E + TM + MS + TS +  
                C:MS + E:MS + N:MS + D:MS + E:TM + TM:MS + C:TM + C:TS +  
                D:TS + TM:TS + MS:TS + N:TM + S:E + N:C + N:TS + D:TM + D:C +  
                C:E + E:TS + S:TM + S:D + S:N + S:MS + N:E,data = dat,
              sigma.formula = ~(re(random = ~1 | M)) + S + D + N + C + E + TM + MS + TS ,family ="LQNO")
Rsq(mod1)
plot(mod1)
p <- predict(mod1)
cor(dat$R,p)
fitDist(y = dat$R,k = log(nrow(dat)),type = "realplus")
wp(object = mod1)
GAIC(mod1)
#-------------------------------------------------------------------



sig <- formula(~ (re(random = ~1 | M)) + S + D + N + C + E + TM + MS + TS )
mod2 <-gamlss(formula = R ~ (re(random = ~1 | M)) + S + D + N + C + E + TM + MS + TS +  
                C:MS + E:MS + N:MS + D:MS + E:TM + TM:MS + C:TM + C:TS +  
                D:TS + TM:TS + MS:TS + N:TM + S:E + N:C + N:TS + D:TM + D:C +  
                C:E + E:TS + S:TM + S:D + S:N + S:MS + N:E,data = dat,
              sigma.formula = sig,family = "LQNO" )
wp(mod2)
Rsq(mod2)
GAIC(mod2)
wp(mod2)
plot(mod2)
p<- predict(mod2)
cor(x = dat$R,y = p)
#------------------------------------------------------------------------------------
#\\\\\\\\\\\\\\\\\\\\\\\\\\\ mejor modelo \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#------------------------------------------------------------------------------------
library(gamlss)
mod3 <-gamlss(formula = R ~ (re(random = ~1 | M))+ S + D  +N + C + E + TM + MS + TS +  
                C:MS + E:MS + N:MS + D:MS + E:TM + TM:MS + C:TM + C:TS +  
                D:TS + TM:TS + MS:TS + N:TM + S:E + N:C + N:TS + D:TM + D:C +  
                C:E + E:TS + S:TM + S:D + S:N + S:MS + N:E,data = dat,
              sigma.formula = ~(re(random = ~1 | M)) + S + D + N + C + E + TM + MS + TS )
Rsq(mod3)
GAIC(mod3)
p <- predict(mod3)
cor(dat$R,p)
#-----------------------------------------------------------------------------------
#////////////////////////// GRAFICAS ///////////////////////////////////////////////
#-----------------------------------------------------------------------------------
dat$M <- as.factor(dat$M)
require(plotly)
plot_ly(data = dat,x=~N,y=~R,color=~M,alpha = 1)
# grafica de residuales
library(plotly) 
Residules <- residuals(mod.1)
Valores_ajustados <- fitted.values(mod.1)
plot_ly(x=~Valores_ajustados,y=~Residules)


# grafica de normalidad

qqnorm(Residules)
qqline(Residules,col="red")
grid()
