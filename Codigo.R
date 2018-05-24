dat <- read.csv2( file = "Base de Datos Tiendas.csv")
## convertir la variable tiempo de servicio en numeros
dat$Tiempo.de.servicio <- as.numeric(substr(x = dat$Tiempo.de.servicio,start = 0,stop = 2))
## Sacar la variables con la que vamos a trabajar
dat <- data.frame(dat[,c(1,2,3,4,5,7,10,11,14,15,16,17)])
colnames(dat)<- c("x1","x2","y","x3","x4","x5","x6","x7","x8","x9","x10","x11")
dat <-na.omit(dat)
#--------------------------------------------------------------------------
#/////////////Nombres de la variables /////////////////////////////////////
#--------------------------------------------------------------------------
## y : numero de registrso
# x1 : id
# x2: mes
# x3 : Ubicacion
# x4 : sector
# x5 : Domicilio 
# x6 : numero de personas
# x7 : categoria 
# x8 : estrato
# x9 : tamaño medio
# x10 : municipio 
# x11: tiempo de cervicio
#---------------------------------------------------------------------------
dat$x3<-as.factor(x = dat$x3)
dat$x2 <- as.integer(dat$x2)
dat$y <- as.numeric(dat$y)
dat$x11 <- as.numeric(dat$x11)
dat$x8 <- as.factor(dat$x8)
dat$x9 <- as.numeric(dat$x9)
dat$x6 <- as.integer(dat$x6)
dat$x1 <- as.integer(dat$x1)
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
d1 <- subset(x = dat,subset = x2== 1, select = c(x1,y))
d2 <- subset(x = dat,subset = x2 == 2, select = c(x1,y))
d3<-subset(x = dat,subset = x2 == 3, select = c(x1,y))
d4<-subset(x = dat,subset = x2 == 4, select = c(x1,y))
d5<- subset(x = dat,subset = x2 == 5, select = c(x1,y))
d6 <- subset(x = dat,subset = x2 == 6, select = c(x1,y))
d7 <- subset(x = dat,subset = x2 == 7, select = c(id,Registros))
d8 <- subset(x = dat,subset = x2 == 8, select = c(id,Registros))
## funcion para busca la variable 
four.hist(k = 2,f = "realplus",p = 2,datos = d1)
four.hist(k = 2,f = "realplus",p = 2,datos = d2)
four.hist(k = 2,f = "realplus",p = 2,datos = d3)
four.hist(k = 2,f = "realplus",p = 2,datos = d6)
##descripcion: "No","WEI3","BCCGo"
#--------------------------------------------------------------------------
#/////////////////Mejores Modelos //////////////////////////////////////////
#----------------------------------------------------------------------------
library(gamlss)
mu <- formula(y~(1+x2|x1)+x3+x4+x5+x6+x7+x8+bs(x=x9,degree = 3)+x10+x11+x3*x6+x3*x7+x3*x8+x3*x9+x4*x8+
                x5:x6+x5*x10+x6*x7+x6*x10+x7*x10+x8*x9+x8*x9+x8*x10)
sig <- formula(y~.)

mod <- gamlss(formula = mu,sigma.formula = sig,data = dat)
Rsq(object = mod,type = "both")       ## R^2
sum((fitted(mod)-dat$y)^2)/nrow(dat) ## MSE
sum(abs(fitted(mod)-dat$y))/nrow(dat) ## MSE
cor(fitted(mod),dat$y)               ## correlacion
GAIC(mod)
plot(mod)
#NOTA: Este aparante ser el mejor modelo pero no tiene interpretacion entre la 
#interracion de las variables

#------------------------------------------------------------------
mod1 <- gamlss(formula = y~(1+x1|x2)+x2+x6+x8+x7+x9+x10+x11+x8*x9+x8*x10,family = "WEI3",
               data = dat)
Rsq(object = mod1,type = "both")       ## R^2
sum((fitted(mod1)-dat$y)^2)/nrow(dat) ## MSE
sum(abs(fitted(mod1)-dat$y))/nrow(dat) ## MSE
cor(fitted(mod1),dat$y)               ## correlacion
GAIC(mod1)
plot(mod1)
#--------------------------------------------------------------------------
library(splines)
sig <- formula(y~x3+x4+x5+x6+x7+x8+x9+x10+x11)
mod2 <- gamlss(formula = y~(1+x1|x2)+x2+x6+x8+x7+bs(x = x9,degree = 3)+x10+bs(x=x11,degree = 3)
               +x8*x9+x8*x10+x3*x8,sigma.formula = sig,data = dat,family = "WEI3")


Rsq(object = mod2,type = "both")       ## R^2
sum((fitted(mod2)-dat$y)^2)/nrow(dat) ## MSE
sum(abs(fitted(mod2)-dat$y))/nrow(dat) ## MSE
cor(fitted(mod2),dat$y)               ## correlacion
GAIC(mod2)
plot(mod2)

#-----------------------------------------------------------------------------
require(lme4)
require(splines)
mod3 <- lmer(formula = y~(1+x1|x2)+x2+x6+x8+x7+bs(x = x9,degree = 3)+x10+bs(x=x11,degree = 3)
             +x8*x9+x8*x10+x3*x8+x5*x6,data = dat)
p <- predict(mod3)
cor(x = p,y = dat$y)
(sum(p-dat$y)^2)/nrow(dat)
dat<-na.omit(dat)
par(mfrow=c(2,1))
plot(fitted(mod3),resid(mod3,type="pearson"),col="blue") #a plot to check the constant standard deviation
abline(h=0,lwd=2)
qqnorm(resid(mod3)) 
qqline(resid(mod3))
library(lattice)
dotplot(ranef(mod3,condVar=TRUE))
summary(mod3)

#-----------------------------------------------------------------------------------
dat$x2 <- as.factor(dat$x2)
require(plotly)
plot_ly(data = dat,x=~x1,y=~y,color=~x2,alpha = 1 ,colors = "set5")
          

