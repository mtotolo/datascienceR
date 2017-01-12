conf<-names(mtcars)[names(mtcars)!="mpg"&names(mtcars)!="am"]
fitlist<-list()
mtcars$am<-as.factor(mtcars$am)
mtcars$vs<-as.factor(mtcars$vs)
mtcars$cyl<-as.factor(mtcars$cyl)
mtcars$gear<-as.factor(mtcars$gear)
mtcars$carb<-as.factor(mtcars$carb)
fitlist[[1]]=lm(mpg~am,mtcars)
for (i in 1:length(conf)) {
      formula<-as.formula(paste0("mpg~am+",paste0(conf[1:i],collapse="+")))
      fitlist[[1+i]]<-lm(formula,data=mtcars)
}
do.call(anova,fitlist)

null<-lm(mpg~1,mtcars)
full<-lm(mpg~.,mtcars)
a<-step(null, scope=list(lower=null, upper=full), direction="both")

ggplot(mtcars,(aes(wt,mpg,fill=hp,shape=am)))+
      geom_point(size=5,alpha=0.5)+
      facet_grid(cyl~.,labeller=label_both)+
      scale_fill_distiller(palette="Spectral")+
      scale_shape_manual(values=c(21,22))