install.packages("dfcrm")
install.packages("wakefield")
k<-age(100,18:65)
p<-as.factor(sample(c(0,1),replace = TRUE,size = 100))
s<-sex(100)
df<-data.frame(age = k,sex = s,tox = p)
df
dose<-sample(c(1:5),replace = TRUE,size = 100)
dose<-as.factor(dose)
df<-cbind(df,dose)
summary(df)
a<-df %>%
  filter(dose==1) 
a
b<-length(a$dose)
b
a<-a %>%
  filter(tox==1)
c<-length(a$dose)
c
proba1<-c/b
proba1
proba2<-sum(df$dose==1 & df$tox==1)/sum(df$dose==1)
proba2
probaAll<-c()
for(i in 1:5){
  probaAll<-c(probaAll,sum(df$dose==i & df$tox==1)/sum(df$dose==i))
}
probaAll



#seance 2

require(dfcrm)
prob<- c(0.05,0.1,0.15,0.2,0.3)
df<-data.frame(d1=(sample(c(0,1),replace = TRUE,size = 30,prob=c(1-prob[1],prob[1]))))
for(i in 2:5){
  p<-sample(c(0,1),replace = TRUE,size = 30,prob=c(1-prob[i],prob[i]))
  df<-cbind(df,p)
}
colnames(df)<-c("d1","d2","d3","d4","d5")
df
prior<-c(0.05,0.1,0.2,0.35,0.5,0.7)
target<-0.2
level<-c(3,4)
y<-c(0,1)

#patient1
foo<-crm(prior,target,y,level)
foo1<-crm(prior,target,y,level,model="logistic")
foo2<-crm(prior,target,y,level,method="mle")
foo
foo1

#patient2
foo_pt2<-crm(prior,target,y,level)
foo1_pt2<-crm(prior,target,y,level,method = "mle")
foo2_pt2<-crm(prior,target,y,level,model="logistic")
foo3_pt2<-crm(prior,target,y,level,model="logistic",method="mle")
foo_pt2
foo1_pt2
foo2_pt2
foo3_pt2

#patient3
level<-c(3,4,2)
y<-c(0,1,0)
foo_pt3<-crm(prior,target,y,level)
foo_pt3

#patient4
level<-c(3,4,2,2)
y<-c(0,1,0,0)
foo_pt4<-crm(prior,target,y,level)
foo_pt4

#patient5
level<-c(3,4,2,2,3)
y<-c(0,1,0,0,0)
foo_pt5<-crm(prior,target,y,level)
foo_pt5

#patient6
level<-c(3,4,2,2,3,3)
y<-c(0,1,0,0,0,0)
foo_pt6<-crm(prior,target,y,level)
foo_pt6

#patient7
level<-c(3,4,2,2,3,3,3)
y<-c(0,1,0,0,0,0,0)
foo_pt7<-crm(prior,target,y,level)
foo_pt7

#patient8
level<-c(3,4,2,2,3,3,3,4)
y<-c(0,1,0,0,0,0,0,1)
foo_pt8<-crm(prior,target,y,level)
foo_pt8

#patient9
level<-c(3,4,2,2,3,3,3,4,3)
y<-c(0,1,0,0,0,0,0,1,0)
foo_pt9<-crm(prior,target,y,level)
foo_pt9

#patient10
level<-c(3,4,2,2,3,3,3,4,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0)
foo_pt10<-crm(prior,target,y,level)
foo_pt10

#patient11
level<-c(3,4,2,2,3,3,3,4,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1)
foo_pt11<-crm(prior,target,y,level)
foo_pt11

#patient12
level<-c(3,4,2,2,3,3,3,4,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0)
foo_pt12<-crm(prior,target,y,level)
foo_pt12

#patient13
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0)
foo_pt13<-crm(prior,target,y,level)
foo_pt13

#patient14
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0)
foo_pt14<-crm(prior,target,y,level)
foo_pt14

#patient15
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0)
foo_pt15<-crm(prior,target,y,level)
foo_pt15

#patient16
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0)
foo_pt16<-crm(prior,target,y,level)
foo_pt16

#patient17
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0)
foo_pt17<-crm(prior,target,y,level)
foo_pt17

#patient18
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0)
foo_pt18<-crm(prior,target,y,level)
foo_pt18

#patient19
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0)
foo_pt19<-crm(prior,target,y,level)
foo_pt19

#patient20
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0)
foo_pt20<-crm(prior,target,y,level)
foo_pt20

#patient21
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0)
foo_pt21<-crm(prior,target,y,level)
foo_pt21

#patient22
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0)
foo_pt22<-crm(prior,target,y,level)
foo_pt22

#patient23
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0)
foo_pt22<-crm(prior,target,y,level)
foo_pt22

#patient24
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0)
foo_pt22<-crm(prior,target,y,level)
foo_pt22

#patient25
level<-c(3,4,2,2,3,3,3,4,3,3,3,3,3,3,3,3,3,3,3,3)
y<-c(0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0)
foo_pt22<-crm(prior,target,y,level)
foo_pt22


#seance_3

prob<- c(0.05,0.1,0.15,0.2,0.3)
df<-data.frame(d1=(sample(c(0,1),replace = TRUE,size = 30,prob=c(1-prob[1],prob[1]))))
for(i in 2:5){
  p<-sample(c(0,1),replace = TRUE,size = 30,prob=c(1-prob[i],prob[i]))
  df<-cbind(df,p)
}
colnames(df)<-c("d1","d2","d3","d4","d5")
df
prior<-c(0.05,0.1,0.2,0.35,0.5)
target<-0.2
level<-c(1)
y<-c(0)
foo<-crm(prior,target,y,level)
foo1<-foo
x<-function(df,level,y,prior,target,foo){
  for (i in 2:25){
    level<-c(level,foo$mtd)
    y<-c(y,df[i,foo$mtd])
    foo<-crm(prior,target,y,level)
  }
}

#courbe de prior et ptox

plot(x=c(1,2,3,4,5),prior,type="l",xlab="dose level" , ylab="prior rate of toxicity", col = "red" ,main="the probability of toxicity at each level " , ylim=c(0,1) )
lines(x=c(1,2,3,4,5),foo$ptox)

#using ggplot2
cols<-c("prior"="purple","ptox1"="blue","ptoxfinal"="green","threshold"="red")
uv<-data.frame(x=c(1,2,3,4,5),y1=prior,y2=foo1$ptox,y3=foo$ptox,y4=c(0.2,0.2,0.2,0.2,0.2))
ggplot(uv,aes(x))+
  geom_line(aes(y=y1,color="prior"))+
  geom_line(aes(y=y2,color="ptox1"))+
  geom_line(aes(y=y3,color="ptoxfinal"))+
  geom_line(aes(y=y4,color="threshold"))+
  labs(x="dose level",y="probability of toxicity",title="the probability of toxicity at each level")+
  scale_colour_manual(values=cols)



#seance 4

logistic<-function(x,a,b){
  return(exp(a+b*x)/(1+exp(a+b*x)))
}

vraisemblance<-function(x=c(1,2,3,3),a=0.2,b,y=c(0,0,1,0)){
  prod=1
  for (i in 1:length(x)){
    if (y[i]==1){
      prod=prod*(logistic(x[i],a,b)^y[i])
    }else{
      prod=prod*(1-logistic(x[i],a,b)^(1-y[i]))
    }
  }
  return (prod)
}



vraisemblance_Opt<-function(x,a,b,y){
  prod=1
  for (i in length(x)){
    prod=prod*(exp(a+b*x[i])/(1+exp(a+b*x[i]))^y[i])*(1-exp(a+b*x[i])/(1+exp(a+b*x[i]))^(1-y[i]))
  }
  return (prod)
}
optimise(vraisemblance_Opt,c(0,1),a=0.2,
         x=c(1,2,3,3),y=c(0,0,1,0),maximum = TRUE)




library(wakefield)

Y<-sample(c(0,1),replace = TRUE,size = 100,prob=c(0.7,0.3))

sex<-as.vector(sex(100))

age<-trunc(rnorm(100,25,10))

tabac<-sample(c(0,1),replace = TRUE,size = 100,prob=c(0.4,0.6))

idE<-c(1:100)

base_tabac<-data.frame(idE,malade=Y,sex,age,tabac)
model_malade<-glm(formula = malade ~ sex+age+tabac ,data=base_tabac,family = binomial)
model_malade$coefficients


  










