prior<-c(0.05,0.10,0.20,0.35,0.50,0.70)
target<-0.2
level <- c(3,4,4,3,3,4,3,2,2,2)
y <- c(0,0,1,0,0,1,1,0,0,0)
foo <-crm(prior , target , y , level)
ptox <- foo$ptox 


prior <- c(0.05, 0.10 , 0.20 , 0.35 , 0.50 , 0.70)
target <- 0.2
level <- c(3)
y <- c(0)
foo <- crm(prior , target , y , level)



#matrice df 
prob<- c(0.05,0.1,0.15,0.2,0.3)
df<-data.frame(d1=(sample(c(0,1),replace = TRUE,size = 30,prob=c(1-prob[1],prob[1]))))
for(i in 2:5){
  p<-sample(c(0,1),replace = TRUE,size = 30,prob=c(1-prob[i],prob[i]))
  df<-cbind(df,p)
}
colnames(df)<-c("d1","d2","d3","d4","d5")
df

#function 

test <- function (n,prob,prior,target)  {
   
   df<-data.frame(d1=(sample(c(0,1),replace = TRUE,size = n,prob=c(1-prob[1],prob[1]))))
    for(i in 2:5){
     p<-sample(c(0,1),replace = TRUE,size = n,prob=c(1-prob[i],prob[i]))
     df<-cbind(df,p)
   }
   colnames(df)<-c("d1","d2","d3","d4","d5")
   
    
    level<-c(1)
    y<-c(0)
    foo<-crm(prior,target,y,level)
    foo1<-foo
    for (i in 2:n){
       level<-c(level,foo$mtd)
       y<-c(y,df[i,foo$mtd])
       foo<-crm(prior,target,y,level)
    }
    return(paste("the required dose is " ,foo$mtd,sep = " "))
}
prob<- c(0.05,0.1,0.15,0.2,0.3)
prior<-c(0.05,0.1,0.2,0.35,0.5)
target<-0.2

test1 <- test(30,prob,prior,target)



#courbe
plot(x=c(1,2,3,4,5), prior,type = "l" , 
     xlab = "dose level ", ylab = "rate of toxity " , 
     col = "red"
     ,main = "the probablilty of toxi at each dose level ",
     ylim = c(0,1)
     ) 
lines(x=c(1,2,3,4,5),foo1$ptox,col = "blue")








