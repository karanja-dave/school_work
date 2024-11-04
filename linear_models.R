ads<-read.csv("/home/dave/Project/data/ads.csv",header = F,col.names = c('a_e','w_s'))
ads
ad<-data.frame(ads);ad
ad
library('ggplot2')
library("dplyr")
ad %>% ggplot(aes(x=a_e,y=w_s,col=w_s))+
  geom_point()+ 
  ggtitle("Graph of Sales vs expenditure") +
  labs(x="Advertising Expenditure",y="Weekly Sales")+ 
  geom_smooth(method="lm")

ad %>% lm(w_s~a_e,data=.)

adi<-ad %>% mutate(e=w_s-343.70-3.221*ad$a_e) %>% select(a_e,e)
adi
adi %>% ggplot(aes(x=a_e,y=e,col=e))+
  geom_point()+
  ggtitle("Graph of Residuals vs Advertising Cost")+
  labs(x="Advertising Expenditure",y="Residuals")+
  geom_smooth(method='lm') 
  
ad %>% lm(y~x,data=.)

(merged<-cbind(ad,adi[,2]))
merged %>% ggplot(aes(col=a_e))+
  geom_point(aes(x=a_e,y=w_s))+
  geom_point(aes(x=a_e,y=adi[,2]))


math_place<-data.frame(x=c(50,35,35,40,55,65,35,60,90,35,90,80,60,60,60,40,55,50,65,50),y=c(53,41,61,56,68,36,11,70,79,59,54,91,48,71,71,47,53,68,57,79))
math_place

math_place %>% ggplot(aes(x=y,y=x))+
  geom_point() +
  scale_x_continuous(breaks = seq(0,90,10))+
  scale_y_continuous(breaks=seq(0,90,5)) +
  geom_smooth(method = 'lm')


;math_place %>% summarise(sum_x=sum(x),
                         sum_y=sum(y),
                         mean_x=mean(x),
                         mean_y=mean(y))
math_place %>% lm(y~x,data=.)



x<-c(2.1,2.4,2.5,3.20,3.60,3.80,4.1,4.2,4.5,5)
y<-c(2.18,2.06,2.54,2.61,3.67,3.25,4.02,3.71,4.38,4.45)
model<-lm(y~x)
summ_model<-summary(model)
summ_model$coefficients[2,"Std. Error"]

