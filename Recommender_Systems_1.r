#installing the required packages 
install.packages("recommenderlab")
install.packages("proxy")
library("recommenderlab");
library("proxy")

#Creating affinity Matrix: 

m1<- matrix(sample(c(NA,0:5),1000000, replace=TRUE, prob=c(0.50,0.30,0.15,0.02,0.01,0.01,0.01)), 
            nrow=1000, ncol=1000, dimnames = list(
              user=paste('U', 1:1000, sep=''),
              item=paste('I', 1:1000, sep='')    
            ))


m1
#Turing it in RealratingMatrix
r<- as(m1, "realRatingMatrix")
r

## get some information
#dimnames(r)
#rowCounts(r)
#colCounts(r)
#rowMeans(r)


## histogram of ratings
#hist(getRatings(r), breaks="FD")

## inspect a subset
image(r)

# create evaluation scheme splitting taking 90% of the date for training and leaving 10% for validation or test
e <- evaluationScheme(r[1:1000], method="split", train=0.9,given=400);
# creation of recommender model based on ubcf
Rec.ubcf <- Recommender(getData(e, "train"), "UBCF")
# creation of recommender model based on ibcf for comparison
Rec.ibcf <- Recommender(getData(e, "train"), "IBCF")
# creation of recommender model based on Hybrid for comparison
Rec.hybrid<-HybridRecommender(Recommender(getData(e, "train"), method = "UBCF"),
                              Recommender(getData(e, "train"), method = "IBCF"),
                              weights = c(.5, .5))
# making predictions on the test data set
p.ubcf <- predict(Rec.ubcf, getData(e, "known"), type="ratings")
# making predictions on the test data set
p.ibcf <- predict(Rec.ibcf, getData(e, "known"), type="ratings")
# making predictions on the test data set
p.hybrid <- predict(Rec.hybrid, getData(e, "known"), type="ratings")
# obtaining the error metrics for both approaches and comparing them
error.ubcf<-calcPredictionAccuracy(p.ubcf , getData(e, "unknown"))
error.ibcf<-calcPredictionAccuracy(p.ibcf, getData(e, "unknown"))
error.hybrid<-calcPredictionAccuracy(p.hybrid, getData(e, "unknown"))
error <- rbind(error.ubcf,error.ibcf,error.hybrid)
rownames(error) <- c("UBCF","IBCF","Hybrid")
error


# Reulst 
#RMSE      MSE       MAE
#UBCF   1.062457 1.128815 0.7472459
#IBCF   1.102898 1.216383 0.7666330
#Hybrid 1.070036 1.144977 0.7503248

#Hence we are going to user UBCF as our method for new user
#Preding best 6 items for the user 5 
model=Recommender(r[1:1000],method="UBCF",param=list(normalize = "Z-score",method="cosine",nn=5, minRating=1))

# recommended top 5 items for user U5
Prediction_U1<- predict(model, r["U5",], n=6)
# to display them
#x=as(Prediction_U1, "matrix")
# to obtain the top 3
#Prediction_U1_top3 <- bestN(x, n = 6)
# to display them
as(Prediction_U1, "list")

#Result

#$U5
#[1] "I736" "I256" "I507" "I936" "I486" "I275"

#List of best 6 items: 
#1)I736
#2)I256
#3)I507
#4)I936
#5)I486
#6)I275