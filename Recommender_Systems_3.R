#installing the required packages 
install.packages("recommenderlab")
install.packages("proxy")
library("recommenderlab");
library("proxy")

#Creating affinity Matrix: 

m1<- matrix(sample(c(NA,0:5),250000, replace=TRUE, prob=c(0.25,0.10,0.10,0.15,0.20,0.10,0.10)), 
            nrow=500, ncol=500, dimnames = list(
              user=paste('U', 1:500, sep=''),
              item=paste('I', 1:500, sep='')    
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
e <- evaluationScheme(r[1:500], method="split", train=0.9,given=340);
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


#       RMSE      MSE      MAE
#UBCF   1.547359 2.394320 1.281912
#IBCF   1.570394 2.466138 1.294071
#Hybrid 1.543242 2.381595 1.281164

#Hence we are going to user HYBRID(50% ubcf+50% ibcf) as our method for new user
#Preding best 6 items for the user 5 
model=HybridRecommender(Recommender(r[1:500], method = "UBCF"),
                        Recommender(r[1:500], method = "IBCF"),
                        weights = c(.5, .5),param=list(normalize = "Z-score",method="cosine",nn=5, minRating=1))

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
#[1] "I481" "I609" "I831" "I265" "I214" "I433"

#List of best 6 items: 
#1)I481
#2)I609
#3)I831
#4)I265
#5)I214
#6)I433