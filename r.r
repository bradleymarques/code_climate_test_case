# Author:     Bradley R C Marques
# Start Date: Tuesday June 30, 2015
# File:       BarloworldRecommender.R
#
# Description:  A simple recommender system for recommending products to customers.
#       Uses the Barloworld data of Plascon paint sales.
#

# Initial setup
rm(list = ls())
gc()
setwd("C:/Users/Xaing/Desktop/BarloworldRecommender/")
library(dplyr)
library(recommenderlab)
library(reshape2)     # Used for acast later (see STEP 3)
library(ggplot2)

CUSTOMER_MIN_PRODUCTS = 50
PRODUCT_MIN_CUSTOMERS = 100

# ========== STEP 1: Data Preparation ==========
# Load the required data
load("Data/RecommenderData.RData")

# ========== STEP 1.1 APPLY NECESSARY FILTERS TO THE DATA ==========

# Remove NAs
salesHistory <- filter(salesHistory, !is.na(QuantityOrdered), !is.na(SellingPrice))
# Remove negatives and zeros
salesHistory <- filter(salesHistory, QuantityOrdered > 0, SellingPrice > 0)


# CHOOSE ONLY PRODUCTS THAT HAVE BEEN PURCHASED MORE THAN PRODUCT_MIN_CUSTOMERS TIMES
validProducts <- filter(summarise(group_by(salesHistory, ProductId),
                  DistinctCustomers = n_distinct(CustomerCode)),
            DistinctCustomers > PRODUCT_MIN_CUSTOMERS)
print(paste("Number of Valid Products =", nrow(validProducts)))
validSales <- filter(salesHistory, ProductId %in% validProducts$ProductId)

# CHOOSE ONLY CUSTOMERS THAT HAVE PURCHASED OVER CUSTOMER_MIN_PRODUCTS PRODUCTS
validCustomers <- filter(summarise(group_by(validSales, CustomerCode),
                   DistinctProducts = n_distinct(ProductId)),
             DistinctProducts > CUSTOMER_MIN_PRODUCTS)
print(paste("Number of Valid Customers =", nrow(validCustomers)))

validSales <- filter(validSales, CustomerCode %in% validCustomers$CustomerCode)

validCustomers <- arrange(filter(summarise(group_by(validSales, CustomerCode),
                   DistinctProducts = n_distinct(ProductId)),
             DistinctProducts > CUSTOMER_MIN_PRODUCTS), (DistinctProducts))



# Aggregate the sales data
aggregatedSales <- summarise(group_by(validSales, ProductId, CustomerCode),
               TotalQuantity = sum(QuantityOrdered),
               TotalPrice = sum(SellingPrice))

aggregatedSales <- ungroup(aggregatedSales)

# ========== STEP 2: Define a "Rating" ==========
aggregatedSales$Rating <- aggregatedSales$TotalPrice
ratingData <- dplyr::select(aggregatedSales, ProductId, CustomerCode, Rating)

# Scale the rating to a number between 1 and 100
ratingData$Rating = scale(ratingData$Rating, center = FALSE, scale = TRUE) * 100
# Take the log of it
ratingData$Rating = log(ratingData$Rating + 1) # +1 to prevent negatives

# Let's look at how many people and items we have
print(paste("Number of people (customers) =", length(unique(ratingData$CustomerCode))))
print(paste("Number of items (products) =", length(unique(ratingData$ProductId))))

# ========== STEP 3: Convert data into something that recommenderlab can use ==========

recommenderMatrix = acast(ratingData, CustomerCode~ProductId, value.var="Rating", drop=FALSE, fill=0)
recommenderMatrix <- as(recommenderMatrix, "dgCMatrix") # First, convert to sparse matrix
recommenderMatrix <- new("realRatingMatrix", data = recommenderMatrix) # Then make into realRatingMatrix

recommenderMatrix
save(recommenderMatrix, file = "Data/RecommenderMatrix.RData")

# ========== STEP 4: Some visualisations ==========

image(recommenderMatrix)

qplot(getRatings(recommenderMatrix), binwidth = 0.2, main = "Histogram of 'Ratings'", xlab = "Rating", ylab = "Frequency")
qplot(colMeans(recommenderMatrix), binwidth = 0.1, main = "Distribution of mean Product Ratings", xlab="Mean Rating", ylab="# of Products to get this Rating")

# ========== STEP 5: Build a recommender testing scheme ==========

maxGiven = min(rowCounts(recommenderMatrix))
print(paste("Max allowable given = ", maxGiven))

PRODUCTS_GIVEN = 40 # must be smaller than maxGiven
GOOD_RATING = 1.5

scheme <- evaluationScheme(data = recommenderMatrix,
               method = "cross-validation",
               train = 0.9,
               k = 10,
               given = PRODUCTS_GIVEN,
               goodRating = GOOD_RATING)

# Define alogirithms to test
algorithms <- list(
  "Random" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "Popular" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "UBCF" = list(name="UBCF", param=list(normalize = "Z-score", method="Cosine", nn=50, minRating = GOOD_RATING)),
  "IBCF" = list(name="IBCF", param=list(normalize = "Z-score"))
)

# ==================== 10: RUN THE ALGORITHMS TO PREDICT NEXT N GAMES ===================
# This can also take a very long time
results <- evaluate(scheme, algorithms, n=c(1, 2, 3, 5, 10))

# ==================== 11: PLOT RESULTS ===================
# Draw ROC (Receiver operating characteristic) curve
plot(results, annotate = 1:4, legend="topright")

# See precision / recall
plot(results, "prec/rec", annotate=c(1,2,3,4), ylim=range(0,1), xlim = range(0,0.35), legend="topright")

# ==================== 12: SOME EXAMPLES ====================
# Produce all recommendations

myRecommender <- Recommender(getData(scheme, "train"), method = "UBCF", param=list(normalize = "Z-score", method="Cosine", nn=50, minRating=GOOD_RATING))
topN = 10
recommendations <- predict(myRecommender, getData(scheme, "known"), n = topN, type="topNList")
recommendations

accuracy <- calcPredictionAccuracy(recommendations, getData(scheme, "unknown"), given = PRODUCTS_GIVEN, goodRating = GOOD_RATING)
accuracy[["precision"]]  * 100 # Precision - The probability that a recommended game will have a high rating by this user
accuracy[["TPR"]] * 100 # Recall - The probability that a game that the player would like is recommended
accuracy[["FPR"]] * 100 # Fallout - The probability that a game that the player doens't like is recommended



# Find 10 biggest customers
biggestCustomers <- arrange(summarise(group_by(salesHistory, CustomerCode), NumberOfPurchases = n(), TotalSpent = sum(SellingPrice)), desc(TotalSpent))
biggestCustomers <- left_join(biggestCustomers, customers, by = "CustomerCode")
biggestCustomers <- head(biggestCustomers, 10)
biggestCustomers <- dplyr::select(biggestCustomers, CustomerCode, CustomerDescription)

bigCustCodes <- as.character(biggestCustomers$CustomerCode)
bigCustData <- recommenderMatrix[bigCustCodes]

recommendations2 <- predict(myRecommender, bigCustData, n = topN, type="topNList")

# Convert recommendations to a dataFrame
recommendations2 <- as(recommendations2, "list") # convert from topNList to List
recommendations2 <- do.call(rbind.data.frame, recommendations2)
colnames(recommendations2) = sapply(seq(1, topN), function(x) { paste("Recommendation ", x) })



# Format for use
recommendations3 <- data.frame()
for (row in 1:10)
{
  for (col in 1:10)
  {
    idFact = recommendations2[row, col]
    idNum = as.numeric(as.character(idFact))
    print(idNum)
    pName <- head(filter(products, ProductId == idNum), 1)$ProductDescription
    recommendations3[row, col] = pName
  }
}
rownames(recommendations3) <- rownames(recommendations2)
colnames(recommendations3) <- colnames(recommendations2)

recommendationsPretty <- biggestCustomers
recommendationsPretty <- cbind(recommendationsPretty, recommendations3)

save(recommendationsPretty, file = "Data/RecommendationsPretty.RData")
write.csv(recommendationsPretty, file = "Data/RecommendationsPretty.csv")





