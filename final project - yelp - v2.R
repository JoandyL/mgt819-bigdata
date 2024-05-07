##Code for Final Project

##############
#Classification Trees 
##############

###loading u p some packages
library(tree)
#install.packages("jsonlite")
library(jsonlite)
#install.packages("tidyr")
library(tidyr)
#install.packages("dplyr")
library(dplyr)

###Load JSON into a dataframe
yelpDS <- stream_in(file("yelp_dataset/yelp_academic_dataset_business.json"))
View(yelpDS)
names(yelpDS)

yelpDS_full <- unnest(yelpDS, attributes) 
#names(yelpDS_full)

##Data Cleaning
##cleaning up data
yelpDS_full <- select(yelpDS_full, stars, NoiseLevel, state, review_count, RestaurantsTakeOut, RestaurantsDelivery, RestaurantsPriceRange2, latitude, longitude)
#yelpDS_full <- na.omit(yelpDS_full)
variable_list <- c( "state", "RestaurantsTakeOut", "RestaurantsDelivery", "RestaurantsPriceRange2" )

for (variable in variable_list) {
  yelpDS_full[[variable]] <- factor(yelpDS_full[[variable]])
  nlevels(yelpDS_full[[variable]])
}
for (variable in variable_list) {
  print(variable)
  print(nlevels(yelpDS_full[[variable]]))
  print("next")
}

##creating training and test data
set.seed(2) #set the seed for the random number generator so that you get the same samples every time in R.
trainindex = sample(nrow(yelpDS_full),nrow(yelpDS_full)*.8)
train = yelpDS_full[trainindex,]
test = yelpDS_full[-trainindex,]


#Fit a regression tree to the training set.
starstree = tree(stars ~   . , data = train)
summary(starstree)

#The full tree has 5 terminal nodes. Use the cv.tree() function to
#decide how many terminal leaf nodes to prune off. The function uses
#k-fold cross validation on the training dataset to determine the
#optimal level of pruning. 


#In this case, the full unpruned tree is
#actually best (lowest cross validation RMS error). 
cv = cv.tree(starstree)
names(cv)
#"size"   "dev"    "k"      "method"

num_nodes = cv$size
RMS_error = cv$dev
plot(num_nodes, RMS_error)
title('Cross validated RMS error as function of #terminal nodes')

#Use the predict() function to fit loan rates to the observations in the validation set.
#Then use this to calculate the validation RMS error.
pred = predict(starstree, new = test)
validRSS = sum( (test$stars - pred)^2 )
validRMS = sqrt( validRSS/nrow(test) )
validRMS #Answer 0.9899 "average magnitude of error", effectively telling you the average size of regression errors.


pred=predict(starstree, train)
Actual.class = train$Status
table(Actual.class, Tree.pred)
overall.error = sum(Actual.class != pred)/1224
overall.error #0.3480392

#Plot the tree.
dev.new()
plot(starstree, type = "uniform")
text(starstree, pretty = 0)
