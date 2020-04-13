## ------------------------------------------------------------------------
library(rForest)

# Data import and sample split
data(iris)
data <- iris
set.seed(1337)
rows <- sample.int(nrow(data), size = floor(0.7*nrow(data)), replace = FALSE)
trainData <- data[rows,]
testData <- data[-rows,]

## ----gini----------------------------------------------------------------
purity <- gini(trainData, "Species")
purity

## ----setSplit------------------------------------------------------------
# categorial
groups <- setSplit(trainData, "Species", "setosa")
unique(groups$left$Species)
unique(groups$right$Species)
# numeric
groups <- setSplit(trainData, "Sepal.Width", 3.1)
max(groups$left$Sepal.Width)
min(groups$right$Sepal.Width)

## ----gain----------------------------------------------------------------
gain(groups$left, groups$right, "Species", purity)

## ----rule----------------------------------------------------------------
# Use all variables other than "Species" as predictors
response <- "Species"
predictors <- setdiff(colnames(iris), "Species")
bestRule <- rule(trainData, response, predictors)
bestRule

## ----setNodeExec---------------------------------------------------------
# Create a new split using best possible split rule from above
groups <- setSplit(trainData, bestRule$var, bestRule$val)
node <- setNode(bestRule, groups$left, groups$right, "Species")
str(node)

## ----setLeafExec---------------------------------------------------------
node$left <- setLeaf(node$left, response)
node$right <- setLeaf(node$right, response)
# Look at node's structure again
str(node)

## ----grow----------------------------------------------------------------
# Function definition within tree()
grow <- function(group, depth = 0) {
  if(nrow(group) < minsplit) {
    setLeaf(group, response, depth)
  }
  # Get best binary split rule
  rule <- rule(group, response, predictors)
  # Do not split, if split does not yield info. gain or maxdepth reached
  if(rule$gain == 0 || depth > maxdepth) {
    setLeaf(group, response, depth)
  } else {
    split <- setSplit(group, rule$var, rule$val)
    # Do not split, if not too few obs. or if resulting leafs/nodes too small
    if(nrow(split$left) < minbucket || nrow(split$right) < minbucket) {
      setLeaf(group, response, depth)
    } else {
      # Recursively process left/right groups resulting from previous split
      left <- grow(split$left, depth = depth + 1)
      right <- grow(split$right, depth = depth + 1)
      # Combine subtrees into node
      setNode(rule, left, right, response, depth = depth)
    }
  }
}

## ----growExec------------------------------------------------------------
# Manually define some option parameters of tree()
minsplit <- 15
minbucket <- 5
# Only grow 2 levels so vignette is not overloaded with output
maxdepth <- 2
  
# Grow tree, show its structure
irisTree <- list(root = grow(trainData))
str(irisTree)

## ----traverse.nodeExec---------------------------------------------------
# The argument 'ans' is never passed to manual function calls (recursives only)
traverse(irisTree$root)

## ----treeExec------------------------------------------------------------
# Generate a fitted tree model using all 4 predictors
fit <- tree(Species ~ .,
            data = trainData,
            m = 4,
            minsplit = 15,
            minbucket = 5,
            maxdepth = 10)

## ----importance.treeExec-------------------------------------------------
importance(fit, predictors)

## ----print.leafExec------------------------------------------------------
# root's left branch leads to a leaf
class(fit$root$left)
fit$root$left

## ----print.nodeExec------------------------------------------------------
# root's right branch leads to another node
class(fit$root$right)
fit$root$right

## ----print.treeExec------------------------------------------------------
fit

## ----singlePred----------------------------------------------------------
# Recursive tree walk to generate single prediction
singlePred <- function(object, data) {
  # Stop recursion if leaf is reached
  if(class(object) == "leaf") {
    object$prediction
  } else {
    splitVar <- object$rule$var
    splitVal <- object$rule$val
    # Construct dynamic relation string for numeric/categorial split cases
    if(object$rule$type == "numeric") {
      rel <- paste(data[[splitVar]],
                   "<",
                   splitVal, sep = "")
    } else {
      rel <- paste("\"", data[[splitVar]], "\"",
                   "!=",
                   "\"", splitVal, "\"", sep = "")
    }
    # Turn the character 'rel' into an expression and evaluate it: Return Bool
    isLeft <- eval(parse(text = rel))
    # Recursively walk through tree until leaf is reached
    if(isLeft) {
      singlePred(object$left, data)
    } else {
      singlePred(object$right, data)
    }
  }
}

## ----singlePredExec------------------------------------------------------
singlePred(fit$root, testData[42,])

## ----predict.treeExec----------------------------------------------------
predict(fit, testData)

## ----summary.treeExec----------------------------------------------------
summary(fit, testData)

## ----bootstrapExec-------------------------------------------------------
set.seed(1337)
for(i in 1:5) {
  bs <- bootstrap(trainData)
  tbl <- table(bs$Species)
  if(i == 1) cat("Class distribution of",parenthesize(tbl, type = "names"),"\n")
  cat(parenthesize(tbl, type = "counts"), "\n", sep = "")
}

## ----forestExec----------------------------------------------------------
# Create Random Forest with m = 3 < 4 = p
set.seed(1337)
start <- Sys.time()
forestFit <- forest(Species ~ .,
                    data = trainData,
                    m = 3,
                    b = 500,
                    minsplit = 15,
                    minbucket = 5,
                    maxdepth = 10,
                    bsinclude = FALSE)
end <- Sys.time()
end - start

## ------------------------------------------------------------------------
forestFit$ensemble[[1]]
forestFit$ensemble[[2]]

## ----predict.ensembleExec------------------------------------------------
# Aggregate/modal prediction
predMode <- predict(forestFit, testData, type = "aggr")
predMode
# Prediction probabilites
predProbs <- predict(forestFit, testData, type = "prob")
predProbs

## ------------------------------------------------------------------------
# High variance prediction
predProbs[20,]
# Low variance prediction
predProbs[23,]

## ------------------------------------------------------------------------
# Single Tree
importance(fit, predictors)
# Random Forest
importance(forestFit, predictors)

## ----print.forestExec----------------------------------------------------
print(forestFit)

## ----summary.forestExec--------------------------------------------------
summary(forestFit, testData)

