rm(list=ls())
library(mxnet)
library(caret)

input <- read.csv("Train_Digits_20171108.csv")
input$Digit <- input$Digit%%2
input$Digit <- as.factor(input$Digit)

#Split to training and test set
sample <- sample(nrow(input)*0.8)
train <- input[sample,]
test <- input[-sample,]

nzr <- nearZeroVar(train[,-1],saveMetrics=T,freqCut=10000/1,uniqueCut=1/7)


cutvar <- rownames(nzr[nzr$nzv==TRUE,])
var <- setdiff(names(train),cutvar)
train <- train[,var]
test <- test[,var]

# multi-layer perceptron
get_mlp <- function() {
  data <- mx.symbol.Variable('data')
  fc1  <- mx.symbol.FullyConnected(data = data, name='fc1', num_hidden=128)
  act1 <- mx.symbol.Activation(data = fc1, name='relu1', act_type="relu")
  fc2  <- mx.symbol.FullyConnected(data = act1, name = 'fc2', num_hidden = 64)
  act2 <- mx.symbol.Activation(data = fc2, name='relu2', act_type="relu")
  fc3  <- mx.symbol.FullyConnected(data = act2, name='fc3', num_hidden=10)
  mlp  <- mx.symbol.SoftmaxOutput(data = fc3, name = 'softmax')
  mlp
}

get_lenet <- function() {
  data <- mx.symbol.Variable('data')
  # first conv
  conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=20)
  tanh1 <- mx.symbol.Activation(data=conv1, act_type="tanh")
  pool1 <- mx.symbol.Pooling(data=tanh1, pool_type="max",
                             kernel=c(2,2), stride=c(2,2))
  # second conv
  conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=50)
  tanh2 <- mx.symbol.Activation(data=conv2, act_type="tanh")
  pool2 <- mx.symbol.Pooling(data=tanh2, pool_type="max",
                             kernel=c(2,2), stride=c(2,2))
  # first fullc
  flatten <- mx.symbol.Flatten(data=pool2)
  fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)
  tanh3 <- mx.symbol.Activation(data=fc1, act_type="tanh")
  # second fullc
  fc2 <- mx.symbol.FullyConnected(data=tanh3, num_hidden=10)
  # loss
  lenet <- mx.symbol.SoftmaxOutput(data=fc2, name='softmax')
  lenet
}

get_iterator <- function(data_shape) {
  get_iterator_impl <- function(args) {
    data_dir = args$data_dir
    if (!grepl('://', args$data_dir))
      download_(args$data_dir)
    flat <- TRUE
    if (length(data_shape) == 3) flat <- FALSE
    
    train           = mx.io.MNISTIter(
      image       = paste0(data_dir, "train-images-idx3-ubyte"),
      label       = paste0(data_dir, "train-labels-idx1-ubyte"),
      input_shape = data_shape,
      batch_size  = args$batch_size,
      shuffle     = TRUE,
      flat        = flat)
    
    val = mx.io.MNISTIter(
      image       = paste0(data_dir, "t10k-images-idx3-ubyte"),
      label       = paste0(data_dir, "t10k-labels-idx1-ubyte"),
      input_shape = data_shape,
      batch_size  = args$batch_size,
      flat        = flat)
    
    ret = list(train=train, value=val)
  }
  get_iterator_impl
}

parse_args <- function() {
  parser <- ArgumentParser(description='train an image classifer on mnist')
  parser$add_argument('--network', type='character', default='mlp',
                      choices = c('mlp', 'lenet'),
                      help = 'the cnn to use')
  parser$add_argument('--data-dir', type='character', default='mnist/',
                      help='the input data directory')
  parser$add_argument('--gpus', type='character',
                      help='the gpus will be used, e.g "0,1,2,3"')
  parser$add_argument('--batch-size', type='integer', default=128,
                      help='the batch size')
  parser$add_argument('--lr', type='double', default=.05,
                      help='the initial learning rate')
  parser$add_argument('--mom', type='double', default=.9,
                      help='momentum for sgd')
  parser$add_argument('--model-prefix', type='character',
                      help='the prefix of the model to load/save')
  parser$add_argument('--num-round', type='integer', default=10,
                      help='the number of iterations over training data to train the model')
  parser$add_argument('--kv-store', type='character', default='local',
                      help='the kvstore type')
  
  parser$parse_args()
}
args = parse_args()
if (args$network == 'mlp') {
  data_shape <- c(784)
  net <- get_mlp()
} else {
  data_shape <- c(28, 28, 1)
  net <- get_lenet()
}
