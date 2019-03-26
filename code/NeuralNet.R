## URL https://keras.rstudio.com/

library(keras)
#install_keras()

## load data
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y


##

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

dim(x_test)

# turn the predictions into categorical with one-hot encoding
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)


# Create sequential model
model <- keras_model_sequential() 

# Add layers
model %>% 
    layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 10, activation = 'softmax')

# check model summary
summary(model)

# Next, compile the model with appropriate loss function, optimizer, and metrics
model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy'))

# using fit, we can train the model for 30 epochs using batches of 128 images
history <- model %>% fit(
    x_train, y_train, 
    epochs = 30, batch_size = 128, 
    validation_split = 0.2)

# we can plot the loss and accuracry metrics
plot(history)

# Evaluate the model’s performance on the test data:
model %>% evaluate(x_test, y_test)

# gererate predictions
model %>% predict_classes(x_test)


imageModel <- application_inception_resnet_v2()

img <- image_load("~/Desktop/puppy-dog.jpg", target_size = c(782,529))
x <- image_to_array(img)
x <- array_reshape(x, c(1, dim(x)))
x <- imagenet_preprocess_input(x)
preds <- model %>% predict(x)
imagenet_decode_predictions(preds, top = 3)[[1]]