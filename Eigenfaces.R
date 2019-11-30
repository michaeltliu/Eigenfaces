
library(jpeg)
library(imager)
library(spatstat)
library(ggfortify)
library(ggrepel) 
library(tidyverse)
# --------------------------------
##read in jpeg and convert each single jpeg image into grayscale image and then into a vector.Collect them in a matrix.

#length is the number of images used
#called upon multiple times later in the files
length <- 16
#path of image folder in your own local directory
prepath <- "/Users/zepingluo/Documents/Eigenfaces/faceimages/"


filename <- c("Danny1.JPG","Danny2.JPG","Danny3.JPG","Danny4.JPG","Jane1.JPG","Jane2.JPG","Jane3.JPG",
              "Jane4.JPG","Andy1.JPG","Andy2.JPG","Andy3.JPG","Andy4.JPG",
              "Jessica1.JPG","Jessica2.JPG","Jessica3.JPG","Jessica4.JPG")

paths <- c()
images <- c()
vecs <- c()

for(index in 1:length){
  #collect file path in the paths
  paths <- c(paths,paste(prepath,filename[index],sep=""))
  #read in image object from path in paths
  im <- load.image(paths[index])
  #convert the image object into grayscale
  grayscale <- grayscale(im,method = "Luma",drop=TRUE)
  #(Optional) plot it
    #  plot(grayscale)
  
  #convert grayscale image to a vector 
  vec <- as.vector(as.matrix(grayscale))
  #collect vectors in vecs
  vecs <- cbind(vecs,vec)
}

#vecs is now a (960*960) by (length) matrix
#vecs collect images as giant vectors 
dim(vecs)


# ------------------------------
##Center the data


mean_vec <- c()
#average face vector
mean_vec <- apply(vecs,1,mean)
dim(mean_vec)
X_mean <- matrix(, nrow = 921600, ncol = 0)
for(i in 1:length){
  X_mean <- cbind(X_mean, mean_vec)
}
dim(X_mean)
#subtract the mean from data
#the vecs now is centerd
vecs <- vecs-X_mean


# ----------------------------------
## computer the eigenvectors and eigenvalues of the covariance
## matrix. Use the small rank of the data matrix to simplify
## eigen computations

#from https://medium.com/@devalshah1619/face-recognition-using-eigenfaces-technique-f221d505d4f7
#algorithm to address computational limitation
#to get eigen vectors of A*t(A): A * eigen vectors of t(A)*A
eig <- eigen(t(vecs)%*%vecs)
eigenvectors <- matrix(, nrow = 921600, ncol = 0)
for(j in 1:length){
 
  eigenvector <- vecs%*%eig$vectors[,j]
  eigenvectors <- cbind(eigenvectors,eigenvector)
}
dim(eigenvectors)

#maybe we need to rank eigenvectors first?

#projection matrix of top k eigen vectors
k <- 2
proj_matrix <- eigenvectors[,0:k]
dim(proj_matrix)
dim(vecs)

result <- t(proj_matrix)%*%vecs
dim(result)

#plot for case of k =2 

table <- as.data.frame(t(result))
table <- cbind(table,filename)
p <- ggplot(data=table,aes(x=V1,y=V2, label=filename))
p+geom_point()+geom_text_repel()+labs(x="PC1",y="PC2")


# Choose top k principal components (for more accurate analysis)
# and project each original face vector onto these principal components
k <- 6
proj_matrix <- eigenvectors[,0:k]
dim(proj_matrix)
dim(vecs)

result <- t(proj_matrix)%*%vecs
dim(result)
table <- as.data.frame(t(result))
table <- cbind(table,filename)
coord_train <- table
#--------------------------
##visualzie eigen faces

#set up frame
par(mfrow=c(2,3))
for(e in 1:6){
  #convert eigen vector back into matrix
  image_matrix <- matrix(eigenvectors[,e],nrow = 960)
  #convert matrix back into image
  image <- im(image_matrix)
  plot(image,main=paste(paste("top ",e),"eigen faces"))
}

#------------------------------
##loading testing images
test_path <- "/Users/zepingluo/Documents/Eigenfaces/testingimages/"
test_filename <- c("Danny.JPG","Andy.JPG","Jane.JPG","Jessica.JPG")
test_paths <- c()
test_images <- c()
test_vecs <- c()
j <- 4

for(index1 in 1:j){
  #collect file path in the paths
  test_paths <- c(test_paths,paste(test_path,test_filename[index1],sep=""))
  #read in image object from path in paths
  im <- load.image(test_paths[index1])
  #convert the image object into grayscale
  grayscale <- grayscale(im,method = "Luma",drop=TRUE)
  #(Optional) plot it
  #  plot(grayscale)
  
  #convert grayscale image to a vector 
  test_vec <- as.vector(as.matrix(grayscale))
  #center the test vec
  test_vec <- test_vec-mean_vec
  #collect vectors in vecs
  test_vecs <- cbind(test_vecs,test_vec)
}
dim(test_vecs)

# -------------------------------------
## project test images onto principal components
## compute new projection point to old projection points
## compare Euclidean distance of coordinates with respect to top k eigen bases
result1 <- t(proj_matrix)%*%test_vecs
coord_test <- as.data.frame(t(result1))
coord_test <- cbind(coord_test,test_filename)
for(index3 in 1:4){
  distances <- c()
  for(index2 in 1:length){
    distance <- dist(rbind(result[,index2],result1[,index3]))
    distance <- as.numeric(distance)
    distances <- c(distances,distance)
  }
  distances <- signif(distances,5)
  data_to_join <- as.data.frame(cbind(distances,filename))
  coord_train <- left_join(coord_train,data_to_join)
  colnames(coord_train)[k+index3+1] <- test_filename[index3]
}
#now coord_train contains the Euclidena distance of all the images in test images to original training images
coord_train
#plot several
p <- ggplot(data=coord_train,aes(filename,Danny.JPG))
p+geom_point()+ theme(axis.text.x = element_text(angle = 60, hjust = 1))

p <- ggplot(data=coord_train,aes(filename,Andy.JPG))
p+geom_point()+ theme(axis.text.x = element_text(angle = 60, hjust = 1))





# ----------------------------------------