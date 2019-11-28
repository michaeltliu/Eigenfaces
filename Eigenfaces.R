# --------------------------------

# convert greyscale jpeg image into data matrix
library(jpeg)
library(imager)
library(spatstat)
library(ggfortify)
library(ggrepel) 
##read in jpeg
#length is the number of images we used  - called upon multiple times later in the files
length <- 19
prepath <- "/Users/zepingluo/Documents/Eigenfaces/faceimages/"
filename <- c("Danny1.JPG","Danny2.JPG","Danny3.JPG","Danny4.JPG","Jane1.JPG","Jane2.JPG","Jane3.JPG",
              "Jane4.JPG","Jane5.JPG","Andy1.JPG","Andy2.JPG","Andy3.JPG","Andy4.JPG","Andy5.JPG",
              "Jessica1.JPG","Jessica2.JPG","Jessica3.JPG","Jessica4.JPG","Jessica5.JPG")
paths <- c()
images <- c()
vecs <- c()
# bind all vectors into a single r*c by 20ish matrix *vecs*
for(index in 1:length){
  #collect the paths in the vector
  paths <- c(paths,paste(prepath,filename[index],sep=""))
  #read single filename
  im <- load.image(paths[index])
  #convert to grayscale
  grayscale <- grayscale(im,method = "Luma",drop=TRUE)
  #plot it
  plot(grayscale)
  #convert grayscale to vector
  vec <- as.vector(as.matrix(grayscale))
  #collect vectors in vecs
 
  vecs <- cbind(vecs,vec)
}
dim(vecs)


# ------------------------------

# find the mean vector then zero out/subtract the mean from 
# all vectors in the matrix. 
mean_vec <- c()
#average face vector
mean_vec <- apply(vecs,1,mean)
dim(mean_vec)
X_mean <- matrix(, nrow = 921600, ncol = 0)
for(i in 1:length){
  X_mean <- cbind(X_mean, mean_vec)
}
dim(X_mean)
#the vecs now is centerd
vecs <- vecs-X_mean


# ----------------------------------

# computer the eigenvectors and eigenvalues of the covariance
# matrix. Use the small rank of the data matrix to simplify
# eigen computations


# -----------------------------------

# choose top 2 or 3 principal components (for visuals)
eig <- eigen(t(vecs)%*%vecs)
eigenvectors <- matrix(, nrow = 921600, ncol = 0)
for(j in 1:length){
  #from https://medium.com/@devalshah1619/face-recognition-using-eigenfaces-technique-f221d505d4f7
  #using data matrix times eigen of (transpose data) * (data)
  eigenvector <- vecs%*%eig$vectors[,j]
  eigenvectors <- cbind(eigenvectors,eigenvector)
}
dim(eigenvectors)
#maybe we need to rank eigenvectors first?
proj_matrix <- eigenvectors[,0:2]
dim(proj_matrix)
dim(vecs)

# and choose top 5 principal components (for more accurate analysis)
# and project each original face vector onto these principal components
result <- t(proj_matrix)%*%vecs
dim(result)

#plot

table <- as.table(t(result))
tag <- filename
table <- cbind(table,tag)
colnames(table) <- c("pc1","pc2","label")
p <- ggplot(data=table,aes(x=pc1,y=pc2,label=label))
p+geom_point()+geom_text_repel()





#--------------------------
#visualzie eigen faces
par(mfrow=c(2,2))
for(e in 1:4){
  image_matrix <- matrix(eigenvectors[,e],nrow = 960)
  dim(image_matrix)
  image <- im(image_matrix)
  plot(image,main=paste(paste("top ",e),"eigen faces"))
}









#----------------------------
# -------------------------------------

# take a new face vector and project onto principal components
# compute new projection point to old projection points
# compare

# ----------------------------------------