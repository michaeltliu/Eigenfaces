# --------------------------------

# convert greyscale jpeg image into data matrix
library(jpeg)
library(imager)
library(spatstat)
library(ggfortify)
##read in jpeg
prepath <- "/Users/zepingluo/Documents/Eigenfaces/faceimages/"
filename <- c("IMG_0733.JPG","IMG_0734.JPG","IMG_0735.JPG","IMG_0749.JPG","IMG_0753.JPG","IMG_0755.JPG","IMG_0759.JPG","IMG_0772.JPG")
paths <- c()
images <- c()
vecs <- c()
# bind all vectors into a single r*c by 20ish matrix *vecs*
for(index in 1:8){
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


##code for onvert matrix back to image
image <- im(as.matrix(grayscale))
plot(image)


#dim(gray)
#https://stackoverflow.com/questions/31800687/how-to-get-a-pixel-matrix-from-grayscale-image-in-r/31804561
#https://www.tutorialspoint.com/dip/grayscale_to_rgb_conversion.htm
#matrix <- 1/3*x[,,1]+1/3*x[,,2]+1/3*x[,,3]
#dim(matrix)



# ------------------------------

# find the mean vector then zero out/subtract the mean from 
# all vectors in the matrix. 
mean_vec <- c()
#average face vector
mean_vec <- apply(vecs,1,mean)
dim(mean_vec)
X_mean <- matrix(, nrow = 921600, ncol = 0)
for(i in 1:8){
  X_mean <- cbind(X_mean, mean_vec)
}
dim(X_mean)
#the vecs now is centerd
vecs <- vecs-X_mean


# ----------------------------------

# computer the eigenvectors and eigenvalues of the covariance
# matrix. Use the small rank of the data matrix to simplify
# eigen computations

pca <- prcomp(t(vecs))
plot(pca)
colnames(vecs) <- c(1:8)
#long runtime??
autoplot(pca, data = t(vecs),  loadings = TRUE,loadings.label=TRUE)

# -----------------------------------

# choose top 2 or 3 principal components (for visuals)
proj_matrix <- pca$rotation[,0:2]
dim(proj_matrix)
dim(vecs)

# and choose top 5 principal components (for more accurate analysis)
# and project each original face vector onto these principal components
result <- proj_matrix%*%vecs

# -------------------------------------

# take a new face vector and project onto principal components
# compute new projection point to old projection points
# compare

# ----------------------------------------