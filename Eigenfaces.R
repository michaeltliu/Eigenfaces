# --------------------------------

# convert greyscale jpeg image into data matrix
library(jpeg)
library(imager)
library(spatstat)
##read in jpeg
im <- load.image("/Users/zepingluo/Documents/Eigenfaces/eigen_face1.JPG")
plot(im)
##convert to grayscale
gray <- grayscale(im,method = "Luma",drop=TRUE)
plot(gray)

##convert gray image to matrix
gray_matrix <- as.matrix(gray)
dim(gray_matrix)
##convert matrix back to image
image <- im(gray_matrix)
plot(image)


#dim(gray)
#https://stackoverflow.com/questions/31800687/how-to-get-a-pixel-matrix-from-grayscale-image-in-r/31804561
#https://www.tutorialspoint.com/dip/grayscale_to_rgb_conversion.htm
#matrix <- 1/3*x[,,1]+1/3*x[,,2]+1/3*x[,,3]
#dim(matrix)

# ------------------------------------

# convert image data matrix into r*c by 1 vector

#it is taken col by col from i=1 to n
vector <- as.vector(gray_matrix)
dim(vector)
matrix1 <- cbind(vector,vector)
dim(matrix1)
matrix2 <- cbind(matrix1,vector)
dim(matrix2)
# bind all vectors into a single r*c by 20ish matrix

# ------------------------------

# find the mean vector then zero out/subtract the mean from 
# all vectors in the matrix. 

# ----------------------------------

# computer the eigenvectors and eigenvalues of the covariance
# matrix. Use the small rank of the data matrix to simplify
# eigen computations

# -----------------------------------

# choose top 2 or 3 principal components (for visuals)
# and choose top 5 principal components (for more accurate analysis)
# and project each original face vector onto these principal components

# -------------------------------------

# take a new face vector and project onto principal components
# compute new projection point to old projection points
# compare

# ----------------------------------------