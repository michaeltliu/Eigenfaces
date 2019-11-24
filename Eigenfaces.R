# --------------------------------

# convert greyscale jpeg image into data matrix
imagedata(myjpg, type="grey")

# ------------------------------------

# convert image data matrix into r*c by 1 vector
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