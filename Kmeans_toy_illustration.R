# Load libraries
library(ggplot2)

# Generate toy dataset (2D, 10 points)
data <- data.frame(
  x = c(1, 1.5, 3, 5, 3.5, 4.5, 3.5, 8, 9, 8.5),
  y = c(1, 2, 1.5, 3, 4, 5, 3.5, 8, 9, 9.5)
)

# Initial random centroids
initial_centroids <- as.data.frame(matrix(rep(NA, 4), nrow = 2, ncol = 2))
colnames(initial_centroids) <- c('x', 'y')
initial_centroids[1,1] <- 4
initial_centroids[1,2] <- 3
initial_centroids[2,1] <- 2.5
initial_centroids[2,2] <- 5
ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 3) +
  geom_point(data = initial_centroids, aes(x = x, y = y),
             color = "forestgreen", size = 5, shape = 8) +
  theme_bw() +
  theme(legend.position = "none")

# Useful helper function
assign_clusters <- function(data, centroids) {
  distances <- sapply(1:nrow(centroids), function(i) {
    sqrt((data$x - centroids$x[i])^2 + (data$y - centroids$y[i])^2)
  })
  cluster <- apply(distances, 1, which.min)
  return(cluster)
}

# Step 1: Assign points to nearest clusters & visualise

data$cluster <- factor(assign_clusters(data, initial_centroids))

ggplot(data, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = initial_centroids, aes(x = x, y = y),
             color = "forestgreen", size = 5, shape = 8) +
  theme_bw() +
  theme(legend.position = "none")

# Step 2: Compute new centroids & visualise
new_centroids <- aggregate(data[, c("x", "y")], by = list(data$cluster), FUN = mean)
colnames(new_centroids) <- c("cluster", "x", "y")

ggplot(data, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = new_centroids, aes(x = x, y = y),
             color = "forestgreen", size = 5, shape = 8) +
  theme_bw() +
  theme(legend.position = "none")

# Step 3: Update cluster assignment & visualise

data$cluster <- factor(assign_clusters(data, new_centroids[, c("x", "y")]))

ggplot(data, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = new_centroids, aes(x = x, y = y),
             color = "forestgreen", size = 5, shape = 8) +
  theme_bw() +
  theme(legend.position = "none")

# Step 4: Update centroids & visualise

new_centroids <- aggregate(data[, c("x", "y")], by = list(data$cluster), FUN = mean)
colnames(new_centroids) <- c("cluster", "x", "y")

ggplot(data, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = new_centroids, aes(x = x, y = y),
             color = "forestgreen", size = 5, shape = 8) +
  theme_bw() +
  theme(legend.position = "none")

# Alternatively, we can do all of this using the kmeans function!

kmeans_result <- kmeans(x = data[, c(1:2)],
                        centers = 2, # We assume we have 2 clusters
                        iter.max = 10, # We set the max number of iterations to 10
                        nstart = 50) # We use 50 random initialisations

# We can plot the points nad color by cluster assignment
plot(data[, c(1:2)], col = kmeans_result$cluster, pch = 16,
     xlab = 'x', ylab = 'y')

# Let's also add the centroids
points(kmeans_result$centers, pch = 8, col = 'forestgreen')