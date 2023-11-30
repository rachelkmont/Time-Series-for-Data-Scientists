#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DS5740: Week 10 | EGA Script ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# For more information, check out: https://r-ega.net

# Load packages
library(EGAnet); library(knitr); library(qgraph)
library(ggplot2); library(igraph); library(psych)
library(psychTools); library(ggpubr)

# Set seed for reproducibility
set.seed(1234)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load bfi data
data <- bfi[,1:25]

# Estimate EGA
ega <- EGA(data, plot.EGA = FALSE)

# Estimate CFA
sink <- capture.output(
  cfa <- CFA(ega, data = data, estimator = "WLSMV", plot.CFA = FALSE) # not adjusting properly to fit to slides
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Custom {ggplot2} heatmap function ----
setup_ggplot2_heatmap <- function(
    correlation_matrix, # input for correlation matrix
    type = c("full", "lower", "upper")
    # whether matrix should depict the full, lower,
    # or upper matrix
)
{
  
  # Ensure correlation matrix is a `matrix` object
  corr_mat <- as.matrix(correlation_matrix)
  
  # Determine triangle
  if(type == "lower"){
    corr_mat[upper.tri(corr_mat)] <- NA
  }else if(type == "upper"){
    corr_mat[lower.tri(corr_mat)] <- NA
  }
  
  # Convert to long format
  corr_df <- data.frame(
    Var1 = rep(colnames(corr_mat), each = ncol(corr_mat)),
    Var2 = rep(colnames(corr_mat), times = ncol(corr_mat)),
    Correlation = as.vector(corr_mat)
  )
  
  # Set levels
  corr_df$Var1 <- factor(
    corr_df$Var1, levels = colnames(corr_mat)
  )
  corr_df$Var2 <- factor(
    corr_df$Var2, levels = rev(colnames(corr_mat))
  )
  corr_df$Correlation <- as.numeric(corr_df$Correlation)
  
  # Return data frame for plotting
  return(corr_df)
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load data
load("../data/openness_br_dk.RData")

# Select variables of interest
openness_voi <- openness_br_dk[, grep("O", colnames(openness_br_dk))]

# Compute correlations
openness_corr <- auto.correlate(openness_voi)

# Set up heatmap data frame
openness_df <- setup_ggplot2_heatmap(openness_corr, type = "full")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot correlation matrix
ggplot(
  data = openness_df,
  aes(x = Var1, y = Var2, fill = Correlation)
) +
  geom_tile(color = "black") +
  scale_fill_gradient2(
    low = "#CD533B", mid = "#EAEBED",
    high = "#588B8B", limits = c(-1, 1),
    guide = guide_colorbar(
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5)
  ) +
  labs(title = "Openness to Experience Correlations")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# On the correlation matrix
openness_network <- network.estimation(
  data = openness_corr, n = nrow(openness_voi), model = "glasso"
)

# On the data
openness_network <- network.estimation(openness_voi, model = "glasso")

# Create class to plot
network_class <- list(
  network = openness_network, 
  wc = rep(1, ncol(openness_network))
)
class(network_class) <- "EGA"

# Plot
plot(network_class) + theme(legend.position = "none")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Apply Walktrap to the network
walktrap_wc <- community.detection(
  openness_network, algorithm = "walktrap"
)

# Create class to plot
network_class <- list(
  network = openness_network, 
  wc = walktrap_wc
)
class(network_class) <- "EGA"

# Plot
plot(network_class)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Data frame of Louvain
louvain_df <- data.frame(
  Run_1 = community.detection(openness_network, algorithm = "louvain"),
  Run_2 = community.detection(openness_network, algorithm = "louvain"),
  Run_3 = community.detection(openness_network, algorithm = "louvain")
)

# Print table using {knitr}
knitr::kable(louvain_df, booktabs = TRUE)

# Your results might even differ from the slides given the
# stochastic nature of the algorithm

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Data frame of consensus Louvain
consensus_df <- data.frame(
  Run_1 = community.consensus(openness_network),
  Run_2 = community.consensus(openness_network),
  Run_3 = community.consensus(openness_network)
)

# Print table using {knitr}
knitr::kable(consensus_df, booktabs = TRUE)

# This result, however, should be the same as the slides
# because the most common solution should (hopefully) be truly
# the most common despite the stochastic-ness

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Apply Louvain to the network
louvain_wc <- community.consensus(
  openness_network, algorithm = "Louvain"
)

# Create class to plot
network_class <- list(
  network = openness_network, 
  wc = louvain_wc
)
class(network_class) <- "EGA"

# Plot
plot(network_class)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Apply EGA with Walktrap
openness_ega_walktrap <- EGA(openness_voi)

# Print summary
summary(openness_ega_walktrap)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Apply EGA with Louvain
openness_ega_louvain <- EGA(
  openness_voi, algorithm = "louvain"
)

# Print summary
summary(openness_ega_louvain)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Apply EGA with Walktrap
openness_walktrap_fit <- EGA.fit(openness_voi)

# Print summary
summary(openness_walktrap_fit)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Apply EGA with Louvain
openness_louvain_fit <- EGA.fit(
  openness_voi, algorithm = "louvain"
)

# Print summary
summary(openness_louvain_fit)
