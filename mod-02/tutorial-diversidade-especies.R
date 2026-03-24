# Install and load vegan
install.packages("vegan")
library(vegan)

# Load example data (Dutch meadow vegetation)
data(dune)
# View structure
head(dune)
#Alpha diversity refers to diversity within a single site. 
#Common metrics include species richness, Shannon index, and Simpson index
# 1. Species Richness (Number of species per site)
richness <- specnumber(dune)

# 2. Shannon Diversity Index (H') - considers richness and evenness
shannon <- diversity(dune, index = "shannon")

# 3. Simpson Diversity Index (D) - considers dominant species
simpson <- diversity(dune, index = "simpson")

# 4. Pielou's Evenness (J') - how close species are in abundance
evenness <- shannon / log(specnumber(dune))

# Combine into a table
diversity_table <- data.frame(richness, shannon, simpson, evenness)
head(diversity_table)
#
#Beta diversity measures how community composition changes between sites. The vegdist function calculates dissimilarity, 
#with Bray-Curtis being the most popular for abundance data
# Calculate Bray-Curtis dissimilarity matrix
bray_dist <- vegdist(dune, method = "bray")

# View the matrix
as.matrix(bray_dist)[1:5, 1:5]
#Non-metric Multidimensional Scaling (NMDS) is used 
#to visualize the dissimilarity between samples
# Run NMDS
nmds_res <- metaMDS(dune, distance = "bray")

# Plot NMDS
plot(nmds_res, type = "t")
#To estimate if sampling effort was sufficient to capture the total richness
# Calculate species accumulation
sp_accum <- specaccum(dune)

# Plot
plot(sp_accum, xlab = "Sites", ylab = "Number of Species")

#Summary of Key Functions
#diversity(): Calculates Shannon/Simpson indices.
#specnumber(): Calculates species richness.
#vegdist(): Calculates distance matrices (Bray-Curtis, Jaccard).
#metaMDS(): Runs Non-metric Multidimensional Scaling.
#betapart package: Specialized for breaking down beta diversity into turnover and nestedness components.
#hillR package: For calculating diversity via Hill numbers
