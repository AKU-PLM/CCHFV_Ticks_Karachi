##Visualize the Maximum likelihood trees

# Load necessary libraries
library(ggplot2)
library(ggtree)
library(readxl)
library(dplyr)
library(ape)
library(treeio)

###other useful packages
library(maps)
library(phytools)
library(tidyr)
library(tidytree)

##S-segment tree
#read in tree
tree_S <- read.tree("S.final.renamed.clean.aln.fasta.treefile")

#read in metadata
metadata_S_01 <- read.csv("edited_S_final_metadata_for_R.csv")

#visualize tree
p_S <- ggtree(tree_S)

#root the tree at midpoint
midrooted_tree_S <- midpoint_root(tree_S)
ggtree(midrooted_tree_S)

#for saving as pdf
pdf(file = "S.isolate.tree.pdf", width = 7,height = 10.5)

# or fro saving as jpeg
jpeg(filename = "S.tree.jpg", width = 3000, height = 3600, quality = 100,res =300)

# or fro saving as png
png(filename = "S.tree.png", width = 3000, height = 3600, res = 300)

#to have diff stroke size and color ac to metadata
new_S <- ggtree(midrooted_tree_S, color='#A6A6A6',size=0.5)  %<+% metadata_S_01 +
  geom_tippoint(aes(fill=Clade, color=isolates, stroke=outline), size=3, shape=21) +
  scale_fill_manual(values = c('limegreen','yellow','plum','red','steelblue','orange','lightgrey'))+
  scale_color_manual(values = c('#676767','black'))+
  geom_treescale(x=0, y=1, width=0.02, fontsize=3) +
  # geom_text2(aes(subset = as.numeric(label) > 70, label = "*"), color="blue", size=4)+
  ylim(c(0.5, 320))

new_S

##M-segment tree
#read in tree
tree_M <- read.tree("M.final.renamed.clean.aln.fasta.treefile")

#read in metadata
metadata_M_01 <- read.csv("edited_M_final_metadata_for_R.csv")

#visualize tree

p_M <- ggtree(tree_M)

#root the tree at midpoint
midrooted_tree_M <- midpoint_root(tree_M)
ggtree(midrooted_tree_M)

#to have diff stroke size and color ac to metadata
new_M <- ggtree(midrooted_tree_M, color='#A6A6A6',size=0.5)  %<+% metadata_M_01 +
  geom_tippoint(aes(fill=Clade, color=isolates, stroke=outline), size=3, shape=21) +
  scale_fill_manual(values = c('limegreen','yellow','plum','red','steelblue','orange','hotpink','lightgrey'))+
  scale_color_manual(values = c('#676767','black'))+
  geom_treescale(x=0, y=1, width=0.03, fontsize=3) + 
  # geom_text2(aes(subset = as.numeric(label) > 70, label = "*"), color="blue", size=4)+
  ylim(c(0.5, 310))

new_M

##L-segment tree
#read in tree
tree_L <- read.tree("L.final.renamed.clean.aln.fasta.treefile")

#read in metadata
metadata_L_01 <- read.csv("edited_L_final_metadata_for_R.csv")

#visualize tree
p <- ggtree(tree_L)

#root the tree at midpoint
midrooted_tree_L <- midpoint_root(tree_L)
ggtree(midrooted_tree_L)

#to have diff stroke size and color ac to metadata
new_L <- ggtree(midrooted_tree_L, color='#A9A9A9',size=0.5)  %<+% metadata_L_01 +
  geom_tippoint(aes(fill=Clade, color=isolates, stroke=outline), size=3, shape=21) +
  scale_fill_manual(values = c('limegreen','yellow','plum','red','steelblue','orange','lightgrey'))+
  scale_color_manual(values = c('#676767','black'))+
  geom_treescale(x=0, y=2, width=0.02, fontsize=3) + 
  # geom_text2(aes(subset = as.numeric(label) > 70, label = "*"), color="blue", size=4)+
  ylim(c(0.5, 310))

new_L

##save all trees in svg format, final edits done in inkscape
