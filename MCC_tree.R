# Load necessary libraries
library(ggnewscale)
library(ggplot2)
library(ggtree)
library(readxl)
library(dplyr)
library(ape)
library(treeio)
library(RColorBrewer)
library(ggtreeExtra)

###other useful packages
library(phytools)
library(tidyr)
library(tidytree)


#read in tree
S_MCCtree <- read.beast("S_Clade-IVa.aln_relaxed_constant_est01_MCC.tree")
M_MCCtree <- read.beast("M_Clade-IVa.aln_relaxed_constant_est01_MCC.tree")
L_MCCtree <- read.beast("L_Clade-IVa.aln_relaxed_constant_est02_MCC.tree")


#read in metadata
metadata_S_01 <- read.csv("edited_S_final_metadata_for_R.csv")
metadata_M_01 <- read.csv("edited_M_final_metadata_for_R.csv")
metadata_L_01 <- read.csv("edited_L_final_metadata_for_R.csv")


#visualize tree
ggtree(S_MCCtree)
ggtree(M_MCCtree)
ggtree(L_MCCtree)

#visualize the tree
# ggtree(S_MCCtree) +
#   geom_tiplab(size = 2, hjust = 0.5)+ #to visualize tip labels
#   theme_tree2()+ # this theme will add x-axis scale
#   labs(title = "MCC Tree Visualization")
#   geom_text(aes(label= node)) #to visualize the node numbers


#first add "mrsd" in yyyy-mm-dd foramt, add metadata then other settings
S_01 <- ggtree(S_MCCtree, mrsd = "2023-01-01", color='#A6A6A6',size=0.5) %<+% metadata_S_01 +
  theme_tree2() +
  scale_x_continuous(name = "Time in Years",
                     breaks = seq(1468, 2023, by = 50),
                     labels = seq(1468, 2023, by = 50))+
  coord_cartesian(xlim = c(1468, 2040))+
  geom_tippoint(aes(fill = Country, color=isolates, stroke=1.5), size=3, shape=21) + # fill tip point based on country and stroke to differentiate study and global isolate
  scale_fill_manual(values = c('#ff0000ff', '#f9f500','#A200F6','#aee3f5','#fa5d05ff','#f702e7','#8486f8ff','#2cb431','#fcc2d8')) + # set fill color
  scale_color_manual(values = c('#676767','black')) #set stroke color

S_01 #visualize annotated tree


M_01 <- ggtree(M_MCCtree, mrsd = "2023-01-01", color='#A6A6A6',size=0.5) %<+% metadata_M_01 +
  theme_tree2() +
  scale_x_continuous(name = "Time in Years",
                     breaks = seq(1758, 2023, by = 26),
                     labels = seq(1758, 2023, by = 26))+
  coord_cartesian(xlim = c(1758, 2032))+
  geom_tippoint(aes(fill = Country, color=isolates, stroke=1.5), size=3, shape=21) +
  scale_fill_manual(values = c('#ff0000ff','#A200F6','#aee3f5','#2cb431','#fcc2d8')) +
  scale_color_manual(values = c('#676767','black'))
M_01

L_01 <- ggtree(L_MCCtree, mrsd = "2023-01-01", color='#A6A6A6',size=0.5) %<+% metadata_L_01 +
  theme_tree2() +
  scale_x_continuous(name = "Time in Years",
                     breaks = seq(1170, 2023, by = 85),
                     labels = seq(1170, 2023, by = 85))+
  coord_cartesian(xlim = c(1170, 2045))+
  geom_tippoint(aes(fill = Country, color=isolates, stroke=1.5), size=3, shape=21) +
  scale_fill_manual(values = c('#ff0000ff','#f9f500','#A200F6','#aee3f5','#8285f8ff','#2cb431','#fcc2d8')) +
  scale_color_manual(values = c('#676767','black'))
L_01


#add layers for heatmap
S_02 <- S_01 + new_scale_fill()+
  geom_fruit(geom = geom_tile, mapping = aes(fill = Host),width=20, offset = 0.05)+
  scale_fill_manual(values = c('#722e39ff','#454cf4ff','#ffb400ff','black','#fffd72ff','#f27fedd8','#00f5d4ff'))+
  theme(legend.key.size =unit(0.3, "cm"), legend.title = element_text(size = 8), axis.text.x = element_text(size = 18, color = 'black'))
S_02

M_02 <- M_01 + new_scale_fill()+
  geom_fruit(geom = geom_tile, mapping = aes(fill = Host),width=9.5, offset = 0.05)+
  scale_fill_manual(values = c('#722e39ff','#454cf4ff','#b8c7ced8','#fffd72ff','#f27fedd8','#00f5d4ff'))+
  theme(legend.key.size =unit(0.3, "cm"), legend.title = element_text(size = 8), axis.text.x = element_text(size = 18, color = 'black'))
M_02

L_02 <- L_01 + new_scale_fill()+
  geom_fruit(geom = geom_tile, mapping = aes(fill = Host),width=30, offset = 0.05)+
  scale_fill_manual(values = c('#722e39ff','#454cf4ff','#ffb400ff','black','#52ff00ff','#b8c7ced8','#fffd72ff','#6ca89fff','#f27fedd8','#00f5d4ff'))+
  theme(legend.key.size =unit(0.3, "cm"), legend.title = element_text(size = 8), axis.text.x = element_text(size = 18, color = 'black'))
L_02
