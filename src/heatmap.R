library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

# SETWD!!!
path <- "../downloaded_data/drug_connections_c_labeled.csv"

drug_con <- read.csv(path, header = F, stringsAsFactors = F)

cols <- c("drug1", drug_con$V1)
colnames(drug_con) <- cols

drug_mat <- as.matrix(drug_con[,-1])
hc <- hclust(dist(drug_mat))
drug_mat <- drug_mat[hc$order,hc$order]

hc_order <- colnames(drug_mat)[hc$order]

drug_plot_clustered <- drug_con %>%
  gather("drug2","overlap", -drug1) %>%
  mutate(Dataset = 1,
         drug1 = factor(drug1, levels = hc_order),
         drug2 = factor(drug2, levels = hc_order))

drug_plot <- drug_con %>%
  gather("drug2","overlap", -drug1) %>%
  mutate(Dataset = 1)

p <- ggplot(drug_plot, aes(x=drug1, y = drug2, fill = overlap)) +
  geom_tile() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  xlab("") +
  ylab("")
p

p <- ggplotly(p)
p
 
htmlwidgets::saveWidget(p, "heatmap.html")
 