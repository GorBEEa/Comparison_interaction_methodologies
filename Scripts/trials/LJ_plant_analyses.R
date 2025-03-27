###     Title: Bombus pascuorum microbiome

# Load libraries
library(Biostrings) ; packageVersion("Biostrings")
library(tidyverse) ; packageVersion("tidyverse")
library(phyloseq) ; packageVersion("phyloseq")
library(ggplot2); packageVersion("ggplot2")
library(ggstatsplot) ; packageVersion("ggstatsplot")
library(vegan) ; packageVersion("vegan") 
library(DESeq2) ; packageVersion("DESeq2") 
library(dendextend) ; packageVersion("dendextend") 
library(viridis) ; packageVersion("viridis")
library(patchwork) ; packageVersion("patchwork")
library (dplyr) ; packageVersion("dplyr")

# Load the phyloseq object
ps.gbp23 <- readRDS("Data/ps.gbp23.RDS")
print(ps.gbp23)

#       1. Comparison between periods     ####

# From the phyloseq contaminant-free file
# Extract the ASV count table, sample_data and tax_table
count_tab.cl <- otu_table(ps.gbp23)
count_tab.cl <- as.data.frame(count_tab.cl)
sample_data_tab.cl <- sample_data(ps.gbp23)
tax_table.cl <- tax_table(ps.gbp23)

deseq_counts <- DESeqDataSetFromMatrix(count_tab.cl, colData = sample_data_tab.cl, design = ~period) 
deseq_counts_vst <- varianceStabilizingTransformation(deseq_counts)
vst_trans_count_tab <- assay(deseq_counts_vst)
euc_dist <- dist(t(vst_trans_count_tab))

# Hierarchical clustering for 
euc_clust <- hclust(euc_dist, method="ward.D2")

# hclust objects like this can be plotted with the generic plot() function
plot(euc_clust) 
# but i like to change them to dendrograms for two reasons:
# 1) it's easier to color the dendrogram plot by groups
# 2) if wanted you can rotate clusters with the rotate() 
#    function of the dendextend package

euc_dend <- as.dendrogram(euc_clust, hang=0.1)
dend_cols <- as.character(sample_data_tab.cl$color_p[order.dendrogram(euc_dend)])
labels_colors(euc_dend) <- dend_cols

plot(euc_dend, ylab="VST Euc. dist.")


# making our phyloseq object with transformed table
vst_count_phy <- otu_table(vst_trans_count_tab, taxa_are_rows=T)
sample_info_tab_phy <- sample_data(sample_data_tab.cl)
vst_physeq <- phyloseq(vst_count_phy, sample_info_tab_phy)

# generating and visualizing the PCoA with phyloseq
vst_pcoa <- ordinate(vst_physeq, method="MDS", distance="euclidean")
eigen_vals <- vst_pcoa$values$Eigenvalues # allows us to scale the axes according to their magnitude of separating apart the samples

plot_ordination(vst_physeq, vst_pcoa, color="period") + 
  geom_point(size=1) + labs(col="period") + 
  geom_text(aes(label=rownames(sample_data_tab.cl), hjust=0.3, vjust=-0.4)) + 
  coord_fixed(sqrt(eigen_vals[2]/eigen_vals[1])) + ggtitle("PCoA") + 
  scale_color_manual(values=unique(sample_data_tab.cl$color_p[order(sample_data_tab.cl$period)])) + 
  theme_bw() + theme(legend.position="none")

#       2. Comparison between sites            ####

deseq_counts <- DESeqDataSetFromMatrix(count_tab.cl, colData = sample_data_tab.cl, design = ~site) 
deseq_counts_vst <- varianceStabilizingTransformation(deseq_counts)
vst_trans_count_tab <- assay(deseq_counts_vst)
euc_dist <- dist(t(vst_trans_count_tab))

# Hierarchical clustering for 
euc_clust <- hclust(euc_dist, method="ward.D2")

# hclust objects like this can be plotted with the generic plot() function
plot(euc_clust) 
# but i like to change them to dendrograms for two reasons:
# 1) it's easier to color the dendrogram plot by groups
# 2) if wanted you can rotate clusters with the rotate() 
#    function of the dendextend package

euc_dend <- as.dendrogram(euc_clust, hang=0.1)
dend_cols <- as.character(sample_data_tab.cl$color_s[order.dendrogram(euc_dend)])
labels_colors(euc_dend) <- dend_cols

plot(euc_dend, ylab="VST Euc. dist.")


# making our phyloseq object with transformed table
vst_count_phy <- otu_table(vst_trans_count_tab, taxa_are_rows=T)
sample_info_tab_phy <- sample_data(sample_data_tab.cl)
vst_physeq <- phyloseq(vst_count_phy, sample_info_tab_phy)

# generating and visualizing the PCoA with phyloseq
vst_pcoa <- ordinate(vst_physeq, method="MDS", distance="euclidean")
eigen_vals <- vst_pcoa$values$Eigenvalues # allows us to scale the axes according to their magnitude of separating apart the samples

plot_ordination(vst_physeq, vst_pcoa, color="site") + 
  geom_point(size=1) + labs(col="site") + 
  geom_text(aes(label=rownames(sample_data_tab.cl), hjust=0.3, vjust=-0.4)) + 
  coord_fixed(sqrt(eigen_vals[2]/eigen_vals[1])) + ggtitle("PCoA") + 
  scale_color_manual(values=unique(sample_data_tab.cl$color_s[order(sample_data_tab.cl$site)])) + 
  theme_bw() + theme(legend.position="none")


#       3. Alpha diversity      ####


##      3.1. Rarefaction curves      ####
rarecurve(t(count_tab.cl), step=100, col=sample_data_tab.cl$color_p, lwd=2, ylab="ASVs", label=F)
# and adding a vertical line at the fewest seqs in any sample
abline(v=(min(rowSums(t(count_tab.cl)))))


##      3.2. Richness and diversity estimates   ####

################################## Perhaps not neccessary
# first we need to create a phyloseq object using our un-transformed count table
count_tab_phy <- otu_table(count_tab.cl, taxa_are_rows=T)
tax_tab_phy <- tax_table(tax_table.cl)

ASV_physeq <- phyloseq(count_tab_phy, tax_tab_phy, sample_data_tab.cl)



# LJ remove?
#ps.gbp23
#sample_data_tab.cl
##################################


plot_richness(ps.gbp23, color="period", measures=c("Chao1", "Shannon")) + 
  scale_color_manual(values=unique(sample_data_tab.cl$color_p[order(sample_data_tab.cl$period)])) +
  theme_bw() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

period_richness <- plot_richness(ps.gbp23, x="period", color="period", measures=c("Chao1", "Shannon")) + 
  scale_color_manual(values=unique(sample_data_tab.cl$color_p[order(sample_data_tab.cl$period)])) +
  theme_bw() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_violin() + geom_jitter(height = 0, width = 0.1)

period_richness + scale_fill_manual(values=c("#440154FF","#414487FF","#2A788EFF","#22A884FF","#7AD151FF","#FDE725FF","red"))

site_richness <- plot_richness(ps.gbp23, x="site", color="site", measures=c("Chao1", "Shannon")) + 
  scale_color_manual(values=unique(sample_data_tab.cl$color_s[order(sample_data_tab.cl$site)])) +
  theme_bw() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

site_richness + geom_violin() + geom_jitter(height = 0, width = 0.1)


# Statistical comparisons

# Estimate richness (Chao1 and Shannon index) by using phyloseq
richness_data <- estimate_richness(ps.gbp23, measures = c("Chao1", "Shannon"))
# Add period and site metadata
richness_data$period <- sample_data(ps.gbp23)$period
richness_data$site <- sample_data(ps.gbp23)$site

# Shapiro-Wilk normality test
shapiro.test(richness_data$Shannon)
shapiro.test(richness_data$Chao1)

# Define our custom color palette
viridis_palette <- c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")


plt1 <- ggbetweenstats(data = richness_data, x = period, y = Chao1, type = "nonparametric"
) +
  scale_color_manual(values = viridis_palette) +
  scale_fill_manual(values = viridis_palette)

plt2 <-ggbetweenstats(data = richness_data, x = period, y = Shannon, type = "nonparametric"
) +
  scale_color_manual(values = viridis_palette) +
  scale_fill_manual(values = viridis_palette)

plt3 <- ggbetweenstats(data = richness_data, x = site, y = Chao1, type = "nonparametric",
                       package = "colorBlindness",
                       palette = "Blue2DarkOrange18Steps",
)

plt4 <- ggbetweenstats(data = richness_data, x = site, y = Shannon, type = "nonparametric",
                       package = "colorBlindness",
                       palette = "Blue2DarkOrange18Steps",
)

combined_plot <- (plt1 + plt2) / (plt3 + plt4)
combined_plot

# Not significant difference between periods and sites.

##      3.3. TAXONOMIC SUMMARIES      ####


#ps.gbp23
#sample_data_tab.cl
tax_table.cl
#count_tab.cl

# using phyloseq to make a count table that has summed all ASVs
# that were in the same phylum
order_counts_tab <- otu_table(tax_glom(ps.gbp23, taxrank="Order")) 


# making a vector of phyla names to set as row names
order_tax_vec <- as.vector(tax_table(tax_glom(ps.gbp23, taxrank= "Order"))[,"Order"]) 
rownames(order_counts_tab) <- as.vector(order_tax_vec)

# we also have to account for sequences that weren't assigned any
# taxonomy even at the phylum level 
# these came into R as 'NAs' in the taxonomy table, but their counts are
# still in the count table
# so we can get that value for each sample by subtracting the column sums
# of this new table (that has everything that had a phylum assigned to it)
# from the column sums of the starting count table (that has all
# representative sequences)
unclassified_tax_counts <- colSums(count_tab.cl) - colSums(order_counts_tab)
# and we'll add this row to our phylum count table:
order_and_unidentified_counts_tab <- rbind(order_counts_tab, "Unclassified"=unclassified_tax_counts)

# now we'll remove the Proteobacteria, so we can next add them back in
# broken down by class
temp_major_taxa_counts_tab <- order_and_unidentified_counts_tab[!row.names(order_and_unidentified_counts_tab) %in% "Poales", ]

# making count table broken down by class (contains classes beyond the
# Proteobacteria too at this point)
family_counts_tab <- otu_table(tax_glom(ps.gbp23, taxrank= "Family")) 

# making a table that holds the phylum and class level info
family_tax_order_tab <- tax_table(tax_glom(ps.gbp23, taxrank= "Family")) 

order_tmp_vec <- family_tax_order_tab[,4]
family_tmp_vec <- family_tax_order_tab[,5]
rows_tmp <- row.names(family_tax_order_tab)
family_tax_tab <- data.frame("Order"=order_tmp_vec, "Family"=family_tmp_vec, row.names = rows_tmp)

# making a vector of just the Proteobacteria classes: LJ I think is not working
poales_families_vec <- as.vector(family_tax_tab[family_tax_tab$Order == "Poales", "Family"])

# changing the row names like above so that they correspond to the taxonomy,
# rather than an ASV identifier
rownames(family_counts_tab) <- as.vector(family_tax_tab$Family) 

# making a table of the counts of the Proteobacteria classes
poales_family_counts_tab <- family_counts_tab[row.names(family_counts_tab) %in% poales_families_vec, ] 

# there are also possibly some some sequences that were resolved to the level
# of Proteobacteria, but not any further, and therefore would be missing from
# our class table
# we can find the sum of them by subtracting the proteo class count table
# from just the Proteobacteria row from the original phylum-level count table
poales_no_fam_annotated_counts <- order_and_unidentified_counts_tab[row.names(order_and_unidentified_counts_tab) %in% "Poales", ] - colSums(poales_family_counts_tab)

# now combining the tables:
major_taxa_counts_tab <- rbind(temp_major_taxa_counts_tab, poales_family_counts_tab, "Unresolved_Poales"=poales_no_fam_annotated_counts)

# and to check we didn't miss any other sequences, we can compare the column
# sums to see if they are the same
# if "TRUE", we know nothing fell through the cracks
identical(colSums(major_taxa_counts_tab), colSums(count_tab.cl)) 

# now we'll generate a proportions table for summarizing:
major_taxa_proportions_tab <- apply(major_taxa_counts_tab, 2, function(x) x/sum(x)*100)

# if we check the dimensions of this table at this point
dim(major_taxa_proportions_tab)
# we see there are currently 42 rows, which might be a little busy for a
# summary figure
# many of these taxa make up a very small percentage, so we're going to
# filter some out
# this is a completely arbitrary decision solely to ease visualization and
# intepretation, entirely up to your data and you
# here, we'll only keep rows (taxa) that make up greater than 5% in any
# individual sample
temp_filt_major_taxa_proportions_tab <- data.frame(major_taxa_proportions_tab[apply(major_taxa_proportions_tab, 1, max) > 5, ])
# checking how many we have that were above this threshold
dim(temp_filt_major_taxa_proportions_tab) 
# now we have 12, much more manageable for an overview figure

# though each of the filtered taxa made up less than 5% alone, together they
# may add up and should still be included in the overall summary
# so we're going to add a row called "Other" that keeps track of how much we
# filtered out (which will also keep our totals at 100%)
filtered_proportions <- colSums(major_taxa_proportions_tab) - colSums(temp_filt_major_taxa_proportions_tab)
filt_major_taxa_proportions_tab <- rbind(temp_filt_major_taxa_proportions_tab, "Other"=filtered_proportions)

## don't worry if the numbers or taxonomy vary a little, this might happen due to different versions being used 
## from when this was initially put together







# first let's make a copy of our table that's safe for manipulating
filt_major_taxa_proportions_tab_for_plot <- filt_major_taxa_proportions_tab

# and add a column of the taxa names so that it is within the table, rather
# than just as row names (this makes working with ggplot easier)
filt_major_taxa_proportions_tab_for_plot$Major_Taxa <- row.names(filt_major_taxa_proportions_tab_for_plot)

# now we'll transform the table into narrow, or long, format (also makes
# plotting easier)
filt_major_taxa_proportions_tab_for_plot.g <- pivot_longer(filt_major_taxa_proportions_tab_for_plot, !Major_Taxa, names_to = "Sample", values_to = "Proportion") %>% data.frame()

# take a look at the new table and compare it with the old one
head(filt_major_taxa_proportions_tab_for_plot.g)
head(filt_major_taxa_proportions_tab_for_plot)
# manipulating tables like this is something you may need to do frequently in R

# now we want a table with "color" and "characteristics" of each sample to
# merge into our plotting table so we can use that more easily in our plotting
# function
# here we're making a new table by pulling what we want from the sample
# information table
sample_info_for_merge<-data.frame("Sample"=row.names(sample_data_tab.cl), "period"=sample_data_tab.cl$period, "color"=sample_data_tab.cl$color_p, stringsAsFactors=F)

# and here we are merging this table with the plotting table we just made
# (this is an awesome function!)
filt_major_taxa_proportions_tab_for_plot.g2 <- merge(filt_major_taxa_proportions_tab_for_plot.g, sample_info_for_merge)

# and now we're ready to make some summary figures with our wonderfully
# constructed table

## a good color scheme can be hard to find, i included the viridis package
## here because it's color-blind friendly and sometimes it's been really
## helpful for me, though this is not demonstrated in all of the following :/ 

# one common way to look at this is with stacked bar charts for each taxon per sample:
ggplot(filt_major_taxa_proportions_tab_for_plot.g2, aes(x=Sample, y=Proportion, fill=Major_Taxa)) +
  geom_bar(width=0.6, stat="identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.4, hjust=1), legend.title=element_blank()) +
  labs(x="Sample", y="% of ITS2 gene copies recovered", title="All samples")

ggplot(filt_major_taxa_proportions_tab_for_plot.g2, aes(Major_Taxa, Proportion)) +
  geom_jitter(aes(color=factor(period), shape=factor(period)), size=2, width=0.15, height=0) +
  scale_color_manual(values=unique(filt_major_taxa_proportions_tab_for_plot.g2$color[order(filt_major_taxa_proportions_tab_for_plot.g2$period)])) +
  geom_boxplot(fill=NA, outlier.color=NA) + theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.title=element_blank()) +
  labs(x="Major Taxa", y="% of ITS2 gene copies recovered", title="All samples")


# Function to generate a ggplot for a given period
generate_major_taxa_plot <- function(period, data, sample_data_tab.cl) {
  # Get sample IDs for the specified period
  sample_IDs <- row.names(sample_data_tab.cl)[sample_data_tab.cl$period == period]
  
  # Filter the data for the given period
  filt_data <- data[data$Sample %in% sample_IDs, ]
  
  # Generate the plot
  plot <- ggplot(filt_data, aes(Major_Taxa, Proportion)) +
    scale_y_continuous(limits = c(0, 100)) +
    geom_jitter(aes(color = factor(period)), size = 2, width = 0.15, height = 0) +
    scale_color_manual(values = unique(filt_data$color[order(filt_data$period)])) +
    geom_boxplot(fill = NA, outlier.color = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    labs(
      x = "Major Taxa", 
      y = "% of ITS2 gene copies recovered", 
      title = paste("Period", gsub("p", "", period), "samples")
    )
  
  return(plot)
}

# List of periods to analyze
periods <- c("p01", "p02", "p03", "p04", "p05", "p06")

# Store all generated plots in a list
plots <- list()
for (period in periods) {
  plots[[period]] <- generate_major_taxa_plot(period, filt_major_taxa_proportions_tab_for_plot.g2, sample_data_tab.cl)
}


# Combine all plots into a grid with patchwork package
major_taxa_combined <- (plots[["p01"]] + plots[["p02"]] + plots[["p03"]]) / 
  (plots[["p04"]] + plots[["p05"]] + plots[["p06"]])
major_taxa_combined

# Save the plots. !LJC Should I include this?
# Save the combined plot
#ggsave("major_taxa_combined_plot.png", major_taxa_combined, width = 10, height = 8)

# Save individual plots
#for (period in periods) {
#  ggsave(paste0("major_taxa_", period, ".png"), plots[[period]], width = 5, height = 4)
#}


## Period flora distribution - Genus ----

# using phyloseq to make a count table that has summed all ASVs
# that were in the same family
fam_counts_tab <- otu_table(tax_glom(ps.gbp23, taxrank="Family")) 

# making a vector of Family names to set as row names
fam_tax_vec <- as.vector(tax_table(tax_glom(ps.gbp23, taxrank= "Family"))[,"Family"]) 
rownames(fam_counts_tab) <- as.vector(fam_tax_vec)

# we also have to account for sequences that weren't assigned any
# taxonomy even at the family level 
# these came into R as 'NAs' in the taxonomy table, but their counts are
# still in the count table
# so we can get that value for each sample by subtracting the column sums
# of this new table (that has everything that had a family assigned to it)
# from the column sums of the starting count table (that has all
# representative sequences)
unclassified_tax_counts <- colSums(count_tab.cl) - colSums(fam_counts_tab)
# and we'll add this row to our phylum count table:
fam_and_unidentified_counts_tab <- rbind(fam_counts_tab, "Unclassified"=unclassified_tax_counts)

# now we'll remove the Proteobacteria, so we can next add them back in
# broken down by class
temp_major_taxa_counts_tab <- fam_and_unidentified_counts_tab[!row.names(fam_and_unidentified_counts_tab) %in% "Fabaceae", ]

# making count table broken down by class (contains classes beyond the
# Proteobacteria too at this point)
genus_counts_tab <- otu_table(tax_glom(ps.gbp23, taxrank= "Genus")) 

# making a table that holds the phylum and class level info
genus_tax_fam_tab <- tax_table(tax_glom(ps.gbp23, taxrank= "Genus")) 

fam_tmp_vec <- genus_tax_fam_tab[,5]
genus_tmp_vec <- genus_tax_fam_tab[,6]
rows_tmp <- row.names(genus_tax_fam_tab)
genus_tax_tab <- data.frame("Family"=fam_tmp_vec, "Genus"=genus_tmp_vec, row.names = rows_tmp)

# making a vector of just the Proteobacteria classes: 
fabaceae_genera_vec <- as.vector(genus_tax_tab[genus_tax_tab$Family == "Fabaceae", "Genus"])

# changing the row names like above so that they correspond to the taxonomy,
# rather than an ASV identifier
rownames(genus_counts_tab) <- as.vector(genus_tax_tab$Genus) 

# making a table of the counts of the Proteobacteria classes
fabaceae_genera_counts_tab <- genus_counts_tab[row.names(genus_counts_tab) %in% fabaceae_genera_vec, ] 

# there are also possibly some some sequences that were resolved to the level
# of Proteobacteria, but not any further, and therefore would be missing from
# our class table
# we can find the sum of them by subtracting the proteo class count table
# from just the Proteobacteria row from the original phylum-level count table
fabaceae_no_genus_annotated_counts <- fam_and_unidentified_counts_tab[row.names(fam_and_unidentified_counts_tab) %in% "Fabaceae", ] - colSums(fabaceae_genera_counts_tab)

# now combining the tables:
major_taxa_counts_tab <- rbind(temp_major_taxa_counts_tab, fabaceae_genera_counts_tab, "Unresolved_Poales"=fabaceae_no_genus_annotated_counts)





major_taxa_counts_tab <- rbind(fam_and_unidentified_counts_tab)
# and to check we didn't miss any other sequences, we can compare the column
# sums to see if they are the same
# if "TRUE", we know nothing fell through the cracks
identical(colSums(major_taxa_counts_tab), colSums(count_tab.cl)) 

# now we'll generate a proportions table for summarizing:
major_taxa_proportions_tab <- apply(major_taxa_counts_tab, 2, function(x) x/sum(x)*100)

# if we check the dimensions of this table at this point
dim(major_taxa_proportions_tab)
# we see there are currently 42 rows, which might be a little busy for a
# summary figure
# many of these taxa make up a very small percentage, so we're going to
# filter some out
# this is a completely arbitrary decision solely to ease visualization and
# intepretation, entirely up to your data and you
# here, we'll only keep rows (taxa) that make up greater than 5% in any
# individual sample
temp_filt_major_taxa_proportions_tab <- data.frame(major_taxa_proportions_tab[apply(major_taxa_proportions_tab, 1, max) > 5, ])
# checking how many we have that were above this threshold
dim(temp_filt_major_taxa_proportions_tab) 
# now we have 12, much more manageable for an overview figure

# though each of the filtered taxa made up less than 5% alone, together they
# may add up and should still be included in the overall summary
# so we're going to add a row called "Other" that keeps track of how much we
# filtered out (which will also keep our totals at 100%)
filtered_proportions <- colSums(major_taxa_proportions_tab) - colSums(temp_filt_major_taxa_proportions_tab)
filt_major_taxa_proportions_tab <- rbind(temp_filt_major_taxa_proportions_tab, "Other"=filtered_proportions)

## don't worry if the numbers or taxonomy vary a little, this might happen due to different versions being used 
## from when this was initially put together



# first let's make a copy of our table that's safe for manipulating
filt_major_taxa_proportions_tab_for_plot <- filt_major_taxa_proportions_tab

# and add a column of the taxa names so that it is within the table, rather
# than just as row names (this makes working with ggplot easier)
filt_major_taxa_proportions_tab_for_plot$Major_Taxa <- row.names(filt_major_taxa_proportions_tab_for_plot)

# now we'll transform the table into narrow, or long, format (also makes
# plotting easier)
filt_major_taxa_proportions_tab_for_plot.g <- pivot_longer(filt_major_taxa_proportions_tab_for_plot, !Major_Taxa, names_to = "Sample", values_to = "Proportion") %>% data.frame()

# take a look at the new table and compare it with the old one
head(filt_major_taxa_proportions_tab_for_plot.g)
head(filt_major_taxa_proportions_tab_for_plot)
# manipulating tables like this is something you may need to do frequently in R

# now we want a table with "color" and "characteristics" of each sample to
# merge into our plotting table so we can use that more easily in our plotting
# function
# here we're making a new table by pulling what we want from the sample
# information table
sample_info_for_merge<-data.frame("Sample"=row.names(sample_data_tab.cl), "period"=sample_data_tab.cl$period, "color"=sample_data_tab.cl$color_p, stringsAsFactors=F)

# and here we are merging this table with the plotting table we just made
# (this is an awesome function!)
filt_major_taxa_proportions_tab_for_plot.g2 <- merge(filt_major_taxa_proportions_tab_for_plot.g, sample_info_for_merge)

# and now we're ready to make some summary figures with our wonderfully
# constructed table

## a good color scheme can be hard to find, i included the viridis package
## here because it's color-blind friendly and sometimes it's been really
## helpful for me, though this is not demonstrated in all of the following :/ 

# one common way to look at this is with stacked bar charts for each taxon per sample:
ggplot(filt_major_taxa_proportions_tab_for_plot.g2, aes(x=Sample, y=Proportion, fill=Major_Taxa)) +
  geom_bar(width=0.6, stat="identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.4, hjust=1), legend.title=element_blank()) +
  labs(x="Sample", y="% of ITS2 gene copies recovered", title="All samples")

ggplot(filt_major_taxa_proportions_tab_for_plot.g2, aes(Major_Taxa, Proportion)) +
  geom_jitter(aes(color=factor(period), shape=factor(period)), size=2, width=0.15, height=0) +
  scale_color_manual(values=unique(filt_major_taxa_proportions_tab_for_plot.g2$color[order(filt_major_taxa_proportions_tab_for_plot.g2$period)])) +
  geom_boxplot(fill=NA, outlier.color=NA) + theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.title=element_blank()) +
  labs(x="Major Taxa", y="% of ITS2 gene copies recovered", title="All samples")


# Function to generate a ggplot for a given period
generate_major_taxa_plot <- function(period, data, sample_data_tab.cl) {
  # Get sample IDs for the specified period
  sample_IDs <- row.names(sample_data_tab.cl)[sample_data_tab.cl$period == period]
  
  # Filter the data for the given period
  filt_data <- data[data$Sample %in% sample_IDs, ]
  
  # Generate the plot
  plot <- ggplot(filt_data, aes(Major_Taxa, Proportion)) +
    scale_y_continuous(limits = c(0, 100)) +
    geom_jitter(aes(color = factor(period)), size = 2, width = 0.15, height = 0) +
    scale_color_manual(values = unique(filt_data$color[order(filt_data$period)])) +
    geom_boxplot(fill = NA, outlier.color = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    labs(
      x = "Major Taxa", 
      y = "% of ITS2 gene copies recovered", 
      title = paste("Period", gsub("p", "", period), "samples")
    )
  
  return(plot)
}

# List of periods to analyze
periods <- c("p01", "p02", "p03", "p04", "p05", "p06")

# Store all generated plots in a list
plots <- list()
for (period in periods) {
  plots[[period]] <- generate_major_taxa_plot(period, filt_major_taxa_proportions_tab_for_plot.g2, sample_data_tab.cl)
}


# Combine all plots into a grid with patchwork package
major_taxa_combined <- (plots[["p01"]] + plots[["p02"]] + plots[["p03"]]) / 
  (plots[["p04"]] + plots[["p05"]] + plots[["p06"]])
major_taxa_combined



# https://benjjneb.github.io/dada2/tutorial.html

# Transform data to proportions as appropriate for Bray-Curtis distances
ASV_physeq.prop <- transform_sample_counts(ps.gbp23, function(otu) otu/sum(otu))
ord.nmds.bray <- ordinate(ASV_physeq.prop, method="NMDS", distance="bray")

# Plot with explicit colors
plot_ordination(ASV_physeq.prop, ord.nmds.bray, color="period", title="Bray NMDS") +
  scale_color_manual(values = viridis_palette)

# If we want to focusing without outliers
plot_ordination(ASV_physeq.prop, ord.nmds.bray, color="period", title="Bray NMDS") +
  scale_color_manual(values = viridis_palette) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))

# Bar plot
top20 <- names(sort(taxa_sums(ps.gbp23), decreasing=TRUE))[1:20]
ASV_physeq.top20 <- transform_sample_counts(ps.gbp23, function(OTU) OTU/sum(OTU))
ASV_physeq.top20 <- prune_taxa(top20, ASV_physeq.top20)
plot_bar(ASV_physeq.top20, x="period", fill="Genus") #+ facet_wrap(~When, scales="free_x")

# Now we will filter out Eukaryotes, Archaea, chloroplasts and mitochondria, because we only intended to amplify bacterial sequences
#ASV_physeq.NW.Bact <- ps.gbp23 %>%
#  subset_taxa(
#    Kingdom == "Bacteria" & #sometimes Kingdom
#      Family  != "Mitochondria" &
#      Order   != "Chloroplast"
#  )

#ASV_physeq.NW.Bact

# Make a data frame with a column for the read counts of each sample
sample_sum_df <- data.frame(sum = sample_sums(ps.gbp23))

ggplot(sample_sum_df, aes(x = sum)) + 
  geom_histogram(color = "black", fill = "indianred", binwidth = 2500) +
  ggtitle("Distribution of sample sequencing depth") + 
  xlab("Read counts") +
  theme(axis.title.y = element_blank())

# mean, max and min of sample read counts
smin <- min(sample_sums(ps.gbp23))
smean <- mean(sample_sums(ps.gbp23))
smax <- max(sample_sums(ps.gbp23))


# melt to long format (for ggploting) 
# prune out phyla below 2% in each sample

### Order ----

GorBEEa_2023_order <- ps.gbp23 %>%
  tax_glom(taxrank = "Order") %>%                     # agglomerate at phylum level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
  filter(Abundance > 0.02) %>%                         # Filter out low abundance taxa
  arrange(Order)                                      # Sort data frame alphabetically by phylum

ggplot(GorBEEa_2023_order, aes(x = period, y = Abundance, fill = Order)) + 
  #facet_grid(site~.) +
  geom_bar(stat = "identity", position="fill") +
  scale_fill_viridis_d(option = "viridis") +
  # scale_fill_manual(values =sample_info_tab$color_p) +
  #scale_x_discrete(
  # breaks = c("7/8", "8/4", "9/2", "10/6"),
  # labels = c("Jul", "Aug", "Sep", "Oct"), 
  #drop = FALSE
  # ) +
  # Remove x axis title
  theme(axis.title.x = element_blank()) + 
  #
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Order > 2%) \n") +
  ggtitle("Order Composition of GorBEEa \n plant Communities by Period") 


### Family ----

GorBEEa_2023_fam <- ps.gbp23 %>%
  tax_glom(taxrank = "Family") %>%                     # agglomerate at phylum level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
  filter(Abundance > 0.02) %>%                         # Filter out low abundance taxa
  arrange(Family)                                      # Sort data frame alphabetically by phylum

ggplot(GorBEEa_2023_fam, aes(x = period, y = Abundance, fill = Family)) + 
  #facet_grid(site~.) +
  geom_bar(stat = "identity", position="fill") +
  scale_fill_viridis_d(option = "viridis") +
  # scale_fill_manual(values =sample_info_tab$color_p) +
  #scale_x_discrete(
  # breaks = c("7/8", "8/4", "9/2", "10/6"),
  # labels = c("Jul", "Aug", "Sep", "Oct"), 
  #drop = FALSE
  # ) +
  # Remove x axis title
  theme(axis.title.x = element_blank()) + 
  #
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Family > 2%) \n") +
  ggtitle("Family Composition of GorBEEa \n plant Communities by Period")

# melt to long format (for ggploting) 
# prune out Genera below 5% in each sample

# Set colors for plotting
genera_colors <- c("#D32F2F", # Deep Red
                   "#2C7975", # Teal Green
                   "#FFC107", # Amber Yellow
                   "#9967CE", # Lavender Purple
                   "#CD9BCD", # Lilac
                   "#43978D", # Deep Aqua
                   "#E91E63", # Pink
                   "#522157", # Deep Purple
                   "#AF4474", # Rose Wine
                   "#1F2F98", # Royal Blue
                   "#F06292", # Light Pink
                   "#FD8F52", # Coral Orange
                   "#7BE495", # Mint Green
                   "#0191B4", # Cyan
                   "#A5CAD2", # Soft Blue
                   "#D3DD18", # Lime Green
                   "#4378A2"  # Steel Blue
)

# Define colors from different palettes
viridis_colors <- rev(viridis(14))  # 10 colors from viridis
mamgma_colors <- magma(14)      # 10 colors from magma

# Combine them
combined_palette <- c(viridis_colors, magma_colors)

### Genus ----

GorBEEa_2023_genus <- ps.gbp23 %>%
  tax_glom(taxrank = "Genus") %>%                     # agglomerate at genus level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
  filter(Abundance > 0.15) %>%                         # Filter out low abundance taxa
  arrange(Genus)                                      # Sort data frame alphabetically by phylum

genus_composition_indv <- ggplot(GorBEEa_2023_genus, aes(x = Sample, y = Abundance, fill = Genus)) + 
  geom_bar(stat = "identity") +
  #scale_fill_viridis_d(option = "viridis") + # I do not recommend this color palette for many genera
  scale_fill_manual(values =color_palette_36) + # Color palette customized for this dataset and 0.05 value
  # Remove x axis title
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6)) +
  #
  guides(fill = guide_legend(reverse = FALSE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Genus > 15%) \n") +
  ggtitle("Genus Composition of GorBEEa \n plant Communities by specimen")

plot(genus_composition_indv)


### LJ: I cannot save good figures in pdf

jpeg(filename = "genus_composition_indv.jpeg", width = 800, height = 600, quality = 100)
print(genus_composition_indv)
dev.off()

ggsave("/Users/luisja/Downloads/genus_composition_indv.png", plot = genus_composition_period, width = 8, height = 6, units = "in", dpi = 300)

###
###     Group by periods
###

sample_counts <- GorBEEa_2023_genus %>%
  group_by(period) %>%
  summarise(n_samples = n_distinct(Sample))

genus_composition_period <- ggplot(GorBEEa_2023_genus, aes(x = period, y = Abundance, fill = Genus)) + 
  geom_bar(stat = "identity", position="fill") +
  #scale_fill_viridis_d(option = "turbo") + # I do not recommend this color palette for many genera
  scale_fill_manual(values =combined_palette) + # Color palette customized for this dataset and 0.05 value
  geom_text(data = sample_counts, aes(x = period, y = 0, label = paste("n=", n_samples)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3, color = "black") +
  # Remove x axis title
  theme(axis.title.x = element_blank()) + 
  #
  guides(fill = guide_legend(reverse = FALSE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Genus > 15%) \n") +
  ggtitle("Genus Composition of GorBEEa \n plant Communities by Period")

plot(genus_composition_period)

### LJ: I cannot save good figures in pdf

jpeg(filename = "genus_composition_period.jpeg", width = 800, height = 600, quality = 100)
print(genus_composition_period)
dev.off()

ggsave("/Users/luisja/Downloads/genus_composition_period.pdf", plot = genus_composition_period, width = 8, height = 6, units = "in", dpi = 300)

###
###   Relative Abundance by site
###

# Calcular el número de muestras por 'site'

sample_counts <- GorBEEa_2023_genus %>%
  group_by(site) %>%
  summarise(n_samples = n_distinct(Sample))

genus_composition_site <- ggplot(GorBEEa_2023_genus, aes(x = site, y = Abundance, fill = Genus)) + 
  #facet_grid(site~.) +
  geom_bar(stat = "identity", position="fill") +
  geom_text(data = sample_counts, aes(x = site, y = 0, label = paste("n=", n_samples)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3, color = "black") +
  #scale_fill_viridis_d(option = "viridis") +
  scale_fill_manual(values =colors) +
  #scale_x_discrete(
  # breaks = c("7/8", "8/4", "9/2", "10/6"),
  # labels = c("Jul", "Aug", "Sep", "Oct"), 
  #drop = FALSE
  # ) +
  # Remove x axis title
  theme(axis.title.x = element_blank()) + 
  #
  guides(fill = guide_legend(reverse = FALSE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Genus > 5%) \n") +
  ggtitle("Genus Composition of GorBEEa \n Bacterial Communities by Site") 

plot(genus_composition_site)

### LJ: I cannot save good figures in pdf

jpeg(filename = "genus_composition_site.jpeg", width = 800, height = 600, quality = 100)
print(genus_composition_site)
dev.off()

ggsave("genus_composition_site.png", plot = genus_composition_site, width = 8, height = 6, units = "in", dpi = 300)


### Group by site and period

# Contar muestras por sitio y periodo

sample_counts <- GorBEEa_2023_genus %>%
  group_by(site, period) %>%
  summarise(n_samples = n_distinct(Sample))

genus_composition_site_period <- ggplot(GorBEEa_2023_genus, aes(x = period, y = Abundance, fill = Genus)) + 
  geom_bar(stat = "identity", position="fill") +
  facet_wrap(site~.) +
  #geom_bar(stat = "identity") +
  #scale_fill_viridis_d(option = "viridis") +
  scale_fill_manual(values = colors) +
  geom_text(data = sample_counts, aes(x = period, y = -0.05, label = paste("n=", n_samples)), inherit.aes = FALSE, vjust = 0.5, size = 3) +
  # Remove x axis title
  theme(axis.title.x = element_blank()) + 
  #
  guides(fill = guide_legend(reverse = FALSE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Genus > 5%) \n") +
  ggtitle("Genus Composition of GorBEEa \n Bacterial Communities by Site & Period") 

plot(genus_composition_site_period)

### LJ: I cannot save good figures in pdf

jpeg(filename = "genus_composition_site_period.jpeg", width = 800, height = 600, quality = 100)
print(genus_composition_site_period)
dev.off()

ggsave("genus_composition_site_period.png", plot = genus_composition_site_period, width = 8, height = 6, units = "in", dpi = 300)

### Without Brandisia genus ----

ps.gbp23_filt <- subset_taxa(ps.gbp23, Genus != "Brandisia")

GorBEEa_2023_genus_filt <- ps.gbp23_filt %>%
  tax_glom(taxrank = "Genus") %>%                     # agglomerate at genus level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
  filter(Abundance > 0.15) %>%                         # Filter out low abundance taxa
  arrange(Genus)                                      # Sort data frame alphabetically by genus

sample_counts <- GorBEEa_2023_genus_filt %>%
  group_by(period) %>%
  summarise(n_samples = n_distinct(Sample))

genus_composition_period <- ggplot(GorBEEa_2023_genus_filt, aes(x = period, y = Abundance, fill = Genus)) + 
  geom_bar(stat = "identity", position="fill") +
  #scale_fill_viridis_d(option = "turbo") + # I do not recommend this color palette for many genera
  scale_fill_manual(values =combined_palette) + # Color palette customized for this dataset and 0.05 value
  geom_text(data = sample_counts, aes(x = period, y = 0, label = paste("n=", n_samples)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3, color = "black") +
  # Remove x axis title
  #theme(axis.title.x = element_blank()) + 
  theme_minimal() +
  guides(fill = guide_legend(reverse = FALSE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Genus > 15%) \n") +
  ggtitle("Genus Composition of GorBEEa \n plant Communities by Period")

plot(genus_composition_period)
