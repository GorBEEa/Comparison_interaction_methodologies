#Script for performing a pairwise analysis of permanova data between methodologies
#pairwiseAdonis has not worked for my data
#depends on R data from prior scripts

#library(vegan)
library(knitr)
library(kableExtra)

#build function for a pairwise permanova ------
pairwise_permanova <- function(sp_matrix, group_var, dist = "raup", adj = "holm", perm = 9999) {
  group_var <- as.factor(group_var)
  groups <- combn(levels(group_var), 2)
  
  results <- data.frame(
    group1 = character(),
    group2 = character(),
    df1 = numeric(),
    df2 = numeric(),
    R2 = numeric(),
    F_value = numeric(),
    p_value = numeric(),
    p_adj = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:ncol(groups)) {
    g1 <- groups[1, i]
    g2 <- groups[2, i]
    subset_idx <- group_var %in% c(g1, g2)
    
    sub_matrix <- sp_matrix[subset_idx, , drop = FALSE]
    sub_group <- droplevels(group_var[subset_idx])
    
    fit <- adonis2(sub_matrix ~ sub_group, method = dist, permutations = perm)
    
    results <- rbind(results, data.frame(
      group1 = g1,
      group2 = g2,
      df1 = fit$Df,
      df2 = fit$Df[1],
      R2 = fit$R2,
      F_value = fit$F,
      p_value = fit$`Pr(>F)`,
      p_adj = NA
    ))
  }
  
  results$p_adj <- p.adjust(results$p_value, method = adj)
  return(results)
}


#execute pairwise permanova using all data -----

#load and prepare my data for using this function
load(here("Data/05_output.RData"))

all.plants.matrix <- as.matrix(all.plants)

pairwise.methodology <- recode_factor(methodology,
                                      count = "flower count",
                                      gut.metabarcoding = "gut metabarcoding",
                                      pollen.metabarcoding = "pollen metabarcoding")

pairwise.results <- pairwise_permanova(all.plants.matrix, pairwise.methodology)

#Make a cleaner visual of results
clean_results <- pairwise.results %>%
  # Remove rows with NA in p_value or F_value (likely redundant summary rows)
  filter(!is.na(p_value) & !is.na(F_value)) %>%
  
  # Round numeric columns nicely for display
  mutate(
    R2 = round(R2, 3),
    F_value = round(F_value, 2),
    p_value = ifelse(p_value < 0.001, "<0.001", signif(p_value, 3)),
    p_adj = ifelse(p_adj < 0.001, "<0.001", signif(p_adj, 3))
  )
  

# Create a markdown/HTML table suitable for reporting
ppermanova.kbl <- kable(clean_results, 
      col.names = c("Methodology 1", "Methodology 2", "DF1", "DF2", "R\u00B2", "F", "p", "Adjusted p"),
      digits = 3) %>% 
  kable_minimal(full_width = F, html_font = "Cambria")


#execute pairwise permanova using only interaction methodology data (no flower counts) -----

#load and prepare my data for using this function
load(here("Data/05.4_output.RData"))

int3.plants.matrix <- as.matrix(int3.plants)

int3.pairwise.results <- pairwise_permanova(int3.plants.matrix, methodology)

#Make a cleaner visual of results
int3_clean_results <- int3.pairwise.results %>%
  # Remove rows with NA in p_value or F_value (likely redundant summary rows)
  filter(!is.na(p_value) & !is.na(F_value)) %>%
  
  # Round numeric columns nicely for display
  mutate(
    R2 = round(R2, 3),
    F_value = round(F_value, 2),
    p_value = signif(p_value, 3),
    p_adj = signif(p_adj, 3)
  ) %>%
  
  # Add significance stars
  mutate(significance = case_when(
    p_adj <= 0.001 ~ "***",
    p_adj <= 0.01 ~ "**",
    p_adj <= 0.05 ~ "*",
    TRUE ~ ""
  ))

# Create a markdown/HTML table suitable for reporting
int3.ppermanova.kbl <- kable(int3_clean_results, 
                        col.names = c("Group 1", "Group 2", "R\u00B2", "F value", "df1", "df2", "p value", "Adjusted p value", "Significance"),
                        caption = "Pairwise PERMANOVA Results",
                        digits = 3) %>% 
  kable_minimal(full_width = F, html_font = "Cambria")






save.image(file = here("Data/06_output.RData"))















