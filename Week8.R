library(tidyverse)
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(stringr)
library(forcats)
library(janitor)
library(multiway)
library(citr)
library(kableExtra)
library(flextable)
library(gt)
library(bibtex)
library(ggplot2)
library(readr)
library("Hmisc")
library(Rmisc)
library(pgirmess)
library(palmerpenguins)
library(GGally)
library(Rtsne)

penguin <- penguins_raw %>%
  janitor::clean_names()

str(penguins_raw)
#look at variables
penguin %>% 
  select(body_mass_g,
         ends_with("_mm")) %>% 
  summary()
#filter out NA
penguin <- penguin %>% 
filter(!is.na(body_mass_g))
#split species by scientific and common names
penguin <- penguin %>% 
  extract(penguin$species, 
          c("common_name", "scientific_name"),
          "([a-zA-Z]+\\s[a-zA-Z]+)\\s\\(([a-zA-Z]+\\s[a-zA-Z]+)\\)")

#pipe into ggpairs
penguin %>%
           select(common_name, 
                  sex, 
                  island,
                  body_mass_g,
                  ends_with("_mm")) %>%
  
#pipe 4 variables into cromp
  pca <- penguin %>% 
  select(body_mass_g,
         ends_with("_mm")) %>%
  prcomp(scale. = TRUE)

summary(pca)
#see importance of of each variable component
pca$rotation

#extract scores into variable on species name
pca_labelled <- data.frame(pca$x, common_name = penguin$common_name)
# a then to do a scatterplot
pca_labelled %>% 
  ggplot(aes(x = PC1, y = PC2, color = common_name)) +
  geom_point()

#import data
file <- "../data-raw/scrna_data.csv"
rna <- read_csv(file)
           ggpairs(aes(color = common_name))
#perform pca
pca <- rna %>% 
  prcomp(scale. = TRUE)
#put pc score in dataframe
dat <-  data.frame(pca$x)
#plot pc1 against pc2
ggplot(dat, aes(x = PC1, y = PC2)) +
  geom_point()
#put first 10 into ggpairs
dat %>%
  select(PC1:PC10) %>% 
  ggpairs()

#perform RTsne
tsne <- rna %>% 
  Rtsne(perplexity = 40,
        check_duplicates = FALSE)

#tSNE scores in dataframe
dat <- data.frame(tsne$Y)

#Plot the first t-SNE dimension against the second:
dat %>% ggplot(aes(x = X1, y = X2)) +
  geom_point(size=0.5)

#import metadata
file <- "../data-raw/scrna_meta.csv"
meta <- read_csv(file)

#8 cell types
unique(meta$louvain)
#add the cell type to the t-SNE scores dataframe:
dat <- data.frame(dat, type = meta$louvain)
#Replot the t*-SNE scores coloured by cell type:
dat %>% ggplot(aes(x = X1, y = X2, colour = type)) +
  geom_point(size = 0.5)



