library(tidyverse)
library(dplyr)
library(palmerpenguins)
library(GGally)
library(Rtsne)
library(tidyr)
setwd("~/University/Data Analysis Y4/Week 8/Week8")
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

penguin$common_name <- "blank"
penguin$scientific_name <- "blank"

penguin <- penguin %>% 
  extract(species, 
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

#wheat
setwd("~/University/Data Analysis Y4/Week 8/Week8")
#file <- "../wheat/data.txt"
#wheat <- read.table(File, header = FALSE)

wheat <- read.table("seeds_dataset.txt", header = FALSE)
#rename columns
names(wheat)[names(wheat) == "V1"] <- "area"
names(wheat)[names(wheat) == "V2"] <- "perimeter"
names(wheat)[names(wheat) == "V3"] <- "compact"
names(wheat)[names(wheat) == "V4"] <- "length.of.kernel"
names(wheat)[names(wheat) == "V5"] <- "width.of.kernel"
names(wheat)[names(wheat) == "V6"] <- "asymmetry.coefficient"
names(wheat)[names(wheat) == "V7"] <- "length.of.kernel.groove"
names(wheat)[names(wheat) == "V8"] <- "strain"

pca <- wheat %>%
  select(wheat$strain) %>%
  prcomp(scale. = TRUE)
summary(pca)
pca$rotation

wheat$strain <- as.factor(wheat$strain)

wheat %>% 
  GGally::ggpairs(aes(color = strain)) 

pca_labelled <- data.frame(pca$x, species = wheat$species)
ggplot(pca_labelled, aes(x = PC1, y = PC2, color = species)) +
  geom_point()

tsne <- wheat %>% 
  select(-strain) %>%
  Rtsne(perplexity = 20,
        check_duplicates = FALSE)

dat <- data.frame(tsne$Y,  strain = wheat$strain)

dat %>% ggplot(aes(x = X1, y = X2, colour = strain)) +
  geom_point()

#mew 
#file <- "../data-raw/sol.txt"
#sol <- read_table2(file)
#names(sol)
sol <- read.table("sol.txt", header = TRUE)
names(sol)

tsol <- sol %>% 
  select(-genename) %>% 
  t() %>% 
  data.frame()

names(tsol) <- sol$genename
tsol$sample <- row.names(tsol)

tsol <- tsol %>% 
  extract(sample, 
          c("lineage","rep"),
          "(Y[0-9]{3,4})\\_([A-C])")

pca <- tsol %>% 
  select(-lineage, -rep) %>%
  prcomp(scale. = TRUE)
summary(pca)

pca_labelled <- data.frame(pca$x, lineage = tsol$lineage)
ggplot(pca_labelled, aes(x = PC1, y = PC2, color = lineage)) +
  geom_point()

tsne <- tsol %>% 
  select(-lineage, -rep) %>%
  Rtsne(perplexity = 4,
        check_duplicates = FALSE)

dat <- data.frame(tsne$Y,  lineage = tsol$lineage)

dat %>% ggplot(aes(x = X1, y = X2, colour = lineage)) +
  geom_point()
