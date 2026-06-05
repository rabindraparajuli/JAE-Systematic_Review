#R codes for 'Heat map' Figure 2 of the Parajuli et al. Journal of Applied Ecology
#1. Raw table formatting and tabulation
#2. Summary table
#3. Heat maps

#install and load packages

#install.packages(patchwork)

library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(patchwork)

#Set working directory, or if using RStudio, it automatically sets working directory to the code's folder

#setwd(path/to/your/folder)

#Start with spatial scale and biodiversity benefits; no need for data wrangling

df.s <- read_excel("data.xlsx", sheet = "spatial")

head(df.s)

df.s$spatial_scale<-as.factor(df.s$spatial_scale)

str(df.s)

#unique spatial scales
unique_scale <- unique(df.s$spatial_scale)
print(unique_scale)

# Count rows per scales; Note some studies were deleted due to incomplete data, so it won't be 100
table(df.s$spatial_scale)


#Summarize proportion for spatial scale * taxa
summary_spatial<- df.s %>%
  group_by(spatial_scale) %>%
  summarise(across(
    c(Birds, Fishes, Inverts, Plants),
    ~ round(sum(.x== 1, na.rm = TRUE) / sum(.x %in% c(0, 1), na.rm = TRUE), 3),
    .names = "{.col}"
  )) %>%
  ungroup()

print(summary_spatial)

#study counts to determine total number of samples per pairings
st_ct<- df.s %>%
  group_by(spatial_scale) %>%
  summarise(across(
    c(Birds, Fishes, Inverts, Plants),
    ~ sum(.x %in% c(0,1), na.rm = TRUE),
    .names = "{.col}_n"
  )) %>%
  ungroup()

print(st_ct)

#plotting part, first long table and then plot

#Proportion
long_spatial<-summary_spatial %>%
  pivot_longer(cols = -spatial_scale,
               names_to = "Taxa",
               values_to = "Proportion")

#total study n 
counts_long <- st_ct %>%
  pivot_longer(cols = -spatial_scale,
               names_to = "Taxa",
               values_to = "n")%>%
  mutate(Taxa = str_remove(Taxa, "_n"))

head(counts_long, 10)
#join proportion and counts
long_spatial <- long_spatial %>%
  left_join(counts_long, by = c("spatial_scale", "Taxa"))

# Define the order and rename them
scale_order <- c("Local (Single-Reach)", "Multi-reach", "Watershed (National)", 
                 "Watershed (Regional)", "Multinational")
scale_labels <- c("Single-reach", "Multi-reach", "Watershed (Nat.)", 
                  "Watershed (Reg.)", "Multinational")

#apply the order
long_spatial<-long_spatial %>%
  mutate(spatial_scale = factor(spatial_scale,
                                levels = scale_order,
                                labels = scale_labels))

# Heatmap Plot for spatial scale * taxa pairings
spatial<-ggplot(long_spatial, aes(x = Taxa, y = spatial_scale, fill = Proportion)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(is.nan(Proportion), "NA", 
                               paste0(sprintf("%.2f", Proportion), "\n(n=", n, ")"))),
            size = 2, color = "black") +
  scale_fill_gradient(low = "red", high = "forestgreen",
                      na.value = "grey85",
                      name = "Proportion\nof positive\nimpacts",
                      limits = c(0, 1)) +
  labs(title = "(a) ",
       x = "", y = " ") +
  theme_minimal(base_size = 8) +
  theme(axis.text.y = element_text(size = 7),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        plot.title = element_text(hjust = 0, margin = margin(l=-10)))

spatial

#export to working directory
ggsave("Spatial_scales_Taxa.tif",
       plot = spatial,
       width = 4.5,
       height = 3,
       units = "in",
       dpi = 600,
       device = "tiff",
       compression = "lzw")




#intervention types and biodiversity (taxa) pairings; no need for data wrangling

df.raw <- read_excel("data.xlsx", sheet = "intervention")

df.i<- df.raw [ , 2:6]

head(df.i)

df.i$interv_types<-as.factor(df.i$interv_types)

str(df.i)

#unique spatial scales
unique_interv <- unique(df.i$interv_types)
print(unique_interv)

# Count rows per scales
table(df.i$interv_types)


#Summarize proportion
summary_interv<- df.i %>%
  group_by(interv_types) %>%
  summarise(across(
    c(Birds, Fishes, Inverts, Plants),
    ~ sum(.x== 1, na.rm = TRUE) / sum(.x %in% c(0, 1), na.rm = TRUE),
    .names = "{.col}"
  )) %>%
  ungroup()

print(summary_interv)

#study counts intervention * taxa pairings
studies<- df.i %>%
  group_by(interv_types) %>%
  summarise(across(
    c(Birds, Fishes, Inverts, Plants),
    ~ sum(.x %in% c(0,1), na.rm = TRUE),
    .names = "{.col}_n"
  )) %>%
  ungroup()

print(studies)

#plotting part, first long table and then plot

#Proportion
long_interv<-summary_interv %>%
  pivot_longer(cols = -interv_types,
               names_to = "Taxa",
               values_to = "Proportion")

#total study n 
int_counts_long <- studies %>%
  pivot_longer(cols = -interv_types,
               names_to = "Taxa",
               values_to = "n")%>%
  mutate(Taxa = str_remove(Taxa, "_n"))

head(int_counts_long)

#join proportion and counts
long_interv <- long_interv %>%
  left_join(int_counts_long, by = c("interv_types", "Taxa"))

# Define the order
interv_order <- c("DAMR", "DIVE", "EFLO", "LOFL", "LSET", "POOL", "PUMP",  
                  "SIDE", "SLDI", "VEGM", "WABO")

interv_label <- c("Dam Removal", "Diversion", "E-Flows", "Lower Floodplain", 
                  "Levee Setback", "Pond Creation", "Pumping",  
                  "Side Channel", "Dike Slotting", "Veg. Management", "Water Body")

#apply the order
long_interv<-long_interv %>%
  mutate(interv_types = factor(interv_types, 
                               levels = interv_order,
                               labels = interv_label))

# Heatmap Plot intervention * taxa pairings
intervention<-ggplot(long_interv, aes(x = Taxa, y = interv_types, fill = Proportion)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(is.nan(Proportion), "NA", 
                               paste0(sprintf("%.2f", Proportion), "\n(n=", n, ")"))),
            size = 2, color = "black") +
  scale_fill_gradient(low = "red", high = "forestgreen",
                      na.value = "grey85",
                      name = "Proportion\nof positive\nimpacts",
                      limits = c(0, 1)) +
  labs(title = "(b) ",
       x = "", y = " ") +
  theme_minimal(base_size = 8) +
  theme(axis.text.y = element_text(size = 7),
        axis.text.x = element_text(angle = 35, hjust = 1, size = 7),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        plot.title = element_text(hjust = 0, margin = margin(l=-10)))

intervention

#export
ggsave("Interventions_Taxa.tif",
       plot = intervention,
       width = 4.5,
       height = 6,
       units = "in",
       dpi = 600,
       device = "tiff",
       compression = "lzw")

#Combining two plots into one
combined <- spatial / intervention +
  plot_layout(heights = c(0.32, 0.68))
combined

#save
ggsave("Combined_heatmap.tif",
       plot = combined,
       width = 5,
       height = 8.5,
       dpi = 600,
       device = "tiff",
       compression = "lzw")

#using patchwork for a shared legend
combined_n <- spatial / intervention +
  plot_layout(heights = c(0.29, 0.71), guides = "collect") &
  theme(legend.position = "right",
        legend.justification = c(0, 0.7))
combined_n

#save
ggsave("Heatmap_combined_legend.tif",
       plot = combined_n,
       width = 5,
       height = 8.5,
       dpi = 600,
       device = "tiff",
       compression = "lzw")
