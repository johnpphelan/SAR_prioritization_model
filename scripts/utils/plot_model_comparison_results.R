library(tidyverse)
library(sf)
library(patchwork)

# Load the data
modelA <- read.xlsx("output/SARA_prioritization_model_2025-06-13.xlsx")
modelB <- read.xlsx("output/SARA_prioritization_model_2025-05-22.xlsx")

modelA <- modelA[, !(is.na(names(modelA)) | names(modelA) == "")]
modelB <- modelB[, !(is.na(names(modelB)) | names(modelB) == "")]
top_wb_A <- modelA |> 
  arrange(desc(!is.na(final_risk))) |> 
  slice(1:30)
top_wb_B <- modelB |>
  arrange(desc(!is.na(final_risk))) |> 
  slice(1:30)
#plotting the top 30 wbs names and final_risk



ggplot() +
  geom_point(data = top_wb_A, 
             aes(x = reorder(waterbody, final_risk), 
                 y = final_risk, 
                 color = "Summed Effect Model", 
                 shape = Common_Name_EN), 
             size = 3) +
  geom_point(data = top_wb_B, 
             aes(x = reorder(waterbody, final_risk), 
                 y = final_risk, 
                 color = "Max Effect Model", 
                 shape = Common_Name_EN), 
             size = 3) +
  labs(title = "Top 30 Waterbodies by Final Risk",
       x = "Waterbody",
       y = "Final Risk Score",
       color = "Model",
       shape = "Species") +
  scale_color_manual(values = c("Summed Effect Model" = "blue", "Max Effect Model" = "brown")) +
  theme_minimal() +
  scale_x_discrete(limits = rev) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# now we do the same but facet by common name
ggplot() +
  geom_point(data = modelA, 
             aes(x = reorder(waterbody, final_risk), 
                 y = final_risk, 
                 color = "Summed Effect Model"), 
             size = 3) +
  geom_point(data = modelB, 
             aes(x = reorder(waterbody, final_risk), 
                 y = final_risk, 
                 color = "Max Effect Model"), 
             size = 3) +
  labs(title = "Top 30 Waterbodies by Final Risk",
       x = "Waterbody",
       y = "Final Risk Score",
       color = "Model") +
  scale_color_manual(values = c("Summed Effect Model" = "blue", "Max Effect Model" = "brown")) +
  theme_minimal() +
  scale_x_discrete(limits = rev) +
  facet_wrap(~ Common_Name_EN) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# investigations of the differences, include the common_name_en
diff <- modelA %>%
  select(waterbody, Common_Name_EN, final_risk) %>%
  rename(final_risk_A = final_risk) %>%
  left_join(modelB %>% 
              select(waterbody, Common_Name_EN, final_risk) %>%
              rename(final_risk_B = final_risk), 
            by = c("waterbody", "Common_Name_EN")) %>%
  mutate(diff = final_risk_B - final_risk_A)


summary(diff)
ggplot(diff, aes(x = final_risk_A, y = final_risk_B)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison of Final Risk Scores",
       x = "Final Risk Score (Summed Effect Model)",
       y = "Final Risk Score (Max Effect Model)") +
  theme_minimal()


# Filter out rows where 'diff' is NA or missing
diff_filtered <- diff |> 
  filter(!is.na(diff))

# Plot
ggplot(diff_filtered, aes(x = reorder(waterbody, diff), y = diff)) +
  geom_bar(stat = "identity") +
  labs(title = "Difference in Final Risk Scores by Waterbody",
       x = "Waterbody",
       y = "Difference (Max - Summed)") +
  theme_minimal() +
  scale_x_discrete(limits = rev) +
  facet_wrap(~ Common_Name_EN, scales = "free_x") +  # Optional: free_x for better spacing
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


unique_species <- unique(diff_filtered$Common_Name_EN)

for (species in unique_species) {
  species_data <- diff_filtered %>% filter(Common_Name_EN == species)
  
  p <- ggplot(species_data, aes(x = reorder(waterbody, diff), y = diff)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Difference in Final Risk Scores by Waterbody\nSpecies:", species),
         x = "Waterbody",
         y = "Difference (Max - Summed)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

#get the waterbodies that are listed in the top 30 of each model 
named_wbs = readRDS("data/named_lakes_and_rivers.rds")
#join by what is in ModelA and then modelB
top_wb_A <- top_wb_A |> 
  left_join(named_wbs, by = "waterbody") |> 
  select(waterbody, Common_Name_EN, final_risk, geometry)
top_wb_B <- top_wb_B |> 
  left_join(named_wbs, by = "waterbody") |> 
  select(waterbody, Common_Name_EN, final_risk, geometry)
#and diff
diff <- diff |> 
  left_join(named_wbs, by = "waterbody") |> 
  select(waterbody, Common_Name_EN, final_risk_A, final_risk_B, diff, geometry)

# Convert to sf objects
top_wb_A_sf <- st_as_sf(top_wb_A)
top_wb_B_sf <- st_as_sf(top_wb_B)

# Plotting the top 30 waterbodies on a map

p1 <- ggplot() +
  geom_sf(data = top_wb_A_sf, aes(fill = final_risk, color = final_risk), size = 3) +
  labs(title = "Top 30 Waterbodies: Summed",
       fill = "Final Risk",
       color = "Final Risk",
       shape = "Species") +
  scale_fill_viridis_c(option = "D") +
  scale_color_viridis_c(option = "D") +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- ggplot() +
  geom_sf(data = top_wb_B_sf, aes(fill = final_risk, color = final_risk), size = 3) +
  labs(title = "Top 30 Waterbodies: Max",
       fill = "Final Risk",
       color = "Final Risk",
       shape = "Species") +
  scale_fill_viridis_c(option = "D") +
  scale_color_viridis_c(option = "D") +
  theme_minimal() +
  theme(legend.position = "bottom")


p1 + p2

# Plotting the differences on a map
diff_sf <- st_as_sf(diff)
ggplot() +
  geom_sf(data = diff_sf, aes(fill = diff), size = 3) +
  labs(title = "Difference in Final Risk Scores (Max - Summed)",
       fill = "Difference") +
  scale_fill_viridis_c(option = "D") +
  facet_wrap(~ Common_Name_EN) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot for each species using inline bbox expansion
for (species in unique_species) {
  species_data <- diff_sf |> filter(Common_Name_EN == species)
  
  bbox <- st_bbox(species_data)
  
  p <- ggplot() +
    geom_sf(data = st_as_sf(species_data), aes(fill = diff), size = 3) +
    coord_sf(xlim = c(bbox["xmin"] - 0.1, bbox["xmax"] + 0.1),
             ylim = c(bbox["ymin"] - 0.1, bbox["ymax"] + 0.1)) +
    labs(title = paste("Difference in Final Risk Scores by Waterbody\nSpecies:", species),
         fill = "Difference") +
    scale_fill_viridis_c(option = "D") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}


# only plot those with a difference of +- 2
diff_filtered <- diff |> 
  filter(!is.na(diff) & abs(diff) >= 2)
# Plot the filtered differences
diff_filtered<-st_as_sf(diff_filtered)
for (species in unique_species) {
  species_data <- diff_filtered |> filter(Common_Name_EN == species)
  
  bbox <- st_bbox(species_data)
  
  p <- ggplot() +
    geom_sf(data = st_as_sf(species_data), aes(fill = diff), size = 3) +
    coord_sf(xlim = c(bbox["xmin"] - 0.1, bbox["xmax"] + 0.1),
             ylim = c(bbox["ymin"] - 0.1, bbox["ymax"] + 0.1)) +
    labs(title = paste("Difference in Final Risk Scores by Waterbody\nSpecies:", species),
         fill = "Difference") +
    scale_fill_viridis_c(option = "D") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}



# Define key columns
key_cols <- c("waterbody", "watershed", "Common_Name_EN", "Population_EN")

# Rows in modelA not in modelB
modelA_only <- anti_join(modelA, modelB, by = key_cols)

# Rows in modelB not in modelA
modelB_only <- anti_join(modelB, modelA, by = key_cols)

# View the differences
nrow(modelA_only)
nrow(modelB_only)


# Calculate Pearson correlation
pearson_result <- cor.test(modelA$final_risk, modelB$final_risk, method = "pearson")

# Calculate Spearman correlation
spearman_result <- cor.test(model_comparison$model_a_risk, model_comparison$model_b_risk, method = "spearman")

# Print results
print(pearson_result)
print(spearman_result)

