#importing my data from excel files
library(readxl)
bird <- read_excel("bird.xlsx")
View(bird)

#bird point count data shannon diversity

point_species_abundance_N <- sum(bird_point$count[1:5]) #sums the first 5 values in the "count" column for the bird point count data - these were all the individuals from the northern plot
point_species_abundance_N
point_species_abundance_S <- sum(bird_point$count[6:9]) #do the same for the values in rows 6-9 from the "count" column - these were all the individuals from the southern plot
point_species_abundance_S
point_relative_abundance_N <- bird_point$count[1:5] / point_species_abundance_N #calculates the proportion of each species in the northern plot (rows 1-5) compared to the total number of individuals from the northern plot
point_relative_abundance_N
point_relative_abundance_S <- bird_point$count[6:9] / point_species_abundance_S #calculates the same for the southern plot
point_relative_abundance_S
point_shannon_index_N <- -sum(point_relative_abundance_N * log(point_relative_abundance_N)) #calculates the sum of all the proportion values multiplied by the natural log of their proportion for the northern plot, then makes all values positive. This is for the northern plot.
point_shannon_index_N #1.146596
point_shannon_index_S <- -sum(point_relative_abundance_S * log(point_relative_abundance_S)) #calculates the same for the southern plot
point_shannon_index_S #1.277034

#repeat the steps above for all datasets of different taxa/sampling methods

#bird transect walk shannon diversity

transect_species_abundance_N <- sum(bird_transect$count[1:9])
transect_species_abundance_N
transect_species_abundance_S <- sum(bird_transect$count[10:19])
transect_species_abundance_S
transect_relative_abundance_N <- bird_transect$count[1:9] / transect_species_abundance_N
transect_relative_abundance_N
transect_relative_abundance_S <- bird_transect$count[10:19] / transect_species_abundance_S
transect_relative_abundance_S
transect_shannon_index_N <- -sum(transect_relative_abundance_N * log(transect_relative_abundance_N))
transect_shannon_index_N #1.619103
transect_shannon_index_S <- -sum(transect_relative_abundance_S * log(transect_relative_abundance_S))
transect_shannon_index_S #1.994778

bird_species_abundance_N <- sum(bird$count[1:11]) #combined bird data from transects and point counts to get an overall measure of bird diversity
bird_species_abundance_N
bird_species_abundance_S <- sum(bird$count[12:22])
bird_species_abundance_S
bird_relative_abundance_N <- bird$count[1:11] / bird_species_abundance_N
bird_relative_abundance_N
bird_relative_abundance_S <- bird$count[12:22] / bird_species_abundance_S
bird_relative_abundance_S
bird_shannon_index_N <- -sum(bird_relative_abundance_N * log(bird_relative_abundance_N))
bird_shannon_index_N #1.827353
bird_shannon_index_S <- -sum(bird_relative_abundance_S * log(bird_relative_abundance_S))
bird_shannon_index_S #2.092463

#terrestrial invertebrate shannon diversity

invert_species_abundance_N <- sum(invert$count[1:37])
invert_species_abundance_N
invert_species_abundance_S <- sum(invert$count[38:66])
invert_species_abundance_S
invert_relative_abundance_N <- invert$count[1:37] / invert_species_abundance_N
invert_relative_abundance_N
invert_relative_abundance_S <- invert$count[38:66] / invert_species_abundance_S
invert_relative_abundance_S
invert_shannon_index_N <- -sum(invert_relative_abundance_N * log(invert_relative_abundance_N))
invert_shannon_index_N #2.840421
invert_shannon_index_S <- -sum(invert_relative_abundance_S * log(invert_relative_abundance_S))
invert_shannon_index_S #2.542197

#aquatic invertebrate shannon diversity

aquatic_species_abundance_N <- sum(aquatic$count[1:6])
aquatic_species_abundance_N
aquatic_species_abundance_S <- sum(aquatic$count[7:13])
aquatic_species_abundance_S
aquatic_relative_abundance_N <- aquatic$count[1:6] / aquatic_species_abundance_N
aquatic_relative_abundance_N
aquatic_relative_abundance_S <- aquatic$count[7:13] / aquatic_species_abundance_S
aquatic_relative_abundance_S
aquatic_shannon_index_N <- -sum(aquatic_relative_abundance_N * log(aquatic_relative_abundance_N))
aquatic_shannon_index_N #1.40483
aquatic_shannon_index_S <- -sum(aquatic_relative_abundance_S * log(aquatic_relative_abundance_S))
aquatic_shannon_index_S #1.826863

#tree shannon diversity

tree_species_abundance_N <- sum(trees$count[1:4])
tree_species_abundance_N
tree_species_abundance_S <- sum(trees$count[5:9])
tree_species_abundance_S
tree_relative_abundance_N <- trees$count[1:4] / tree_species_abundance_N
tree_relative_abundance_N
tree_relative_abundance_S <- trees$count[5:9] / tree_species_abundance_S
tree_relative_abundance_S
tree_shannon_index_N <- -sum(tree_relative_abundance_N * log(tree_relative_abundance_N))
tree_shannon_index_N #0.953271
tree_shannon_index_S <- -sum(tree_relative_abundance_S * log(tree_relative_abundance_S))
tree_shannon_index_S #1.157338

#mammal shannon diveristy (captured by camera traps) discounted this as there was not a sufficient amount of data

camera_species_abundance_N <- sum(cameras$count[1:4])
camera_species_abundance_N
camera_species_abundance_S <- sum(cameras$count[5])
camera_species_abundance_S
camera_relative_abundance_N <- cameras$count[1:4] / camera_species_abundance_N
camera_relative_abundance_N
camera_relative_abundance_S <- cameras$count[5] / camera_species_abundance_S
camera_relative_abundance_S
camera_shannon_index_N <- -sum(camera_relative_abundance_N * log(camera_relative_abundance_N))
camera_shannon_index_N #1.153742
camera_shannon_index_S <- -sum(camera_relative_abundance_S * log(camera_relative_abundance_S))
camera_shannon_index_S #0

#calculating the sum of all the diveristies and their mean

sum(c(bird_shannon_index_N,invert_shannon_index_N,aquatic_shannon_index_N,tree_shannon_index_N))
#For northern plot sum=7.025875, mean=1.756469

sum(c(bird_shannon_index_S,invert_shannon_index_S,aquatic_shannon_index_S,tree_shannon_index_S))
#For southern plot sum=7.618861, mean= 1.904715

# Load ggplot2 library
library(ggplot2)

# Create a data frame of shannon diversity values for each group
diversity_data <- data.frame(
  Plot = c("Northern plot", "Southern plot"), #rows named "Northern plot" and "Southern plot"
  Shannon_diversity_index = c(1.756469, 1.904715) #values in columns
)
# Create a bar plot of this data frame
ggplot(diversity_data, aes(x = Plot, y = Shannon_diversity_index, fill = Plot)) +
  geom_bar(stat = "identity") +
  labs(x = "Plot",
       y = "Mean Shannon Diversity Index") +
  theme_minimal()

