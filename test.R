#Categorical data

#to do
## [x] create a dummy dataframe
## [x] create dummy meta data
## [x] importing meta data
## [] apply meta data to dataframe using a fun
##    [] - check if data is valid
##    [x] - return labels for keys
##    [] - if not in meta than error
## [] import meta data ad check if meets specified conditions
## [] if time is left: as a package
## [] implement download ressources to other classification systemsö

# packages ####

require(dplyr)
library(devtools)
load_all()
data("nace_rev2")

df = data.frame(
  ID = sample(nace_rev2[[4]]$id, 200),
  x = runif(200)
)

table(cast_level(df$ID, nace_rev2, 4, T)) %>% as.data.frame()


df %>% filter(cast_level(ID, nace_rev2, 1,F) == "A") %>%
  group_by(cast_level(ID, nace_rev2, 2)) %>%
  summarise(x = sum(x))



