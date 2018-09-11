

require(dplyr)
library(devtools)
load_all()
data("nace_rev2")

df = data.frame(
  ID = sample(nace_rev2[[4]]$id, 200),
  x = runif(200)
)

table(cast_level(df$ID, nace_rev2, 1, T)) %>% as.data.frame()


df %>% filter(cast_level(ID, nace_rev2, 1,F) == "A") %>%
  group_by(cast_level(ID, nace_rev2, 3)) %>%
  summarise(x = sum(x))


