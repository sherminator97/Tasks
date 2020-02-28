#Data
library(spocc)
out <- occ(query = 'Egernia richardi', from = 'gbif', limit = 10, has_coords = TRUE)
dat <- occ2df(out)
head(dat); tail(dat)

out2 <-occ(query = "Ctenophorus maculosus", from ="gbif", limit = 10, has_coords = TRUE)
dat2 <- occ2df(out2)
head(dat2); tail(dat2)

out3 <- occ(query = "Pogona barbata", from ="gbif", limit = 10, has_coords = TRUE)
dat3 <- occ2df(out3)
head(dat3); tail(dat3)

out4 <- occ(query = "Tympanocryptis intima", from ="gbif", limit = 10, has_coords = TRUE)
dat4 <- occ2df(out4)
head(dat4); tail(dat4)

out5 <- occ(query = "Diporiphora australis", from ="gbif", limit = 10, has_coords = TRUE)
dat5 <- occ2df(out5)
head(dat5); tail(dat5)

object <- rbind(dat, dat2, dat3, dat4, dat5)

write.csv(object, "Australian lizards.csv", quote = T, row.names=TRUE)
