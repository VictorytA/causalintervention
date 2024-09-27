#Creates a blacklist matrix for the scorer

labels <- colnames(data)

blacklist <- matrix(FALSE, nrow = length(labels), ncol = length(labels))
rownames(blacklist) <- labels
colnames(blacklist) <- labels

for (i in 1:14) {
  if (grepl("_after", labels[i])) {
    for (j in 1:14) {
      if (grepl("_base", labels[j])) {
        blacklist[i, j] <- TRUE
      }
    }
  }
}
save(blacklist, file = "blacklist.RD")
