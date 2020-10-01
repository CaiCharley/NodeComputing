# run scott data without isoform labelling

rm_label <- function(rep) {
  prots <- rownames(rep)
  no_iso <- str_replace_all(prots, "-*", "")
  rownames(rep) <- no_iso
  return(rep)
}

no_isoforms <- map(bn_unstim, ~rm_label(.))

no_iso_ppis <- PrInCE(
  no_isoforms,
  goldstd,
  classifier = "NB",
  models = 1
)

ml_ppis_noisocorum <- PrInCE(ml_bn_unstim_feats, goldstd_noiso, classifier = "NB", models = 1)

ggplot(scott_precision %>%
    filter(dataset == "bn_unstim"), aes(index, precision)) +
  geom_path(aes(color = lang)) +
  geom_path(data = no_iso_ppis[1:1e4,], aes(1:1e4, precision)) +
  ggtitle("R with isoforms (Blue), R without isoforms (black)")