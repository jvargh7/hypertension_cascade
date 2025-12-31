fig = ggplot(data=nfhs5_df,aes(x=sbp,y=dbp)) +
  # geom_point(alpha = 0.1) +
  geom_bin2d(bins = 100) +
  geom_vline(xintercept = c(130,140)) +
  geom_hline(yintercept = c(80,90)) +
  theme_bw() +
  scale_fill_continuous(type = "viridis")

fig
