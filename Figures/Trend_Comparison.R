Z_prob_truth = matrix(nrow = K*t.T, ncol = 2)

for(k in 1:K){
  for(t in 1:t.T){
    index = (k-1)*t.T + t 
    Z_prob_truth[index,1] = sum(Z_truth[[t]]==k)/length(Z_truth[[t]])
    Z_prob_truth[index,2] = k
  }
}



pdf("~/SCIOME/Writing/Figures/Linear_Prob_Trend.pdf")
Comparison = data.frame(Time = rep(1:t.T, times = K), Truth = Z_prob_truth[,1], Probability = c(  t(P.Z.mean) ), CI.025 = c( t(CI.025) ), CI.975 = c( t(CI.975) ), Topic = factor(rep(1:K, each=t.T)) ) 
g = ggplot(Comparison, aes(x=Time) ) + 
  geom_line(aes(y=Probability, group = Topic, color = Topic), size = 3) + 
  geom_line(aes(y=Truth, group = Topic, color = Topic), size = 3, linetype=3)+
  #geom_segment(mapping = aes(x = Time, y=CI.025, xend = Time, yend = CI.975, color = Topic),size = 3, alpha = .40 ) +
  geom_line(aes(x = Time, y=CI.025, group = Topic, color = Topic), size = 1, alpha = .40, linetype=2 ) +
  geom_line(aes(x = Time, y=CI.975, group = Topic, color = Topic), size = 1, alpha = .40, linetype=2 ) +
  xlab("Time") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()