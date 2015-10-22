t = 1
v = 1:V

pdf("~/SCIOME/Writing/Figures/Truth_Post_Mean_piecewise_101_1.pdf")
k = 1
Comparison = data.frame(v, MCMC = Prob_Beta[[t]][k,], Truth = Prob_Beta_truth[[t]][3,],  CI.025 = Prob_Beta_CI[[t]][[k]][1,], CI.975 = Prob_Beta_CI[[t]][[k]][2,]) 
g = ggplot(Comparison, aes(x=v) ) + 
  geom_point(aes(y=MCMC, color = "MCMC")) + 
  geom_segment(mapping = aes(x = v, y=CI.025, xend = v, yend = CI.975, color = "MCMC"),size = .5, alpha = .10 ) +
  geom_point(aes(y =Truth, color = "Truth")) +
  scale_color_manual("", breaks = c("MCMC", "Truth"), values =c("MCMC" = "blue", "Truth" = "black") ) + 
  xlab("Vocabulary Term") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

pdf("~/SCIOME/Writing/Figures/Truth_Post_Mean_piecewise_101_2.pdf")
k = 2
Comparison = data.frame(v, MCMC = Prob_Beta[[t]][k,], Truth = Prob_Beta_truth[[t]][3,],  CI.025 = Prob_Beta_CI[[t]][[k]][1,], CI.975 = Prob_Beta_CI[[t]][[k]][2,]) 
g = ggplot(Comparison, aes(x=v) ) + 
  geom_point(aes(y=MCMC, color = "MCMC")) + 
  geom_segment(mapping = aes(x = v, y=CI.025, xend = v, yend = CI.975, color = "MCMC"),size = .5, alpha = .10 ) +
  geom_point(aes(y =Truth, color = "Truth")) +
  scale_color_manual("", breaks = c("MCMC", "Truth"), values =c("MCMC" = "blue", "Truth" = "black") ) + 
  xlab("Vocabulary Term") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

pdf("~/SCIOME/Writing/Figures/Truth_Post_Mean_piecewise_101_3.pdf")
k = 3
Comparison = data.frame(v, MCMC = Prob_Beta[[t]][k,], Truth = Prob_Beta_truth[[t]][3,],  CI.025 = Prob_Beta_CI[[t]][[k]][1,], CI.975 = Prob_Beta_CI[[t]][[k]][2,]) 
g = ggplot(Comparison, aes(x=v) ) + 
  geom_point(aes(y=MCMC, color = "MCMC")) + 
  geom_segment(mapping = aes(x = v, y=CI.025, xend = v, yend = CI.975, color = "MCMC"),size = .5, alpha = .10 ) +
  geom_point(aes(y =Truth, color = "Truth")) +
  scale_color_manual("", breaks = c("MCMC", "Truth"), values =c("MCMC" = "blue", "Truth" = "black") ) + 
  xlab("Vocabulary Term") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

pdf("~/SCIOME/Writing/Figures/Truth_Post_Mean_piecewise_101_4.pdf")
k = 4
Comparison = data.frame(v, MCMC = Prob_Beta[[t]][k,], Truth = Prob_Beta_truth[[t]][3,],  CI.025 = Prob_Beta_CI[[t]][[k]][1,], CI.975 = Prob_Beta_CI[[t]][[k]][2,]) 
g = ggplot(Comparison, aes(x=v) ) + 
  geom_point(aes(y=MCMC, color = "MCMC")) + 
  geom_segment(mapping = aes(x = v, y=CI.025, xend = v, yend = CI.975, color = "MCMC"),size = .5, alpha = .10 ) +
  geom_point(aes(y =Truth, color = "Truth")) +
  scale_color_manual("", breaks = c("MCMC", "Truth"), values =c("MCMC" = "blue", "Truth" = "black") ) + 
  xlab("Vocabulary Term") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"),legend.text=element_text(size=20) )
dev.off()

pdf("~/SCIOME/Writing/Figures/Truth_Post_Mean_piecewise_101_5.pdf")
k = 5
Comparison = data.frame(v, MCMC = Prob_Beta[[t]][k,], Truth = Prob_Beta_truth[[t]][3,],  CI.025 = Prob_Beta_CI[[t]][[k]][1,], CI.975 = Prob_Beta_CI[[t]][[k]][2,]) 
g = ggplot(Comparison, aes(x=v) ) + 
  geom_point(aes(y=MCMC, color = "MCMC")) + 
  geom_segment(mapping = aes(x = v, y=CI.025, xend = v, yend = CI.975, color = "MCMC"),size = .5, alpha = .10 ) +
  geom_point(aes(y =Truth, color = "Truth")) +
  scale_color_manual("", breaks = c("MCMC", "Truth"), values =c("MCMC" = "blue", "Truth" = "black") ) + 
  xlab("Vocabulary Term") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"),legend.text=element_text(size=20) )
dev.off()

pdf("~/SCIOME/Writing/Figures/Truth_Post_Mean_piecewise_101_6.pdf")
k = 6
Comparison = data.frame(v, MCMC = Prob_Beta[[t]][k,], Truth = Prob_Beta_truth[[t]][3,],  CI.025 = Prob_Beta_CI[[t]][[k]][1,], CI.975 = Prob_Beta_CI[[t]][[k]][2,]) 
g = ggplot(Comparison, aes(x=v) ) + 
  geom_point(aes(y=MCMC, color = "MCMC")) + 
  geom_segment(mapping = aes(x = v, y=CI.025, xend = v, yend = CI.975, color = "MCMC"),size = .5, alpha = .10 ) +
  geom_point(aes(y =Truth, color = "Truth")) +
  scale_color_manual("", breaks = c("MCMC", "Truth"), values =c("MCMC" = "blue", "Truth" = "black") ) + 
  xlab("Vocabulary Term") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


# Plot the uncertainty bands for the trends
K = 6

pdf("~/SCIOME/Writing/Figures/Topic_Proportions_K6.pdf")
Comparison = data.frame(Time = rep(1:t.T, times = K), Probability = c( sapply(1:K, function(k) Prob_Z[[k]][1,] ) ), CI.025 = c( sapply(1:K, function(k) Prob_Z[[k]][2,] ) ), CI.975 = c( sapply(1:K, function(k) Prob_Z[[k]][3,] ) ), Topic = factor(rep(1:K, each=t.T)) ) 
g = ggplot(Comparison, aes(x=Time) ) + 
  geom_line(aes(y=Probability, group = Topic, color = Topic), size = 4) + 
  #geom_segment(mapping = aes(x = Time, y=CI.025, xend = Time, yend = CI.975, color = Topic),size = 3, alpha = .40 ) +
  geom_line(aes(x = Time, y=CI.025, group = Topic, color = Topic), size = 1, alpha = .80, linetype=2 ) +
  geom_line(aes(x = Time, y=CI.975, group = Topic, color = Topic), size = 1, alpha = .80, linetype=2 ) +
  xlab("Time") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))

dev.off()


