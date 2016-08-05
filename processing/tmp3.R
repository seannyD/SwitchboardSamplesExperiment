## Plots based on model esimates

(NOT QUITE WORKING YET!)

```{r}
fitfram <- stats::model.frame(m3)
fitfram$predicted.values <- stats::predict(m3, newdata = fitfram, 
                                           type = "response", re.form = NA)

sumStatsX = group_by(fitfram, partID, context,responsePhoneme ) %>%
  summarise(mean = mean(predicted.values))
sumStatsX2 = summarySE(sumStatsX, measurevar="mean",   
                       groupvars=c("context","responsePhoneme"))

model.plot <- ggplot(sumStatsX2,
                     aes(x = responsePhoneme, y = mean, colour=context)) +
  geom_point() + geom_line(aes(group=context)) +
  geom_errorbar(aes(ymax=mean+ci, ymin=mean-ci), width=0.25) +
  xlab("First Phoneme in Response") +
  ylab("Proportion of 'Question' responses") +
  coord_cartesian(ylim=c(0,1)) + 
  scale_color_discrete(breaks=c("ST","IN"),
                       labels=c("Statement","Initiating"),
                       name="Context")
model.plot

pdf("../results/graphs/PropQResponses_by_firstPhoneme_withPartCI_ModelEstimates.pdf",
    width = 4, height=3)
model.plot
dev.off()

main.plot2 <- ggplot(sumStatsX2,
                     aes(x = context, y = mean, colour=responsePhoneme)) +
  geom_point(position=dodge) + geom_line(aes(group=responsePhoneme), position=dodge) +
  geom_errorbar(aes(ymax=mean+ci, ymin=mean-ci), width=0.25, position=dodge) +
  xlab("Previous turn type (context)") +
  ylab("Proportion of 'Question' responses") +
  coord_cartesian(ylim=c(0,1)) + 
  scale_color_discrete(breaks=c("none","other",'wh'),
                       labels=c("None","Non-wh","wh"),
                       name="First phoneme") +
  scale_x_discrete(breaks=c("ST", "IN"),
                   labels=c("Statement", "Initiating"))

main.plot2

pdf("../results/graphs/PropQResponses_by_context_withPartCI_ModelEstimates.pdf",
    width = 4, height=3)
main.plot2
dev.off()


```