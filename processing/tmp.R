

# Mixed effects models

Make a series of mixed effects models.

```{r}
m0 = glmer(
  answer ~ 1 + 
    (1 | partID) +         # participant
    (0 + context | partID) +         
    (1 | contextSample) +   # context sample 
    (1 | responseSample),   # response sample
  data = d,
  family = binomial
)


m1 = glmer(
  answer ~ 1 + context +
    (1 | partID) +         # participant
    (0 + context | partID) +         
    (1 | contextSample) +   # context sample 
    (1 | responseSample),   # response sample
  data = d,
  family = binomial
)

m2 = glmer(
  answer ~ 1 + context + responsePhoneme +
    (1 | partID) +         # participant
    (0 + context | partID) +         
    (1 | contextSample) +   # context sample 
    (1 | responseSample),   # response sample
  data = d,
  family = binomial
)

m3 = glmer(
  answer ~ 1 + context * responsePhoneme +
    (1 + responsePhoneme*context| partID) +         # participant
    (1 | contextSample),    # context sample 
    #(1 | responseSample),   # response sample
  data = d,
  family = binomial
)
```

## Results

Model comparison

```{r}
anova(m0,m1,m2,m3)
```

Model estimates:
  
  ```{r}
summary(m1)
```

`r getMEText(anova(m0,m1), "main effect of context")`

`r getMEText(anova(m1,m2), "main effect of phoneme")`

`r getMEText(anova(m2,m3), "interaction between context and phoneme")`

Work out model esimates for probabilities in each condition:
  
  ```{r}
# prob of responding 'yes' when:
# Context = IN, no response
logit2per(fixef(m3)[1])
# Context = ST, no response
logit2per(fixef(m3)[1] + fixef(m3)[2])

```



## Plots

Fixed effects estimates:
  
  ```{r}

feLabels = matrix(c(
  "(Intercept)"             ,"Intercept"      , NA,           
  "contextST", "Context = Statement", "m1",
  "responsePhonemenone", "no response", 'm2',
  "responsePhonemewh", "wh response", 'm2'
  "contextIN:responsePhonemenone", "Context: no response", "m3",
  "contextIN:responsePhonemewh", "Context: wh response", "m3"
), ncol=3, byrow = T)

feLabels2 = as.vector(feLabels[match(names(fixef(m3)),feLabels[,1]),2])


```



```{r}
sjp.glmer(m3,'fe',
          show.intercept = T,
          geom.colors = c(1,1),
          axis.title = "Odds of selecting question"
)
```




# Raw data plots

```{r}
plotmeans(answer ~ responsePhoneme, data=d)
plotmeans(answer ~ context, data=d)
plotmeans(answer ~ responseType, data=d)

plotmeans(answer ~ paste(responseType,responsePhoneme), data=d)

plotmeans(answer ~ responsePhoneme, data=d[d$context=="IN",], col=1, barcol = 1,ylim=c(0,1))
par(new=T)
plotmeans(answer ~ responsePhoneme, data=d[d$context=="ST",] ,col=2, barcol = 2,ylim=c(0,1))



plotmeans(answer ~ responsePhoneme, data=d[d$context=="IN" & d$responseType=="Q",], col=1, barcol = 1,ylim=c(0,1))
par(new=T)
plotmeans(answer ~ responsePhoneme, data=d[d$context=="ST" & d$responseType=="Q",] ,col=2, barcol = 2,ylim=c(0,1))

```




```{r}
p <- ggplot(d, aes(responsePhoneme,answer, fill=context))
#p + geom_violin() + stat_summary(fun.y=mean, geom="point", size=4, color="red")

p + geom_boxplot(width=0.1) 
#+
#  theme(text=element_text(size=20), legend.position="none") +
#  scale_y_continuous(name="Score ranking")+
#  scale_x_discrete(name="")+
#  scale_fill_grey(start = 0.55, end=0.8)
```

# Is there an effect of the type of turn the repsonse sample is taken from?

```{r}

m_X0 = glmer(
  answer ~ 1 + context + (responsePhoneme + responseType) +
    (1 | partID) +         # participant
    (0 + context | partID) +         
    (1 | contextSample) +   # context sample 
    (1 | responseSample),   # response sample
  data = d[d$responseType!='none',],
  family = binomial
)


m_X = glmer(
  answer ~ 1 + context + (responsePhoneme * responseType) +
    (1 | partID) +         # participant
    (0 + context | partID) +         
    (1 | contextSample) +   # context sample 
    (1 | responseSample),   # response sample
  data = d[d$responseType!='none',],
  family = binomial
)

anova(m_X0, m_X)
summary(m_X)
```