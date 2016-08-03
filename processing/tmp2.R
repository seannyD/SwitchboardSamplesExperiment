d = read.csv("../Data/Lab_and_Online_data_Processed.csv")

d = d[d$contextSample != 'IN 18',]
d = d[as.character(d$partID)!="13",]

d$answer = d$answer=="Yes"

m3 = bglmer(
  answer ~ 1 + context * responsePhoneme +
    (1 + context + responsePhoneme | partID)+         # participant
    (1 | contextSample)+ 
    (1 | responseSample) ,    # context sample 
  data = d,
  family = binomial,cov.prior = gamma()
)

m3 = glmer(
  answer ~ 1 + context * responsePhoneme +
    (1 + context + responsePhoneme | partID) +         # participant
    (1 | contextSample) +
    (1 | responseSample) ,    # context sample 
  data = d,
  family = binomial
)


m3 = glmer(
  answer ~ 1 + paste(context, responsePhoneme) +
    (1 + paste(context, responsePhoneme) | partID) +         # participant
    (1 | contextSample) +
    (1 | responseSample) ,    # context sample 
  data = d,
  family = binomial
)

# what if choices were random?
d$answer = sample(c(T,F),nrow(d), replace=T)



#########

library("numDeriv")

m3 = glmer(
  answer ~ 1 + context * responsePhoneme +
    (1 + context + responsePhoneme | partID) +         # participant
    (1 | contextSample) +
    (1 | responseSample),    # context sample 
  data = d,
  family = binomial
)

# Check singularity
tt <- getME(m3,"theta")
ll <- getME(m3,"lower")
min(tt[ll==0])

# Check gradient calculations

derivs1 <- m3@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))

# Low but not problematic

max(pmin(abs(sc_grad1),abs(derivs1$gradient)))

# Low but not problematic

# Try recalculaing with numDeriv library
dd <- update(m3,devFunOnly=TRUE)
pars <- unlist(getME(m3,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2)))
# Not really different

# Try longer convergence time (restarting)
ss <- getME(m3,c("theta","fixef"))
m3B <- update(m3,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

# lots more iterations
m3_alt = glmer(
  answer ~ 1 + context * responsePhoneme +
    (1 + context + responsePhoneme | partID) +         # participant
    (1 | contextSample) +
    (1 | responseSample),    # context sample 
  data = d,
  family = binomial,
  control = glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e4))
)
m3B <- update(m3,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

# This seems to reach convergence, but note that we've changed two things here:
#  The maximum number of iterations and the optimizer used in the second phase
#  (by deafault, glmer uses bobyqa phase 1, Nelder-Mead for stage 2, explicitly
#   stating bobyqa as the optimiser sets it for both phases)

#####

#Try lots of different optimisers
library("RCurl")
library("ggplot2")
afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/inst/utils/allFit.R"
eval(parse(text=getURL(afurl)))

aa <- allFit(m3)

is.OK <- sapply(aa,is,"merMod")  ## nlopt NELDERMEAD failed, others succeeded
aa.OK <- aa[is.OK]
lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)


# look at log likelihoods
(lliks <- sort(sapply(aa.OK,logLik)))

# Fixed effects estimates
aa.fixef <- t(sapply(aa.OK,fixef))
aa.fixef.m <- melt(aa.fixef)
models <- levels(aa.fixef.m$Var1)
ylabs <- substr(models,1,3)
aa.fixef.m <- transform(aa.fixef.m,Var1=factor(Var1,levels=names(lliks)))
(gplot1 <- ggplot(aa.fixef.m,aes(x=value,y=Var1,colour=Var1))+geom_point()+
  facet_wrap(~Var2,scale="free")+
  scale_colour_brewer(palette="Dark2")+
  scale_y_discrete(breaks=models,
                   labels=ylabs)+
  labs(x="",y=""))
