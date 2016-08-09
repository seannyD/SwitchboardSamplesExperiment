# Follows https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

library("numDeriv")
library("RCurl")
library("ggplot2")
library("lme4")
library("reshape")

setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/InitialPhonemeExperiment/SwitchboardSamplesExperiment/processing/")
d = read.csv("../Data/Lab_and_Online_data_Processed.csv")

d = d[d$contextSample != 'IN 18',]
d = d[as.character(d$partID)!="13",]




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
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))

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

afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/inst/utils/allFit.R"
eval(parse(text=getURL(afurl)))

aa <- allFit(m3)

# bobyqa : [OK]
# Nelder_Mead : [OK]
# nlminbw : [OK]
# nmkbw : [OK]
# optimx.L-BFGS-B : [OK]
# nloptwrap.NLOPT_LN_NELDERMEAD : [OK]
# nloptwrap.NLOPT_LN_BOBYQA : [OK]


save.image(file="lotsOfModels.Rdat")

is.OK <- sapply(aa,is,"merMod")  ## nlopt NELDERMEAD failed, others succeeded
aa.OK <- aa[is.OK]
lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)

# $bobyqa
# [1] "unable to evaluate scaled gradient"                                       
# [2] "Model failed to converge: degenerate  Hessian with 1 negative eigenvalues"
# 
# $Nelder_Mead
# [1] "Model failed to converge with max|grad| = 2.39857 (tol = 0.001, component 1)"
# 
# $nlminbw
# NULL
# 
# $nmkbw
# $nmkbw[[1]]
# [1] "Model failed to converge with max|grad| = 0.105952 (tol = 0.001, component 1)"
# 
# $nmkbw[[2]]
# [1] "Model is nearly unidentifiable: very large eigenvalue\n - Rescale variables?"
# 
# 
# $`optimx.L-BFGS-B`
# [1] "Model failed to converge with max|grad| = 0.00153727 (tol = 0.001, component 1)"
# 
# $nloptwrap.NLOPT_LN_NELDERMEAD
# [1] "unable to evaluate scaled gradient"                                       
# [2] "Model failed to converge: degenerate  Hessian with 1 negative eigenvalues"
# 
# $nloptwrap.NLOPT_LN_BOBYQA
# [1] "unable to evaluate scaled gradient"                                       
# [2] "Model failed to converge: degenerate  Hessian with 1 negative eigenvalues"


# look at log likelihoods
t(t(lliks <- sort(sapply(aa.OK,logLik))))

# [,1]
# Nelder_Mead                   -897.8444
# nmkbw                         -896.2955
# nloptwrap.NLOPT_LN_NELDERMEAD -896.2457
# nloptwrap.NLOPT_LN_BOBYQA     -896.2457
# bobyqa                        -896.2228
# optimx.L-BFGS-B               -896.1928
# nlminbw                       -896.1928


# Differences in fixed effects estimates
aa.fixef <- t(sapply(aa.OK,fixef))
aa.fixef.m <- melt(aa.fixef)
models <- levels(aa.fixef.m$X1)
ylabs <- substr(models,1,3)
aa.fixef.m <- transform(aa.fixef.m,X1=factor(X1,levels=names(lliks)))
(gplot1 <- ggplot(aa.fixef.m,aes(x=value,y=X1,colour=X1))+geom_point()+
  facet_wrap(~X2,scale="free")+
  scale_colour_brewer(palette="Dark2")+
  scale_y_discrete(breaks=models,
                   labels=ylabs)+
  labs(x="",y=""))


# nlminbw, optimx.L-BFGS-B and bobyqa have very similar esimates
