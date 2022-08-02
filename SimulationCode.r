# relevant libraries

install.packages("MASS")
library("MASS")

install.packages("randomNames")
library("randomNames")


# 12 correlations basic
simulate_test <- function(label, sample_size, Noise, Creativity_SS, Creativity_Plus, 
                          Critical_Thinking_SS, Critical_Thinking_Plus, 
                          Communication_SS, Communication_Plus, 
                          Collaboration_SS, Collaboration_Plus,
                          Mindfulness_SS, Mindfulness_Plus,
                          Curiosity_SS, Curiosity_Plus,
                          Courage_SS, Courage_Plus,
                          Resilience_SS, Resilience_Plus,
                          Ethics_SS, Ethics_Plus,
                          Leadership_SS, Leadership_Plus,
                          Metacognition_SS, Metacognition_Plus,
                          Growth_Mindset_SS, Growth_Mindset_Plus) {
  #PC = point change, from the final points FP minus the starting points SP
  Creativity_SP <- 0.95*Creativity_SS^2+0.5*Creativity_SS
  Creativity_FP <- Creativity_SP + rnorm(sample_size, Creativity_Plus + .14*Critical_Thinking_Plus, Noise*Creativity_Plus)
  Creativity_PC <- Creativity_FP - Creativity_SP 
  
  Critical_Thinking_SP <- 0.95*Critical_Thinking_SS^2+0.5*Critical_Thinking_SS
  Critical_Thinking_FP <- Critical_Thinking_SP + rnorm(sample_size, Critical_Thinking_Plus + .13*Creativity_Plus, Noise*Critical_Thinking_Plus)
  Critical_Thinking_PC <- Critical_Thinking_FP - Critical_Thinking_SP
  
  Communication_SP <- 0.95*Communication_SS^2+0.5*Communication_SS
  Communication_FP <- Communication_SP + rnorm(sample_size, Communication_Plus + .06*Metacognition_Plus, Noise*Communication_Plus)
  Communication_PC <- Communication_FP - Communication_SP
  
  Collaboration_SP <- 0.95*Collaboration_SS^2+0.5*Collaboration_SS
  Collaboration_FP <-  Collaboration_SP + rnorm(sample_size,  Collaboration_Plus + .14*Leadership_Plus, Noise*Collaboration_Plus)
  Collaboration_PC <- Collaboration_FP - Collaboration_SP
  
  Mindfulness_SP <- 0.95*Mindfulness_SS^2+0.5*Mindfulness_SS
  Mindfulness_FP <- Mindfulness_SP + rnorm(sample_size, Mindfulness_Plus, Noise*Mindfulness_Plus)
  Mindfulness_PC <- Mindfulness_FP - Mindfulness_SP
  
  Curiosity_SP <- 0.95*Curiosity_SS^2+0.5*Curiosity_SS
  Curiosity_FP <- Curiosity_SP + rnorm(sample_size, Curiosity_Plus + .04*Growth_Mindset_Plus, Noise*Curiosity_Plus)
  Curiosity_PC <- Curiosity_FP - Curiosity_SP 
  
  Courage_SP <- 0.95*Courage_SS^2+0.5*Courage_SS
  Courage_FP <- Courage_SP + rnorm(sample_size, Courage_Plus + .2*Resilience_Plus, Noise*Courage_Plus)
  Courage_PC <- Courage_FP - Courage_SP 
  
  Resilience_SP <- 0.95*Resilience_SS^2+0.5*Resilience_SS
  Resilience_FP <- Resilience_SP + rnorm(sample_size, Resilience_Plus, Noise*Resilience_Plus)
  Resilience_PC <- Resilience_FP - Resilience_SP
  
  Ethics_SP <- 0.95*Ethics_SS^2+0.5*Ethics_SS
  Ethics_FP <- Ethics_SP + rnorm(sample_size, Ethics_Plus, Noise*Ethics_Plus)
  Ethics_PC <- Ethics_FP - Ethics_SP
  
  Leadership_SP <- 0.95*Leadership_SS^2+0.5*Leadership_SS
  Leadership_FP <- Leadership_SP + rnorm(sample_size, Leadership_Plus + .22*Collaboration_Plus, Noise*Leadership_Plus)
  Leadership_PC <- Leadership_FP - Leadership_SP
  
  Metacognition_SP <- 0.95*Metacognition_SS^2+0.5*Metacognition_SS
  Metacognition_FP <- Metacognition_SP + rnorm(sample_size, Metacognition_Plus + .07* Communication_Plus + .1*Mindfulness_Plus + .11*Curiosity_Plus + .14*Ethics_Plus, Noise*Metacognition_Plus)
  Metacognition_PC <- Metacognition_FP - Metacognition_SP
  
  Growth_Mindset_SP <- 0.95*Growth_Mindset_SS^2+0.5*Growth_Mindset_SS
  Growth_Mindset_FP <- Growth_Mindset_SP + rnorm(sample_size, Growth_Mindset_Plus, Noise*Growth_Mindset_Plus)
  Growth_Mindset_PC <- Growth_Mindset_FP - Growth_Mindset_SP 
  
  return(data.frame(Creativity=Creativity_PC, Critical_Thinking=Critical_Thinking_PC, Communication=Communication_PC, Collaboration=Collaboration_PC,
                    Mindfulness=Mindfulness_PC, Curiosity=Curiosity_PC, Courage=Courage_PC, Resilience=Resilience_PC,
                    Ethics=Ethics_PC, Leadership=Leadership_PC, Metacognition=Metacognition_PC, Growth_Mindset=Growth_Mindset_PC, label=label))
}







# 12 correlations extreme
simulate_test <- function(label, sample_size, Noise, Creativity_SS, Creativity_Plus, 
                          Critical_Thinking_SS, Critical_Thinking_Plus, 
                          Communication_SS, Communication_Plus, 
                          Collaboration_SS, Collaboration_Plus,
                          Mindfulness_SS, Mindfulness_Plus,
                          Curiosity_SS, Curiosity_Plus,
                          Courage_SS, Courage_Plus,
                          Resilience_SS, Resilience_Plus,
                          Ethics_SS, Ethics_Plus,
                          Leadership_SS, Leadership_Plus,
                          Metacognition_SS, Metacognition_Plus,
                          Growth_Mindset_SS, Growth_Mindset_Plus) {
  #PC = point change, from the final points FP minus the starting points SP
  Creativity_SP <- 0.95*Creativity_SS^2+0.5*Creativity_SS
  Creativity_FP <- Creativity_SP + rnorm(sample_size, Creativity_Plus + 1.4*Critical_Thinking_Plus, Noise*Creativity_Plus)
  Creativity_PC <- Creativity_FP - Creativity_SP 
  
  Critical_Thinking_SP <- 0.95*Critical_Thinking_SS^2+0.5*Critical_Thinking_SS
  Critical_Thinking_FP <- Critical_Thinking_SP + rnorm(sample_size, Critical_Thinking_Plus + 1.3*Creativity_Plus, Noise*Critical_Thinking_Plus)
  Critical_Thinking_PC <- Critical_Thinking_FP - Critical_Thinking_SP
  
  Communication_SP <- 0.95*Communication_SS^2+0.5*Communication_SS
  Communication_FP <- Communication_SP + rnorm(sample_size, Communication_Plus + .6*Metacognition_Plus, Noise*Communication_Plus)
  Communication_PC <- Communication_FP - Communication_SP
  
  Collaboration_SP <- 0.95*Collaboration_SS^2+0.5*Collaboration_SS
  Collaboration_FP <-  Collaboration_SP + rnorm(sample_size,  Collaboration_Plus + 1.4*Leadership_Plus, Noise*Collaboration_Plus)
  Collaboration_PC <- Collaboration_FP - Collaboration_SP
  
  Mindfulness_SP <- 0.95*Mindfulness_SS^2+0.5*Mindfulness_SS
  Mindfulness_FP <- Mindfulness_SP + rnorm(sample_size, Mindfulness_Plus, Noise*Mindfulness_Plus)
  Mindfulness_PC <- Mindfulness_FP - Mindfulness_SP
  
  Curiosity_SP <- 0.95*Curiosity_SS^2+0.5*Curiosity_SS
  Curiosity_FP <- Curiosity_SP + rnorm(sample_size, Curiosity_Plus + .4*Growth_Mindset_Plus, Noise*Curiosity_Plus)
  Curiosity_PC <- Curiosity_FP - Curiosity_SP 
  
  Courage_SP <- 0.95*Courage_SS^2+0.5*Courage_SS
  Courage_FP <- Courage_SP + rnorm(sample_size, Courage_Plus + 2*Resilience_Plus, Noise*Courage_Plus)
  Courage_PC <- Courage_FP - Courage_SP 
  
  Resilience_SP <- 0.95*Resilience_SS^2+0.5*Resilience_SS
  Resilience_FP <- Resilience_SP + rnorm(sample_size, Resilience_Plus, Noise*Resilience_Plus)
  Resilience_PC <- Resilience_FP - Resilience_SP
  
  Ethics_SP <- 0.95*Ethics_SS^2+0.5*Ethics_SS
  Ethics_FP <- Ethics_SP + rnorm(sample_size, Ethics_Plus, Noise*Ethics_Plus)
  Ethics_PC <- Ethics_FP - Ethics_SP
  
  Leadership_SP <- 0.95*Leadership_SS^2+0.5*Leadership_SS
  Leadership_FP <- Leadership_SP + rnorm(sample_size, Leadership_Plus + 2.2*Collaboration_Plus, Noise*Leadership_Plus)
  Leadership_PC <- Leadership_FP - Leadership_SP
  
  Metacognition_SP <- 0.95*Metacognition_SS^2+0.5*Metacognition_SS
  Metacognition_FP <- Metacognition_SP + rnorm(sample_size, Metacognition_Plus + .7* Communication_Plus + 1*Mindfulness_Plus + 1.1*Curiosity_Plus + 1.4*Ethics_Plus, Noise*Metacognition_Plus)
  Metacognition_PC <- Metacognition_FP - Metacognition_SP
  
  Growth_Mindset_SP <- 0.95*Growth_Mindset_SS^2+0.5*Growth_Mindset_SS
  Growth_Mindset_FP <- Growth_Mindset_SP + rnorm(sample_size, Growth_Mindset_Plus, Noise*Growth_Mindset_Plus)
  Growth_Mindset_PC <- Growth_Mindset_FP - Growth_Mindset_SP 
  
  return(data.frame(Creativity=Creativity_PC, Critical_Thinking=Critical_Thinking_PC, Communication=Communication_PC, Collaboration=Collaboration_PC,
                    Mindfulness=Mindfulness_PC, Curiosity=Curiosity_PC, Courage=Courage_PC, Resilience=Resilience_PC,
                    Ethics=Ethics_PC, Leadership=Leadership_PC, Metacognition=Metacognition_PC, Growth_Mindset=Growth_Mindset_PC, label=label))
}







# everything separately, only positive noise
simulate_test <- function(label, sample_size, Noise, Creativity_SS, Creativity_Plus, 
                          Critical_Thinking_SS, Critical_Thinking_Plus, 
                          Communication_SS, Communication_Plus, 
                          Collaboration_SS, Collaboration_Plus,
                          Mindfulness_SS, Mindfulness_Plus,
                          Curiosity_SS, Curiosity_Plus,
                          Courage_SS, Courage_Plus,
                          Resilience_SS, Resilience_Plus,
                          Ethics_SS, Ethics_Plus,
                          Leadership_SS, Leadership_Plus,
                          Metacognition_SS, Metacognition_Plus,
                          Growth_Mindset_SS, Growth_Mindset_Plus) {
  #PC = point change, from the final points FP minus the starting points SP
  Creativity_SP <- 0.95*Creativity_SS^2+0.5*Creativity_SS
  Creativity_FP <- Creativity_SP + rnorm(sample_size, Creativity_Plus, Noise)
  Creativity_PC <- Creativity_FP - Creativity_SP 
  
  Critical_Thinking_SP <- 0.95*Critical_Thinking_SS^2+0.5*Critical_Thinking_SS
  Critical_Thinking_FP <- Critical_Thinking_SP + rnorm(sample_size, Critical_Thinking_Plus, Noise)
  Critical_Thinking_PC <- Critical_Thinking_FP - Critical_Thinking_SP
  
  Communication_SP <- 0.95*Communication_SS^2+0.5*Communication_SS
  Communication_FP <- Communication_SP + rnorm(sample_size, Communication_Plus, Noise)
  Communication_PC <- Communication_FP - Communication_SP
  
  Collaboration_SP <- 0.95*Collaboration_SS^2+0.5*Collaboration_SS
  Collaboration_FP <-  Collaboration_SP + rnorm(sample_size,  Collaboration_Plus, Noise)
  Collaboration_PC <- Collaboration_FP - Collaboration_SP
  
  Mindfulness_SP <- 0.95*Mindfulness_SS^2+0.5*Mindfulness_SS
  Mindfulness_FP <- Mindfulness_SP + rnorm(sample_size, Mindfulness_Plus, Noise)
  Mindfulness_PC <- Mindfulness_FP - Mindfulness_SP
  
  Curiosity_SP <- 0.95*Curiosity_SS^2+0.5*Curiosity_SS
  Curiosity_FP <- Curiosity_SP + rnorm(sample_size, Curiosity_Plus, Noise)
  Curiosity_PC <- Curiosity_FP - Curiosity_SP 
  
  Courage_SP <- 0.95*Courage_SS^2+0.5*Courage_SS
  Courage_FP <- Courage_SP + rnorm(sample_size, Courage_Plus, Noise)
  Courage_PC <- Courage_FP - Courage_SP 
  
  Resilience_SP <- 0.95*Resilience_SS^2+0.5*Resilience_SS
  Resilience_FP <- Resilience_SP + rnorm(sample_size, Resilience_Plus, Noise)
  Resilience_PC <- Resilience_FP - Resilience_SP
  
  Ethics_SP <- 0.95*Ethics_SS^2+0.5*Ethics_SS
  Ethics_FP <- Ethics_SP + rnorm(sample_size, Ethics_Plus, Noise)
  Ethics_PC <- Ethics_FP - Ethics_SP
  
  Leadership_SP <- 0.95*Leadership_SS^2+0.5*Leadership_SS
  Leadership_FP <- Leadership_SP + rnorm(sample_size, Leadership_Plus, Noise)
  Leadership_PC <- Leadership_FP - Leadership_SP
  
  Metacognition_SP <- 0.95*Metacognition_SS^2+0.5*Metacognition_SS
  Metacognition_FP <- Metacognition_SP + rnorm(sample_size, Metacognition_Plus, Noise)
  Metacognition_PC <- Metacognition_FP - Metacognition_SP
  
  Growth_Mindset_SP <- 0.95*Growth_Mindset_SS^2+0.5*Growth_Mindset_SS
  Growth_Mindset_FP <- Growth_Mindset_SP + rnorm(sample_size, Growth_Mindset_Plus, Noise)
  Growth_Mindset_PC <- Growth_Mindset_FP - Growth_Mindset_SP 
  
  return(data.frame(Creativity=Creativity_PC, Critical_Thinking=Critical_Thinking_PC, Communication=Communication_PC, Collaboration=Collaboration_PC,
                    Mindfulness=Mindfulness_PC, Curiosity=Curiosity_PC, Courage=Courage_PC, Resilience=Resilience_PC,
                    Ethics=Ethics_PC, Leadership=Leadership_PC, Metacognition=Metacognition_PC, Growth_Mindset=Growth_Mindset_PC, label=label))
}








# generating everything separately, negative noise allowed

simulate_test <- function(label, sample_size, Noise, Creativity_SS, Creativity_Plus, 
                          Critical_Thinking_SS, Critical_Thinking_Plus, 
                          Communication_SS, Communication_Plus, 
                          Collaboration_SS, Collaboration_Plus,
                          Mindfulness_SS, Mindfulness_Plus,
                          Curiosity_SS, Curiosity_Plus,
                          Courage_SS, Courage_Plus,
                          Resilience_SS, Resilience_Plus,
                          Ethics_SS, Ethics_Plus,
                          Leadership_SS, Leadership_Plus,
                          Metacognition_SS, Metacognition_Plus,
                          Growth_Mindset_SS, Growth_Mindset_Plus) {
  #PC = point change, from the final points FP minus the starting points SP
  Creativity_SP <- 0.95*Creativity_SS^2+0.5*Creativity_SS
  Creativity_FP <- Creativity_SP + rnorm(sample_size, Creativity_Plus, Noise*Creativity_Plus)
  Creativity_PC <- Creativity_FP - Creativity_SP 
  
  Critical_Thinking_SP <- 0.95*Critical_Thinking_SS^2+0.5*Critical_Thinking_SS
  Critical_Thinking_FP <- Critical_Thinking_SP + rnorm(sample_size, Critical_Thinking_Plus, Noise*Critical_Thinking_Plus)
  Critical_Thinking_PC <- Critical_Thinking_FP - Critical_Thinking_SP
  
  Communication_SP <- 0.95*Communication_SS^2+0.5*Communication_SS
  Communication_FP <- Communication_SP + rnorm(sample_size, Communication_Plus, Noise*Communication_Plus)
  Communication_PC <- Communication_FP - Communication_SP
  
  Collaboration_SP <- 0.95*Collaboration_SS^2+0.5*Collaboration_SS
  Collaboration_FP <-  Collaboration_SP + rnorm(sample_size,  Collaboration_Plus, Noise*Collaboration_Plus)
  Collaboration_PC <- Collaboration_FP - Collaboration_SP
  
  Mindfulness_SP <- 0.95*Mindfulness_SS^2+0.5*Mindfulness_SS
  Mindfulness_FP <- Mindfulness_SP + rnorm(sample_size, Mindfulness_Plus, Noise*Mindfulness_Plus)
  Mindfulness_PC <- Mindfulness_FP - Mindfulness_SP
  
  Curiosity_SP <- 0.95*Curiosity_SS^2+0.5*Curiosity_SS
  Curiosity_FP <- Curiosity_SP + rnorm(sample_size, Curiosity_Plus, Noise*Curiosity_Plus)
  Curiosity_PC <- Curiosity_FP - Curiosity_SP 
  
  Courage_SP <- 0.95*Courage_SS^2+0.5*Courage_SS
  Courage_FP <- Courage_SP + rnorm(sample_size, Courage_Plus, Noise*Courage_Plus)
  Courage_PC <- Courage_FP - Courage_SP 
  
  Resilience_SP <- 0.95*Resilience_SS^2+0.5*Resilience_SS
  Resilience_FP <- Resilience_SP + rnorm(sample_size, Resilience_Plus, Noise*Resilience_Plus)
  Resilience_PC <- Resilience_FP - Resilience_SP
  
  Ethics_SP <- 0.95*Ethics_SS^2+0.5*Ethics_SS
  Ethics_FP <- Ethics_SP + rnorm(sample_size, Ethics_Plus, Noise*Ethics_Plus)
  Ethics_PC <- Ethics_FP - Ethics_SP
  
  Leadership_SP <- 0.95*Leadership_SS^2+0.5*Leadership_SS
  Leadership_FP <- Leadership_SP + rnorm(sample_size, Leadership_Plus, Noise*Leadership_Plus)
  Leadership_PC <- Leadership_FP - Leadership_SP
  
  Metacognition_SP <- 0.95*Metacognition_SS^2+0.5*Metacognition_SS
  Metacognition_FP <- Metacognition_SP + rnorm(sample_size, Metacognition_Plus, Noise*Metacognition_Plus)
  Metacognition_PC <- Metacognition_FP - Metacognition_SP
  
  Growth_Mindset_SP <- 0.95*Growth_Mindset_SS^2+0.5*Growth_Mindset_SS
  Growth_Mindset_FP <- Growth_Mindset_SP + rnorm(sample_size, Growth_Mindset_Plus, Noise*Growth_Mindset_Plus)
  Growth_Mindset_PC <- Growth_Mindset_FP - Growth_Mindset_SP 
  
  return(data.frame(Creativity=Creativity_PC, Critical_Thinking=Critical_Thinking_PC, Communication=Communication_PC, Collaboration=Collaboration_PC,
                    Mindfulness=Mindfulness_PC, Curiosity=Curiosity_PC, Courage=Courage_PC, Resilience=Resilience_PC,
                    Ethics=Ethics_PC, Leadership=Leadership_PC, Metacognition=Metacognition_PC, Growth_Mindset=Growth_Mindset_PC, label=label))
}







# with Meta correlation
simulate_test <- function(label, sample_size, Noise, Creativity_SS, Creativity_Plus, 
                          Critical_Thinking_SS, Critical_Thinking_Plus, 
                          Communication_SS, Communication_Plus, 
                          Collaboration_SS, Collaboration_Plus,
                          Mindfulness_SS, Mindfulness_Plus,
                          Curiosity_SS, Curiosity_Plus,
                          Courage_SS, Courage_Plus,
                          Resilience_SS, Resilience_Plus,
                          Ethics_SS, Ethics_Plus,
                          Leadership_SS, Leadership_Plus,
                          Metacognition_SS, Metacognition_Plus,
                          Growth_Mindset_SS, Growth_Mindset_Plus) {
  #PC = point change, from the final points FP minus the starting points SP
  Creativity_SP <- 0.95*Creativity_SS^2+0.5*Creativity_SS
  Creativity_FP <- Creativity_SP + rnorm(sample_size, Creativity_Plus + .1*Metacognition_Plus*6, Noise*Creativity_Plus)
  Creativity_PC <- Creativity_FP - Creativity_SP 
  
  Critical_Thinking_SP <- 0.95*Critical_Thinking_SS^2+0.5*Critical_Thinking_SS
  Critical_Thinking_FP <- Critical_Thinking_SP + rnorm(sample_size, Critical_Thinking_Plus + .14*Metacognition_Plus*6, Noise*Critical_Thinking_Plus)
  Critical_Thinking_PC <- Critical_Thinking_FP - Critical_Thinking_SP
  
  Communication_SP <- 0.95*Communication_SS^2+0.5*Communication_SS
  Communication_FP <- Communication_SP + rnorm(sample_size, Communication_Plus + .07*Metacognition_Plus*6, Noise*Communication_Plus)
  Communication_PC <- Communication_FP - Communication_SP
  
  Collaboration_SP <- 0.95*Collaboration_SS^2+0.5*Collaboration_SS
  Collaboration_FP <-  Collaboration_SP + rnorm(sample_size,  Collaboration_Plus + .03*Metacognition_Plus*6, Noise*Collaboration_Plus)
  Collaboration_PC <- Collaboration_FP - Collaboration_SP
  
  Mindfulness_SP <- 0.95*Mindfulness_SS^2+0.5*Mindfulness_SS
  Mindfulness_FP <- Mindfulness_SP + rnorm(sample_size, Mindfulness_Plus + .1*Metacognition_Plus*6, Noise*Mindfulness_Plus)
  Mindfulness_PC <- Mindfulness_FP - Mindfulness_SP
  
  Curiosity_SP <- 0.95*Curiosity_SS^2+0.5*Curiosity_SS
  Curiosity_FP <- Curiosity_SP + rnorm(sample_size, Curiosity_Plus + .11*Metacognition_Plus*6, Noise*Curiosity_Plus)
  Curiosity_PC <- Curiosity_FP - Curiosity_SP 
  
  Courage_SP <- 0.95*Courage_SS^2+0.5*Courage_SS
  Courage_FP <- Courage_SP + rnorm(sample_size, Courage_Plus + .05*Metacognition_Plus*6, Noise*Courage_Plus)
  Courage_PC <- Courage_FP - Courage_SP 
  
  Resilience_SP <- 0.95*Resilience_SS^2+0.5*Resilience_SS
  Resilience_FP <- Resilience_SP + rnorm(sample_size, Resilience_Plus + .1*Metacognition_Plus*6, Noise*Resilience_Plus)
  Resilience_PC <- Resilience_FP - Resilience_SP
  
  Ethics_SP <- 0.95*Ethics_SS^2+0.5*Ethics_SS
  Ethics_FP <- Ethics_SP + rnorm(sample_size, Ethics_Plus + .14*Metacognition_Plus*6, Noise*Ethics_Plus)
  Ethics_PC <- Ethics_FP - Ethics_SP
  
  Leadership_SP <- 0.95*Leadership_SS^2+0.5*Leadership_SS
  Leadership_FP <- Leadership_SP + rnorm(sample_size, Leadership_Plus + .06*Metacognition_Plus*6, Noise*Leadership_Plus)
  Leadership_PC <- Leadership_FP - Leadership_SP
  
  Metacognition_SP <- 0.95*Metacognition_SS^2+0.5*Metacognition_SS
  Metacognition_FP <- Metacognition_SP + rnorm(sample_size, Metacognition_Plus, Noise*Metacognition_Plus)
  Metacognition_PC <- Metacognition_FP - Metacognition_SP
  
  Growth_Mindset_SP <- 0.95*Growth_Mindset_SS^2+0.5*Growth_Mindset_SS
  Growth_Mindset_FP <- Growth_Mindset_SP + rnorm(sample_size, Growth_Mindset_Plus + .01*Metacognition_Plus*6, Noise*Growth_Mindset_Plus)
  Growth_Mindset_PC <- Growth_Mindset_FP - Growth_Mindset_SP 
  
  return(data.frame(Creativity=Creativity_PC, Critical_Thinking=Critical_Thinking_PC, Communication=Communication_PC, Collaboration=Collaboration_PC,
                    Mindfulness=Mindfulness_PC, Curiosity=Curiosity_PC, Courage=Courage_PC, Resilience=Resilience_PC,
                    Ethics=Ethics_PC, Leadership=Leadership_PC, Metacognition=Metacognition_PC, Growth_Mindset=Growth_Mindset_PC, label=label))
}





# example input code from model generation document
Anouchig.O.GP.NS =simulate_test(label =  ' O.GP.NS ', sample_size= 8400 , Noise= 0.2 , Creativity_SS= 1.00 , Creativity_Plus= 33.3 , Critical_Thinking_SS= 4.00 , Critical_Thinking_Plus= 37.43 , Communication_SS= 4.00 , Communication_Plus= 36.6 , Collaboration_SS= 3.00 , Collaboration_Plus= 29.2 , Mindfulness_SS= 2.00 , Mindfulness_Plus= 26.9 , Curiosity_SS= 4.00 , Curiosity_Plus= 28.23 , Courage_SS= 2.00 , Courage_Plus= 28.53 , Resilience_SS= 2.00 , Resilience_Plus= 21.35 , Ethics_SS= 2.00 , Ethics_Plus= 27.85 , Leadership_SS= 3.00 , Leadership_Plus= 29.65 , Metacognition_SS= 2.00 , Metacognition_Plus= 41.9 , Growth_Mindset_SS= 2.00 , Growth_Mindset_Plus= 39.8 )

# exporting the results to a csv
all_students <- rbind(Anouchig.O.GP.NS, Anouchig.O.BP.NS, Anouchig.O.GP.HS, Anouchig.O.BP.HS, Anouchig.O.GP.LS, Anouchig.O.BP.LS, Anouchig.O.GP.NS.BU, Anouchig.O.BP.NS.BU, Anouchig.O.GP.HS.BU, Anouchig.O.BP.HS.BU, Anouchig.O.GP.LS.BU, Anouchig.O.BP.LS.BU, Anouchig.O.GP.NS.SS, Anouchig.O.BP.NS.SS, Anouchig.O.GP.HS.SS, Anouchig.O.BP.HS.SS, Anouchig.O.GP.LS.SS, Anouchig.O.BP.LS.SS, Anouchig.O.GP.NS.BU.SS, Anouchig.O.BP.NS.BU.SS, Anouchig.O.GP.HS.BU.SS, Anouchig.O.BP.HS.BU.SS, Anouchig.O.GP.LS.BU.SS, Anouchig.O.BP.LS.BU.SS, LingLing.E.GP.NS, LingLing.E.BP.NS, LingLing.E.GP.HS, LingLing.E.BP.HS, LingLing.E.GP.LS, LingLing.E.BP.LS, LingLing.E.GP.NS.BU, LingLing.E.BP.NS.BU, LingLing.E.GP.HS.BU, LingLing.E.BP.HS.BU, LingLing.E.GP.LS.BU, LingLing.E.BP.LS.BU, LingLing.E.GP.NS.SS, LingLing.E.BP.NS.SS, LingLing.E.GP.HS.SS, LingLing.E.BP.HS.SS, LingLing.E.GP.LS.SS, LingLing.E.BP.LS.SS, LingLing.E.GP.NS.BU.SS, LingLing.E.BP.NS.BU.SS, LingLing.E.GP.HS.BU.SS, LingLing.E.BP.HS.BU.SS, LingLing.E.GP.LS.BU.SS, LingLing.E.BP.LS.BU.SS, Bof.C.GP.NS, Bof.C.BP.NS, Bof.C.GP.HS, Bof.C.BP.HS, Bof.C.GP.LS, Bof.C.BP.LS, Bof.C.GP.NS.BU, Bof.C.BP.NS.BU, Bof.C.GP.HS.BU, Bof.C.BP.HS.BU, Bof.C.GP.LS.BU, Bof.C.BP.LS.BU, Bof.C.GP.NS.SS, Bof.C.BP.NS.SS, Bof.C.GP.HS.SS, Bof.C.BP.HS.SS, Bof.C.GP.LS.SS, Bof.C.BP.LS.SS, Bof.C.GP.NS.BU.SS, Bof.C.BP.NS.BU.SS, Bof.C.GP.HS.BU.SS, Bof.C.BP.HS.BU.SS, Bof.C.GP.LS.BU.SS, Bof.C.BP.LS.BU.SS, Uruk.N.GP.NS, Uruk.N.BP.NS, Uruk.N.GP.HS, Uruk.N.BP.HS, Uruk.N.GP.LS, Uruk.N.BP.LS, Uruk.N.GP.NS.BU, Uruk.N.BP.NS.BU, Uruk.N.GP.HS.BU, Uruk.N.BP.HS.BU, Uruk.N.GP.LS.BU, Uruk.N.BP.LS.BU, Uruk.N.GP.NS.SS, Uruk.N.BP.NS.SS, Uruk.N.GP.HS.SS, Uruk.N.BP.HS.SS, Uruk.N.GP.LS.SS, Uruk.N.BP.LS.SS, Uruk.N.GP.NS.BU.SS, Uruk.N.BP.NS.BU.SS, Uruk.N.GP.HS.BU.SS, Uruk.N.BP.HS.BU.SS, Uruk.N.GP.LS.BU.SS, Uruk.N.BP.LS.BU.SS, Mignon.A.GP.NS, Mignon.A.BP.NS, Mignon.A.GP.HS, Mignon.A.BP.HS, Mignon.A.GP.LS, Mignon.A.BP.LS, Mignon.A.GP.NS.BU, Mignon.A.BP.NS.BU, Mignon.A.GP.HS.BU, Mignon.A.BP.HS.BU, Mignon.A.GP.LS.BU, Mignon.A.BP.LS.BU, Mignon.A.GP.NS.SS, Mignon.A.BP.NS.SS, Mignon.A.GP.HS.SS, Mignon.A.BP.HS.SS, Mignon.A.GP.LS.SS, Mignon.A.BP.LS.SS, Mignon.A.GP.NS.BU.SS, Mignon.A.BP.NS.BU.SS, Mignon.A.GP.HS.BU.SS, Mignon.A.BP.HS.BU.SS, Mignon.A.GP.LS.BU.SS, Mignon.A.BP.LS.BU.SS)
write.csv(all_students, "C:\\Users\\User\\Downloads\\Capstone_Data\\all_students.csv", row.names = FALSE)




