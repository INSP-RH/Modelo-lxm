rm(list=ls())
library(Rcpp)
library(dplyr)


load("lambda_estimates.rda")

sourceCpp(code='
#include <Rcpp.h>
using namespace Rcpp;


// set.seed function
// [[Rcpp::export]]
void set_seed(unsigned int seed) {
    Rcpp::Environment base_env("package:base");
    Rcpp::Function set_seed_r = base_env["set.seed"];
    set_seed_r(seed);
}


// Function for concatenating vectors
// [[Rcpp::export]]
NumericVector concat(NumericVector x,
NumericVector y) {
    NumericVector z(x.size() + y.size());
    
    std::copy(x.begin(), x.end(), z.begin());
    std::copy(y.begin(), y.end(), z.begin() + x.size());
    
    return(z);}


// [[Rcpp::export]]
double daysNotQuarantined(double node_birth, double node_end,  int l, int m,
bool quarantine_flag){
    /* daysNotQuarantined
     * ---------------------------------------------------------------------------
     * Function to estimate the amount of days an individual spends without
     * being isolated and not being contagious. We consider days in quarantine
     * occur in an interval [node_birth, node_end] where the quarantine happens
     * with l days outside and m days inside the house in a cyclic fashion.
     * node_birth       (double) .- Start day of the node when it is infected
     * node_end         (double) .- End day of the node when it isolates forever
     * l                (int)    .- Number of days outside the house
     * m                (int)    .- Number of days inside the house
     * quarantine_flag  (bool)   .- TRUE if quarantine is in place
     */
    
    //Quarantine period
    int period = l+m;
    int int_birth  = ceil(node_birth);
    int int_end    = floor(node_end);
    
    int startday = int_birth % period;
    int endday   = int_end % period;
    int Days = 0;
    int P = 0;
    int Q = 0;
    
    
    // Without quarantine
    if (quarantine_flag == FALSE){
        
        Days =  int_end - int_birth +1;
        
        //With quarantine
        
        //Scenario 0:
        
    } else {
        
        if (int_end == int_birth){
            
            Days = 1;
            
        } else {
            
            // Scenario 1: startday <=l & endday <=l
            
            if (startday != 0 & endday != 0 & startday <= l & endday  <= l){
                
                Days = int_end - int_birth;
                
                P = int_birth +(period-startday);
                
                Q = int_end - (endday);
                
                // P and Q are auxiliary variables to check if the nodes int_birth and int_end belong to
                // the same period (week).
                
                if (P > Q){
                    
                    Days = Days + 1;
                    
                } else {
                    
                    if (P == Q){
                        
                        Days = Days - m + 1;  }
                    
                    else {
                        
                        Days =  Days - (((Q-P) / period) + 1)*m + 1;
                    }
                }
            }
            
            //Scenario 2: startday <=l & endday > l
            
            if ((startday <=l & startday != 0) & (endday > l | endday == 0)){
                
                //Part 1:
                
                if (endday == 0){
                    
                    Days = int_end - int_birth - m +1;
                    
                    P = int_birth + (period - startday);
                    
                    Q = int_end - period;
                    
                    if ( P>Q ){
                        
                        Days = Days+0;
                        
                    } else {
                        
                        if (P == Q){
                            
                            Days = Days- m;
                            
                        }
                        
                        else {
                            
                            Days =  Days - (((Q-P)/period) + 1) *m;}
                        
                    }
                }
                
                
                //Part 2
                
                if (endday != 0){
                    
                    Days = int_end - int_birth - (endday - l) +1;
                    
                    P = int_birth +(period-startday);
                    
                    Q = int_end - endday;
                    
                    if (P>Q){
                        
                        Days = Days + 0;
                        
                    } else {
                        
                        if (P == Q){
                            
                            Days = Days - m;
                            
                        } else {
                            
                            Days =  Days - (((Q - P) / period) + 1) *m; }
                        
                    }
                }
                
            }
            
            
            // Case 3: startday >l & endday <= l
            
            // This condition guarantees that int_birth and int_endday
            // belong to different periods (weeks)
            
            if ((startday > l |  startday == 0) & (endday <= l & endday != 0)){
                
                
                //Part 1:
                
                if(startday == 0){
                    
                    Days = int_end - int_birth;
                    
                    P = int_birth;
                    
                    Q = int_end - (endday);
                    
                    if (P == Q){
                        
                        Days = Days + 0;
                        
                    } else {
                        
                        Days =  Days - (((Q - P) / period))*m; }
                    
                }
                
                //Part 2:
                
                if (startday != 0){
                    
                    Days = int_end - int_birth;
                    
                    Days = Days - (period - startday);
                    
                    P = int_birth + (period - startday);
                    
                    Q = int_end - (endday);
                    
                    if (P == Q){
                        
                        Days = Days + 0;
                        
                    } else {
                        
                        Days =  Days - (((Q - P) / period))*m;}
                    
                }
            }
            
            
            // Case 4: startday >l & endday > l
            
            
            if ((startday > l |  startday == 0) & (endday > l | endday == 0)){
                
                
                //Part 1: startday == 0 & endday == 0
                
                if (startday == 0 & endday == 0){
                    
                    Days = ((int_end - int_birth) / period)*l;
                    
                }
                
                //Part 2: startday > l & endday == 0
                
                if (startday > l & endday == 0 ){
                    
                    P = int_birth + (period - startday);
                    Q = int_end - (endday);
                    
                    if (P>Q){
                        
                        Days = 0;
                        
                    } else {
                        
                        Days = ((int_end - (int_birth + period - startday)) / period)*l;}
                    
                }
                
                //Part 3: startday == 0 & endday > l
                
                // This condition guarantees that int_birth and int_endday
                // belong to different periods (weeks)
                
                if(startday == 0 & endday > l){
                    
                    Days = (((int_end + (period - endday)) - int_birth) / period)*l;
                    
                }
                
                //Parte 4: startday > l & endday > l
                
                if (startday > l & endday > l){
                    
                    P = int_birth + (period - startday);
                    
                    Q = int_end - (endday);
                    
                    if (P > Q){
                        
                        Days = 0;
                        
                    } else {
                        
                        Days = (((int_end + (period - endday)) - P) / period)*l;
                        
                    }
                }
            }
        }
        
        
    }
    
    return Days;
    
}







// [[Rcpp::export]]

double daysNotQuarantined56(double node_birth, double node_end,  int l, int m,
bool quarantine_flag){
    
    /* daysNotQuarantined56
     * ---------------------------------------------------------------------------
     * Function to estimate the amount of days an individual spends without
     * being isolated and not being contagious. For the estimates, this function
     * considers the scheme 6x1 during the first 56 days (8 weeks), and the
     * scheme lxm from the day 57.
     * We consider days in quarantine occur in an interval [node_birth, node_end]
     * where the quarantine happens with l days outside and m days inside the
     * house in a cyclic fashion.
     * node_birth       (double) .- Start day of the node when it is infected
     * node_end         (double) .- End day of the node when it isolates forever
     * l                (int)    .- Number of days outside the house
     * m                (int)    .- Number of days inside the house
     * quarantine_flag  (bool)   .- TRUE if quarantine is in place
     */
    
    double x;
    double y;
    double z;
    
    if((node_birth <= 56) & (node_end > 56)){
        
        x = daysNotQuarantined(node_birth, 56, 6,1, quarantine_flag);
        
        
        y = daysNotQuarantined(57, node_end, l, m, quarantine_flag);
        
        z = x+y;
        
    }else{
        
        z = daysNotQuarantined(node_birth, node_end, l,m, quarantine_flag);
    }
    
    return z;
}





#include <Rcpp.h>
#include <iostream>
#include <random>
#include <string>
#include <iterator>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]

NumericVector mod(NumericVector a, NumericVector n){
    // /*mod
    //   * ---------------------------------------------------------------------------
    //   * Function to estimate module for vectors
    //   */
    
    return a - floor(a/n)*n;
}


#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int InfectionOnset(int Parent_time, int s,  int W, int M,
bool quarantine_flag){
    
    
    // / * InfectionOnset
    //   * ---------------------------------------------------------------------------
    //   * Function to estimate the day of infection onset for each secondary case, based on the
    //   * primary case infectious period (which may follow an lxm days quarantine scheme)
    //   * Parent_time       (int)    .- Primary case infection onset
    //    * s                (int)    .- Primary case symptoms onset. Takes into account if the
    //                                   individual self-quarantines when developing symptoms or
    //                                   stays contagious the whole time
    //    * W                (int)    .- Number of days outside the house (l)
    //    * M                (int)    .- Number of days inside the house (m)
    //    * quarantine_flag  (bool)   .- TRUE if quarantine is in place
    //    */
    
    NumericVector contagioNoCuarent;
    NumericVector contagioNoCuarent1;
    NumericVector contagioNoCuarent2;
    NumericVector period;
    NumericVector period1;
    NumericVector modulo;
    NumericVector modulo1;
    NumericVector modulo2;
    NumericVector daysContagious;
    NumericVector daysContagious1;
    NumericVector daysContagious2;
    NumericVector noCuarent;
    int Uik;
    
    
    
    if(s == 0){
        s = s + 1;
    }
    
    if (quarantine_flag){
        
        
        // 6x1 scheme during the first 8 weeks (56 days)
        
        if(Parent_time <= 56 & Parent_time + s > 56){
            
            contagioNoCuarent1 = seq(Parent_time, 56);
            period = rep(6 + 1, contagioNoCuarent1.size());
            modulo1 = mod(contagioNoCuarent1, period);
            daysContagious1 = contagioNoCuarent1[modulo1 > 0 & modulo1 <= 6];
            
            contagioNoCuarent2 = seq(57, Parent_time + s);
            period1 = rep(W + M, contagioNoCuarent2.size());
            modulo2 = mod(contagioNoCuarent2, period1);
            daysContagious2 = contagioNoCuarent2[modulo2 > 0 & modulo2 <= W]; //
            
            daysContagious = concat(daysContagious1, daysContagious2);
            
        }
        
        
        if(Parent_time > 56){
            contagioNoCuarent = seq(Parent_time, Parent_time + s);
            period1 = rep(W + M, contagioNoCuarent.size());
            modulo = mod(contagioNoCuarent, period1);
            daysContagious = contagioNoCuarent[modulo > 0 & modulo <= W];
        }
        
        
        if(Parent_time + s <= 56){
            contagioNoCuarent = seq(Parent_time, Parent_time + s);
            period = rep(6 + 1, contagioNoCuarent.size());
            modulo = mod(contagioNoCuarent, period);
            daysContagious = contagioNoCuarent[modulo > 0 & modulo <= 6];
        }
        
        
        
        Uik = sample(daysContagious, 1)[0];
        
    } else {
        
        Uik = R::runif(Parent_time, Parent_time + s);} //Secondary case infection onset
    
    Uik = ceil(Uik);
    
    return Uik;
    
}

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List iterateChildren(NumericVector Ncases, int currentK, int currentT,
int currentSim, double parent_time, double lambda,
int maxk, int maxsim, int maxt, double proba_autoquarantine,
double meanlog, double sdlog, int l, int m, bool quarantine_flag){
    
    int l1;
    int m1;
    
    //Check that we have not arrived yet
    currentSim = currentSim + 1;
    List L(2);
    
    //Account for current simulation
    L(0) = currentSim;
    L(1) = Ncases;
    
    if (currentK < maxk & currentT < maxt & currentSim < maxsim){
        
        
        
        //Symptom onset simulation
        double S = R::rlnorm(meanlog, sdlog);
        
        //Check if individual self-quarantines when developing symptoms or
        //stays contagious the whole time
        if (R::runif(0,1) > proba_autoquarantine){
            S = S + 8; //Viral load after symptom onset heavily declines after 8 days
        }
        
        //R0 throughout days
        //double lambda = (R::pgamma(S, shape, scale)*R0)/S;
        
        //Add new count to current time
        for (int i = floor(currentT); i < maxt; i++){
            Ncases(i) = Ncases(i) + 1;
            
        }
        
        // 6x1 scheme during the first 8 weeks (56 days)
        if(currentT + S <= 56){
            
            l1 = 6;
            m1 = 1;
        
        } else {
            
            l1 = l;
            m1 = m;
        }
        
        //Measure the amount of days contagious
        double measure = daysNotQuarantined56(parent_time, parent_time + S, l1, m1, quarantine_flag);
        
        
        NumericVector lambda_t;
        
        lambda_t = Environment::global_env()["lambda.t"];
        
        lambda = lambda_t[currentT];
        
        int hijos;
        hijos = R::rpois(measure*lambda);
        
        
        if (hijos > 0){
            for (int k = 0; k < hijos; k++){
                
                int Uik;
                
                Uik = InfectionOnset(parent_time, S, l1, m1, quarantine_flag );
                
                
                L  = iterateChildren(Ncases,currentK + 1, ceil(Uik), L(0),
                Uik, lambda, maxk, maxsim, maxt,
                proba_autoquarantine, meanlog,
                sdlog, l , m, quarantine_flag);
            }
        }
    } else if (currentSim == maxsim){
        Rcout << "Number of simulations exceeds maxsim" << std::endl;
        L(0) = currentSim + 1;
    }
    
    return L;
}


#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix Model_l_times_m_cpp(int nsim, double lambda, int maxk, int maxsim,
int maxt, double proba_autoquarantine, double meanlog,
double sdlog, int l, int m, bool quarantine_flag){
    
    NumericMatrix Simulations(maxt, nsim);
    List L;
    
    // set a seed:
    set_seed(374590);
    
    for (int i = 0; i < nsim; i++){
        NumericVector Ncases(maxt);
        //int initCases;
        //initCases = 3;
        //Ncases = Ncases + initCases;
        L = iterateChildren(Ncases, 0, 0, 0, 0, lambda, maxk, maxsim, maxt,
        proba_autoquarantine, meanlog,
        sdlog, l, m, quarantine_flag);
        Ncases = L(1);
        Simulations(_, i) = Ncases;
    }
    return Simulations;
}')


#This model on C++
model_k_times_m <- function(nsim = 100,
                            params = list(lambda = 0.554902, maxk = 10, maxt = 10,
                                          maxsim = 100000,
                                          proba_autoquarantine = 1,
                                          meanlog = 1.621, sdlog = 0.418,
                                          l = 3, m = 4),
                            quarantine_flag = TRUE){
  
  return(Model_l_times_m_cpp(nsim, params$lambda,
                             floor(params$maxk),  floor(params$maxsim), floor(params$maxt),
                             params$proba_autoquarantine, params$meanlog, params$sdlog,
                             floor(params$l), floor(params$m),
                             quarantine_flag)
  )
  
}

#============================================================================================
#                            Run simulations for lxm schemes
#============================================================================================

days <- 91 #max days to simulate

start <- Sys.time()

raw410<- model_k_times_m(nsim = 5000,
                        params = list(lambda = 1, maxk = 1000000000, maxt = days,
                                      maxsim = 1000000000,
                                      proba_autoquarantine = 1,
                                      meanlog = 1.621, sdlog = 0.418,
                                      l = 4, m = 10),
                        quarantine_flag = TRUE)

end <- Sys.time()

end - start

beepr::beep(3)

#Get model summary
qest = c(0.4, 0.5, 0.6)
t(apply(raw410, 1, quantile, qest))

save(raw410, file= "Results/raw.4x10.rda")



