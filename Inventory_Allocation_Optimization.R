rm(list=ls())
library(GA)
# fitness function 

check_revenue_fitness <- function(x1,x2,y,x1d,x2d,cost) 
{ 
   revenue = x1*cost+x2*cost 
   total_inventory_penalty <- revenue * abs((x1+x2)-y) 
   discount_penalty = x1d*x1*cost + x2d*x2*cost
   revenue - (discount_penalty + total_inventory_penalty)
}

Optimize_Allocation_for_revenue <- function(inventory_in_hand,C1discount_min,C1discount_max,C2discount_min,C2discount_max,cost_of_product,popsize,iter,seed) {
 
  GA <- ga(type = "real-valued", fitness = function(x) check_revenue_fitness(x[1],x[2],inventory_in_hand,x[3],x[4],cost_of_product),      
           lower = c(1,1,C1discount_min,C2discount_min), upper = c(inventory_in_hand,inventory_in_hand,C1discount_max,C2discount_max), popSize =popsize,   
           maxiter = iter ,seed = seed)
  print(summary(GA))
  cat('\n')
  cat('Expected Revenue:',( (GA@solution[1]*cost_of_product-(GA@solution[3]*GA@solution[1]*cost_of_product)) +     
                            (GA@solution[2]*cost_of_product-(GA@solution[4]*GA@solution[2]*cost_of_product)) ) )   
                          
  return(GA) 
}    

GA=Optimize_Allocation_for_revenue(1572,0.6435,0.87,0.93,0.94,3995,100,1000,133) 
