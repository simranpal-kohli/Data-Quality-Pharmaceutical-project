library(qcc)                  

# Create a vector with the number of defects per defect type
defects <- c(27, 389, 65, 9, 15, 30, 12, 109, 45, 321)            

# Create a vector with the names of the defects 
names(defects) <- c("Defect 1", "Defect 2", "Defect 3", "Defect 4",
                    "Defect 5", "Defect 6", "Defect 7", "Defect 8",
                    "Defect 9", "Defect 10")   

# Create the Pareto chart
pareto.chart(defects,
             ylab = "Frequency",
             ylab2 ="Cumulative Percentage",
             main = "Pareto Chart",
             cumperc = seq(0, 100, by = 20))
