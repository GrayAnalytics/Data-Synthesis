library(plyr)

##### Creating the dataset #####
class_list1 <- c('A', 'B')
class_list2 <- c('Z', 'X')
class_list3 <- c('I', 'K')
#sample(class_list1, size = 10, replace = TRUE)

dataset <- data.frame(
cbind(sample(class_list1, size = 20, replace = TRUE), 
      sample(class_list2, size = 20, replace = TRUE),
      sample(class_list3, size = 20, replace = TRUE),
      runif(20,0,100),
      runif(20,0,1)
      )
)

names(dataset) <- c('var1', 'var2', 'var3', 'var4', 'var5')  # fix this

#not sure why the numbers came back as factors. blame runif I guess
dataset$var4 <- as.numeric(levels(dataset$var4))[dataset$var4]
dataset$var5 <- as.numeric(levels(dataset$var5))[dataset$var5]

#distribution_table <- count(dataset, vars = factor_list)


synthesize_data <- function(dataset, number_of_rows){
  
  synthetic_data <- data.frame(matrix(vector(), 0, ncol(dataset), dimnames = list(c(), names(dataset))))
  distribution_table <- count(dataset, vars = factor_list)
  factor_list <- names(Filter(is.factor, dataset))
  numeric_list <- names(Filter(is.numeric, dataset))
  
  ## checks if the datasets are too unique
  if(max(distribution_table$freq) ==1){
    return(NULL)
  }
  
  for (synthetic_row in 1:number_of_rows){
    ## select a row to 'duplicate'
    ## Repeat function is to ensure that we're selecting a combo with some variance
    repeat{
      fake_row_num <- sample(nrow(dataset), 1);
      fake_row <- dataset[fake_row_num,];
      if(merge(distribution_table, fake_row[,factor_list], by = factor_list)$freq!=1) break
    }
    
    dataset_filtered <- merge(dataset, fake_row[,factor_list], by = factor_list)
    
    ## Loop through numeric columns to synthesize
    for (synth_col in numeric_list){
      #print(synth_col)
      
      std <- sd(dataset_filtered[,synth_col])
      
      ## new continuous value within a standard deviation of the original value.
      ## should conform to original filtered distribution's min/max
      repeat{
        proposed_value <- runif(1, min = fake_row[,synth_col] - std, max = fake_row[,synth_col] + std);
        if (proposed_value >= min(dataset_filtered[,synth_col]) & 
            proposed_value <= max(dataset_filtered[,synth_col])) break
      }
      fake_row[1, synth_col] <- proposed_value
      
    }
    synthetic_data <- rbind(synthetic_data, fake_row[1,])
    
  }
  
  return(synthetic_data)
}

