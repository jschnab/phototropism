#script for data analysis and plotting of hypocotyl bending time series in an automatic fashion
#run line number 6 (Ctrl/Cmd +Enter) to create the function called "hypocotyl_phototrop" then call the function from the console
#then select the file containing the data from the pop-up window
#see the file "hypocotyl_curvature.csv" for a data formatting example

hypocotyl_phototrop <-  function() {
  #Analyzes hypocotyl phototropism time series and plots hypocotyl vertical curvature against time
  
  #=====================#  
  #=== DATA ANALYSIS ===#
  #=====================#
  #transfer data from csv file into variable, this opens a popup window
  donnees_raw <-  read.csv(file.choose())
  
  #calculates number of observation
  n_obs <-  length(donnees_raw$genotype)
  
  #identifies name of genotypes
  gen <-  unique(donnees_raw$genotype)
  
  #calculates number of plants per genotype
  time_points <-  unique(donnees_raw$time)
  number_plants <-  numeric(length(gen))
  for (g in seq_along(gen)) {
    number_plants[g] <-  sum(donnees_raw$time==0 & donnees_raw$genotype==gen[g])
  }
  number_plants <-  as.data.frame(matrix(number_plants, ncol=length(gen), byrow=T))
  colnames(number_plants) <-  gen
  
  #formats donnees_raw into a dataframe with grouping by time
  genotype <-  c()
  time <-  c()
  direction <-  c()
  hypocotyl_curvature <-  c()
  for (g in gen) {
    for (t in time_points) {
      genotype <-  c(genotype, donnees_raw$genotype[donnees_raw$genotype==g & donnees_raw$time==t])
      time <-  c(time, donnees_raw$time[donnees_raw$time==t & donnees_raw$genotype==g])
      direction <-  c(direction, donnees_raw$direction[donnees_raw$genotype==g & donnees_raw$time==t])
      hypocotyl_curvature <-  c(hypocotyl_curvature, 
                              donnees_raw$hypocotyl_curvature[donnees_raw$genotype==g & donnees_raw$time==t])
    }
  }
  
  direction2 <-  numeric(length(direction))
  for (d in seq_along(direction)) {
    if (direction[d]==1) {
      direction2[d] <-  'n'
    }
    else {
      direction2[d] <-  'p'
    }
  }
  
  genotype2 <- numeric(length(genotype))
  for (g in seq_along(genotype)) {
    genotype2[g] <- as.character(gen[genotype[g]])
  }
  
  donnees <-  data.frame(genotype=genotype2, 
                         time=time, 
                         direction=direction2, 
                         hypocotyl_curvature=hypocotyl_curvature)
  
  #calculates deviation from 180 degrees and appends into new vector before adding to dataframe
  dev_180 <-  c()
  i <-  1
  while (i <= n_obs) {
    dev_180 <-  c(dev_180, 180 - donnees$hypocotyl_curvature[i])
    i <-  i +1
  }
  donnees$dev_180 <-  dev_180
  
  #memorizes deviation from 180 at t=0 in a dataframe
  t_0 <-  c()
  for (i in gen) {
    t_0 <-  c(t_0, donnees$dev_180[donnees$time==0 & donnees$genotype==i])
  }
  df_t_0 <-  as.data.frame(matrix(t_0, ncol=length(gen), byrow=F))
  colnames(df_t_0) <-  gen
  
  #memorizes direction at t=0 in a dataframe
  dir_0 <-  c()
  for (i in gen) {
    dir_0 <-  c(dir_0, donnees$direction[donnees$time==0 & donnees$genotype==i])
  }
  df_dir_0 <-  as.data.frame(matrix(dir_0, ncol=length(gen), byrow=F))
  colnames(df_dir_0) <-  gen
  
  #calculates deviation from vertical and appends into new vector before adding to dataframe
  dev_vert <-  c(n_obs * 0)
  donnees$dev_vert <-  dev_vert
  i <-  1
  j <-  1
  while (i <= n_obs) {
    geno <-  donnees[i, 1]
    if (j > number_plants[1, geno]) {j <-  1}
    
    if (df_dir_0[j, geno]==1) { #if direction is 'n' at t = 0
      if (donnees$direction[i]=='n') {
        donnees[i, 'dev_vert'] <-  df_t_0[j, geno] -donnees[i, 'dev_180']
      }
      else {
        donnees[i, 'dev_vert'] <-  donnees[i, 'dev_180'] +df_t_0[j, geno]
      }
    }
    
    if (df_dir_0[j, geno]==2) { #if direction is 'p' at t = 0
      if (donnees$direction[i]=='n') {
        donnees[i, 'dev_vert'] <-  -df_t_0[j, geno] -donnees[i, 'dev_180']
      }
      else {
        donnees[i, 'dev_vert'] <-  donnees[i, 'dev_180'] -df_t_0[j, geno]
      }
    }
    i <-  i +1
    j <-  j +1
  }
  
  #calculate means and standard error of the mean for each time point and put data in a dataframe
  #generate a vector containing genotype names
  geno <-  c()
  for (i in gen) {
    j <-  0
    while (j < length(time_points)) {
      geno <-  c(geno, i)
      j <-  j +1
    }
  }
  
  #generate a vector with list of increasing numbers matching genotype names 
  #(useful for drawing different datapoints symbols)
  symbols <-  c()
  i <-  1
  while (i < length(gen) +1) {
    j <-  0
    while (j < length(time_points)) {
      symbols = c(symbols, i +14)
      j <-  j +1
    }
    i <-  i +1
  }
  
  #calculate means
  means <-  c()
  for (i in gen) {
    for (t in time_points) {
      means <-  c(means, mean(donnees$dev_vert[donnees$time==t & donnees$genotype==i])) 
    }
  }
  
  #calculate standard errors of the mean
  sem <-  c()
  for (i in gen) {
    for (t in time_points) {
      sem <-  c(sem, sd(donnees$dev_vert[donnees$time==t & donnees$genotype ==i]) / sqrt(number_plants[,i])) 
    }
  }
  
  #put data in dataframe
  df <-  data.frame(symbols=symbols, genotype=geno, time=time_points, mean=means, sem=sem)
  
  #================#
  #=== GRAPHICS ===#
  #================#
  #plot data from dataframe (with custom x-axis) and add legend
  #plot the first genotype
  plot(df$time[df$genotype==gen[1]], 
       df$mean[df$genotype==gen[1]],
       type='o',
       pch =df$symbols[df$genotype==gen[1]], 
       xlab ='Time (minutes)', 
       ylab ='Hypocotyl curvature (degrees)',
       ylim =c(-10,100),
       xaxt ='n',
       panel.first=abline(v=c(seq(0,240,30)), h=c(seq(0,100,20)), col='grey90')) #add grid
  #then loop to plot other genotypes
  for(i in gen[2:length(gen)]){
    lines(df$time[df$genotype==i], 
          df$mean[df$genotype==i],
          type='o',
          pch =df$symbols[df$genotype==i], 
          xlab ='Time (minutes)', 
          ylab ='Hypocotyl curvature (degrees)',
          ylim =c(-10,100), xaxt ='n')
  }
  axis(1, at=seq(0,240,30), labels=seq(0,240,30))
  legend ('topleft', inset=0.05, legend=gen, pch=15:(15+length(gen)), box.lty=0)
  
  #plot standard error of the mean on the same graph
  arrows(x0=df$time, 
         y0=df$mean, 
         y1=c(df$mean +df$sem, df$mean -df$sem), 
         length=0.05, 
         col='black', 
         angle=90)
}
