# Exercise 5. se versus p
#Write a line of code that calculates the standard error se of a sample average when you poll 25 people in the population. Generate a sequence of 100 proportions of Democrats p that vary from 0 (no Democrats) to 1 (all Democrats).
#Plot se versus p for the 100 different proportions.
#Use the seq function to generate a vector of 100 values of p that range from 0 to 1.
#Use the sqrt function to generate a vector of standard errors for all values of p.
#Use the plot function to generate a plot with p on the x-axis and se on the y-axis.

# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length.out = 100)
p
length(p)
# Create a variable `se` that contains the standard error of each sample average
se <- sqrt((p*(1-p))/N)

# Plot `p` on the x-axis and `se` on the y-axis
plot(p, se)

# Exercise 6. Multiple plots of se versus p
#Using the same code as in the previous exercise, create a for-loop that generates three plots of p versus se when the sample sizes equal N=25
#, N=100, and N=1000
#Instructions
#Your for-loop should contain two lines of code to be repeated for three different values of N.
#The first line within the for-loop should use the sqrt function to generate a vector of standard errors se for all values of p.
#The second line within the for-loop should use the plot function to generate a plot with p on the x-axis and se on the y-axis.
#Use the ylim argument to keep the y-axis limits constant across all three plots. The lower limit should be equal to 0 and the upper limit should equal 0.1 (it can be shown that this value is the highest calculated standard error across all values of p and N).

# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

sample_sizes[1]

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.

for(N in sample_sizes)
{
  se <- sqrt(((p*(1-p))/N))
  plot(p, se, ylim = c(0,.1))
}

# Exercise 9. Standard error of the spread
#Say the actual proportion of Democratic voters is p=0.45
#. In this case, the Republican party is winning by a relatively large margin of d=???0.1, or a 10% margin of victory. What is the standard error of the spread 2X¯???1
#in this case?
#Instructions
#Use the sqrt function to calculate the standard error of the spread 2X¯???1

# `N` represents the number of people polled
N <- 25
# `p` represents the proportion of Democratic voters
p <- 0.45
# Calculate the standard error of the spread. Print this value to the console.
2 * sqrt((p*(1-p))/N)

#--------------------------
#Code: Monte Carlo simulation using a set value of p

p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

# Code: Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)

p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)


#----------------------------
#ASSESSMENT SECTION 2
#--------------------------
# Exercise 1. Sample average
#Write function called take_sample that takes the proportion of Democrats p
#and the sample size N
#as arguments and returns the sample average of Democrats (1) and Republicans (0).
#Calculate the sample average if the proportion of Democrats equals 0.45 and the sample size is 100.
#Instructions
#Define a function called take_sample that takes p and N as arguments.
#Use the sample function as the first statement in your function to sample N
#elements from a vector of options where Democrats are assigned the value '1' and Republicans are assigned the value '0' in that order.
#Use the mean function as the second statement in your function to find the average value of the random sample.

# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample <- exact_prob <- function(p, N){
  mean(sample(c(1, 0), N, replace = TRUE, prob = c(p, 1-p)))
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p, N)

# Exercise 2. Distribution of errors - 1
#Assume the proportion of Democrats in the population p
#equals 0.45 and that your sample size N is 100 polled voters. The take_sample function you defined previously generates our estimate, X¯.
#Replicate the random sampling 10,000 times and calculate p???X¯
#for each random sample. Save these differences as a vector called errors. Find the average of errors and plot a histogram of the distribution.
#Instructions
#The function take_sample that you defined in the previous exercise has already been run for you.
#Use the replicate function to replicate subtracting the result of take_sample from the value of p 10,000 times.
#Use the mean function to calculate the average of the differences between the sample average and actual value of p.
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, p - take_sample(p, N))
head(errors)

# Calculate the mean of the errors. Print this value to the console.
mean(errors)
hist(errors)

# Exercise 3. Distribution of errors - 2
#In the last exercise, you made a vector of differences between the actual value for p
#and an estimate, X¯. We called these differences between the actual and estimated values errors.
#The errors object has already been loaded for you. Use the hist function to plot a histogram of the values contained in the vector errors. Which statement best describes the distribution of the errors?
# ANSWER : The errors are symmetrically distributed around 0.

# Exercise 4. Average size of error
#The error p???X¯ is a random variable. In practice, the error is not observed because we do not know the actual proportion of Democratic voters, p. However, we can describe the size of the error by constructing a simulation.
#What is the average size of the error if we define the size by taking the absolute value ???p???X¯????
#Instructions
#Use the sample code to generate errors, a vector of ???p???X¯???.
#Calculate the absolute value of errors using the abs function.
#Calculate the average of these values using the mean function.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the absolute value of each simulated error. Print this value to the console.
mean(abs(errors))


# Exercise 5. Standard deviation of the spread
#The standard error is related to the typical size of the error we make when predicting. We say size because, as we just saw, the errors are centered around 0. In that sense, the typical error is 0. For mathematical reasons related to the central limit theorem, we actually use the standard deviation of errors rather than the average of the absolute values.
#As we have discussed, the standard error is the square root of the average squared distance (X¯???p)2. The standard deviation is defined as the square root of the distance squared.
#Calculate the standard deviation of the spread.
#Instructions
#Use the sample code to generate errors, a vector of ???p???X¯???.
#Use ^2 to square the distances.
#Calculate the average squared distance using the mean function.
#Calculate the square root of these values using the sqrt function.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the standard deviation of `errors`
sqrt(mean(errors^2))

# Exercise 6. Estimating the standard error
#The theory we just learned tells us what this standard deviation is going to be because it is the standard error of X¯.
#Estimate the standard error given an expected value of 0.45 and a sample size of 100.
#Instructions
#Calculate the standard error using the sqrt function

# Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error
sqrt((p*(1-p))/N)


# Exercise 7. Standard error of the estimate
#In practice, we don't know p, so we construct an estimate of the theoretical prediction based by plugging in X¯ for p. Calculate the standard error of the estimate: SE^(X¯)
#Instructions
#Simulate a poll X using the sample function.
#When using the sample function, create a vector using c() that contains all possible polling options where '1' indicates a Democratic voter and '0' indicates a Republican voter.
#When using the sample function, use replace = TRUE within the sample function to indicate that sampling from the vector should occur with replacement.
#When using the sample function, use prob = within the sample function to indicate the probabilities of selecting either element (0 or 1) within the vector of possibilities.
#Use the mean function to calculate the average of the simulated poll, X_bar.
#Calculate the standard error of the X_bar using the sqrt function and print the result.

# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(c(1, 0), N, replace = TRUE, prob = c(p, 1-p))

# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)

# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)


# Exercise 8. Plotting the standard error
#The standard error estimates obtained from the Monte Carlo simulation, the theoretical prediction, and the estimate of the theoretical prediction are all very close, which tells us that the theory is working. This gives us a practical approach to knowing the typical error we will make if we predict p
#with X^. The theoretical result gives us an idea of how large a sample size is required to obtain the precision we need. Earlier we learned that the largest standard errors occur for p=0.5.
#Create a plot of the largest standard error for N
#ranging from 100 to 5,000. Based on this plot, how large does the sample size have to be to have a standard error of about 1%?

N <- seq(100, 5000, len = 100)
N
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N, se)

# Exercise 11. Plotting the errors
#Make a qq-plot of the errors you generated previously to see if they follow a normal distribution.
#Instructions
#Run the supplied code
#Use the qqnorm function to produce a qq-plot of the errors.
#Use the qqline function to plot a line showing a normal distribution.
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqnorm(errors); qqline(errors)

# Exercise 12. Estimating the probability of a specific value of X-bar
#If p=0.45and N=100, use the central limit theorem to estimate the probability that X¯>0.5.
#Instructions
#Use pnorm to define the probability that a value will be greater than 0.5.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.

1 - pnorm(0.5, p, sqrt(p*(1-p)/N))

# Exercise 13. Estimating the probability of a specific error size
#Assume you are in a practical situation and you don't know p. Take a sample of size N=100 and obtain a sample average of X¯=0.51
#What is the CLT approximation for the probability that your error size is equal or larger than 0.01?
#Instructions
#Calculate the standard error of the sample average using the sqrt function.
#Use pnorm twice to define the probabilities that a value will be less than -0.01 or greater than 0.01.
#Combine these results to calculate the probability that the error size will be 0.01 or larger.

# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger
1- pnorm(.01, 0, se_hat) + pnorm(-0.01, 0, se_hat)

#-------------------------------------
#-ASSESSMENT SECTION #3
#---------------------------------------
# Exercise 1. Confidence interval for p
#For the following exercises, we will use actual poll data from the 2016 election. The exercises will contain pre-loaded data from the dslabs package.
#library(dslabs)
#data("polls_us_election_2016")
#We will use all the national polls that ended within a few weeks before the election.
#Assume there are only two candidates and construct a 95% confidence interval for the election night proportion p.
#Instructions
#Use filter to subset the data set for the poll data you want. Include polls that ended on or after October 31, 2016 (enddate). Only include polls that took place in the United States. Call this filtered object polls.
#Use nrow to make sure you created a filtered object polls that contains the correct number of rows.
#Extract the sample size N from the first poll in your subset object polls.
#Convert the percentage of Clinton voters (rawpoll_clinton) from the first poll in polls to a proportion, X_hat. Print this value to the console.
#Find the standard error of X_hat given N. Print this result to the console.
#Calculate the 95% confidence interval of this estimate using the qnorm function.
#Save the lower and upper confidence intervals as an object called ci. Save the lower confidence interval first.

library(dslabs)

# Load the data
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
#head(polls_us_election_2016)
#dim(polls_us_election_2016)
#glimpse(polls_us_election_2016)
#summary(polls_us_election_2016)
#str(polls_us_election_2016)
#sapply(polls_us_election_2016, class)
#levels(polls_us_election_2016$state)
polls <- polls_us_election_2016 %>% filter(state=="U.S.") %>% filter(enddate >= "2016-10-31")

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
print(N)

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
head(polls)
X_hat<- polls$rawpoll_clinton[1] / 100
print(X_hat)

# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
print(se_hat)

# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(aux-qnorm(0.975)*se_hat, aux+qnorm(0.975)*se_hat)
ci

# Exercise 2. Pollster results for p
#Create a new object called pollster_results that contains the pollster's name, the end date of the poll, the proportion of voters who declared a vote for Clinton, the standard error of this estimate, and the lower and upper bounds of the confidence interval for the estimate.
#Instructions
#Use the mutate function to define four new columns: X_hat, se_hat, lower, and upper. Temporarily add these columns to the polls object that has already been loaded for you.
#In the X_hat column, convert the raw poll results for Clinton to a proportion.
#In the se_hat column, calculate the standard error of X_hat for each poll using the sqrt function.
#In the lower column, calculate the lower bound of the 95% confidence interval using the qnorm function.
#In the upper column, calculate the upper bound of the 95% confidence interval using the qnorm function.
#Use the select function to select the columns from polls to save to the new object pollster_results.

# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results <- polls %>% mutate("X_hat"=rawpoll_clinton/100) %>% mutate("se_hat"=sqrt(X_hat*(1-X_hat)/samplesize)) %>% mutate("lower"=X_hat-qnorm(0.975)*se_hat) %>% mutate("upper"=X_hat+qnorm(0.975)*se_hat) %>% select("pollster", "enddate", "X_hat", "se_hat", "lower", "upper")

pollster_results

# Exercise 3. Comparing to actual results - p
#The final tally for the popular vote was Clinton 48.2% and Trump 46.1%. Add a column called hit to pollster_results that states if the confidence interval included the true proportion p=0.482 or not. What proportion of confidence intervals included p?
#Instructions
#Finish the code to create a new object called avg_hit by following these steps.
#Use the mutate function to define a new variable called 'hit'.
#Use logical expressions to determine if each values in lower and upper span the actual proportion.
#Use the mean function to determine the average value in hit and summarize the results using summarize.

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
p<- 0.482
pollster_results <- pollster_results %>% mutate("hit"=lower < p & upper > p)

avg_hit <- pollster_results %>% summarize(mean(hit))

# Exercise 4. Theory of confidence intervals
#If these confidence intervals are constructed correctly, and the theory holds up, what proportion of confidence intervals should include p?
#Instructions
#Possible Answers
#0.95


# Exercise 5. Confidence interval for d
#A much smaller proportion of the polls than expected produce confidence intervals containing p. Notice that most polls that fail to include p are underestimating. The rationale for this is that undecided voters historically divide evenly between the two main candidates on election day.
#In this case, it is more informative to estimate the spread or the difference between the proportion of two candidates d, or 0.482???0.461=0.021 for this election.
#Assume that there are only two parties and that d=2p???1. Construct a 95% confidence interval for difference in proportions on election night.
#Instructions
#Use the mutate function to define a new variable called 'd_hat' in polls as the proportion of Clinton voters minus the proportion of Trump voters.
#Extract the sample size N from the first poll in your subset object polls.
#Extract the difference in proportions of voters d_hat from the first poll in your subset object polls.
#Use the formula above to calculate p from d_hat. Assign p to the variable X_hat.
#Find the standard error of the spread given N. Save this as se_hat.
#Calculate the 95% confidence interval of this estimate of the difference in proportions, d_hat, using the qnorm function.
#Save the lower and upper confidence intervals as an object called ci. Save the lower confidence interval first.

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]

# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
polls <- polls %>% mutate("d_hat"=(rawpoll_clinton - rawpoll_trump) / 100)
d_hat <- polls$d_hat[1]
d_hat

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat+1)/2
X_hat

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(d_hat-qnorm(0.975)*se_hat, d_hat+qnorm(0.975)*se_hat)
ci

# Exercise 6. Pollster results for d
#Create a new object called pollster_results that contains the pollster's name, the end date of the poll, the difference in the proportion of voters who declared a vote either, and the lower and upper bounds of the confidence interval for the estimate.
#Instructions
#Use the mutate function to define four new columns: 'X_hat', 'se_hat', 'lower', and 'upper'. Temporarily add these columns to the polls object that has already been loaded for you.
#In the X_hat column, calculate the proportion of voters for Clinton using d_hat.
#In the se_hat column, calculate the standard error of the spread for each poll using the sqrt function.
#In the lower column, calculate the lower bound of the 95% confidence interval using the qnorm function.
#In the upper column, calculate the upper bound of the 95% confidence interval using the qnorm function.
#Use the select function to select the pollster, enddate, d_hat, lower, upper columns from polls to save to the new object pollster_results.

# The subset `polls` data with 'd_hat' already calculated has been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <-
  polls %>% 
  mutate("X_hat"=(d_hat+1)/2) %>%
  mutate("se_hat"=2*sqrt(X_hat*(1-X_hat)/samplesize)) %>%
  mutate("lower"=d_hat-qnorm(0.975)*se_hat) %>%
  mutate("upper"=d_hat+qnorm(0.975)*se_hat) %>%
  select("pollster", "enddate", "d_hat", "lower", "upper")


# Exercise 7. Comparing to actual results - d
#What proportion of confidence intervals for the difference between the proportion of voters included d, the actual difference in election day?
#Instructions
#Use the mutate function to define a new variable within pollster_results called hit.
#Use logical expressions to determine if each values in lower and upper span the actual difference in proportions of voters.
#Use the mean function to determine the average value in hit and summarize the results using summarize.
#Save the result of your entire line of code as an object called avg_hit.
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
pollster_results <- pollster_results %>% mutate("hit"=0.021>lower&0.021<upper)
avg_hit <- pollster_results %>% summarize(mean(hit))
avg_hit

# Exercise 8. Comparing to actual results by pollster
#Although the proportion of confidence intervals that include the actual difference between the proportion of voters increases substantially, it is still lower that 0.95. In the next chapter, we learn the reason for this.
#To motivate our next exercises, calculate the difference between each poll's estimate d¯ and the actual d=0.021. Stratify this difference, or error, by pollster in a plot.
#Instructions
#Define a new variable errors that contains the difference between the estimated difference between the proportion of voters and the actual difference on election day, 0.021.
#To create the plot of errors by pollster, add a layer with the function geom_point. The aesthetic mappings require a definition of the x-axis and y-axis variables. So the code looks like the example below, but you fill in the variables for x and y.
#The last line of the example code adjusts the x-axis labels so that they are easier to read.
##data %>% ggplot(aes(x = , y = )) +
##  geom_point() +
##  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls <- polls %>% mutate("errors"=d_hat-0.021)
polls %>% ggplot(aes(x = errors, y = pollster)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exercise 9. Comparing to actual results by pollster - multiple polls
#Remake the plot you made for the previous exercise, but only for pollsters that took five or more polls.
#You can use dplyr tools group_by and n to group data by a variable of interest and then count the number of observations in the groups. The function filter filters data piped into it by your specified condition.
#For example:
##data %>% group_by(variable_for_grouping) 
##%>% filter(n() >= 5)
#Instructions
#Define a new variable errors that contains the difference between the estimated difference between the proportion of voters and the actual difference on election day, 0.021.
#Group the data by pollster using the group_by function.
#Filter the data by pollsters with 5 or more polls.
#Use ggplot to create the plot of errors by pollster.
#Add a layer with the function geom_point.
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls <-
  polls %>% mutate("errors"=d_hat-0.021) 

polls %>% group_by(pollster) %>%
  filter(n() >= 5) %>%
  ggplot(aes(x = errors, y = pollster)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#------------------------------
#---ASSESSMENT SECTION 4
#------------------------------
#Exercise 1 - Heights Revisited
#We have been using urn models to motivate the use of probability models. However, most data science applications are not related to data obtained from urns. More common are data that come from individuals. Probability plays a role because the data come from a random sample. The random sample is taken from a population and the urn serves as an analogy for the population.
#Let's revisit the heights dataset. For now, consider x to be the heights of all males in the data set. Mathematically speaking, x is our population. Using the urn analogy, we have an urn with the values of x in it.
#What are the population average and standard deviation of our population?
#Instructions
#Execute the lines of code that create a vector x that contains heights for all males in the population.
#Calculate the average of x.
#Calculate the standard deviation of x.
# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

names(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
mean(x)

# Calculate the population standard deviation. Print this value to the console.
sd(x)

# Exercise 2 - Sample the population of heights
#Call the population average computed above ?? and the standard deviation ??. Now take a sample of size 50, with replacement, and construct an estimate for ?? and ??.
#Instructions
#Use the sample function to sample N values from x.
#Calculate the mean of the sampled heights.
#Calculate the standard deviation of the sampled heights.

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

# Exercise 3 - Sample and Population Averages
#What does the central limit theory tell us about the sample average and how it is related to ??, the population average?
#Instructions
#Possible Answers
#It is a random variable with expected value ?? and standard error ??/???N

# Exercise 4 - Confidence Interval Calculation
#We will use X¯ as our estimate of the heights in the population from our sample size N. We know from previous exercises that the standard estimate of our error X¯????? is ??/N?????????.
#Construct a 95% confidence interval for ??.
#Instructions
#Use the sd and sqrt functions to define the standard error se
#Calculate the 95% confidence intervals using the qnorm function. Save the lower then the upper confidence interval to a variable called ci.
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value to the console.
se <- sd(X)/sqrt(N) 
se

# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(mean(X)-qnorm(0.975)*se, mean(X)+qnorm(0.975)*se)
ci

# Exercise 5 - Monte Carlo Simulation for Heights
#Now run a Monte Carlo simulation in which you compute 10,000 confidence intervals as you have just done. What proportion of these intervals include ???
#Instructions
#Use the replicate function to replicate the sample code for B <- 10000 simulations. Save the results of the replicated code to a variable called res. The replicated code should complete the following steps: -1. Use the sample function to sample N values from x. Save the sampled heights as a vector called X. -2. Create an object called interval that contains the 95% confidence interval for each of the samples. Use the same formula you used in the previous exercise to calculate this interval. -3. Use the between function to determine if ?? is contained within the confidence interval of that simulation.
#Finally, use the mean function to determine the proportion of results in res that contain mu.

# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu

res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  mean_X <- mean(X)
  se <- sd(X)/sqrt(N)
  lower <- mean_X-qnorm(0.975)*se
  upper <- mean_X+qnorm(0.975)*se
  between(mu, lower, upper)
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.

mean(res)

# Exercise 6 - Visualizing Polling Bias
#In this section, we used visualization to motivate the presence of pollster bias in election polls. Here we will examine that bias more rigorously. Lets consider two pollsters that conducted daily polls and look at national polls for the month before the election.
#Is there a poll bias? Make a plot of the spreads for each poll.
#Instructions
#Use ggplot to plot the spread for each of the two pollsters.
#Define the x- and y-axes usingusing aes() within the ggplot function.
#Use geom_boxplot to make a boxplot of the data.
#Use geom_point to add data points to the plot.
# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(pollster, spread)) +
  geom_boxplot() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exercise 7 - Defining Pollster Bias
#The data do seem to suggest there is a difference between the pollsters. However, these data are subject to variability. Perhaps the differences we observe are due to chance. Under the urn model, both pollsters should have the same expected value: the election day difference, d.
#We will model the observed data Yij in the following way:
#Yij=d+bi+??ij
#with i=1,2 indexing the two pollsters, bi the bias for pollster i, and ??ij poll to poll chance variability. We assume the ?? are independent from each other, have expected value 0 and standard deviation ??i regardless of j.
#Which of the following statements best reflects what we need to know to determine if our data fit the urn model?
#Instructions
#Possible Answers
#Is b1???b2?

# Exercise 8 - Derive Expected Value
#We modelled the observed data Yij as:
#Yij=d+bi+??ij
#On the right side of this model, only ??ij is a random variable. The other two values are constants.
#What is the expected value of Y1j?
#Instructions
#Possible Answers
#d+b

# Exercise 9 - Expected Value and Standard Error of Poll 1
#Suppose we define Y¯1 as the average of poll results from the first poll and ??1 as the standard deviation of the first poll.
#What is the expected value and standard error of Y¯1?
#Instructions
#Possible Answers
#The expected value is d+b1 and the standard error is ??1???/N1

# Exercise 10 - Expected Value and Standard Error of Poll 2
#Now we define Y¯2 as the average of poll results from the second poll.
#What is the expected value and standard error of Y¯2?
#Instructions
#Possible Answers
#The expected value is d+b2 and the standard error is ??2/???N2

# Exercise 11 - Difference in Expected Values Between Polls
#Using what we learned by answering the previous questions, what is the expected value of Y¯2???Y¯1?
#Instructions
#Possible Answers
#b2???b1

# Exercise 12 - Standard Error of the Difference Between Polls
#Using what we learned by answering the questions above, what is the standard error of Y¯2???Y¯1?
#Instructions
#Possible Answers
#???(??22/N2+??21/N1)

# Exercise 13 - Compute the Estimates
#The answer to the previous question depends on ??1 and ??2, which we don't know. We learned that we can estimate these values using the sample standard deviation.
#Compute the estimates of ??1 and ??2.
#Instructions
#Group the data by pollster.
#Summarize the standard deviation of the spreads for each of the two pollsters. Name the standard deviation s.
#Store the pollster names and standard deviations of the spreads (??) in an object called sigma.

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
#pools %>% 

sigma <-  polls %>% 
  group_by(pollster) %>%
  summarize(s = sd(spread)) 

# Print the contents of sigma to the console
sigma

# Exercise 14 - Probability Distribution of the Spread
#What does the central limit theorem tell us about the distribution of the differences between the pollster averages, Y¯2???Y¯1?
#Instructions
#Possible Answers
#The central limit theorem cannot tell us anything because this difference is not the average of a sample.
#Because Yij are approximately normal, the averages are normal too.
#If we assume N2 and N1 are large enough, Y¯2 and Y¯1, and their difference, are approximately normal.

# Exercise 15 - Calculate the 95% Confidence Interval of the Spreads
#We have constructed a random variable that has expected value b2???b1, the pollster bias difference. If our model holds, then this random variable has an approximately normal distribution. The standard error of this random variable depends on ??1 and ??2, but we can use the sample standard deviations we computed earlier. We have everything we need to answer our initial question: is b2???b1 different from 0?
#Construct a 95% confidence interval for the difference b2 and b1. Does this interval contain zero?
#Instructions
#Use pipes %>% to pass the data polls on to functions that will group by pollster and summarize the average spread, standard deviation, and number of polls per pollster.
#Calculate the estimate by subtracting the average spreads. Save this estimate to a variable called estimate.
#Calculate the standard error using the standard deviations of the spreads and the sample size. Save this value to a variable called se_hat.
#Calculate the 95% confidence intervals using the qnorm function. Save the lower and then the upper confidence interval to a variable called ci.

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.

res <-    polls %>% 
  group_by(pollster) %>%
  summarize(avg=mean(spread), s = sd(spread), N = n()) 
res

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- res$avg[2]-res$avg[1]
estimate

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$s[1]^2/res$N[1] + res$s[2]^2/res$N[2])
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate-qnorm(0.975)*se_hat, estimate+qnorm(0.975)*se_hat)
ci

# Exercise 16 - Calculate the P-value
#The confidence interval tells us there is relatively strong pollster effect resulting in a difference of about 5%. Random variability does not seem to explain it.
#Compute a p-value to relay the fact that chance does not explain the observed pollster effect.
#Instructions
#Use the pnorm function to calculate the probability that a random value is larger than the observed ratio of the estimate to the standard error.
#Multiply the probability by 2, because this is the two-tailed test.
# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value
2*(1 - (pnorm(estimate/se_hat)))

# Exercise 17 - Comparing Within-Poll and Between-Poll Variability
#e compute statistic called the t-statistic by dividing our estimate of b2???b1 by its estimated standard error:
#Y¯2???Y¯1s22/N2+s21/???N1
#Later we learn will learn of another approximation for the distribution of this statistic for values of N2 and N1 that aren't large enough for the CLT.
#Note that our data has more than two pollsters. We can also test for pollster effect using all pollsters, not just two. The idea is to compare the variability across polls to variability within polls. We can construct statistics to test for effects and approximate their distribution. The area of statistics that does this is called Analysis of Variance or ANOVA. We do not cover it here, but ANOVA provides a very useful set of tools to answer questions such as: is there a pollster effect?
#Compute the average and standard deviation for each pollster and examine the variability across the averages and how it compares to the variability within the pollsters, summarized by the standard deviation.
#Instructions
#Group the polls data by pollster.
#Summarize the average and standard deviation of the spreads for each pollster.
#Create an object called var that contains three columns: pollster, mean spread, and standard deviation.
#Be sure to name the column for mean avg and the column for standard deviation s.

# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()
head(polls)
# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var <-  polls %>% group_by(pollster) %>%
  summarize(avg=mean(spread), s=sd(spread))
var

#---------------------------------------------
#---------ASSESSMENT #5
#---------------------------------------------

# Exercise 1 - Statistics in the Courtroom
#In 1999 in England Sally Clark was found guilty of the murder of two of her sons. Both infants were found dead in the morning, one in 1996 and another in 1998, and she claimed the cause of death was sudden infant death syndrome (SIDS). No evidence of physical harm was found on the two infants so the main piece of evidence against her was the testimony of Professor Sir Roy Meadow, who testified that the chances of two infants dying of SIDS was 1 in 73 million. He arrived at this figure by finding that the rate of SIDS was 1 in 8,500 and then calculating that the chance of two SIDS cases was 8,500 × 8,500 ??? 73 million.
#Based on what we've learned throughout this course, which statement best describes a potential flaw in Sir Meadow's reasoning?
#Instructions
#Possible Answers
#Sir Meadow assumed the second death was independent of the first son being affected, thereby ignoring possible genetic causes.

# Exercise 2 - Recalculating the SIDS Statistics
#Let's assume that there is in fact a genetic component to SIDS and the the probability of Pr(second case of SIDS???first case of SIDS)=1/100, is much higher than 1 in 8,500.
#What is the probability of both of Sally Clark's sons dying of SIDS?
#Instructions
#Calculate the probability of both sons dying to SIDS.
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Calculate the probability of both sons dying of SIDS. Print this value to the console.
(Pr_1*Pr_2)

# Exercise 4 - Calculate the Probability
#Assume that the probability of a murderer finding a way to kill her two children without leaving evidence of physical harm is:
#Pr(two children found dead with no evidence of harm???mother is a murderer)=0.50
#Assume that the murder rate among mothers is 1 in 1,000,000.
#Pr(mother is a murderer)=1/1,000,000
#According to Bayes' rule, what is the probability of:
#Pr(mother is a murderer???two children found dead with no evidence of harm)
#Instructions
#Use Bayes' rule to calculate the probability that the mother is a murderer, considering the rates of murdering mothers in the population, the probability that two siblings die of SIDS, and the probability that a murderer kills children without leaving evidence of physical harm.
#Print your result to the console.
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
(Pr_BA * Pr_A) / Pr_B

# Exercise 6 - Back to Election Polls
#Florida is one of the most closely watched states in the U.S. election because it has many electoral votes and the election is generally close. Create a table with the poll spread results from Florida taken during the last days before the election using the sample code.
#The CLT tells us that the average of these spreads is approximately normal. Calculate a spread average and provide an estimate of the standard error.
#Instructions
#Calculate the average of the spreads. Call this average avg in the final table.
#Calculate an estimate of the standard error of the spreads. Call this standard error se in the final table.
#Use the mean and sd functions nested within summarize to find the average and standard deviation of the grouped spread data.
#Save your results in an object called results.

# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results <- polls %>% summarise(avg=mean(spread) ,se=sd(spread)/sqrt(n()))
results

# Exercise 7 - The Prior Distribution
#Assume a Bayesian model sets the prior distribution for Florida's election night spread d to be normal with expected value ?? and standard deviation ??.
#What are the interpretations of ?? and ???
#Instructions
#Possible Answers
#?? and ?? summarize what we would predict for Florida before seeing any polls.

# Exercise 8 - Estimate the Posterior Distribution
#The CLT tells us that our estimate of the spread d^ has a normal distribution with expected value d and standard deviation ??, which we calculated in a previous exercise.
#Use the formulas for the posterior distribution to calculate the expected value of the posterior distribution if we set ??=0 and ??=0.01.
#Instructions
#Define ?? and ??
#Identify which elements stored in the object results represent ?? and Y
#Estimate B using ?? and ??
#Estimate the posterior distribution using B, ??, and Y

# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- pull(results[2])
sigma

# Define a variable called `Y` that contains the average in the object `results`
Y <- pull(results[1])
Y

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B <- sigma^2/(sigma^2+tau^2)
B

# Calculate the expected value of the posterior distribution
B*mu+(1-B)*Y


# Exercise 9 - Standard Error of the Posterior Distribution
#Compute the standard error of the posterior distribution.
#Instructions
#Using the variables we have defined so far, calculate the standard error of the posterior distribution.
#Print this value to the console.
# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

# Compute the standard error of the posterior distribution. Print this value to the console.
sqrt(1/((1/sigma^2)+(1/tau^2)))

# Exercise 10- Constructing a Credible Interval
#Using the fact that the posterior distribution is normal, create an interval that has a 95% of occurring centered at the posterior expected value. Note that we call these credible intervals.
#Instructions
#Calculate the 95% credible intervals using the qnorm function.
#Save the lower and upper confidence intervals as an object called ci. Save the lower confidence interval first.


#---------------------------------------------
#-----ASSESSMENT 6.1
#---------------------------------------------

# Exercise 1 - Confidence Intervals of Polling Data
#For each poll in the polling data set, use the CLT to create a 95% confidence interval for the spread. Create a new table called cis that contains columns for the lower and upper limits of the confidence intervals.
#Instructions
#Use pipes %>% to pass the poll object on to the mutate function, which creates new variables.
#Create a variable called X_hat that contains the estimate of the proportion of Clinton voters for each poll.
#Create a variable called se that contains the standard error of the spread.
#Calculate the confidence intervals using the qnorm function and your calculated se.
#Use the select function to keep the following columns: state, startdate, enddate, pollster, grade, spread, lower, upper.

# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% 
  mutate("X_hat"=(spread+1)/2) %>%
  mutate("se_hat"=2*sqrt(X_hat*(1-X_hat)/samplesize)) %>%
  mutate("lower"=spread-qnorm(0.975)*se_hat) %>%
  mutate("upper"=spread+qnorm(0.975)*se_hat) %>%
  select("state", "startdate", "enddate", "pollster", 
         "grade", "spread", "lower", "upper")

head(cis)

# Exercise 2 - Compare to Actual Results
#You can add the final result to the cis table you just created using the left_join function as shown in the sample code.
#Now determine how often the 95% confidence interval includes the actual result.
#Instructions
#Create an object called p_hits that contains the proportion of intervals that contain the actual spread using the following two steps.
#Use the mutate function to create a new variable called hit that contains a logical vector for whether the actual_spread falls between the lower and upper confidence intervals.
#Summarize the proportion of values in hit that are true using the mean function inside of summarize.

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
p_hits <- ci_data %>% 
  mutate(hit= actual_spread>lower&actual_spread<upper) %>%
  summarize(mean(hit))

p_hits

# Exercise 3 - Stratify by Pollster and Grade
#Now find the proportion of hits for each pollster. Show only pollsters with at least 5 polls and order them from best to worst. Show the number of polls conducted by each pollster and the FiveThirtyEight grade of each pollster.
#Instructions
#Create an object called p_hits that contains the proportion of intervals that contain the actual spread using the following steps.
#Use the mutate function to create a new variable called hit that contains a logical vector for whether the actual_spread falls between the lower and upper confidence intervals.
#Use the group_by function to group the data by pollster.
#Use the filter function to filter for pollsters that have at least 5 polls.
#Summarize the proportion of values in hit that are true as a variable called proportion_hits. Also create new variables for the number of polls by each pollster (n) using the n() function and the grade of each poll (grade) by taking the first row of the grade column.
#Use the arrange function to arrange the proportion_hits in descending order.

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
head(ci_data)
p_hits <-  ci_data %>% 
  mutate(hit= actual_spread>lower&actual_spread<upper) %>%
  group_by(pollster) %>% 
  filter(n() >= 5) %>%
  summarize(proportion_hits=mean(hit), n = n(), grade=grade[1]) %>%
  arrange(desc(proportion_hits))

p_hits

# Exercise 4 - Stratify by State
#Repeat the previous exercise, but instead of pollster, stratify by state. Here we can't show grades.
#Instructions
#Create an object called p_hits that contains the proportion of intervals that contain the actual spread using the following steps.
#Use the mutate function to create a new variable called hit that contains a logical vector for whether the actual_spread falls between the lower and upper confidence intervals.
#Use the group_by function to group the data by state.
#Use the filter function to filter for states that have more than 5 polls.
#Summarize the proportion of values in hit that are true as a variable called proportion_hits. Also create new variables for the number of polls in each state using the n() function.
#Use the arrange function to arrange the proportion_hits in descending order.

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.

p_hits <- ci_data %>%
  mutate(hit= actual_spread>lower&actual_spread<upper) %>%
  group_by(state) %>% 
  filter(n() >= 5) %>%
  summarize(proportion_hits=mean(hit), n = n()) %>%
  arrange(desc(proportion_hits))

head(p_hits)

# Exercise 5- Plotting Prediction Results a barplot based on the result from the previous exercise.
#Instructions
#Reorder the states in order of the proportion of hits.
#Using ggplot, set the aesthetic with state as the x-variable and proportion of hits as the y-variable.
#Use geom_bar to indicate that we want to plot a barplot. Specifcy stat = "identity" to indicate that the height of the bar should match the value.
#Use coord_flip to flip the axes so the states are displayed from top to bottom and proportions are displayed from left to right.

# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(state, proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()


# Exercise 6 - Predicting the Winner
#Even if a forecaster's confidence interval is incorrect, the overall predictions will do better if they correctly called the right winner.
#Add two columns to the cis table by computing, for each poll, the difference between the predicted spread and the actual spread, and define a column hit that is true if the signs are the same.
#Instructions
#Use the mutate function to add two new variables to the cis object: error and hit.
#For the error variable, subtract the actual spread from the spread.
#For the hit variable, return "TRUE" if the poll predicted the actual winner. Use the sign function to check if their signs match - learn more with ?sign.
#Save the new table as an object called errors.
#Use the tail function to examine the last 6 rows of errors.

# The `cis` data have already been loaded. Examine it using the `head` function.
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Examine the last 6 rows of `errors`
tail(errors)

# Exercise 7 - Plotting Prediction Results
#Create an object called p_hits that contains the proportion of instances when the sign of the actual spread matches the predicted spread for states with 5 or more polls.
#Make a barplot based on the result from the previous exercise that shows the proportion of times the sign of the spread matched the actual result for the data in p_hits.
#Instructions
#Use the group_by function to group the data by state.
#Use the filter function to filter for states that have 5 or more polls.
#Summarize the proportion of values in hit that are true as a variable called proportion_hits. Also create a variable called n for the number of polls in each state using the n() function.
#To make the plot, follow these steps:
#Reorder the states in order of the proportion of hits.
#Using ggplot, set the aesthetic with state as the x-variable and proportion of hits as the y-variable.
#Use geom_bar to indicate that we want to plot a barplot.
#Use coord_flip to flip the axes so the states are displayed from top to bottom and proportions are displayed from left to right.

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

head(errors)

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls
p_hits <- errors %>%
  group_by(state) %>% 
  filter(n() >= 5) %>%
  summarize(proportion_hits=mean(hit), n = n()) %>%
  arrange(proportion_hits)
# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(state, proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Exercise 8 - Plotting the Errors
#In the previous graph, we see that most states' polls predicted the correct winner 100% of the time. Only a few states polls' were incorrect more than 25% of the time. Wisconsin got every single poll wrong. In Pennsylvania and Michigan, more than 90% of the polls had the signs wrong.
#Make a histogram of the errors. What is the median of these errors?
#Instructions
#Use the hist function to generate a histogram of the errors
#Use the median function to compute the median error

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate a histogram of the error
hist(errors$error)

# Calculate the median of the errors. Print this value to the console.
median(errors$error)

# Exercise 9- Plot Bias by State
#We see that, at the state level, the median error was slightly in favor of Clinton. The distribution is not centered at 0, but at 0.037. This value represents the general bias we described in an earlier section.
#Create a boxplot to examine if the bias was general to all states or if it affected some states differently. Filter the data to include only pollsters with grades B+ or higher.
#Instructions
#Use the filter function to filter the data for polls with grades equal to A+, A, A-, or B+.
#Use the reorder function to order the state data by error.
#Using ggplot, set the aesthetic with state as the x-variable and error as the y-variable.
#Use geom_boxplot to indicate that we want to plot a boxplot.
#Use geom_point to add data points as a layer.

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for polls with grades B+ or higher

errors %>% 
  filter(grade=="A+"|grade=="A"|grade=="B+"|grade=="B") %>%
  arrange(error) %>%
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exercise 10 - Filter Error Plot
#Some of these states only have a few polls. Repeat the previous exercise to plot the errors for each state, but only include states with five good polls or more.
#Instructions
#Use the filter function to filter the data for polls with grades equal to A+, A, A-, or B+.
#Group the filtered data by state using group_by.
#Use the filter function to filter the data for states with at least 5 polls. Then, use ungroup so that polls are no longer grouped by state.
#Use the reorder function to order the state data by error.
#Using ggplot, set the aesthetic with state as the x-variable and error as the y-variable.
#Use geom_boxplot to indicate that we want to plot a boxplot.
#Use geom_point to add data points as a layer.

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher

errors %>% 
  filter(grade=="A+"|grade=="A"|grade=="B+"|grade=="B") %>%
  group_by(state) %>%
  filter(n()>=5) %>%
  ungroup() %>%
  arrange(error) %>%
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#---------------------------------------------
#-----ASSESSMENT 6.2
#---------------------------------------------

# Exercise 1 - Using the t-Distribution
#We know that, with a normal distribution, only 5% of values are more than 2 standard deviations away from the mean.
#Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when the degrees of freedom are 3.
#Instructions
#Use the pt function to calculate the probability of seeing a value less than or equal to the argument. Your output should be a single value.

# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.

1 - pt(2, df = 3) + pt(-2, df = 3)

# Exercise 2 - Plotting the t-distribution
#Now use sapply to compute the same probability for degrees of freedom from 3 to 50.
#Make a plot and notice when this probability converges to the normal distribution's 5%.
#Instructions
#Make a vector called df that contains a sequence of numbers from 3 to 50.
#Using function, make a function called pt_func that recreates the calculation for the probability that a value is greater than 2 as an absolute value for any given degrees of freedom.
#Use sapply to apply the pt_func function across all values contained in df. Call these probabilities probs.
#Use the plot function to plot df on the x-axis and probs on the y-axis.

# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df<-3:50

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(degreeF){
  1 - pt(2, degreeF) + pt(-2, degreeF)
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df, pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df,probs)

# Exercise 3 - Sampling From the Normal Distribution
#In a previous section, we repeatedly took random samples of 50 heights from a distribution of heights. We noticed that about 95% of the samples had confidence intervals spanning the true population mean.
#Re-do this Monte Carlo simulation, but now instead of N=50, use N=15. Notice what happens to the proportion of hits.
#Instructions
#Use the replicate function to carry out the simulation. Specify the number of times you want the code to run and, within brackets, the three lines of code that should run.
#First use the sample function to randomly sample N values from x.
#Second, create a vector called interval that calculates the 95% confidence interval for the sample. You will use the qnorm function.
#Third, use the between function to determine if the population mean mu is contained between the confidence intervals.
#Save the results of the Monte Carlo function to a vector called res.
#Use the mean function to determine the proportion of hits in res.

# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res<-replicate(B,
               {
                 aux<-sample(x, N, replace = TRUE)
                 X_hat <- mean(aux)
                 se <- sd(aux)/sqrt(N) 
                 interval<-c(X_hat-qnorm(0.975)*se, 
                             X_hat+qnorm(0.975)*se)
                 between(mu, interval[1], interval[2])
               })

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

# Exercise 4 - Sampling from the t-Distribution
#N=15 is not that big. We know that heights are normally distributed, so the t-distribution should apply. Repeat the previous Monte Carlo simulation using the t-distribution instead of using the normal distribution to construct the confidence intervals.
#What are the proportion of 95% confidence intervals that span the actual mean height now?
#Instructions
#Use the replicate function to carry out the simulation. Specify the number of times you want the code to run and, within brackets, the three lines of code that should run.
#First use the sample function to randomly sample N values from x.
#Second, create a vector called interval that calculates the 95% confidence interval for the sample. Remember to use the qt function this time to generate the confidence interval.
#Third, use the between function to determine if the population mean mu is contained between the confidence intervals.
#Save the results of the Monte Carlo function to a vector called res.
#Use the mean function to determine the proportion of hits in res.

# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution

res<-replicate(B,
               {
                 aux<-sample(x, N, replace = TRUE)
                 X_hat <- mean(aux)
                 se <- sd(aux)/sqrt(N) 
                 interval<-c(X_hat-qt(0.975, N-1)*se, 
                             X_hat+qt(0.975, N-1)*se)
                 between(mu, interval[1], interval[2])
               })

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

# Exercise 5 - Why the t-Distribution?
#Why did the t-distribution confidence intervals work so much better?
#Instructions
#Possible Answers
#The t-distribution takes the variability into account and generates larger confidence intervals.


#---------------------------------------------
#-----ASSESSMENT 7
#---------------------------------------------

#Exercise 1 - Comparing Proportions of Hits
#In a previous exercise, we determined whether or not each poll predicted the correct winner for their state in the 2016 U.S. presidential election. Each poll was also assigned a grade by the poll aggregator. Now we're going to determine if polls rated A- made better predictions than polls rated C-.
#In this exercise, filter the errors data for just polls with grades A- and C-. Calculate the proportion of times each grade of poll predicted the correct winner.
#Instructions
#Filter errors for grades A- and C-.
#Group the data by grade and hit.
#Summarize the number of hits for each grade.
#Generate a two-by-two table containing the number of hits and misses for each grade. Try using the spread function to generate this table.
#Calculate the proportion of times each grade was correct.

# The 'errors' data have already been loaded. Examine them using the `head` function.
#head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-

totals <- errors %>% 
  filter(grade=="A-"|grade=="C-") %>%
  group_by(grade) %>%
  summarize(Num_hits=sum(hit)/n(), Num_misses=sum(hit==FALSE)/n())


totals

# Print the proportion of hits for grade A- polls to the console
pull(totals[1,2])

# Print the proportion of hits for grade C- polls to the console
pull(totals[2,2])

# Exercise 2 - Chi-squared Test
#We found that the A- polls predicted the correct winner about 80% of the time in their states and C- polls predicted the correct winner about 86% of the time.
#Use a chi-squared test to determine if these proportions are different.
#Instructions
#Use the chisq.test function to perform the chi-squared test. Save the results to an object called chisq_test.
#Print the p-value of the test to the console.

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.

# chi-squared test
chisq_test <- totals %>%
  select(-hit) %>% chisq.test()


# Print the p-value of the chi-squared test to the console
chisq_test$p.value

# Exercise 3 - Odds Ratio Calculation
#It doesn't look like the grade A- polls performed significantly differently than the grade C- polls in their states.
#Calculate the odds ratio to determine the magnitude of the difference in performance between these two grades of polls.
#Instructions
#Calculate the odds that a grade C- poll predicts the correct winner. Save this result to a variable called odds_C.
#Calculate the odds that a grade A- poll predicts the correct winner. Save this result to a variable called odds_A.
#Calculate the odds ratio that tells us how many times larger the odds of a grade A- poll is at predicting the winner than a grade C- poll.

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- (pull(totals[2,2]) / sum(pull(totals[2]))) /
  (pull(totals[1,2]) / sum(pull(totals[2])))

odds_C

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- (pull(totals[2,3]) / sum(pull(totals[3]))) /
  (pull(totals[1,3]) / sum(pull(totals[3])))

odds_A

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A / odds_C

# Exercise 4 - Significance
#We did not find meaningful differences between the poll results from grade A- and grade C- polls in this subset of the data, which only contains polls for about a week before the election. Imagine we expanded our analysis to include all election polls and we repeat our analysis. In this hypothetical scenario, we get that the p-value for the difference in prediction success if 0.0015 and the odds ratio describing the effect size of the performance of grade A- over grade B- polls is 1.07.
#Based on what we learned in the last section, which statement reflects the best interpretation of this result?
#Instructions
#Possible Answers
#The p-value is below 0.05, but the odds ratio is very close to 1. There is not a scientifically significant difference in performance.


#---------------------------------------------
#-----ASSESSMENT FINAL
#---------------------------------------------
install.packages("dslabs")

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

# Question 1: Expected value and standard error of a poll
#The final proportion of voters choosing "Remain" was p=0.481. Consider a poll with a sample of N=1500 voters.
#What is the expected total number of voters in the sample choosing "Remain"?
N<- 1500
print(p*N)

#What is the standard error of the total number of voters in the sample choosing "Remain"?
print(sqrt(p*(1-p)*N))

#What is the expected value of X^, the proportion of "Remain" voters?
print(p)

#What is the standard error of X^, the proportion of "Remain" voters?
print(sqrt(p*(1-p)/N))

#What is the expected value of d, the spread between the proportion of "Remain" voters and "Leave" voters?
print(d)

#What is the standard error of  d , the spread between the proportion of "Remain" voters and "Leave" voters?
print(2*sqrt(p*(1-p)/N))

# Question 2: Actual Brexit poll estimates
#Load and inspect the brexit_polls dataset from dslabs, which contains actual polling data for the 6 months before the Brexit vote. Raw proportions of voters preferring "Remain", "Leave", and "Undecided" are available (remain, leave, undecided) The spread is also available (spread), which is the difference in the raw proportion of voters choosing "Remain" and the raw proportion choosing "Leave".
#Calculate x_hat for each poll, the estimate of the proportion of voters choosing "Remain" on the referendum day (p=0.481), given the observed spread and the relationship d^=2X^???1. Use mutate() to add a variable x_hat to the brexit_polls object by filling in the skeleton code below:
  #brexit_polls <- brexit_polls %>%
  #mutate(x_hat = __________)
#What is the average of the observed spreads (spread)?

head(brexit_polls)

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

head(brexit_polls)
mean(brexit_polls$spread)

#What is the standard deviation of the observed spreads?
sd(brexit_polls$spread)

#What is the average of x_hat, the estimates of the parameter p?
mean(brexit_polls$x_hat)

#What is the standard deviation of x_hat?
sd(brexit_polls$x_hat)

# Question 3: Confidence interval of a Brexit poll
#Consider the first poll in brexit_polls, a YouGov poll run on the same day as the Brexit referendum:
#brexit_polls[1,]
#Use qnorm() to compute the 95% confidence interval for X^.
#What is the lower bound of the 95% confidence interval?
brexit_polls[1,]

q3_se<- sqrt(brexit_polls[1,]$x_hat*(1-brexit_polls[1,]$x_hat)/brexit_polls[1,]$samplesize)
brexit_polls[1,]$x_hat-qnorm(0.975)*q3_se

#What is the upper bound of the 95% confidence interval?
brexit_polls[1,]$x_hat+qnorm(0.975)*q3_se

# Question 4: Confidence intervals for polls in June
#Create the data frame june_polls containing only Brexit polls ending in June 2016 (enddate of "2016-06-01" and later). We will calculate confidence intervals for all polls and determine how many cover the true value of d.
#First, use mutate() to calculate a plug-in estimate se_x_hat for the standard error of the estimate SE^[X] for each poll given its sample size and value of X^ (x_hat). Second, use mutate() to calculate an estimate for the standard error of the spread for each poll given the value of se_x_hat. Then, use mutate() to calculate upper and lower bounds for 95% confidence intervals of the spread. Last, add a column hit that indicates whether the confidence interval for each poll covers the correct spread d=???0.038.
#How many polls are in june_polls?
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

head(brexit_polls)
june_polls <- brexit_polls %>% filter(enddate>="2016-06-01")
q4_nPolls <- nrow(june_polls)

#What proportion of polls have a confidence interval that covers the value 0?
d<- -0.038
june_polls <- june_polls %>% 
              mutate(se_x_hat = 2*sqrt(x_hat*(1-x_hat)/samplesize)) %>% 
              mutate("lower"=spread-qnorm(0.975)*se_x_hat) %>% 
              mutate("upper"=spread+qnorm(0.975)*se_x_hat) %>%
              mutate("hit"=lower < d & upper > d)
head(june_polls)
sum(june_polls$lower < 0 & june_polls$upper > 0)/q4_nPolls

#What proportion of polls predict "Remain" (confidence interval entirely above 0)?
sum(june_polls$lower > 0 & june_polls$upper > 0)/q4_nPolls

#What proportion of polls have a confidence interval covering the true value of  d ?
sum(june_polls$hit)/q4_nPolls

# Question 5: Hit rate by pollster
#Group and summarize the june_polls object by pollster to find the proportion of hits for each pollster and the number of polls per pollster. Use arrange() to sort by hit rate.
#Which of the following are TRUE?
head(june_polls)
june_polls %>%  group_by(pollster) %>%
                summarize(hit_rate = sum(hit)/n()) %>%
                arrange(hit_rate) %>%
                select(pollster, hit_rate)

# Question 6: Boxplot of Brexit polls by poll type
#Make a boxplot of the spread in june_polls by poll type.
#Which of the following are TRUE?

head(june_polls)

june_polls %>% 
  group_by(poll_type) %>%
  ggplot(aes(poll_type, spread)) +
  geom_boxplot() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Question 7: Combined spread across poll type
#Calculate the confidence intervals of the spread combined across all polls in june_polls, grouping by poll type. Recall that to determine the standard error of the spread, you will need to double the standard error of the estimate.
#Use this code (which determines the total sample size per poll type, gives each spread estimate a weight based on the poll's sample size, and adds an estimate of p from the combined spread) to begin your analysis:

combined_by_type <- june_polls %>%
        group_by(poll_type) %>%
        summarize(N = sum(samplesize),
                  spread = sum(spread*samplesize)/N,
                  p_hat = (spread + 1)/2,
                  se=2*sqrt(p_hat*(1-p_hat)/N),
                  lower=spread-qnorm(0.975)*se,
                  upper=spread+qnorm(0.975)*se)
#What is the lower bound of the 95% confidence interval for online voters?
head(combined_by_type)
#What is the upper bound of the 95% confidence interval for online voters?

# Brexit poll analysis - Part 3
#This problem set is continued from the previous page. Make sure you have run the following code:
  
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481
#Question 9: Chi-squared p-value
#Define brexit_hit, with the following code, which computes the confidence intervals for all Brexit polls in 2016 and then calculates whether the confidence interval covers the actual value of the spread d=???0.038:
  
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
#Use brexit_hit to make a two-by-two table of poll type and hit status. Then use the chisq.test() function to perform a chi-squared test to determine whether the difference in hit rate is significant.
#What is the p-value of the chi-squared test comparing the hit rate of online and telephone polls?
head(brexit_hit)

# construct two-by-two table for observed data
brexit_hit


# construct two-by-two table for observed data
a<-sum(brexit_hit$poll_type=="Online"&brexit_hit$hit==TRUE)
a
b<-sum(brexit_hit$poll_type=="Telephone"&brexit_hit$hit==TRUE)
b
c<-sum(brexit_hit$poll_type=="Online"&brexit_hit$hit==FALSE)
c
d<-sum(brexit_hit$poll_type=="Telephone"&brexit_hit$hit==FALSE)
d
two_by_two <- tibble(awarded = c("online", "telephone"),
                     n_true = c(a, b),
                     n_false = c(c, d))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%   chisq.test()
chisq_test$p.value
chisq_test

#Question 10: Odds ratio of online and telephone poll hit rate
#Use the two-by-two table constructed in the previous exercise to calculate the odds ratio between the hit rate of online and telephone polls to determine the magnitude of the difference in performance between the poll types.
#Calculate the odds that an online poll generates a confidence interval that covers the actual value of the spread.
two_by_two
total_online <- two_by_two[1,2] + two_by_two[1,3]
q10_odds_on <-  (two_by_two[1,2] / total_online) /
                (two_by_two[1,3] / total_online)
q10_odds_on
#Calculate the odds that a telephone poll generates a confidence interval that covers the actual value of the spread.
total_tele <- two_by_two[2,2] + two_by_two[2,3]
q10_odds_tele <-  (two_by_two[2,2] / total_tele) /
                  (two_by_two[2,3] / total_tele)
q10_odds_tele
#Calculate the odds ratio to determine how many times larger the odds are for online polls to hit versus telephone polls.
q10_odds_on / q10_odds_tele

# Question 11: Plotting spread over time
#Use brexit_polls to make a plot of the spread (spread) over time 
#(enddate) colored by poll type (poll_type). 
#Use geom_smooth() with method = "loess" to plot 
#smooth curves with a span of 0.4. Include the individual 
#data points colored by poll type. Add a horizontal line indicating 
#the final value of  d=???.038 .
brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -.038))

# Question 12: Plotting raw percentages over time
#Use the following code to create the object brexit_long, 
#which has a column vote containing the three possible votes on a Brexit poll 
#("remain", "leave", "undecided") and a column proportion containing the 
#raw proportion choosing that vote option on the given poll:
  brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
#Make a graph of proportion over time colored by vote. 
#Add a smooth trendline with geom_smooth() and method = "loess" with a 
#span of 0.3.
  brexit_long %>%
    ggplot(aes(enddate, proportion, color = vote)) +
    geom_smooth(method = "loess", span = 0.3) +
    geom_point() #+
    #geom_hline(aes(yintercept = -.038))












