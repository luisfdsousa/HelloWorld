# VIDEO: Discrete Probability
#Textbook link: https://rafalab.github.io/dsbook/probability.html#discrete-probability
#The probability of an event is the proportion of times the event occurs when we repeat the experiment independently under the same conditions.
#Pr(A)=probability of event A
#An event is defined as an outcome that can occur when when something happens by chance.
#We can determine probabilities related to discrete variables (picking a red bead, choosing 48 Democrats and 52 Republicans from 100 likely voters) and continuous variables (height over 6 feet).


# VIDEO: Monte Carlo Simulations
#Textbook link: https://rafalab.github.io/dsbook/probability.html#monte-carlo-simulations
#Monte Carlo simulations model the probability of different outcomes by repeating a random process a large enough number of times that the results are similar to what would be observed if the process were repeated forever.
#The sample function draws random outcomes from a set of options.
#The replicate function repeats lines of code a set number of times. It is used with sample and similar functions to run Monte Carlo simulations.
#Code: The rep function and the sample function
beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
length(beads)
sample(beads, 1)    # sample 1 bead at random
length(beads)
#Code: Monte Carlo simulation
#Note that your exact outcome values will differ because the sampling is random.

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

# we dont need to use replicate() in this case..
# pick 1 out of 5 over and over again under the same conditions
events <- sample(beads, B, replace = TRUE)
prop.table(table(events))

# Setting the Random Seed
#The set.seed function
#Before we continue, we will briefly explain the following important line of code:
set.seed(1986) 
#Throughout this book, we use random number generators. This implies that many of the results presented can actually change by chance, which then suggests that a frozen version of the book may show a different result than what you obtain when you try to code as shown in the book. This is actually fine since the results are random and change from time to time. However, if you want to to ensure that results are exactly the same every time you run them, you can set R's random number generation seed to a specific number. Above we set it to 1986. We want to avoid using the same seed every time. A popular way to pick the seed is the year - month - day. For example, we picked 1986 on December 20, 2018:  2018 ??? 12 ??? 20 = 1986.
#You can learn more about setting the seed by looking at the documentation:
?set.seed
#In the exercises, we may ask you to set the seed to assure that the results you obtain are exactly what we expect them to be.
#Important note on seeds in R 3.5 and R 3.6
#R was recently updated to version 3.6 in early 2019. In this update, the default method for setting the seed changed. This means that exercises, videos, textbook excerpts and other code you encounter online may yield a different result based on your version of R.
#If you are running R 3.6, you can revert to the original seed setting behavior by adding the argument sample.kind="Rounding". For example:
set.seed(1)
set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5
#Using the sample.kind="Rounding" argument will generate a message:
#non-uniform 'Rounding' sampler used
#This is not a warning or a cause for alarm - it is a confirmation that R is using the alternate seed generation method, and you should expect to receive this message in your console.
#If you use R 3.6, you should always use the second form of set.seed in this course series (outside of DataCamp assignments). Failure to do so may result in an otherwise correct answer being rejected by the grader. In most cases where a seed is required, you will be reminded of this fact.

# Using the mean Function for Probability
#An important application of the mean function
#In R, applying the mean function to a logical vector returns the proportion of elements that are TRUE. It is very common to use the mean function in this way to calculate probabilities and we will do so throughout the course.
#Suppose you have the vector beads from a previous video:
beads <- rep(c("red", "blue"), times = c(2,3))
beads
#To find the probability of drawing a blue bead at random, you can run:
mean(beads == "blue")
#This code is broken down into steps inside R. First, R evaluates the logical statement beads == "blue", which generates the vector:
#FALSE FALSE TRUE TRUE TRUE
#When the mean function is applied, R coerces the logical values to numeric values, changing TRUE to 1 and FALSE to 0:
#0 0 1 1 1
#The mean of the zeros and ones thus gives the proportion of TRUE values. As we have learned and will continue to see, probabilities are directly related to the proportion of events that satisfy a requirement.

# VIDEO: Probability Distributions
#Textbook link: https://rafalab.github.io/dsbook/probability.html#discrete-probability-distributions
#This video corresponds to the textbook section on probability distributions.
#The probability distribution for a variable describes the probability of observing each possible outcome.
#For discrete categorical variables, the probability distribution is defined by the proportions for each group.


# VIDEO: Independence
#Textbook link: https://rafalab.github.io/dsbook/probability.html#independence
#This video corresponds to the textbook section on independence, conditional probability and the multiplication rule.
#Errors and clarifications
#At 4:32 in the video, it should be 16 out of 51, not 12 out of 51. While the audio is incorrect, the transcript and the text on screen both now have the correct number.
#Note that this calculation only applies to getting blackjack in the order "Ace, Face/10". We consider the full probability including the possible order "Face/10, Ace" later when discussing the addition rule.
#Conditional probabilities compute the probability that an event occurs given information about dependent events. For example, the probability of drawing a second king given that the first draw is a king is:
#Pr(Card 2 is a king???Card 1 is a king)=3/51
#If two events A and B are independent, Pr(A???B)=Pr(A).
#To determine the probability of multiple events occurring, we use the multiplication rule.
#Equations
#The multiplication rule for independent events is:
#Pr(A and B and C)=Pr(A)×Pr(B)×Pr(C)
#The multiplication rule for dependent events considers the conditional probability of both events occurring:
#Pr(A and B)=Pr(A)×Pr(B???A)
#We can expand the multiplication rule for dependent events to more than 2 events:
#Pr(A and B and C)=Pr(A)×Pr(B???A)×Pr(C???A and B)

#----ASSESSMENT------------------------------------

# Exercise 1. Probability of cyan - generalized
#In the edX exercises for this section, we calculated some probabilities by hand. Now we'll calculate those probabilities using R.
#One ball will be drawn at random from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.
#What is the probability that the ball will be cyan?
#Instructions
#Define a variable p as the probability of choosing a cyan ball from the box.
#Print the value of p.
cyan <- 3
magenta <- 5
yellow <- 7

# Assign a variable `p` as the probability of choosing a cyan ball from the box
p <- cyan / (cyan+magenta+yellow)

# Print the variable `p` to the console
p

# Exercise 2. Probability of not cyan - generalized
#We defined the variable p as the probability of choosing a cyan ball from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.
#What is the probability that the ball you draw from the box will NOT be cyan?
#Instructions
#Using the probability of choosing a cyan ball, p, calculate the probability of choosing any other ball.

# `p` is defined as the probability of choosing a cyan ball from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.
# Using variable `p`, calculate the probability of choosing any ball that is not cyan from the box
1-p

# Exercise 3. Sampling without replacement - generalized
#Instead of taking just one draw, consider taking two draws. You take the second draw without returning the first draw to the box. We call this sampling without replacement.
#What is the probability that the first draw is cyan and that the second draw is not cyan?
#Instructions
#Calculate the conditional probability p_2 of choosing a ball that is not cyan after one cyan ball has been removed from the box.
#Calculate the joint probability of both choosing a cyan ball on the first draw and a ball that is not cyan on the second draw using p_1 and p_2.

cyan <- 3
magenta <- 5
yellow <- 7

# The variable `p_1` is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable `p_2` as the probability of not choosing a cyan ball on the second draw without replacement.
p_2 <- (magenta + yellow)/(cyan + magenta + yellow - 1)
# Calculate the probability that the first draw is cyan and the second draw is not cyan using `p_1` and `p_2`.
p_1 * p_2

# Exercise 4. Sampling with replacement - generalized
#Now repeat the experiment, but this time, after taking the first draw and recording the color, return it back to the box and shake the box. We call this sampling with replacement.
#What is the probability that the first draw is cyan and that the second draw is not cyan?
#Instructions
#Calculate the probability p_2 of choosing a ball that is not cyan on the second draw, with replacement.
#Next, use p_1 and p_2 to calculate the probability of choosing a cyan ball on the first draw and a ball that is not cyan on the second draw (after replacing the first ball).

cyan <- 3
magenta <- 5
yellow <- 7

# The variable 'p_1' is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable 'p_2' as the probability of not choosing a cyan ball on the second draw with replacement.
p_2 <- (magenta + yellow) / (cyan + magenta + yellow)
# Calculate the probability that the first draw is cyan and the second draw is not cyan using `p_1` and `p_2`.
p_1*p_2

#--------------------------------------------------
#Combinations and Permutations
# Key points
#paste joins two strings and inserts a space in between.
#expand.grid gives the combinations of 2 vectors or lists.
##permutations(n,r) from the gtools package lists the different ways that r items can be selected from a set of n options when order matters.
#combinations(n,r) from the gtools package lists the different ways that r items can be selected from a set of n options when order does not matter.
#Code: Introducing paste and expand.grid

# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("Shirt", "pants"), shirt = c("white", "grey", "plaid"))

#Code: Generating a deck of cards

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck
deck <- paste(deck$number, deck$suit)
deck

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)
mean(kings %in% deck)

#Code: Permutations and combinations
install.packages("gtools")
library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5

all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
n
index <- sample(n, 5)
index
all_phone_numbers[index,]
all_phone_numbers

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

#Code: Probability of drawing a second king given that one king is drawn

hands <- permutations(52,2, v = deck)
hands
first_card <- hands[,1]
first_card
second_card <- hands[,2]
second_card
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

#Code: Probability of a natural 21 in blackjack

aces <- paste("Ace", suits)
aces
facecard <- c("King", "Queen", "Jack", "Ten")
facecard
facecard <- expand.grid(number = facecard, suit = suits)
facecard
facecard <- paste(facecard$number, facecard$suit)
facecard

hands <- combinations(52, 2, v=deck) # all possible hands
hands[,1]
# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

#Code: Monte Carlo simulation of natural 21 in blackjack

#Note that your exact values will differ because the process is random and the seed is not set.

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

# The Birthday Problem
# Key points
#duplicated takes a vector and returns a vector of the same length with TRUE for any elements that have appeared previously in that vector.
#We can compute the probability of shared birthdays in a group of people by modeling birthdays as random draws from the numbers 1 through 365. We can then use this sampling model of birthdays to run a Monte Carlo simulation to estimate the probability of shared birthdays.
#Code: The birthday problem

# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays

# sapply
# The textbook discussion of the basics of sapply can be found in this textbook section.
#The textbook discussion of sapply for the birthday problem can be found within the birthday problem section.
#Key points
#Some functions automatically apply element-wise to vectors, such as sqrt and *.
#However, other functions do not operate element-wise by default. This includes functions we define ourselves.
#The function sapply(x, f) allows any other function f to be applied element-wise to the vector x.
#The probability of an event happening is 1 minus the probability of that event not happening:
#Pr(event)=1???Pr(no event)
#We can compute the probability of shared birthdays mathematically:
#Pr(shared birthdays)=1???Pr(no shared birthdays)=1???(1×364365×363365×...×365???n+1365)
#Code: Function for calculating birthday problem Monte Carlo simulations for any value of n
#Note that the function body of compute_prob is the code that we wrote in the previous video. If we write this code as a function, we can use sapply to apply this function to several values of n.
# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)
n
#Code: Element-wise operation over vectors and sapply

x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

#Computing birthday problem probabilities with sapply

# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

# How many Monte Carlo experiments are enough?
# Here is a link to the matching textbook section.
#Key points
#The larger the number of Monte Carlo replicates B, the more accurate the estimate.
#Determining the appropriate size for B can require advanced statistics.
#One practical approach is to try many sizes for B and look for sizes that provide stable estimates.
#Code: Estimating a practical value of B
#This code runs Monte Carlo simulations to estimate the probability of shared birthdays using several B values and plots the results. When B is large enough that the estimated probability stays stable, then we have selected a useful value of B. 

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
B
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

#-------------------------------------------------------
#---------ASSESSMENT

# Exercise 2. Sampling with replacement
#Say you've drawn 5 balls from the a box that has 3 cyan balls, 5 magenta balls, and 7 yellow balls, with replacement, and all have been yellow.
#What is the probability that the next one is yellow?
#Instructions
#Assign the variable p_yellow as the probability of choosing a yellow ball on the first draw.
#Using the variable p_yellow, calculate the probability of choosing a yellow ball on the sixth draw.

cyan <- 3
magenta <- 5
yellow <- 7

# Assign the variable 'p_yellow' as the probability that a yellow ball is drawn from the box.
p_yellow <- yellow / (cyan + magenta + yellow)
# Using the variable 'p_yellow', calculate the probability of drawing a yellow ball on the sixth draw. Print this value to the console.
p_yellow 

# Exercise 3. Rolling a die
#If you roll a 6-sided die once, what is the probability of not seeing a 6? If you roll a 6-sided die six times, what is the probability of not seeing a 6 on any of those rolls?
#Instructions
#Assign the variable p_no6 as the probability of not seeing a 6 on a single roll.
#Then, calculate the probability of not seeing a 6 on six rolls using p_no6.
# Assign the variable 'p_no6' as the probability of not seeing a 6 on a single roll.
p_no6 <- 5/6

# Calculate the probability of not seeing a 6 on six rolls using `p_no6`. Print your result to the console: do not assign it to a variable.
p_no6^6

# Exercise 4. Probability the Celtics win a game
#Two teams, say the Celtics and the Cavs, are playing a seven game series. The Cavs are a better team and have a 60% chance of winning each game.
#What is the probability that the Celtics win at least one game? Remember that the Celtics must win one of the first four games, or the series will be over!
#Instructions
#Calculate the probability that the Cavs will win the first four games of the series.
#Calculate the probability that the Celtics win at least one game in the first four games of the series.
# Assign the variable `p_cavs_win4` as the probability that the Cavs will win the first four games of the series.
p_cavs_win4 <- .6^4

# Using the variable `p_cavs_win4`, calculate the probability that the Celtics win at least one game in the first four games of the series.
1-p_cavs_win4

# Exercise 5. Monte Carlo simulation for Celtics winning a game
#Create a Monte Carlo simulation to confirm your answer to the previous problem by estimating how frequently the Celtics win at least 1 of 4 games. Use B <- 10000 simulations.
#The provided sample code simulates a single series of four random games, simulated_games.
#Instructions
#Use the replicate function for B <- 10000 simulations of a four game series. The results of replicate should be stored to a variable named celtic_wins.
#Within each simulation, replicate the sample code to simulate a four-game series named simulated_games. Then, use the any function to indicate whether the four-game series contains at least one win for the Celtics. Perform these operations in two separate steps.
#Use the mean function on celtic_wins to find the proportion of simulations that contain at least one win for the Celtics out of four games.
# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that replicates two steps for B iterations: (1) generating a random four-game series `simulated_games` using the example code, then (2) determining whether the simulated series contains at least one win for the Celtics.
celtic_wins <- replicate(B, {
  c_w <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(c_w == "win")
})
head (celtic_wins)
# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console.
mean(celtic_wins)    # calculates proportion of celtic wins


# ---------------------------
# The Addition Rule
# Here is a link to the textbook section on the addition rule.
#Clarification
#By "facecard", the professor means a card with a value of 10 (K, Q, J, 10).
#Key points
#The addition rule states that the probability of event A or event B happening is the probability of event A plus the probability of event B minus the probability of both events A and B happening together.
#Pr(A or B)=Pr(A)+Pr(B)???Pr(A and B)
#Note that (A or B) is equivalent to (A|B).
#Example: The addition rule for a natural 21 in blackjack
#We apply the addition rule where A = drawing an ace then a facecard and B = drawing a facecard then an ace. Note that in this case, both events A and B cannot happen at the same time, so Pr(A and B)=0.
#Pr(ace then facecard)=452×1651
#Pr(facecard then ace)=1652×451
#Pr(ace then facecard | facecard then ace)=452×1651+1652×451=0.0483 

# The Monty Hall Problem
# Here is the textbook section on the Monty Hall Problem.
#Error in Monty Hall explanation
#At 0:32, the professor incorrectly says that Monty Hall opens one of the two remaining doors only if the contestant did not pick the prize door on the first try. In the actual problem, Monty Hall always opens one of the two remaining doors (never revealing the prize). The Monte Carlo simulation code is correct for the actual problem.
#Key points
#Monte Carlo simulations can be used to simulate random outcomes, which makes them useful when exploring ambiguous or less intuitive problems like the Monty Hall problem.
#In the Monty Hall problem, contestants choose one of three doors that may contain a prize. Then, one of the doors that was not chosen by the contestant and does not contain a prize is revealed. The contestant can then choose whether to stick with the original choice or switch to the remaining unopened door.
#Although it may seem intuitively like the contestant has a 1 in 2 chance of winning regardless of whether they stick or switch, Monte Carlo simulations demonstrate that the actual probability of winning is 1 in 3 with the stick strategy and 2 in 3 with the switch strategy.
#For more on the Monty Hall problem, you can watch a detailed explanation here or read an explanation here.
#Code: Monte Carlo simulation of stick strategy

B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

#Code: Monte Carlo simulation of switch strategy

switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching

#-----------------------------------------------------------
#------------ASSESSMENT-----------------#

# Exercise 1. The Cavs and the Warriors
#Two teams, say the Cavs and the Warriors, are playing a seven game championship series. The first to win four games wins the series. The teams are equally good, so they each have a 50-50 chance of winning each game.
#If the Cavs lose the first game, what is the probability that they win the series?
#Instructions
#Assign the number of remaining games to the variable n.
#Assign a variable outcomes as a vector of possible outcomes in a single game, where 0 indicates a loss and 1 indicates a win for the Cavs.
#Assign a variable l to a list of all possible outcomes in all remaining games. Use the rep function to create a list of n games, where each game consists of list(outcomes).
#Use the expand.grid function to create a data frame containing all the combinations of possible outcomes of the remaining games.
#Use the rowSums function to identify which combinations of game outcomes result in the Cavs winning the number of games necessary to win the series.
#Use the mean function to calculate the proportion of outcomes that result in the Cavs winning the series and print your answer to the console.

# Assign a variable 'n' as the number of remaining games.
n <- 6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <- c(0:1) # 0 = lose, 1 = win

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
list(outcomes)
l <- rep(list(outcomes), n)
l

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- expand.grid(l)
possibilities

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- rowSums(possibilities) >= 4
results

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results)

# Exercise 2. The Cavs and the Warriors - Monte Carlo
#Confirm the results of the previous question with a Monte Carlo simulation to estimate the probability of the Cavs winning the series after losing the first game.
#Instructions
#Use the replicate function to replicate the sample code for B <- 10000 simulations.
#Use the sample function to simulate a series of 6 games with random, independent outcomes of either a loss for the Cavs (0) or a win for the Cavs (1) in that order. Use the default probabilities to sample.
#Use the sum function to determine whether a simulated series contained at least 4 wins for the Cavs.
#Use the mean function to find the proportion of simulations in which the Cavs win at least 4 of the remaining games. Print your answer to the console.

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results <- replicate(B, {
  c_w <- sample(c(0, 1), 6, replace = TRUE)
  sum(c_w) >= 4
})
mean(results)

# Exercise 3. A and B play a series - part 1
#Two teams, A
#and B, are playing a seven series game series. Team A is better than team B and has a p>0.5
#chance of winning each game.
#Instructions
#Use the function sapply to compute the probability, call it Pr of winning for p <- seq(0.5, 0.95, 0.025).
#Then plot the result plot(p, Pr).

# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr <- sapply(p, prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)

# Exercise 4. A and B play a series - part 2
#Repeat the previous exercise, but now keep the probability that team A
#wins fixed at p <- 0.75 and compute the probability for different series lengths. For example, wins in best of 1 game, 3 games, 5 games, and so on through a series that lasts 25 games.
#Instructions
#Use the seq function to generate a list of odd numbers ranging from 1 to 25.
#Use the function sapply to compute the probability, call it Pr, of winning during series of different lengths.
#Then plot the result plot(N, Pr)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
N <- seq(1,25, 2)
N

# Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this object `Pr`.
Pr <- sapply(N, prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N, Pr)

#----- ASSESSMENT FINAL SECTION

library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits

# Question 1: Olympic running
#In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).
#Use the information above to help you answer the following four questions.
#Question 1a
#How many different ways can the 3 medals be distributed across 8 runners?

distrubution <- permutations(8, 3)
#distrubution <- combinations(8, 3)
nrow(distrubution)

#  Question 1b
#How many different ways can the three medals be distributed among the 3 runners from Jamaica?
permutations(3, 3)

#  Question 1c
#What is the probability that all 3 medals are won by Jamaica?
nrow(permutations(3, 3)) / nrow(permutations(8, 3))

# Question 1d
#Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
#For each iteration of the Monte Carlo simulation, within a replicate loop, select 3 runners representing the 3 medalists and check whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
#Calculate the probability that all the runners are from Jamaica.
set.seed(1)
B<- 10000

Pr <- replicate(B, {
  winners <- sample(runners, 3) == c("Jamaica", "Jamaica", "Jamaica")
  sum(winners) == 3
  })
mean(Pr)

# Question 2: Restaurant management
#Use the information below to answer the following five questions.
#A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices.
#A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.

#Question 2a
#How many meal combinations are possible with the current menu?

desired_choices <- 365
food_types <- c("entree", "sides", "drink")
food_types_quant <- c(1,2,1)

#1 entree from a list of 6 options, 
#a choice of 2 different sides from a list of 6 options, 
#and a choice of 1 drink from a list of 2 options
current_special <- c(6,6,2)
n_entree <- combinations(6,1)
n_sides <- combinations(6,2)
n_drink <- combinations(2,1)

nrow(n_entree)*nrow(n_sides)*nrow(n_drink)

#Question 2b
#The manager has one additional drink he could add to the special.
#How many combinations are possible if he expands his original special to 3 drink options?
n_drink <- combinations(3,1)

nrow(n_entree)*nrow(n_sides)*nrow(n_drink)

#  Question 2c
#The manager decides to add the third drink but needs to expand the number of options. The manager would prefer not to change his menu further and wants to know if he can meet his goal by letting customers choose more sides.
#How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
n_sides <- combinations(6,3)
nrow(n_entree)*nrow(n_sides)*nrow(n_drink)


#  Question 2d
#The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order to meet his goal.
#- Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
#- Use sapply to apply the function to entree option counts ranging from 1 to 12.
#What is the minimum number of entree options required in order to generate more than 365 combinations?
n_sides <- combinations(6,2)
entree_options_n <- 1:12

func_total <- function(yy) {
  n_e <- combinations(yy,1)
  n_s <- combinations(6,2)
  n_d <- combinations(3,1)
  nrow(n_e)*nrow(n_s)*nrow(n_d)
}

total <- sapply(entree_options_n, func_total)
index <- entree_options_n[total >= 365] 
print(index)
plot(entree_options_n, total)

#  Question 2e
#The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.
#- Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.
#- Use sapply to apply the function to side counts ranging from 2 to 12.
#What is the minimum number of side options required in order to generate more than 365 combinations?

sides_options_n <- 2:12

func_total <- function(yy) {
  n_e <- combinations(6,1)
  n_s <- combinations(yy,2)
  n_d <- combinations(3,1)
  nrow(n_e)*nrow(n_s)*nrow(n_d)
}
total <- sapply(sides_options_n, func_total)
index <- sides_options_n[total >= 365] 
print(index)
plot(sides_options_n, total)

# Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1
#Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped by age range (agegp).
#The dataset is available in base R and can be called with the variable name esoph:
head(esoph)
#You will be using this dataset to answer the following four multi-part questions (Questions 3-6).
#You may wish to use the tidyverse package:
library(tidyverse)
#The following three parts have you explore some basic characteristics of the dataset.
#Each row contains one group of the experiment. Each group has a different combination of age, alcohol consumption, and tobacco consumption. The number of cancer cases and number of controls (individuals without cancer) are reported for each group.
#Question 3a
#How many groups are in the study?

nrow(esoph)

#  Question 3b
#How many cases are there?
all_cases <- sum(esoph$ncases)
all_cases

#  Question 3c
#How many controls are there?
all_controls <- sum(esoph$ncontrols)
all_controls
all_cases_total <- all_cases + all_controls
# Question 4a
#What is the probability that a subject in the highest alcohol consumption group is a cancer case?
head(esoph)
typeof(esoph$alcgp)
hac <- esoph %>% filter(alcgp == max(esoph$alcgp))
nrow(hac)
hac
sum(hac$ncases) / (sum(hac$ncases) + sum(hac$ncontrols))
q_4a <- sum(hac$ncases) / (sum(hac$ncases) + sum(hac$ncontrols))

# Question 4b
#What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
head(esoph)
hac <- esoph %>% filter(alcgp == min(esoph$alcgp))
nrow(hac)
hac
sum(hac$ncases) / (sum(hac$ncases) + sum(hac$ncontrols))

#  Question 4c
#Given that a person is a case, what is the probability that they smoke 10g or more a day?
head(esoph)
typeof(esoph$tobgp)
levels(esoph$tobgp)
hac <- esoph %>% filter(tobgp >= "10-19")
nrow(hac)
hac
sum(hac$ncases) / (all_cases)

#  Question 4d
#Given that a person is a control, what is the probability that they smoke 10g or more a day?
head(esoph)
hac <- esoph %>% filter(tobgp >= "10-19")
nrow(hac)
hac
sum(hac$ncontrols) / (all_controls)

#  Question 5a
#For cases, what is the probability of being in the highest alcohol group?

head(esoph)
typeof(esoph$alcgp)
hac <- esoph %>% filter(alcgp == max(esoph$alcgp))
nrow(hac)
hac
q_5a <- sum(hac$ncases) / (all_cases)

#  Question 5b
#For cases, what is the probability of being in the highest tobacco group?

head(esoph)
typeof(esoph$tobgp)
levels(esoph$tobgp)
hac <- esoph %>% filter(tobgp == max(esoph$tobgp))
nrow(hac)
hac
sum(hac$ncases) / (all_cases)

#  Question 5c
#For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?

head(esoph)
hac <- esoph %>% 
  filter((tobgp == max(esoph$tobgp)) & (alcgp == max(esoph$alcgp)))
nrow(hac)
hac
sum(hac$ncases) / (all_cases)

#  Question 5d
#For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?

head(esoph)
hac <- esoph %>% 
  filter((tobgp == max(esoph$tobgp)) | (alcgp == max(esoph$alcgp)))
nrow(hac)
hac
q_5d <- sum(hac$ncases) / (all_cases)

# Question 6a
#For controls, what is the probability of being in the highest alcohol group?

head(esoph)
typeof(esoph$alcgp)
hac <- esoph %>% filter(alcgp == max(esoph$alcgp))
nrow(hac)
hac
sum(hac$ncontrols) / (all_controls)
q_6a <- sum(hac$ncontrols) / (all_controls)

#  Question 6b
#How many times more likely are cases than controls to be in the highest alcohol group?
q_5a / q_6a

#  Question 6c
#For controls, what is the probability of being in the highest tobacco group?

head(esoph)
typeof(esoph$tobgp)
hac <- esoph %>% filter(tobgp == max(esoph$tobgp))
nrow(hac)
hac
sum(hac$ncontrols) / (all_controls)
q_6c <- sum(hac$ncontrols) / (all_controls)

#Question 6d
#For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?

head(esoph)
hac <- esoph %>% 
  filter((tobgp == max(esoph$tobgp)) & (alcgp == max(esoph$alcgp)))
nrow(hac)
hac
sum(hac$ncontrols) / (all_controls)

#  Question 6e
#For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?

head(esoph)
hac <- esoph %>% 
  filter((tobgp == max(esoph$tobgp)) | (alcgp == max(esoph$alcgp)))
nrow(hac)
hac
q_6e <- sum(hac$ncontrols) / (all_controls)

#  Question 6f
#How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
q_5d / q_6e

#--------------------
#-----------SESSION 2
# Continuous Probability
#Ths video corresponds to the textbook section on continuous probability.
#The previous discussion of CDF is from the Data Visualization course. Here is the textbook section on the CDF.
#Key points
#The cumulative distribution function (CDF) is a distribution function for continuous data x that reports the proportion of the data below a for all values of a:
#F(a)=Pr(x???a)
#The CDF is the probability distribution function for continuous variables. For example, to determine the probability that a male student is taller than 70.5 inches given a vector of male heights x, we can use the CDF:
#Pr(x>70.5)=1???Pr(x???70.5)=1???F(70.5)
#The probability that an observation is in between two values a,b is F(b)???F(a).
#Code: Cumulative distribution function
#Define x as male heights from the dslabs dataset:
  
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#Given a vector x, we can define a function for computing the CDF of x using:
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

#Theoretical Distribution
# This video corresponds to the textbook section on the theoretical distribution and the normal approximation.
#Key points
#pnorm(a, avg, s) gives the value of the cumulative distribution function F(a) for the normal distribution defined by average avg and standard deviation s.
#We say that a random quantity is normally distributed with average avg and standard deviation s if the approximation pnorm(a, avg, s) holds for all values of a.
#If we are willing to use the normal approximation for height, we can estimate the distribution simply from the mean and standard deviation of our values.
#If we treat the height data as discrete rather than categorical, we see that the data are not very useful because integer values are more common than expected due to rounding. This is called discretization.
#With rounded data, the normal approximation is particularly useful when computing probabilities of intervals of length 1 that include exactly one integer.
#Code: Using pnorm to calculate probabilities
#Given male heights x:
  
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

#We can estimate the probability that a male is taller than 70.5 inches using:
  
1 - pnorm(70.5, mean(x), sd(x))

#Code: Discretization and the normal approximation

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

# Probability Density
# This video corresponds to the textbook section on probability density.
#Key points
#The probability of a single value is not defined for a continuous distribution.
#The quantity with the most similar interpretation to the probability of a single value is the probability density function f(x).
#The probability density f(x) is defined such that the integral of f(x) over a range gives the CDF of that range.
#F(a)=Pr(X???a)=???a??????f(x)dx
#In R, the probability density function for the normal distribution is given by dnorm. We will see uses of dnorm in the future.
#Note that dnorm gives the density function and pnorm gives the distribution function, which is the integral of the density function.

# Monte Carlo Simulations
# This video corresponds to the textbook section on Monte Carlo simulations for continuous variables.
#Key points
#rnorm(n, avg, s) generates n random numbers from the normal distribution with average avg and standard deviation s.
#By generating random numbers from the normal distribution, we can simulate height data with similar properties to our dataset. Here we generate simulated height data using the normal distribution.
#Code: Generating normally distributed random numbers for Monte Carlo simulations

# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
ggplot(aes(simulated_heights)) +
geom_histogram(color="black", binwidth = 2)
#Code: Monte Carlo simulation of probability of tallest person being over 7 feet

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height 
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

#Other Continuous Distributions
# This video corresponds to the textbook section on other continuous distributions.
#Key points
##You may encounter other continuous distributions (Student t, chi-squared, exponential, gamma, beta, etc.).
#R provides functions for density (d), quantile (q), probability distribution (p) and random number generation (r) for many of these distributions.
#Each distribution has a matching abbreviation (for example, norm or t) that is paired with the related function abbreviations (d, p, q, r) to create appropriate functions.
#For example, use rt to generate random numbers for a Monte Carlo simulation using the Student t distribution.
#Code: Plotting the normal distribution with dnorm

#Use d to plot the density function of a continuous distribution. Here is the density function for the normal distribution (abbreviation norm):
  
  x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()

# ---------------------------------
# ----ASSESSMENT 

# Exercise 1. Distribution of female heights - 1
#Assume the distribution of female heights is approximated by a normal distribution with a mean of 64 inches and a standard deviation of 3 inches. If we pick a female at random, what is the probability that she is 5 feet or shorter?
#Instructions
#Use pnorm to define the probability that a height will take a value less than 5 feet given the stated distribution.

# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is shorter than 5 feet. Print this value to the console.

#1 foot = 12 inches
pnorm(5 * 12, female_avg, female_sd)

# Exercise 2. Distribution of female heights - 2
#Assume the distribution of female heights is approximated by a normal distribution with a mean of 64 inches and a standard deviation of 3 inches. If we pick a female at random, what is the probability that she is 6 feet or taller?
#Instructions
#Use pnorm to define the probability that a height will take a value of 6 feet or taller.

# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is 6 feet or taller. Print this value to the console.
1 - pnorm(6*12, female_avg, female_sd)

# Exercise 3. Distribution of female heights - 3
#Assume the distribution of female heights is approximated by a normal distribution with a mean of 64 inches and a standard deviation of 3 inches. If we pick a female at random, what is the probability that she is between 61 and 67 inches?
#Instructions
#Use pnorm to define the probability that a randomly chosen woman will be shorter than 67 inches.
#Subtract the probability that a randomly chosen will be shorter than 61 inches.

# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is between the desired height range. Print this value to the console.
pnorm(67, female_avg, female_sd) - pnorm(61,female_avg, female_sd)

# Exercise 4. Distribution of female heights - 4
#Repeat the previous exercise, but convert everything to centimeters. That is, multiply every height, including the standard deviation, by 2.54. What is the answer now?
#Instructions
#Convert the average height and standard deviation to centimeters by multiplying each value by 2.54.
#Repeat the previous calculation using pnorm to define the probability that a randomly chosen woman will have a height between 61 and 67 inches, converted to centimeters by multiplying each value by 2.54.

# Assign a variable 'female_avg' as the average female height. Convert this value to centimeters.
female_avg <- 64*2.54

# Assign a variable 'female_sd' as the standard deviation for female heights. Convert this value to centimeters.
female_sd <- 3*2.54

# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is between the desired height range. Print this value to the console.
pnorm(67*2.54, female_avg, female_sd) - pnorm(61*2.54,female_avg, female_sd)

# Exercise 5. Probability of 1 SD from average
#Compute the probability that the height of a randomly chosen female is within 1 SD from the average height.
#Instructions
#Calculate the values for heights one standard deviation taller and shorter than the average.
#Calculate the probability that a randomly chosen woman will be within 1 SD from the average height.

# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# To a variable named 'taller', assign the value of a height that is one SD taller than average.
taller <- female_avg + female_sd

# To a variable named 'shorter', assign the value of a height that is one SD shorter than average.
shorter <- female_avg - female_sd

# Calculate the probability that a randomly selected female is between the desired height range. Print this value to the console.
pnorm(taller, female_avg, female_sd) - pnorm(shorter, female_avg, female_sd)

# Exercise 6. Distribution of male heights
#Imagine the distribution of male adults is approximately normal with an average of 69 inches and a standard deviation of 3 inches. How tall is a male in the 99th percentile?
#Instructions
#Determine the height of a man in the 99th percentile, given an average height of 69 inches and a standard deviation of 3 inches.

# Assign a variable 'male_avg' as the average male height.
male_avg <- 69

# Assign a variable 'male_sd' as the standard deviation for male heights.
male_sd <- 3

# Determine the height of a man in the 99th percentile of the distribution.
qnorm(.99, male_avg, male_sd)

# Exercise 7. Distribution of IQ scores
#The distribution of IQ scores is approximately normally distributed. The average is 100 and the standard deviation is 15. Suppose you want to know the distribution of the person with the highest IQ in your school district, where 10,000 people are born each year.
#Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest IQ scores.
#Instructions
#Use the function rnorm to generate a random distribution of 10,000 values with a given average and standard deviation.
#Use the function max to return the largest value from a supplied vector.
#Repeat the previous steps a total of 1,000 times. Store the vector of the top 1,000 IQ scores as highestIQ.
#Plot the histogram of values using the function hist.
# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ <- replicate(B, {
  max(rnorm(10000, 100, 15))
})

# Make a histogram of the highest IQ scores.
hist(highestIQ)

#---------------------------------
#---ASSESSMENT 2.2
#Questions 1 and 2: ACT scores, part 1
#The ACT is a standardized college admissions test used in the United States. The four multi-part questions in this assessment all involve simulating some ACT test scores and answering probability questions about them.
#For the three year period 2016-2018, ACT standardized test scores were approximately normally distributed with a mean of 20.9 and standard deviation of 5.7. (Real ACT scores are integers between 1 and 36, but we will ignore this detail and use continuous values instead.)
#First we'll simulate an ACT test score dataset and answer some questions about it.
#Set the seed to 16, then use rnorm to generate a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of 5.7. Save these values as act_scores. You'll be using this dataset throughout these four multi-part questions.
#(IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.)
#Question 1a
#What is the mean of act_scores?
R.Version()
set.seed(16, sample.kind = "Rounding")

B <- 10000
act_scores <- rnorm(B, 20.9, 5.7)

mean(act_scores)
sd(act_scores)

#  Question 1c
#A perfect score is 36 or greater (the maximum reported score is 36).
#In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores >=36)

#  Question 1d
#In act_scores, what is the probability of an ACT score greater than 30?

sum(act_scores >30) / length(act_scores)

#  Question 1e
#In act_scores, what is the probability of an ACT score less than or equal to 10?
sum(act_scores <=10) / length(act_scores)

# Question 2
#Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density function over x given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x.
#Which of the following plots is correct?
  
x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

# or

x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line()

# Question 3a
#What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
act_avg <- 20.9
act_sd <-5.7
1- pnorm(act_avg + 2*act_sd, act_avg, act_sd)

#or

z_scores <- (act_scores - mean(act_scores))/sd(act_scores)
mean(z_scores > 2)

#  Question 3b
#What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
p_2sigma <- pnorm(act_avg + 2*act_sd, act_avg, act_sd)
p_2sigma

qnorm(p_2sigma, act_avg, act_sd)

#or

2*sd(act_scores) + mean(act_scores)

#  Question 3c
#A Z-score of 2 corresponds roughly to the 97.5th percentile.
#Use qnorm to determine the 97.5th percentile of normally distributed data with the mean and standard deviation observed in act_scores.
#What is the 97.5th percentile of act_scores?

#quartile(...)
summary(act_scores)
quantile(act_scores)
quantile(act_scores, probs = c(0.125,0.375,0.625,0.875, .975))

#or

qnorm(.975, mean(act_scores), sd(act_scores))

# In this 4-part question, you will write a function to create a CDF for ACT scores.
#Write a function that takes a value and produces the probability of an ACT score less than or equal to that value (the CDF). Apply this function to the range 1 to 36.
#Question 4a
#What is the minimum integer score such that the probability of that score or lower is at least .95?
#Your answer should be an integer 1-36.

prob_ACT_leX <- function(a){
  pnorm(a, act_avg, act_sd)
}

yy <- sapply(x, prob_ACT_leX)

x[sum(yy < .95)]

#or 

cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))

#  Question 4b
#Use qnorm to determine the expected 95th percentile, the value for which the probability of receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.
#What is the expected 95th percentile of ACT scores?

qnorm(.95, act_avg, act_sd)

#  Question 4c
#As discussed in the Data Visualization course, we can use quantile to determine sample quantiles from the data.
#Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores data. Save these as sample_quantiles.
#In what percentile is a score of 26?
#Note that a score between the 98th and 99th percentile should be considered the 98th percentile, for example, and that quantile numbers are used as names for the vector sample_quantiles.
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, probs = p)
sample_quantiles
plot(sample_quantiles, act_scores)

#or 

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])


#  Question 4d
#Make a corresponding set of theoretical quantiles using qnorm over the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles. Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
#Which of the following graphs is correct?
  
theoretical_quantiles <- qnorm(p, act_avg, act_sd)
plot(theoretical_quantiles, sample_quantiles )

#or 

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()


#------------------------
#---SESSION 3
# Random Variables
#This video corresponds to the textbook section on random variables.
#Key points
#Random variables are numeric outcomes resulting from random processes.
#Statistical inference offers a framework for quantifying uncertainty due to randomness.
#Code: Modeling a random variable

# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

# Sampling Models
# This video corresponds to the textbook section on sampling models.
#You can read more about the binomial distribution here.
#Key points
#A sampling model models the random behavior of a process as the sampling of draws from an urn.
#The probability distribution of a random variable is the probability of the observed value falling in any given interval.
#We can define a CDF F(a)=Pr(S???a) to answer questions related to the probability of S being in any interval.
#The average of many draws of a random variable is called its expected value.
#The standard deviation of many draws of a random variable is called its standard error.
#Monte Carlo simulation: Chance of casino losing money on roulette
#We build a sampling model for the random variable S that represents the casino's total winnings. 

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)    # 1000 draws from urn, -1 if red, else +1
X[1:10]    # first 10 outcomes

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

#We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
    X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 roulette spins
    sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

#We can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S.

library(tidyverse)

s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
    ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

# Distributions versus Probability Distributions
#This video corresponds to the textbook section on distributions versus probability distributions.
#Key points
#A random variable X has a probability distribution function F(a) that defines Pr(X???a) over all values of a.
#Any list of numbers has a distribution. The probability distribution function of a random variable is defined mathematically and does not depend on a list of numbers.
#The results of a Monte Carlo simulation with a large enough number of observations will approximate the probability distribution of X.
#If a random variable is defined as draws from an urn:
#The probability distribution function of the random variable is defined as the distribution of the list of values in the urn.
#The expected value of the random variable is the average of values in the urn.
#The standard error of one draw of the random variable is the standard deviation of the values of the urn.

#Notation for Random Variables
# This video corresponds to the textbook section on notation for random variables.
#Key points
#Capital letters denote random variables (X) and lowercase letters denote observed values (x).
#In the notation Pr(X=x), we are asking how frequently the random variable X is equal to the value x. For example, if x=6, this statement becomes Pr(X=6).

# Central Limit Theorem
# This video corresponds to the textbook sections on expected value and standard error and the Central Limit Theorem.
#Key points
#The Central Limit Theorem (CLT) says that the distribution of the sum of a random variable is approximated by a normal distribution.
#The expected value of a random variable, E[X]=??, is the average of the values in the urn. This represents the expectation of one draw. 
#The standard error of one draw of a random variable is the standard deviation of the values in the urn.
#The expected value of the sum of draws is the number of draws times the expected value of the random variable. 
#The standard error of the sum of independent draws of a random variable is the square root of the number of draws times the standard deviation of the urn. 

#-------------------------------
#-----ASSESSMENT 3.1
# Exercise 1. American Roulette probabilities
#An American roulette wheel has 18 red, 18 black, and 2 green pockets. Each red and black pocket is associated with a number from 1 to 36. The two remaining green slots feature "0" and "00". Players place bets on which pocket they think a ball will land in after the wheel is spun. Players can bet on a specific number (0, 00, 1-36) or color (red, black, or green).
#What are the chances that the ball lands in a green pocket?
#Instructions
#Define a variable p_green as the probability of the ball landing in a green pocket.
#Print the value of p_green.
# The variables `green`, `black`, and `red` contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green + black + red)

# Print the variable `p_green` to the console
print(p_green)

# Exercise 2. American Roulette payout
#In American roulette, the payout for winning on green is $17. This means that if you bet $1 and it lands on green, you get $17 as a prize.
#Create a model to predict your winnings from betting on green one time.
#Instructions
#Use the sample function return a random value from a specified range of values.
#Use the prob = argument in the sample function to specify a vector of probabilities for returning each of the values contained in the vector of values being sampled.
#Take a single sample (n = 1).
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 -p_green
color <- rep(c("Not-Green", "Green"), c(36, 2)) # define the urn 

# Create a model to predict the random variable `X`, your winnings from betting on green. Sample one time.
#?sample
X <- sample(c(-1, 17), size=1, prob = c(p_not_green, p_green))

# Print the value of `X` to the console
X

# Exercise 3. American Roulette expected value
#In American roulette, the payout for winning on green is $17. This means that if you bet $1 and it lands on green, you get $17 as a prize.In the previous exercise, you created a model to predict your winnings from betting on green.
#Now, compute the expected value of X
#, the random variable you generated previously.
#Instructions
#Using the chances of winning $17 (p_green) and the chances of losing $1 (p_not_green), calculate the expected outcome of a bet that the ball will land in a green pocket.

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green
p_green * 17 + p_not_green * -1


# Exercise 4. American Roulette standard error
#The standard error of a random variable X
#tells us the difference between a random variable and its expected value. You calculated a random variable X
#in exercise 2 and the expected value of that random variable in exercise 3.
#Now, compute the standard error of that random variable, which represents a single outcome after one spin of the roulette wheel.
#Instructions
#Compute the standard error of the random variable you generated in exercise 2, or the outcome of any one spin of the roulette wheel.
#Recall that the payout for winning on green is $17 for a $1 bet.

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Compute the standard error of the random variable
a <- 17
b <- -1
p <- p_green
abs(b-a)*sqrt(p*(1-p))

# Exercise 5. American Roulette sum of winnings
#You modeled the outcome of a single spin of the roulette wheel, X
#, in exercise 2.
#Now create a random variable S
#that sums your winnings after betting on green 1,000 times.
#Instructions
#Use set.seed to make sure the result of your random operation matches the expected answer for this problem.
#Specify the number of times you want to sample from the possible outcomes.
#Use the sample function to return a random value from a vector of possible values.
#Be sure to assign a probability to each outcome and to indicate that you are sampling with replacement.
#Do not use replicate as this changes the output of random sampling and your answer will not match the grader.

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 1000

# Create a vector called 'X' that contains the outcomes of 1000 samples
#?sample
X <- sample(c(17, -1), n, replace = TRUE, prob = c(p_green, p_not_green))

# Assign the sum of all 1000 outcomes to the variable 'S'
S <- sum(X)

# Print the value of 'S' to the console
print(S)

# Exercise 6. American Roulette winnings expected value
#In the previous exercise, you generated a vector of random outcomes, S
#, after betting on green 1,000 times.
#What is the expected value of S
#Instructions
#Using the chances of winning $17 (p_green) and the chances of losing $1 (p_not_green), calculate the expected outcome of a bet that the ball will land in a green pocket over 1,000 bets.

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Calculate the expected outcome of 1,000 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
a <- 17
b <- -1
p <- p_green
n*(p_green * 17 + p_not_green * -1)

# Exercise 7. American Roulette winnings expected value
#You generated the expected value of S
#, the outcomes of 1,000 bets that the ball lands in the green pocket, in the previous exercise.
#What is the standard error of S
#Instructions
#Compute the standard error of the random variable you generated in exercise 5, or the outcomes of 1,000 spins of the roulette wheel.

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Compute the standard error of the sum of 1,000 outcomes
a <- 17
b <- -1
p <- p_green
sqrt(n)*abs(b-a)*sqrt(p*(1-p))

#------------------------------------
#---SESSION 3.2

# Averages and Proportions
#This video corresponds to the textbook section on the statistical properties of averages.
#Key points
#Random variable times a constant
#The expected value of a random variable multiplied by a constant is that constant times its original expected value:
#E[aX]=a??
#The standard error of a random variable multiplied by a constant is that constant times its original standard error:
#SE[aX]=a??
#Average of multiple draws of a random variable
#The expected value of average of multiple draws from an urn is the expected value of the urn (??).
#The standard deviation of the average of multiple draws from an urn is the standard deviation of the urn divided by the square root of the number of draws (??/n?????????).
#The sum of multiple draws of a random variable
#The expected value of the sum of n draws of random variable is n times its original expected value:
#E[nX]=n??
#The standard error of the sum of n draws of random variable is n????????? times its original standard error:
#SE[nX]=n???????????
#The sum of multiple different random variables
#The expected value of the sum of different random variables is the sum of the individual expected values for each random variable:
#E[X1+X2+???+Xn]=??1+??2+???+??n
#The standard error of the sum of different random variables is the square root of the sum of squares of the individual standard errors:
#SE[X1+X2+???+Xn]=??21+??22+???+??2n????????????????????????????????????????????????
#Transformation of random variables
#If X is a normally distributed random variable and a and b are non-random constants, then aX+b is also a normally distributed random variable.


#Law of Large Numbers
#This video corresponds to the textbook section on the law of large numbers.
#Key points
#The law of large numbers states that as n increases, the standard error of the average of a random variable decreases. In other words, when n is large, the average of the draws converges to the average of the urn.
#The law of large numbers is also known as the law of averages.
#The law of averages only applies when n is very large and events are independent. It is often misused to make predictions about an event being "due" because it has happened less frequently than expected in a small sample size.


#How Large is Large in CLT?
# This video corresponds to the textbook section on sample size for CLT..
#You can read more about the Poisson distribution at this link.
#Key points
#The sample size required for the Central Limit Theorem and Law of Large Numbers to apply differs based on the probability of success.
#If the probability of success is high, then relatively few observations are needed.
#As the probability of success decreases, more observations are needed.
#If the probability of success is extremely low, such as winning a lottery, then the Central Limit Theorem may not apply even with extremely large sample sizes. The normal distribution is not a good approximation in these cases, and other distributions such as the Poisson distribution (not discussed in these courses) may be more appropriate.

#------------------------------------
#------------ASSESSMENT 3.2

# Exercise 1. American Roulette probability of winning money
#The exercises in the previous chapter explored winnings in American roulette. In this chapter of exercises, we will continue with the roulette example and add in the Central Limit Theorem.
#In the previous chapter of exercises, you created a random variable S
#that is the sum of your winnings after betting on green a number of times in American Roulette.
#What is the probability that you end up winning money if you bet on green 100 times?
#Instructions
#Execute the sample code to determine the expected value avg and standard error se as you have done in previous exercises.
#Use the pnorm function to determine the probability of winning money.
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1- pnorm(0, avg, se)

# Exercise 2. American Roulette Monte Carlo simulation
#Create a Monte Carlo simulation that generates 10,000 outcomes of S
#, the sum of 100 bets.
#Compute the average and standard deviation of the resulting list and compare them to the expected value (-5.263158) and standard error (40.19344) for S
#that you calculated previously.
#Instructions
#Use the replicate function to replicate the sample code for B <- 10000 simulations.
#Within replicate, use the sample function to simulate n <- 100 outcomes of either a win (17) or a loss (-1) for the bet. Use the order c(17, -1) and corresponding probabilities. Then, use the sum function to add up the winnings over all iterations of the model. Make sure to include sum or DataCamp may crash with a "Session Expired" error.
#Use the mean function to compute the average winnings.
#Use the sd function to compute the standard deviation of the winnings.

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S <- replicate(B, {
  sum(sample(c(17, -1), n, replace= TRUE, prob=c(p_green, p_not_green)))
})

# Compute the average value for 'S'
mean(S)

# Calculate the standard deviation of 'S'
sd(S)

# Exercise 3. American Roulette Monte Carlo vs CLT
#In this chapter, you calculated the probability of winning money in American roulette using the CLT.
#Now, calculate the probability of winning money from the Monte Carlo simulation. The Monte Carlo simulation from the previous exercise has already been pre-run for you, resulting in the variable S that contains a list of 10,000 simulated outcomes.
#Instructions
#Use the mean function to calculate the probability of winning money from the Monte Carlo simulation, S.

# Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S>0)

# Exercise 5. American Roulette average winnings per bet
#Now create a random variable Y
#that contains your average winnings per bet after betting on green 10,000 times.
#Instructions
#Run a single Monte Carlo simulation of 10,000 bets using the following steps. (You do not need to replicate the sample code.)
#Specify n as the number of times you want to sample from the possible outcomes.
#Use the sample function to return n values from a vector of possible values: winning $17 or losing $1. Be sure to assign a probability to each outcome and indicate that you are sampling with replacement.
#Calculate the average result per bet placed using the mean function.

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Create a vector called `X` that contains the outcomes of `n` bets
X <- sample(c(17,-1), n, replace=TRUE, prob=c(p_green, p_not_green))

# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y <- mean(X)
print(Y)

# Exercise 6. American Roulette per bet expected value
#What is the expected value of Y
#, the average outcome per bet after betting on green 10,000 times?
#Instructions
#Using the chances of winning $17 (p_green) and the chances of losing $1 (p_not_green), calculate the expected outcome of a bet that the ball will land in a green pocket.
#Use the expected value formula rather than a Monte Carlo simulation.
#Print this value to the console (do not assign it to a variable).
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Calculate the expected outcome of `Y`, the mean outcome per bet in 10,000 bets
n <- 10000
a <- 17
b <- -1
(p_green * a + b * p_not_green) 

# Exercise 7. American Roulette per bet standard error
#What is the standard error of Y
#, the average result of 10,000 spins?
#Instructions
#Compute the standard error of Y
#, the average result of 10,000 independent spins.
# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
n <- 10000
a <- 17
b <- -1
(abs(b-a) * sqrt(p_green * p_not_green))/ sqrt(n)

# Exercise 8. American Roulette winnings per game are positive
#What is the probability that your winnings are positive after betting on green 10,000 times?
#Instructions
#Execute the code that we wrote in previous exercises to determine the average and standard error.
#Use the pnorm function to determine the probability of winning more than $0.

# We defined the average using the following code
avg <- 17*p_green + -1*p_not_green

# We defined standard error using this equation
se <- 1/sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Given this average and standard error, determine the probability of winning more than $0. Print the result to the console.
1-pnorm(0, avg, se)

# Exercise 9. American Roulette Monte Carlo again
#Create a Monte Carlo simulation that generates 10,000 outcomes of S
#, the average outcome from 10,000 bets on green.
#Compute the average and standard deviation of the resulting list to confirm the results from previous exercises using the Central Limit Theorem.
#Instructions
#Use the replicate function to model 10,000 iterations of a series of 10,000 bets.
#Each iteration inside replicate should simulate 10,000 bets and determine the average outcome of those 10,000 bets. If you forget to take the mean, DataCamp will crash with a "Session Expired" error.
#Find the average of the 10,000 average outcomes. Print this value to the console.
#Compute the standard deviation of the 10,000 simulations. Print this value to the console.
## Make sure you fully follow instructions, including printing values to the console and correctly running the `replicate` loop. If not, you may encounter "Session Expired" errors.

# The variable `n` specifies the number of independent bets on green
n <- 10000

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)

# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
S <- replicate(B, {
  mean(sample(c(17, -1), n, replace = TRUE, prob=c(p_green, p_not_green)))
})

# Compute the average of `S`
mean(S)

# Compute the standard deviation of `S`
sd(S)

# Exercise 10. American Roulette comparison
#In a previous exercise, you found the probability of winning more than $0 after betting on green 10,000 times using the Central Limit Theorem. Then, you used a Monte Carlo simulation to model the average result of betting on green 10,000 times over 10,000 simulated series of bets.
#What is the probability of winning more than $0 as estimated by your Monte Carlo simulation? The code to generate the vector S that contains the the average outcomes of 10,000 bets modeled 10,000 times has already been run for you.
#Instructions
#Calculate the probability of winning more than $0 in the Monte Carlo simulation from the previous exercise using the mean function.
#You do not need to run another simulation: the results of the simulation are in your workspace as the vector S.

# Compute the proportion of outcomes in the vector 'S' where you won more than $0
mean(S > 0)

#--------------------------
#-------ASSESSMENT 3.3

#Questions 1 and 2: SAT testing
#The SAT is a standardized college admissions test used in the United States. The following two multi-part questions will ask you some questions about SAT testing.
#This is a 6-part question asking you to determine some probabilities of what happens when a student guessed for all of their answers on the SAT. Use the information below to inform your answers for the following questions.
#An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses answers by guessing for all questions on the test.
#Question 1a
#What is the probability of guessing correctly for one question?

penalty <- -.25
award <- 1.0
n_quant_questions_total <- 44
n_quant_questions_options <- 5

q1_a <- 1/5

#  Question 1b
#What is the expected value of points for guessing on one question?
q1_b <- q1_a * award + (1-q1_a) * penalty
print(q1_b)

#  Question 1c
#What is the expected score of guessing on all 44 questions?
q1_c <- q1_b^n_quant_questions_total
print(q1_c)

#  Question 1d
#What is the standard error of guessing on all 44 questions?
q1_d <- sqrt(n_quant_questions_total) * 
        abs(penalty-award) * 
        sqrt(q1_a * (1-q1_a))
print(q1_d)

# Question 1e
#Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.
q1_e <- 1 - pnorm(8, q1_c, q1_d)
print(q1_e)

#  Question 1f
#Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
#(IMPORTANT! If you use R 3.6 or later, you will need to use the command set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.)
#What is the probability that a guessing student scores 8 points or higher?
set.seed(21, sample.kind = "Rounding")
B <- 10000
sum_sim_SAT <- replicate(B, {
  result <- sample(c(award, penalty), 
            n_quant_questions_total, 
            replace = TRUE, 
            prob = c(q1_a, 1-q1_a))
  sum(result)
})
q1_f <- mean(sum_sim_SAT >= 8)
print(q1_f)


#The SAT was recently changed to reduce the number of multiple choice options from 5 to 4 and also to eliminate the penalty for guessing.
#In this three-part question, you'll explore how that affected the expected values for the test.
#Question 2a
#Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question gives a score of 0.
#What is the expected value of the score when guessing on this new test?

penalty <- 0
award <- 1
n_quant_questions_total <- 44
n_quant_questions_options <- 4
p_award <- 1/4

q2_a <- p_award * award * n_quant_questions_total
print(q2_a)


#  Question 2b
#Using the normal approximation, what is the estimated probability of scoring over 30 when guessing?
#Report your answer using scientific notation with 3 significant digits in the format x.xx*10^y. Do not round the values passed to pnorm or you will lose precision and have an incorrect answer.
sigma <-  sqrt(n_quant_questions_total) * 
          abs(award) * 
          sqrt(p_award*(1-p_award))
sigma
q2_b <- 1 - pnorm(30, q2_a, sigma)
print(q2_b)

#  Question 2c
#Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.
#What is the lowest p such that the probability of scoring over 35 exceeds 80%?

p <- seq(0.25, 0.95, 0.05)
E_student_skills <- p * award * n_quant_questions_total
print(E_student_skills)

sigma <-  sqrt(n_quant_questions_total) * 
  abs(award) * 
  sqrt(p*(1-p))
print(sigma)

prob_greater_35 <- 1 - pnorm(35, E_student_skills, sigma)
p[prob_greater_35 > .8]

# Question 3: Betting on Roulette
#A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special.
#The following 7-part question asks you to do some calculations related to this scenario.
#Question 3a
#What is the expected value of the payout for one bet?

lose <- -1
win <- 6
n <- 500
p_win <- 5/38
p_lose <- 1-p_win

q3_a <- p_win * win + p_lose * lose
print(q3_a)

#  Question 3b
#What is the standard error of the payout for one bet?
q3_b <- abs(lose-win) * sqrt(p_win * p_lose)
print(q3_b)

#  Question 3c
#What is the expected value of the average payout over 500 bets?
#Remember there is a difference between expected value of the average and expected value of the sum.

q3_c <- q3_a
print(q3_c)

#  Question 3d
#What is the standard error of the average payout over 500 bets?
#Remember there is a difference between the standard error of the average and standard error of the sum.

q3_d <- (abs(lose-win) * sqrt(p_win*p_lose)) / sqrt(n) 
print(q3_d)

#  Question 3e
#What is the expected value of the sum of 500 bets?
q3_e <- n * q3_a
print(q3_e)

#  Question 3f
#What is the standard error of the sum of 500 bets?
q3_f <- sqrt(n) * q3_b
print(q3_f)

#  Question 3g
#Use pnorm with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets, \( \mbox{Pr}(X \leq 0) \).
q3_g <- pnorm(0, q3_e, q3_f)
print(q3_g)


#DIFFERNCE BETWEEN EXPECTED VALUE AND MEAN:
#Expected value of a  random variable is a weighted
#(based on density function) mean. Sample mean of a 
#random variable corresponds to average of samples 
 
#------------------------------
# -----------------SESSION 4

# The Big Short: Interest Rates Explained
# The Big Short
#See word document: Key Learnings

#--------------
#----ASSESSMENT 4.1

# Exercise 1. Bank earnings
#Say you manage a bank that gives out 10,000 loans. The default rate is 0.03 and you lose $200,000 in each foreclosure.
#Create a random variable S
#that contains the earnings of your bank. Calculate the total amount of money lost in this scenario.
#Instructions
#Using the sample function, generate a vector called defaults that contains n samples from a vector of c(0,1), where 0 indicates a payment and 1 indicates a default
#Multiply the total number of defaults by the loss per foreclosure.

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults <- sample(c(0,1), n, replace=TRUE, prob=c(1-p_default, p_default))

# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S <- sum(defaults) * loss_per_foreclosure
S

# Exercise 2. Bank earnings Monte Carlo
#Run a Monte Carlo simulation with 10,000 outcomes for S
#, the sum of losses over 10,000 loans. Make a histogram of the results.
#Instructions
#Within a replicate loop with 10,000 iterations, use sample to generate a list of 10,000 loan outcomes: payment (0) or default (1). Use the outcome order c(0,1) and probability of default p_default.
#Still within the loop, use the function sum to count the number of foreclosures multiplied by loss_per_foreclosure to return the sum of all losses across the 10,000 loans. If you do not take the sum inside the replicate loop, DataCamp may crash with a "Session Expired" error.
#Plot the histogram of values using the function hist.

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Generate a list of summed losses 'S'. Replicate the code from the previous exercise over 'B' iterations to generate a list of summed losses for 'n' loans.  Ignore any warnings for now.
S <- replicate( B, {
  x <- sample(c(0,1), n, replace=TRUE, prob=c(1-p_default, p_default))
  sum(x) * loss_per_foreclosure
})

# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(S)

# Exercise 3. Bank earnings expected value
#What is the expected value of S
#, the sum of losses over 10,000 loans? For now, assume a bank makes no money if the loan is paid.
#Instructions
#Using the chances of default (p_default), calculate the expected losses over 10,000 loans.

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Calculate the expected loss due to default out of 10,000 loans
n * (loss_per_foreclosure * p_default)

# Exercise 4. Bank earnings standard error
#What is the standard error of S?
#Instructions
#Compute the standard error of the random variable S you generated in the previous exercise, the summed outcomes of 10,000 loans.

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Compute the standard error of the sum of 10,000 loans
sqrt(n) * abs(loss_per_foreclosure) * sqrt(p_default*(1-p_default)) 

# Exercise 5. Bank earnings interest rate - 1
#So far, we've been assuming that we make no money when people pay their loans and we lose a lot of money when people default on their loans. Assume we give out loans for $180,000. How much money do we need to make when people pay their loans so that our net loss is $0?
#In other words, what interest rate do we need to charge in order to not lose money?
#Instructions
#If the amount of money lost or gained equals 0, the probability of default times the total loss per default equals the amount earned per probability of the loan being paid.
# Divide the total amount needed per loan by the loan amount to determine the interest rate.

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Assign a variable `x` as the total amount necessary to have an expected outcome of $0
x <- (-loss_per_foreclosure*p_default) / (1-p_default)

# Convert `x` to a rate, given that the loan amount is $180,000. Print this value to the console.
x/180000    # interest rate

# Exercise 6. Bank earnings interest rate - 2
#With the interest rate calculated in the last example, we still lose money 50% of the time. What should the interest rate be so that the chance of losing money is 1 in 20?
#In math notation, what should the interest rate be so that Pr(S<0)=0.05?
#Instructions
#Use the qnorm function to compute a continuous variable at given quantile of the distribution to solve for z.
#In this equation, l, p, and n
#are known values. Once you've solved for z, solve for x.
#Divide x by the loan amount to calculate the rate.

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Generate a variable `z` using the `qnorm` function
z = qnorm(0.05)

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x <- -loss_per_foreclosure*( n*p_default - z*sqrt(n*p_default*(1-p_default)))/ ( n*(1-p_default) + z*sqrt(n*p_default*(1-p_default)))

# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x <- x / 180000
x

# -------------------------------------------
#--------ASSESSMENT 4.2
update.packages()
options(digits = 3)
library(tidyverse)
library(dslabs)

data(death_prob)
head(death_prob)

# Questions 1 and 2: Insurance rates, part 1
#Use the information below as you answer this 6-part question.
#An insurance company offers a one-year term life insurance policy 
#that pays $150,000 in the event of death within one year. 
#The premium (annual cost) for this policy for a 50 year old female 
#is $1,150. Suppose that in the event of a claim, the company 
#forfeits the premium and loses a total of $150,000, and if there 
#is no claim the company gains the premium amount of $1,150. The 
#company plans to sell 1,000 policies to this demographic.
#Question 1a
#The death_prob data frame contains information about the estimated
#probability of death within 1 year (prob) for different ages and
#sexes.
#Use death_prob to determine the death probability of a 50 year
#old female, p.

term <- 1 #year
death_penalty <- -150000
premium <- 1150 # female
age <- 50
n <- 1000

head(death_prob)
q1_a <- death_prob %>% 
        filter(sex=="Female" & age==50) %>% 
        pull(prob)
death_prob_f_50 <- q1_a
print(q1_a)

#  Question 1b
#The loss in the event of the policy holder's death 
#is -$150,000 and the gain if the policy holder remains alive is 
#the premium $1,150.
#What is the expected value of the company's net profit on one 
#policy for a 50 year old female?

q1_b <- death_prob_f_50 * death_penalty + 
        (1-death_prob_f_50) * premium
print(q1_b)

#  Question 1c
#Calculate the standard error of the profit on one policy 
#for a 50 year old female.
q1_c <- abs(premium-death_penalty)*
        sqrt(death_prob_f_50*(1-death_prob_f_50))
print(q1_c)

#  Question 1d
# What is the expected value of the company's profit over all 
#1,000 policies for 50 year old females?
q1_d <- q1_b * n
print(q1_d)

#  Question 1e
#What is the standard error of the sum of the expected value over 
#all 1,000 policies for 50 year old females?
q1_e <- q1_c * sqrt(n)
print(q1_e)

#  Question 1f
#Use the Central Limit Theorem to calculate the probability 
#that the insurance company loses money on this set of 1,000 
#policies
q1_f <- pnorm(0, q1_d, q1_e)
print(q1_f)

# 50 year old males have a different probability of death than 
#50 year old females. We will calculate a profitable premium for 
#50 year old males in the following four-part question.
#Question 2a
#Use death_prob to determine the probability of death within one 
#year for a 50 year old male.

q2_a <- death_prob %>% 
  filter(sex=="Male" & age==50) %>% 
  pull(prob)
death_prob_m_50 <- q2_a
print(q2_a)


#  Question 2b
#Suppose the company wants its expected profits from 1,000 
#50 year old males with $150,000 life insurance policies to 
#be $700,000. Use the formula for expected value of the sum 
#of draws with the following values and solve for the 
#premium b:
#E[S]=??S=700000
#n=1000
#p=death probability of age 50 male
#a=150000
#b=premium
#What premium should be charged?
n
desired_expected_profits_per_policy <- 700000 / n
desired_expected_profits_per_policy
death_penalty
q2_b = (desired_expected_profits_per_policy - (death_prob_m_50 * death_penalty)) / (1-death_prob_m_50)
print(q2_b)

#  Question 2c
#Using the new 50 year old male premium rate, calculate the standard error 
#of the sum of 1,000 premiums.

q2_c <- sqrt(n) * abs(q2_b - death_penalty) * 
        sqrt(death_prob_m_50*(1-death_prob_m_50))
print(q2_c)

#  Question 2d
#What is the probability of losing money on a series of 1,000 policies 
#to 50 year old males?
#Use the Central Limit Theorem.
m <- n * (death_prob_m_50 * death_penalty + 
  (1-death_prob_m_50) * q2_b)
q2_d <- pnorm(0, m, q2_c)
print(q2_d)

# Questions 3 and 4: insurance rates, part 2
#Life insurance rates are calculated using mortality statistics from the 
#recent past. They are priced such that companies are almost assured to profit 
#as long as the probability of death remains similar. If an event occurs that 
#changes the probability of death in a given age group, the company risks 
#significant losses.
#In this 6-part question, we'll look at a scenario in which a lethal pandemic 
#disease increases the probability of death within 1 year for a 50 year old to 
#.015. Unable to predict the outbreak, the company has sold 1,000 $150,000 life 
#insurance policies for $1,150.
n <- 1000
premium <- 1150
p_death <- .015
payout <- -150000

#Question 3a
#What is the expected value of the company's profits over 1,000 policies?
q3_a <- n* (p_death*payout + (1-p_death)*premium)
print(q3_a)

#  Question 3b
#What is the standard error of the expected value of the company's 
#profits over 1,000 policies?
q3_b <- sqrt(n) * abs(payout-premium) *sqrt(p_death*(1-p_death))
print(q3_b)

#  Question 3c
#What is the probability of the company losing money? Question 3c
q3_c <- pnorm(0, q3_a, q3_b)
print(q3_c)

#  Question 3d
#Suppose the company can afford to sustain one-time losses of $1 million, 
#but larger losses will force it to go out of business.
#What is the probability of losing more than $1 million?
q3_d <- pnorm(-1000000, q3_a, q3_b)
print(q3_d)

# Question 3e
#Investigate death probabilities p <- seq(.01, .03, .001).
#What is the lowest death probability for which the chance of 
#losing money exceeds 90%?
p <- seq(.01, .03, .001)

p_sim <- function(probability){
  m <- n* (probability*payout + (1-probability)*premium)
  sigma <- sqrt(n) * abs(payout-premium) *sqrt(probability*(1-probability))
  pnorm(0, m, sigma)
}
results <- sapply(p, p_sim)
q3_e <- first(p[results>.9])
print(q3_e)

#  Question 3f
#Investigate death probabilities p <- seq(.01, .03, .0025).
#What is the lowest death probability for which the chance of losing over 
#$1 million exceeds 90%?
p <- seq(.01, .03, .001)

p_sim2 <- function(probability){
  m <- n* (probability*payout + (1-probability)*premium)
  sigma <- sqrt(n) * abs(payout-premium) *sqrt(probability*(1-probability))
  pnorm(-1000000, m, sigma)
}
results <- sapply(p, p_sim2)
q3_f <- first(p[results>.9])
print(q3_f)

#Question 4, which has two parts, continues the scenario from Question 3.
#Question 4a
#Define a sampling model for simulating the total profit over 1,000 loans 
#with probability of claim p_loss = .015, loss of -$150,000 on a claim, 
#and profit of $1,150 when there is no claim. Set the seed to 25, then run 
#the model once.
#(IMPORTANT! If you use R 3.6 or later, you will need to use the command 
#set.seed(x, sample.kind = "Rounding") instead of set.seed(x). 
#Your R version will be printed at the top of the Console window when you 
#start RStudio.)
#What is the reported profit (or loss) in millions (that is, divided by 106)?
set.seed(25, sample.kind = "Rounding")
p_loss <- .015
premium <- 1150
loss <- -150000

q4_a <- sum(sample(c(1150,-150000), 
                   n, 
                   replace = TRUE, 
                   prob = c(1-p_loss, p_loss))) / 10^6
print(q4_a)

#  Question 4b
#Set the seed to 27, then run a Monte Carlo simulation of your sampling model 
#with 10,000 replicates to simulate the range of profits/losses over 1,000 loans.
#(IMPORTANT! If you use R 3.6 or later, you will need to use the 
#command set.seed(x, sample.kind = "Rounding") instead of set.seed(x). 
#Your R version will be printed at the top of the Console window when you 
#start RStudio.)
#What is the observed probability of losing $1 million or more?
set.seed(27, sample.kind = "Rounding")
p_loss <- .015
premium <- 1150
loss <- -150000
B<- 10000

sim_q4_b <- replicate(B, {
  sum(sample(c(1150,-150000), 
             n, 
             replace = TRUE, 
             prob = c(1-p_loss, p_loss)))
})
length(sim_q4_b)
q4_b <- mean(sim_q4_b < -1000000)
print(q4_b)


# Questions 5 and 6: Insurance rates, part 3
#Question 5, which has 4 parts, continues the pandemic scenario from 
#Questions 3 and 4.
#Suppose that there is a massive demand for life insurance due to 
#the pandemic, and the company wants to find a premium cost for which 
#the probability of losing money is under 5%, assuming the death rate 
#stays stable at p=0.015.
#Question 5a
#Calculate the premium required for a 5% chance of losing money 
#given n=1000 loans, probability of death p=0.015, 
#and loss per claim l=???150000. Save this premium as x for use 
#in further questions.
p <- 0.015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
q5_a <- x
print(q5_a)

#  Question 5b
#What is the expected profit per policy at this rate?
q5_b <- (p * l + (1-p) * x)
print(q5_b)
             
#  Question 5c
#What is the expected profit over 1,000 policies?
q5_c <- n * q5_b
print(q5_c)

#  Question 5d
#Run a Monte Carlo simulation with B=10000to determine the probability 
#of losing money on 1,000 policies given the new premium x, 
#loss on a claim of $150,000, and probability of claim p=.015. 
#Set the seed to 28 before running your simulation.
#(IMPORTANT! If you use R 3.6 or later, you will need to use 
#the command set.seed(x, sample.kind = "Rounding") instead of set.seed(x). 
#Your R version will be printed at the top of the Console window when you 
#start RStudio.)
#What is the probability of losing money here?
set.seed(28, sample.kind = "Rounding")
B <- 10000

sim_q5_d <- replicate(B, {
  sum(sample(c(l, x), n, replace = TRUE, prob = c(p, 1-p)))
})
length(sim_q5_d)
q5_d <- mean(sim_q5_d < 0)
print(q5_d)

# The company cannot predict whether the pandemic death rate will stay stable. 
#Set the seed to 29, then write a Monte Carlo simulation that for each of 
#B=10000 iterations:
#randomly changes p by adding a value between -0.01 and 0.01 with 
#sample(seq(-0.01, 0.01, length = 100), 1)
#uses the new random p to generate a sample of n=1,000 policies with premium x 
#and loss per claim l=???150000
#returns the profit over n policies (sum of random variable)
#The outcome should be a vector of B total profits. Use the results of the 
#Monte Carlo simulation to answer the following three questions.
#(Hint: Use the process from lecture for modeling a situation for loans that 
#changes the probability of default for all borrowers simultaneously.)
#Question 6a
#What is the expected value over 1,000 policies?
set.seed(29, sample.kind = "Rounding")
sim_q6 <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample(c(l, x), n, replace = TRUE, prob = c(new_p, 1-new_p))
  sum(draws)
})
q6_a <- mean(sim_q6)
print(q6_a)

#Question 6b
#What is the probability of losing money?
q6_b <- mean(sim_q6 < 0)
print(q6_b)

#Question 6c
#What is the probability of losing more than $1 million?
q6_c <- mean(sim_q6 < -1000000)
print(q6_c)
