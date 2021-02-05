# humans_strategies_analysis

A framework to analyse how humans’ decision-making strategies under uncertainty manage the trade-off between information gathering (exploration) and reward seeking (exploitation).
The target problem considered is active learning in a black-box optimization task and more specifically how the exploration/exploitation dilemma can be modelled within
Gaussian Process based Bayesian Optimization framework, which is in turn based on uncertainty quantification.

Data about about humans’ strategies are collected via a gaming application based on the implementation used in (Candelieri et al., 2020).
Fourteen volunteers have been enrolled (among families and friends, which had no competences in computer science and/or optimization), asking for playing ten different games
all consisting in clicking on a white screen in order to search for the location with the highest score, given 20 clicks per game.
Each task refers to a global optimization test function, which subjects “learn and optimize” by clicking at a location and observing the associated score.

Humans’ decisions are analysed with respect to Pareto rationality where the two objectives are improvement expected and uncertainty quantification.
The distance from the Pareto frontier determines whether a choice is (Pareto) rational (i.e., lays on the frontier) or is associated to “exasperate” exploration.

Since the uncertainty is one of the two objectives defining the Pareto frontier, we investigate three different uncertainty quantification measures:
- standard deviation of the GP
- entropy
- distance from previous oservations

For the uncertainty quantification resulting the most compliant with the Pareto rationality, the relationship between the Average Cumulated Reward up to every choice and the Pareto rationality of the next choice is also analysed.

Candelieri, A., Perego, R., Giordani, I., Ponti, A., Archetti, F. Modelling human active search in optimizing black-box functions. Soft Comput 24, 17771–17785 (2020). https://doi.org/10.1007/s00500-020-05398-2 


# How to use the R project
- First: run the script 'processing.game.data.R', it will analyse gaming data in 'Dataset_Amazon_Turk_complete.csv', which contains the gaming data of the 14 volunteers. I will overwrite the file "RESULTS.RData".
- Second: run, sequentially, the following three scripts:
  - 'analysis1.R' - aimed at computing the number of Pareto rational decisions, depending on the uncertainty quantification measure, and comparing them.
  - 'analysis2.R' - aimed at computing the length of consecutive Pareto-rational decisions, depending on the uncertainty quantification measure, and comparing them. 
  - 'analysis3.R' - aimed at comparing the Average Cumulative Reward between Pareto and non-Pareto rational decisions. 
- Finally, use the scripts 'charting_by_player.R' and 'charting_by_function.R' for summaring charts.

- 'core.functions.R' is the script containing all the basis functions (i.e., uncertainty quantification measures, info about the 10 optimization functions underlying the tasks addressed by the 14 players)

