library(arules)
library(arulesViz)

train = read.transactions('data/movielikes_demo_train.txt', 'single', cols = c('UserID', 'movie'))
test = read.transactions('data/movielikes_demo_test.txt', 'single', cols = c('UserID', 'movie'))
test.list = as(test, 'list')
test.count = length(test)

# summary and visualisation of training transactions
summary(train)
itemFrequencyPlot(train, 'absolute', topN = 20)

re.movie = '^m[0-9]+$'
rules <- apriori(train, list(supp = 0.07, conf = 0.95, minlen = 2))
rules.lhs = lhs(rules)
rules.rhs = rhs(rules)
rules.lhs.list = as(rules.lhs, 'list')
rules.rhs.list = as(rules.rhs, 'list')
#rules.lhs.movie = as(sapply(rules.lhs.list, function(x) x[grepl(re.movie, x)]), 'itemMatrix')

# filter for movie only consequent
is.rhs.movie = grepl(re.movie, rules.rhs.list)

print('Top 10 rules by lift:')
inspect(head(sort(rules, by = "lift"), 10))

correct = 0
total = 0
for (i in 1:test.count) { # for each transaction (user)
        # find rules with antecedent (LHS) in transaction
        # also include multi antecedent rules
        is.lhs.in.tx <- is.subset(rules.lhs, test[i])
        
        # filter movies from consequent (RHS) of rules and union
        predictions = Reduce(union, as(rules.rhs[is.lhs.in.tx & is.rhs.movie], 'list'), vector('character'))
        
        # increment correct and total predictions
        correct = correct + sum(predictions %in% test.list[[i]])
        total = total + length(predictions)
}

precision = correct / total

cat('No. of training transactions (users): ', test.count, '\n')
cat('Correct predictions: ', correct, '\n')
cat('Total predictions: ', total, '\n')
cat('Precision: ', precision, '\n')
cat('Correct predictions / transaction: ', correct / test.count, '\n')