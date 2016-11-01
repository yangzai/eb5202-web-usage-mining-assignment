train = read.transactions('data/movielikes_train.txt', 'single', cols = c('UserID', 'movie'))
test = read.transactions('data/movielikes_test.txt', 'single', cols = c('UserID', 'movie'))
rules <- apriori(train, parameter = list(supp=0.07, conf=0.95, minlen=2))

test.list = as(test, 'list')
test.count = length(test)

correct = 0
total = 0
rules.lhs = lhs(rules)
rules.rhs = rhs(rules)
for (i in 1:test.count) { # for each transaction (user)
        # find rules with antecedent (LHS) in transaction
        is.lhs.in.tx <- is.subset(rules.lhs, test[i])
        
        # filter movies from RHS of rules and union 
        predictions = Reduce(union, as(rules.rhs[is.lhs.in.tx], 'list'), vector('character'))
        
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