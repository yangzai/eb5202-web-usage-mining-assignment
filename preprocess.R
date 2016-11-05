set.seed(42)

like = read.table('data/movielikes.txt', T, colClasses = 'character')
demo = read.csv('data/u-user.csv', colClasses = 'character')

users = unique(like$UserID)
user.count = length(users)

# train-test split: 80:20 of users, not records
train.filter = sample(seq_len(user.count), size = round(user.count*.8))
users.train = users[train.filter]
like.train.filter = like$UserID %in% users.train
like.train = like[like.train.filter,]
like.test = like[!like.train.filter,]

#demographics
demo = demo[demo$userid %in% users,]
demo$age = sapply(demo$age, function (a) {
        if (a < 18) 'Under 18'
        else if (a < 25) '18-24'
        else if (a < 35) '25-34'
        else if (a < 45) '35-44'
        else if (a < 50) '45-49'
        else if (a < 56) '50-55'
        else '56+'
})
demo = demo[,c('userid', 'gender', 'age', 'occupation')]

temp = vector('character')
apply(demo, 1, function (row) {
        sapply(row[-1], function (e) {
                temp <<- rbind(temp, c(row[1], e))
        })
})

demo = data.frame(temp, stringsAsFactors = F)
colnames(demo) = colnames(like)
demo.train.filter = demo$UserID %in% users.train
demo.train = demo[demo.train.filter,]
demo.test = demo[!demo.train.filter,]

demo.train = rbind(demo.train, like.train)
demo.train = demo.train[order(as.numeric(demo.train$UserID)),]
rownames(demo.train) = NULL
demo.test = rbind(demo.test, like.test)
demo.test = demo.test[order(as.numeric(demo.test$UserID)),]
rownames(demo.test) = NULL

# genre
genre = read.csv('data/u_item.csv', colClasses = 'character')
genre = genre[-(2:5)]
genre = data.frame(sapply(genre, as.numeric))

temp = vector('character')
genre.names = colnames(genre)
like.clean = substring(like$movie, 2)
sapply(users, function (u) {
        m = like.clean[like$UserID == u]
        genre.base = genre[-(1:2)] #drop id, unknown
        s = colSums(genre.base[genre$movie.id %in% m,])
        
        # top 3 genres for user
        genre.top = names(head(sort(s[s!=0], decreasing = T), 3))
        
        sapply(genre.top, function(g) {
                temp <<- rbind(temp, c(u, g))
        })
})

genre = data.frame(temp, stringsAsFactors = F)
colnames(genre) = colnames(like)
genre.train.filter = genre$UserID %in% users.train
genre.train = genre[genre.train.filter,]
genre.test = genre[!genre.train.filter,]

genre.train = rbind(genre.train, like.train)
genre.train = genre.train[order(as.numeric(genre.train$UserID)),]
rownames(genre.train) = NULL
genre.test = rbind(genre.test, like.test)
genre.test = genre.test[order(as.numeric(genre.test$UserID)),]
rownames(genre.test) = NULL

write.table(like.train, 'data/movielikes_train.txt', sep = '\t', quote = F, row.names = F)
write.table(like.test, 'data/movielikes_test.txt', sep = '\t', quote = F, row.names = F)
write.table(demo.train, 'data/movielikes_demo_train.txt', sep = '\t', quote = F, row.names = F)
write.table(demo.test, 'data/movielikes_demo_test.txt', sep = '\t', quote = F, row.names = F)
write.table(genre.train, 'data/movielikes_genre_train.txt', sep = '\t', quote = F, row.names = F)
write.table(genre.test, 'data/movielikes_genre_test.txt', sep = '\t', quote = F, row.names = F)
