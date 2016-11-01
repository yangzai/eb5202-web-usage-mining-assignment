set.seed(42)

df = read.table('data/movielikes.txt', T)
users = unique(df$UserID)
user.count = length(users)

# train-test split: 80:20 of users, not records
train.filter = sample(seq_len(user.count), size = round(user.count*.8))
users.train = users[train.filter]
df.train.filter = df$UserID %in% users.train
df.train = df[df.train.filter,]
df.test = df[!df.train.filter,]

write.table(df.train, 'data/movielikes_train.txt', sep = '\t', quote = F, row.names = F)
write.table(df.test, 'data/movielikes_test.txt', sep = '\t', quote = F, row.names = F)