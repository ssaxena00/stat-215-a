
load("./SplitEachImage")
ntr = nrow(train_r)

set.seed(12345)
cv_rng = sample(ntr, ntr)

# Index for val CV split
val_ind = split(cv_rng, rep_len(1:10, ntr))

# Index for tr CV split
tr_ind = list()
for (i in 1:10) {
  tr_ind[[i]] = setdiff(1:ntr, val_ind[[i]])
}

# -------------
# EXAMPLE
# -------------

for (i in 1:10) {
  cv_train = train_r[ tr_ind[[i]] ]    # OR  = train_r[ setdiff(1:ntr, val_ind[[i]]) ]
  cv_val = train_r[ val_ind[[i]] ]
  
  # DO VALIDATION HERE
}