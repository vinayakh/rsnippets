# Description : Find the words with largest sum (assigned cipher codes)
# Website : http://is-r.tumblr.com/post/38299197161/finding-numeric-values-of-strings-using-strsplit

My.Words <- read.csv("data/words.txt")
My.Words$RANDOM.WORDS <- as.character(My.Words$RANDOM.WORDS)

#Step 1: Split Character Vectors into Sub-strings:
strsplit(My.Words$RANDOM.WORDS, "")

Split.List <- strsplit(My.Words$RANDOM.WORDS, "")

#Step 2: Assign a numeric value to each of the 26 letters:
LETTERS
rank(LETTERS)
Letter.Values<- rank(LETTERS)
names(Letter.Values) <- LETTERS
Letter.Values

#Step 3: Extract the value of each word by summing the letters' values

Word.Sums <- unlist(lapply(Split.List, function(Word){sum(Letter.Values[Word])}))
Word.Sums

#Step 4: Which words have the highest and lowest values?

My.Words$RANDOM.WORDS[Word.Sums==max(Word.Sums)]
My.Words$RANDOM.WORDS[Word.Sums==min(Word.Sums)]