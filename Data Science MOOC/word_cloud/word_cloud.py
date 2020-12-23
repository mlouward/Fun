import collections

file = open('98-0.txt', encoding="utf8")
stopwords = set(line.strip() for line in open('stopwords'))
wordcount = {}

for word in file.read().lower().split():
    word = word.replace(".", "")
    word = word.replace(",", "")
    word = word.replace("\"", "")
    word = word.replace("“", "")
    if word not in stopwords:
        if word not in wordcount:
            wordcount[word] = 1
        else:
            wordcount[word] += 1

d = collections.Counter(wordcount)

for word, count in d.most_common(10):
    print(word + ":", count)