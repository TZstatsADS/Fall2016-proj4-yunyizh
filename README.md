# Project: Words 4 Music

### [Project Description](doc/Project4_desc.md)

![image](http://cdn.newsapi.com.au/image/v1/f7131c018870330120dbe4b73bb7695c?width=650)

Term: Fall 2016

+ [Data link](https://courseworks2.columbia.edu/courses/11849/files/folder/Project_Files?preview=763391)-(**courseworks login required**)
+ [Data description](doc/readme.html)
+ Contributor's name: Yunyi Zhang
+ Projec title: Association Mining of Music and Text
+ Project summary: The goal of the project is to develop a system to recommend lyrics words based on the features of a piece of music. General speaking, I used topic modeling and random forest to dig in associations between lyrics and music features in order to develop the model with good performance.The predicted final evaluation index is around 0.27, which means with the 5000 vocab dictionary, the average prediction rank is around 680. 

+ Project details: Here is the process of my model development:
1. Process the music feauture of the 2350 songs in training set.The final variables I used are number of bars, number of beats, duration(calculated by the last start time of bar), tempo(calculated by beats per min), number of sections, number of segments, mean of maximum loudness in each segment, mean of starting loudness in each segment, median pitch for all segments(12 dimension), median timbre for all segments(12 dimensions). 

2. I looked at the frequency of each word in the vocab appear in the 2350 songs and decided to divide the vocab into three sets. The most frequent 20 words are grouped and are assigned rank 1-20 in my final result no matter what the music feature is. The least frequent 600 words are grouped and are assigned rank 4673.5 no matter what the music feature is. The middle part with 4353 words are grouped to develop the topic model.

3. I developed a topic model with 20 resulted topics based on the lyrics of the 2350 songs in training set. Each topic has its own ranking in the vocab (meaning that words have different probability of appearing in each topic). Based on the result model, I extracted the top FOUR topics that each song should be assigned to with the highest probability. 

4. Based on the probability of each assigned topic, I "bootstrap" the music feature training set. This is an example. For one of the songs n the dataset, the top topics assigned to it by the topic model are: 5,8,10,15 with a probability of 0.3, 0.3, 0.2, 0.1. Then I copy the music features of this song for 9 times (then they are 9 observations). Three of them are assigned with a topic label of 5, three are with a topic label of 8, 2 are of 10 and finally 1 is of 15. In this way I simulated the distribution of topics of the songs in the training set and develop the whole training set into a more representative sample.

5. Now that I had a training set with each song's music features as well as their responded topics, I used different classification methods to train a model in order to predict topics with music features. The methods I used were logistic regression, random forest, knn classifcation and support vector machine. Random forest outperformed. 

6. For final prediction, the random forest model was able to return prediction probability. I usd the top 5 probability to weight the result in order to achieve better performance. Here is an example: given music features, the random forest model gives a prediction of topic 4 with probability of 0.6, a prediction of topic 3 with probability of 0.4. The word "love" is ranked 15th in topic 4 and 30th in topic 3. Then a weighted rank I calculated for word "love" was 0.6*15+0.4*30=21.


```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
