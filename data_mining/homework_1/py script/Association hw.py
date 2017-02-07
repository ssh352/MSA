import pandas as pd
import numpy as np
import re

df = pd.read_csv('C:/Users/Will/Documents/MSA/fall/data_mining/homework_1/data/restaurantData.csv')
df['orderNumber']

n_orderNumber = pd.crosstab(index=df["orderNumber"], columns="count")
n_orderNumber.hist()
# Notice that there is not a unique order number for the wine side meat combo in each instance
# We need to convert it into a unique orderNumber

# notice that the type, meat side wine, appears to be consistent throughout the data
df['type'].head(30)
df['type'].tail(30)

# Now we make unique order numbers
new_orderNumber = [1, 1, 1]
y = 0
max_range = df.shape[0] / 3
for i in range(2, max_range + 1):
    add = np.asarray([i] * 3)
    new_orderNumber = np.hstack([new_orderNumber, add])

# Check to make sure the lengths are the same, should = 26682
len(new_orderNumber)

# Look at histogram again to make sure each order number has three observations
df['orderNumber'] = new_orderNumber

n_orderNumber = pd.crosstab(index=df.orderNumber, columns='count')
n_orderNumber.hist()


########################################
# Now we have clean data to work with

# Problem:

# A restaurant owner has a core menu that has remained constant throughout the past two years, while many
# other dishes have rotated on and off the menu. The owner has prepared a data set of all orders containing
# elements of that core menu in which the customer also ordered wine. The dataset has 3 columns: orderNumber
# which identifies each individual person’s order at a table, order which itemizes the orders components, and
# type which provides the type of item in the order (takes one of 3 values: ‘Meat’, ‘Side’, ‘Wine’).
# The restaurant owner would like to add wine suggestions to the menu next to each meat and analyze her
# customer ordering habits in general. She is also interested in showing a picture of a popular meal of hers in
# an ad in Gourmet Diner magazine. Your job is to address her interests and also to report anything else of
# interest in terms of customer ordering habits, all in a nice summary report that is complete but to the point.

# Deliverables:
# 1. Determine which wine should be assigned with each meat. To solve this we want to look at the lift of
# combinations. Lift is chosen because it tells you how much more likely an event is to occur compared to
# its natural occurrence probability.  It displays which combinations have the strongest associations

# The Apriori algorithm is used to calculate the highly associated pairs, but for this data set,
# since it only has a small number of pairs we don't need to use it. Instead we define a function to
# calculate support, confidence, and lift for each pair.

# 2. Determine the most popular meal. To solve this we will concatenate the meat, side, wine combinations 
# and return a frequency table ordered from most frequent to least frequent

wide_df = df.pivot(index='orderNumber', columns='type', values='order')


def sup_conf_lift(Wide_df):
    item1 = wide_df.Meat
    item2 = wide_df.Wine
    meat_wine= []
    for i in range(1, len(item1)+1):
        meat_wine.append(item1[i] + ', ' + item2[i])
    
    wide_df['combined'] = meat_wine
    meat_wine = pd.crosstab(wide_df.combined, columns = 'count')
    meat_wine['support'] = meat_wine['count'] / meat_wine['count'].sum()
    meat_freq = pd.crosstab(wide_df.Meat, columns = 'count')
    meat_prob = meat_freq/meat_freq['count'].sum()
    wine_freq = pd.crosstab(wide_df.Wine, columns='count')
    wine_prob = wine_freq / wine_freq['count'].sum()
    
    meat_wine_conf = []
    meat_wine_lift = []
    for i in range(0,len(meat_wine.support)):
        print(i)
        combo_meat = re.search('(.+),',meat_wine.index[i]).group(1)
        combo_wine = re.search(', (.+)',meat_wine.index[i]).group(1)
        p_meat = meat_prob['count'][meat_prob.index == combo_meat]
        p_wine = wine_prob['count'][wine_prob.index == combo_wine]
        support = meat_wine.support[i]
        meat_wine_conf.append(support / p_meat.sum())
        meat_wine_lift.append(meat_wine_conf[i] /p_wine.sum())
    
    meat_wine['confidence'] = meat_wine_conf
    meat_wine['lift'] = meat_wine_lift
    meat_wine_freq.plot(kind='bar')
    return meat_wine
    
    






df2 = df.groupby(['orderNumber','type'])
#look into heirarchical indexing

