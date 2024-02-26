# -*- coding: utf-8 -*-

# -- Sheet --

# # Google Play Store Apps Data [Data from Data Camp]
# 
# This dataset consists of web scraped data of more than 10,000 Google Play Store apps and 60,000 app reviews. `apps_data.csv` consists of data about the apps such as category, number of installs, and price. `review_data.csv` holds reviews of the apps, including the text of the review and sentiment scores. You can join the two tables on the column, `App`.


# ### Data Dictionary (from source)
# 
# ***data_app.csv***
# 
# | variable       | class     | description                                                                  |
# |:---------------|:----------|:-----------------------------------------------------------------------------|
# | App            | character | The application name                                                         |
# | Category       | character | The category the app belongs to                                              |
# | Rating         | numeric   | Overall user rating of the app                                               |
# | Reviews        | numeric   | Number of user reviews for the app                                           |
# | Size           | character | The size of the app                                                          |
# | Installs       | character | Number of user installs for the app                                          |
# | Type           | character | Either "Paid" or "Free"                                                      |
# | Price          | character | Price of the app                                                             |
# | Content Rating | character | The age group the app is targeted at - "Children" / "Mature 21+" / "Adult"   |
# | Genres         | character | Possibly multiple genres the app belongs to                                  |
# | Last Updated   | character | The date the app was last updated                                            |
# | Current Ver    | character | The current version of the app                                               |
# | Android Ver    | character | The Android version needed for this app                                      |
# 
# **data_reviews.csv**
# 
# | variable               | class        | description                                           |
# |:-----------------------|:-------------|:------------------------------------------------------|
# | App                    | character    | The application name                                  |
# | Translated_Review      | character    | User review (translated to English)                   |
# | Sentiment              | character    | The sentiment of the user - Positive/Negative/Neutral |
# | Sentiment_Polarity     | character    | The sentiment polarity score                          |
# | Sentiment_Subjectivity | character    | The sentiment subjectivity score                      |


#load library
library(tidyverse)
library(dplyr)

#observe data_1
df_app <- read_csv('data_apps.csv.gz', show_col_types = FALSE)
head(df_app,20)

df_app %>%
    summarise(min = min(Rating))

#observe data_2
df_rew <- read_csv('data_reviews.csv.gz', show_col_types = FALSE)

df_rew

# # 01. Find highest review from 10 most Category


# ## Set data for ready to manipulate


library(dplyr)
library(ggplot2)
names(df_app) <- tolower(names(df_app))#for easy to use
names(df_rew) <- tolower(names(df_rew))

#clean out nan
df_rew <- df_rew %>%
	filter(sentiment != "nan")

head(df_app)#for check
head(df_rew)

df_check_dup <-df_app %>%
	count(category,app)%>%
	arrange(desc(n),category,app)

df_check_dup

df_app %>%
	summarise(count_app = n()) # for check n of app

# ### Remove duplicate app
# by sort data column reviews ascending when duplicate it duplicate data is lower score 


df_app %>%
	filter(category == "COMMUNICATION")%>%
	arrange(reviews, app)

df_app_sort_fordup <- df_app %>% 
	arrange(desc(reviews),category, app)

df_app_sort_fordup

# ### Remove duplicate data by column app


df_rmdupapp <- df_app[!duplicated(df_app$app), ]#remove dup app
df_rmdupapp %>%
	summarise(total_app = n())# for check n of app

df_rmdupapp%>%
	arrange(desc(reviews),category, app)

# ### Calculate total reviews each category


top_cat <- df_rmdupapp %>%
	select(app:reviews) %>%
	group_by(category)%>%
	summarise(total_reviews = sum(reviews)/1000000, count_app = n())%>%
	arrange(desc(total_reviews))%>%
	head(10)

# ## Graph 10 categorys have most reviews.


top_cat
	
top_cat %>%
	ggplot(aes(x= reorder(category, total_reviews), y= total_reviews, fill = reorder(category, total_reviews) ))+
	geom_col()+
	coord_flip()+
	ylab("Total Reviews (million)")+xlab("Category")+
	ggtitle("Top 10 category most reviews")+
	scale_fill_brewer(type = "div", palette = 1)+
	theme_minimal()+
	scale_y_continuous()+
	labs(fill = "category")+
	theme(plot.title = element_text(hjust = 0.5,size= 30),
		  axis.text=element_text(size=20),
		 axis.title=element_text(size=20),
		 legend.text = element_text(size=14),
		 legend.title = element_text(size=20))#set center
	
	options(repr.plot.width=30, repr.plot.height=10)

# After analyze data that show top 10 category most reviews 
# 1. GAME have a 623 million reviews from 959 app
# 2. COMMUNICATION have 286 million reviews from 315 app
# 3. TOOLS have 230 million reviews from 827 app
# 4. SOCIAL have 228 million reviews from 239 app
# 5. FAMILY have 144 million reviews from 1832 app
# 6. PHOTOGRAPHY have 106 million reviews from 281 app
# 7. VIDEO_PLAYERS have 68 million reviews from 163 app
# 8. PRODUCTIVITY have 56 million reviews from 374 app
# 9. PERSONALIZATION have 54 million reviews from 376 app
# 10. SHOPPING have 45 million reviews from 202 app


# # 02. Find distribution of sentiment polarity, split by content rating.


df_rmdupapp <- rename(df_rmdupapp,"content_rating" = "content rating")
df_rmdupapp

df_rew_join <- df_rew %>%
	inner_join(df_rmdupapp, by = "app")%>%
	select(1:13)%>%
	arrange(sentiment_polarity,content_rating)

glimpse(df_rew_join)
head(df_rew_join)

df_rmdupapp %>%
		filter(content_rating == "Adults only 18+")

# ### Check content rating


df_rew_join %>%
		count(content_rating, sort = T)

df_rew_join %>%
	filter(content_rating == "Adults only 18+")

# ### Create plot


df_rew_join %>%
	ggplot()+
	geom_density(aes(x= sentiment_polarity, fill = content_rating, alpha = 0.5))+
	facet_wrap(.~content_rating)+
	scale_fill_brewer(type = "seq", palette = 11)+
	ggtitle("Distribution of sentiment polarity of each content rating")+
	theme(plot.title = element_text(hjust = 0.5,size= 30),
		  axis.text=element_text(size=20),
		 axis.title=element_text(size=20),
		 legend.text = element_text(size=14),
		 legend.title = element_text(size=20),
		 strip.text.x = element_text(size = 20))

	options(repr.plot.width=30, repr.plot.height=10)

# The Result show sentiment polarity each content rating is normal distribution except content rating : Adult only 18+ is left skewed


# ### Find Median sentiment each content rating and number of positive negative sentiment polarity


df_rew_join %>%
		group_by(content_rating)%>%
		summarise(median = median(sentiment_polarity))

df_neu <- df_rew_join %>% 
		group_by(content_rating)%>%
		filter(sentiment_polarity == 0)%>%
		summarise(neutral = n())

df_neg <- df_rew_join %>% 
		group_by(content_rating)%>%
		filter(sentiment_polarity < 0)%>%
		summarise(negative = n())

df_pos <- df_rew_join %>% 
		group_by(content_rating)%>%
		filter(sentiment_polarity > 0)%>%
		summarise(positive = n())

df_sentiment <- df_neg %>%
					left_join(df_neu, by = "content_rating")%>%
					left_join(df_pos, by = "content_rating")%>%
					mutate(percent_negative = (negative/(negative+neutral+positive))*100,
						   percent_neutral = (neutral/(negative+neutral+positive))*100,
							 percent_positive = (positive/(negative+neutral+positive))*100)

df_sentiment

# From median and data that show all content rating have a percent positive reviews more than negative reviews


# # 03. What impact does the content rating an app receives have on its sentiment and rating


# ### Join data and find quantity


df_conrate <- df_rew %>%
	inner_join(df_rmdupapp, by = "app")%>%
	select(app:rating, content_rating,type,installs)

df_conrate

df_conrate %>%
	group_by(content_rating)%>%
	summarise(quantity = n())

# ### find impact content rating vs rating


# find impact content rating vs rating
df_conrateclean <- df_conrate %>%
		filter(content_rating != "Adults only 18+")%>%
		group_by(app,rating,content_rating)%>%
		summarise(positive_sentiment = sum(sentiment == "Positive"),
				 negative_sentiment = sum(sentiment == "Negative"),
				  nuetral_sentiment = sum(sentiment == "Neutral"), .groups = "drop")%>%
		arrange(content_rating)

df_conrateclean_graph <- df_conrateclean %>%
		ggplot()+
		geom_density(aes(x= rating , fill = content_rating))+
		facet_wrap(.~content_rating)+
		ggtitle("Distribution of rating with content rating")+
		theme(axis.text = element_text(size = 20),
			 strip.text.x = element_text(size = 20),
			 legend.title = element_text(size=20),
			 legend.text = element_text(size=20),
			 plot.title = element_text(hjust = 0.5,size= 30),
			 axis.title = element_text(size=20))+
		scale_fill_brewer(type = "seq", palette = 2)
df_conrateclean_graph


df_conrateclean2 <- df_conrate %>%
		filter(content_rating != "Adults only 18+")%>%
		mutate(rating_type = ifelse(rating >= 3.5,"high rate",ifelse(rating >=2, "medium rate","low rate")))%>%
		group_by(content_rating, rating_type)%>%
		summarise(quantity = n(), .groups = 'drop')

df_conrateclean2

sum_rating <- df_conrateclean %>%
		group_by(content_rating)%>%
		summarise(average_rating = mean(rating), quantity = n())%>%
		arrange(content_rating)

sum_rating


df_conrateclean2 %>%
	ggplot()+
	geom_bar(aes(x = content_rating, y = quantity, fill = rating_type),stat="identity", position = "dodge")+
	ggtitle("Relation about content rating and quantity of rating")+
	theme(axis.text = element_text(size = 20),
			 strip.text.x = element_text(size = 20),
			 legend.title = element_text(size=20),
			 legend.text = element_text(size=20),
			 plot.title = element_text(hjust = 0.5,size= 30),
		 axis.title = element_text(size=20))

# Content rating recieve impact from rating is quantity of reviews that show content rating Everyone have a many review in high rating. And every content rating have average rating more than 4.


# ### find impact content rating vs sentiment


# find impact content rating vs sentiment
df_con_vs_sent <- df_conrate %>%
	filter(content_rating != "Adults only 18+")%>%
	group_by(content_rating,sentiment)%>%
	summarise(count = n(), .groups = "drop")

df_totalsen <- df_conrate %>%
	filter(content_rating != "Adults only 18+")%>%
	group_by(content_rating)%>%
	summarise(count = n(), .groups = "drop")

df_con_vs_sent2 <- df_con_vs_sent %>%
	left_join(df_totalsen, by = "content_rating", suffix = c("_sentiment_type","_sentiment_total"))%>%
	mutate(percent_sentiment_type = (count_sentiment_type/count_sentiment_total)*100)%>%
	arrange(desc(sentiment), content_rating)

df_con_vs_sent2

df_con_vs_sent_graph <- df_con_vs_sent%>%
							ggplot(aes(x= content_rating, y= count, fill = sentiment))+
							geom_bar(stat="identity", position = "dodge")+
							geom_text(aes(label = count), color = "black", size = 5, position = position_dodge(0.9)) +
                            ggtitle("Relation about content rating and each type of sentiment")+
							theme(plot.title = element_text(hjust = 0.5,size= 30),
		  					axis.text=element_text(size=20),
		 					axis.title=element_text(size=20),
		 					legend.text = element_text(size=20),
							legend.title = element_text(size=20))

df_con_vs_sent_graph

# Content rating receive impact from sentiment is number of sentiment. If content rating have most reviews that have a many negative sentiment more than less reviews content rating but graph that show positive sentiment always more than negative sentiment. Every content rating have a same percent positive sentiment from total sentiment at 60% plus.


