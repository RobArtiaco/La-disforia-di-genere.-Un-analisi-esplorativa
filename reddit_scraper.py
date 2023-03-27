import praw
import pandas as pd

reddit_read_only = praw.Reddit(client_id="CbV_L17sAuVRrCAwbqRU6g", client_secret="puqWZ1oLMrueiqkwnVGOhqVY_aPqQA", user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0")
subreddit = reddit_read_only.subreddit("trans")
posts = subreddit.top("2020", limit=1000)

# Scraping the top posts of the current month

posts_dict = {"Title": [], "Post Text": [], "Username": [],
			"ID": [], "Score": [],
			"Total Comments": [], "Date": [], "Post URL": []
			}


for post in posts:
	posts_dict["Date"].append(post.created_utc)
	# Title of each post
	posts_dict["Title"].append(post.title)
	
	# Text inside a post
	posts_dict["Post Text"].append(post.selftext)
	
	posts_dict ["Username"].append(post.author)
	# Unique ID of each post
	posts_dict["ID"].append(post.id)
	
	# The score of a post
	posts_dict["Score"].append(post.score)
	
	# Total number of comments inside the post
	posts_dict["Total Comments"].append(post.num_comments)
 
	# URL of each post
	posts_dict["Post URL"].append(post.url)
 

# Saving the data in a pandas dataframe
top_posts = pd.DataFrame(posts_dict)
top_posts
top_posts.to_csv(".csv", index=True)

