# Starter Guide to R Plumber + Slack

I was inspired this month by RStudio::conf to start publishing some of the fun R work we have been doing at Retina. About a year ago a good friend of mine Brett Kubold turned me on to Jeff Allen’s  plumber package. This package allows you to easily turn your R scripts into a deployable API. Given my extensive use of RShiny, I struggled to find a good use case for this until recently when we decided to build an internal chatbot for my startup Retina. 

The goal of our bot `/tina` was to enable employees at the company query for random stuff like: status of current Facebook campaigns, snapshot of our trello board, querying data using a menu built in Slack, looking up holidays and getting notified about birthdays. 

For this purpose, we needed the Slackbot to do two main tasks (1) listen to queries from users and respond to them and (2) have a schedule for notifying employees about events or notifications. Below is a quick video of some of the cool things this bot could do for you:

![Video](https://youtu.be/DDOrpJP8AlI)

In this blog post, I am going to try to do a detailed walk through of how we set this but and show you a simple example of the code.

[To use this code follow this blog post](https://docs.google.com/document/d/1fk33UoPnVkLnbd5w6uHQ3zlMFQAOy5YGq2zBHCy9XmU/edit#)

