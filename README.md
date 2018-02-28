# Zappos Recommendation System Challenge
Recommendation system using collaborative filtering identifying top 6 items for new, returning and loyal customers .Integration data from various sources like stored customer history and evaluate user based, Item based and hybrid collaborative filtering techniques performance and selected best technique for recommendation

## CONTEXT AND SCENARIO
Pricing is a key driver for any business' profitability. In this challenge, we want you to think about pricing from a number of different perspectives: as a customer, as a marketer, as a merchandiser, etc. As a retail business, our merchandisers decide which products to carry and in what amounts. From there, the pricing team is responsible for determining the optimal prices for each product over time. The marketing team generates demand and drives traffic to the website.

The merchandising team has decided to carry a brand new boot designed by Tony Hsieh himself. They have bought 200,000 units of this product. This is a one-time design, and we will not be able to get more inventory for this product. The boot's MSRP is $250. It will be available for sale through an exclusive new website, tonyboots.com. Tonyboots.com is estimated to get anywhere from 5,000-10,000 visits a month once it launches in September 2017.

## OBJECTIVE
Now that this merchandising decision has been made, your objective as a pricing team member is to maximize the product's value for the business. Now what does "value" mean? In this challenge, we'll define value as the profit this product will generate for the business either directly or indirectly over the course of its lifetime (meaning as long as we carry inventory in it). You can only influence the product's pricing strategy to achieve this objective.

## PART 1 - Investigate
What questions would you need to ask to determine the optimal pricing strategy for this new boot? Please list at least three questions.

## PART 11 - Design
Provide your own hypothetical answers to the questions you listed in Part I! Use these answers as assumptions to put together a pricing strategy for this boot.

What do we mean by pricing strategy? A pricing strategy should be a system or plan to answer the following questions: What is the optimal price for the product at any given time? When should you put a product on sale and at what percentage off? How are you evaluating whether the product's price is serving your objective?

## Here are three possible scenarios your pricing strategy should be able to address. How would you determine the optimal price in each case?
New product launch The boot is going live on tonyboots.com for the first time!

Seasonal changes The news reports record high temperatures in January. Shoppers are choosing to buy sneakers over boots.

Demand slowing Following the launch of the product, the boots are getting less and less attention as other companies have come out with their own versions of Tony's signature boot style.

Assume that you can only design a pricing strategy for this product once before it launches, and that it cannot be modified once it does. Design a system that takes into account various scenarios like the three above to output the optimal price for the product at any given point in time. Your system can be a combination of logical rules, ranking systems, pseudocode, algorithms, optimization models, decision trees anything that will produce an optimal price. For example, here is a combination of logical rules that is simple but very sub-optimal:

Start from MSRP at launch. If the traffic to tonyboots.com is at least 7,000 visits a month, the price should be equal to MSRP If the traffic is less than 7, 000 visits/month, for every 500 visits less than 7, 000, decrease the current price by $10 with a minimum price of $100. The price will update every week based on the last 30 days ' visits.

The example above would be able to output a price for all three scenarios, but it does not address the overall objective of maximizing the product's lifetime value. What if traffic is very high, but customers are not buying the product? What if we find that we are only selling 50 units a month? Are we willing to pay warehousing fees to hold this product in our inventory for the next 300 years? This example does not address or optimize for these sorts of questions.

## Deliverables:
l. An outline of your system that produces the optimal price for the boot over time. Feel free to get as technical as you'd like. PDF, PPT, Excel, or Word documents only, please. 2. An executive summary in layman's terms of your pricing strategy that could be presented to non-technical leaders in under five minutes. PDF or PPT only.

## PART 111 - Measure
How will you measure the performance of your pricing strategy? Please provide at least three metrics that will measure the value the product is providing to the business. Explain why those metrics are useful and how they could be used to suggest changes to your pricing strategy.


