# Beginner-friendly Real World Haskell Web Development

### Why Haskell
Learning Haskell changed the way I think about programming. Objective-oriented programming taught me to think in terms of "object" with its mutable fields, and "methods" for their interactions between each other. In Haskell, I learned to think in terms of "data" and "functions" for data transformation. I found it's more straightforward and allows me to write less code.

Haskell is a static typed, pure functional language. While the functional programming allows you to reuse a lot of code for data transformation, the Haskell compiler also checks the types you defined for the data and the data transformation functions to catch mistakes before they get run.

### Why this book
I think the best way to learn something is to use it in practice. There are many books about Haskell, but none that I found explained how to use it for real-world web development. Even after learning the language basics, it took a lot of effort to figure out how to solve the unique challenges that arise in web development.

For this book, I’ve designed a comprehensive real-world scenario to teach Haskell and how to build a web app with it. Haskell provides many paths to solve different challenges, but for this book, I’ve chosen the most beginner-friendly methods.

Choosing to go beginner-friendly is a tradeoff. Sometimes, the most efficient solution (the one that involves the least code) is not the easiest one. But a beginner-friendly approach is important for building things in a collaborative team setting.

Most Haskell books teach sophisticated features like monad transformer and advanced typeclasses, which are both great abstractions that allow you to reuse lots of code. However, they’re intimidating for beginners to understand and master in practice, and aren’t required to build a real-world web app.

So I decided to build a web app without using these advanced abstractions, only basic language building blocks, to keep the code accessible to beginners. I think beginners will better understand the point of those advanced abstractions by first building something without them, and then refactoring with them later on.

### About you
This book is for people with basic knowledge of web programming. You should know how HTTP works and how SQL query works.

You don't need any knowledge of Haskell. This book teaches the language basics and uses that foundation to build a real-world web app.

### Book structure
My goal is to show how to build a RESTful API for getting, creating, updating and deleting users from a PostgreSQL database.

#### Chapter 1. Getting Started
The first chapter shows how to set up the development environment and start a Haskell project.

#### Chapter 2. Functions
Chapters 2 through 4 cover the language basics and provide data structures with examples for modeling the user data.
#### Chapter 3. Recursion, Pattern Matching, Higher Order Functions
#### Chapter 4. Typeclass

#### Chapter 5. Configure
Chapter 5 introduces IO and how to parse an app's configuration.

#### Chapter 6. JSON
Chapter 6 introduces how to encode data type into JSON and how to decode JSON into our type.

#### Chapter 7. Strings
Chapter 7 introduces the difference between the various string types and their use cases.

#### Chapter 8. HTTP Client
Chapter 8 demonstrates how to build an HTTP client for sending HTTP calls and parsing the response.

#### Chapter 9. Database
Chapter 9 explains how to connect to a PostgreSQL database, how to migrate a database, how to make queries to create, retrieve, and delete a user.

#### Chapter 10. Error Handling
Chapter 10 introduces exceptions, how to handle exceptions from the database queries, and how to turn them into specific errors.

#### Chapter 11. HTTP Service
Chapter 11 shows how to set up an HTTP service and how to parse and validate the inputs. We will build endpoints for making database queries.

#### Chapter 12. Logging
Chapter 12 introduces loggings, how to log HTTP requests and responses, and how to log exceptions.

#### Chapter 13. Middlware
Chapter 13 explains  how to create a middleware for adding a unique id to each request, and tie all the logs about a request and its response with that id.

#### Chapter 14. Testing
Chapter 14 introduces how to write tests for the functions for database queries and how to write integration tests for HTTP endpoints.

#### Chapter 15. Deployment
Chapter 15 shows how to build a docker container for the web app and deploy it to Heroku.

### How to get this book?
Feel free to contact me if you are interested in this book: zhangchiqing+beginnerfriendlyhaskell@gmail.com
