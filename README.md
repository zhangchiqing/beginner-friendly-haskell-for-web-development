# Beginner-friendly Real World Haskell Web Development

### Why Haskell
Learning Haskell changed the way I think about programming. Objective oriented programming taught me to think in terms of "object" with its mutable fields, and "methods" for their interactions between each others. In Haskell, I learned to think in terms of "data", and "functions" for data transformation. I found it's more straightforward and allows me to write less code.

Haskell is a static typed pure functional language. While the functional programming allows you to reuse a lot of code for data transformation, the Haskell compiler also checks the types you defined for the data and the data transformation functions in order to catch mistakes before they got run.

### Why this book
To me, the best way to learn things is by using it in practice. Despite there are a lot of books about Haskell, I found there isn't any book focusing on how to use it for real world web development. After learning the language basics from those books, it still requires a lot effort to figure out how to solve those unique challenges in web development. However, I've gone though all of them and designed a complete real world scenario to teach Haskell and show how to build a web app with it. Given there are many ways of solving these challenges in Haskell, my decision of choosing them is to keep it beginner friendly.

Beginner friendly is a trade off that it's sometimes hard to archieve with both of writing less code and keeping the code easy for beginner. Beginner friendly is an important factor to allow building things in a collaborative team setting. As most Haskell books teach Monad Transformer, which is very powerful of an abstraction, but it's also a bit of mind twist for beginners to master and understand it in practice. It decided to build a real world web app without using Monad Transformers, but only use basic language building blocks.

### About you
The intended audience of this book is people who have basic knowledge of web programming. You should know how HTTP works, how SQL query works.

You don't need any knowledge of Haskell. This book teaches language basics and uses what you learned from the beginning to build a real world web app.

### Book structure

This book's goal is to show how to build a RESTful API for getting, creating, updating and deleting users from PostgreSQL database.

#### Chapter 1. Getting Started
The first chapter teaches how to setup the development environment and setup a Haskell project.

#### Chapter 2. Functions
#### Chapter 3. Recursion, Pattern Matching, Higher Order Functions
#### Chapter 4. Type class
#### Chapter 5. Configure
Chapters 2-5 covers the language basics, data structures with examples for modelling the user data.

#### Chapter 6. JSON
Introduces IO, and how to parse app's configuration.

#### Chapter 7. Strings
Introduces how to encode out data type into json and and how to decode json into our type

#### Chapter 8. HTTP Client
Chapter 8 introduces the difference between the string types and their use cases.

#### Chapter 9. Database
Chapter 9 will build a http client for sending http calls and parsing the response.

#### Chapter 10. Error Handling
Chapter 10 introduces how to connect to a postgres databade. How to migrate database, and make queries to create, get and delete user.

#### Chapter 11. HTTP Service
Chapter 11 introduces exceptions, how to handle exceptions from the database queries, and turn them into specific errors.

#### Chapter 12. Logging
Chapter 12 introduces how to setup http service, and how to parse and validate the inputs. we will build endpoints for making database queries.

#### Chapter 13. Middlware
Chapter 13 introduces loggings, how to log http requests and response, and how to log exceptions.

#### Chapter 14. Testing
Chapter 14 introduces how to write tests to test the functions for database queries, and how to write integration tests to test http endpoints.

#### Chapter 15. Deployment
Chapter 15 introduces how to build docker container for the web app and deploy it to heroku.
