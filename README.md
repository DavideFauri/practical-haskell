# Practical Haskell (with test suite!)

This repository is helpful to whoever wants to attempt the exercises contained in [Practical Haskell](https://www.apress.com/9781484244791), a book by Alejandro Serrano Mena.

While I am reading through the book chapters, I'm writing a test suite for each exercise, using the [Tasty](https://hackage.haskell.org/package/tasty) framework. As it turns out, writing tests is a bit more time consuming than to actually write the solutions, so I might be slow in the update. Please be patient, or submit a pull request and help me :)

There are two branches:

- `master` contains the test suite (in the `/test` folder, of course) which I run with `stack test`. To have a more granular control over which tests to run, you can try: 
    
      stack test --ta '-p "Chapter 2"'
      
  Even though I'm trying to stay as close as possible to the spirit of the book, please remember that these tests are influenced by what *I* think the solution should look like. Learning a language through Test-Driven Design might be too constraining to you, and Haskell is already filled enough with "pleasing a compiler without really understanding" what you're writing. If you think your solution is better, ignore the test results ;)
  
  Along with the test suite, I also include some code from the "official" accompanying code, available [here](https://github.com/Apress/practical-haskell).
    
- `solutions` contains my own solutions to the exercises, which should pass all the tests (I hope).

## Contributing

To everyone, and in particular to members of the Haskell.amsterdam's study group... feel free to contribute! I can't promise to answer quickly, but I'd like if this small project can help the community.
