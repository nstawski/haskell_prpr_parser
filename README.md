# Haskell parser for PR-PR language

To run:

```
stack repl
```
Then load the main module
```
ghci> main
```
Then you can paste lines from the sample config to see how they are rendered
```
config> COMPONENT Water        PL8:A1+4,F1  LC_W_Lev_Air
```
The first line will be the parsed component and the second one that starts from `fromList` is the environment.


To run tests:

```
stack test
```