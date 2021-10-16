# handy
* handy module collection for string operation and calculation.
* export modules to use in repl

## usage
### basic
```
touch data/a.in.txt
```
```
stack ghci
```
#### run
```
run (some func) "a" 
```
#### manually
```
x <- words <$> readFile (inData "a.in.txt") 
-- some operation
writeFile outfile x
```
### make in query
```
writeFile outfile <$> mkIn1k "id" . words <$> readFile infile
```

### make like or words
```
writeFile outfile <$> mkLike "val" . words <$> readFile infile
```
