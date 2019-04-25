# lc

Install this package by
 1. cloning this repo, 
 2. `cd`ing into it, and
 3. running the command `raco pkg install`.
 
 Try this program:
 
 ```
 #lang lc
 (λ (f) (λ (x) (f (f x))))
 ```
Read more of the docs by running the command `raco docs lc` in the shell or using `f1` in DrRacket and searching for `lc`.