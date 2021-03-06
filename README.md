# Advent of Code 2021 in Haskell

Solutions for the [Advent of Code 2021](https://adventofcode.com/2021) challenges.

Solutions found in `src/Day[N]/Soln.hs`, where [N] is the day of December. 
e.g. The solution for day 3 will be in `src/Day3/Soln.hs`.

They can be evaluated by running the `soln` function in any given `Soln.hs` module.

    bash> stack ghci
    ghci> Day3.Soln.soln
    ...
    903810
  
In many cases solutions for part 1 are destroyed in the process of 
implementing solutions for part 2. In most (if not all) cases, the 
solution for part 1 is a special case of part 2, so an understading
of the part 2 solution is usually enough to identify a solution for 
part 1.
