> This project has two homes.
> It is ok to work in github, still, for a better decentralized web
> please consider contributing (issues, PR, etc...) throught:
>
> https://gitlab.esy.fun/yogsototh/adventofcode

---


# Adventofcode 2017

My solution to [advent of code 2017](http://adventofcode.com) in Haskell.

I try to use protolude and to write down the tests I made GHCi.

Mainly to play with it:

~~~
> stack build; and stack exec adventofcode-exe
~~~

To launch the tests:

~~~
> stack test
~~~

To launch only the tests of one day:

~~~
> stack test --test-arguments="-p \"Day 14\""
~~~
