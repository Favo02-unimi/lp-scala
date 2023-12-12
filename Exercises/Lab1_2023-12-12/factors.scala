def primes(n : Int) = {
  for (x <- (2 to n).toList if
    (for (y <- (2 to x-1).toList if x % y == 0) yield y)
  .length == 0) yield x
}

def factors(n : Int) : List[Int] =
  for (p <- primes(n) if n % p == 0) yield p

Console.println(factors(30))
