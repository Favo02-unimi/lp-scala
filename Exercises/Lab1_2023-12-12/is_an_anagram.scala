
def string_sum (str : String) =
  str
    .toLowerCase
    .filter(c => (c >= 'a') && (c <= 'z'))
    .map(c => c.toInt)
    .reduce((c, acc) => acc + c)

def is_an_anagram(str : String, list : List[String]) : Boolean =
  list
    .map(s => string_sum(s))
    .exists(ls => ls == string_sum(str))

Console.println(is_an_anagram("ciao", List("sakdj", "domani")))
Console.println(is_an_anagram("ciao", List("ciaos", "domani", "ciA,,,o")))
