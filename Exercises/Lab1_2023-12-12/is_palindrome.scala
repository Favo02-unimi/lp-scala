def is_palindrome(s : String) : Boolean = {
  val filtered = s
    .toLowerCase
    .filter(c => 'a' <= c && c <= 'z')
  filtered.reverse == filtered
}

Console.println(is_palindrome("ciao"))
Console.println(is_palindrome("Do geese see God?"))
Console.println(is_palindrome("Rise to vote, sir."))
Console.println(is_palindrome("Rise to ote, sir."))
