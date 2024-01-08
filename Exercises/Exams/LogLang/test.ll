task TaskOne {
    remove "a"
    rename "b" "a"
}

task TaskTwo {
    backup "c" "d"
    backup "e" "f"
}

task TaskThree {
    merge "d" "f" "g"
}
