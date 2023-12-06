app "file-read"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task, await },
        "./input" as input : Str,
        #"./test_input" as input : Str,
    ]
    provides [main] to pf

main =
    lines = Str.split input "\n"
    task =
        dbg part1 lines
        Stdout.line "foo"
        #{} <- Stdout.line "part1:" |> Task.await
        #msg1 = part1 lines |> Result.map Num.toStr |> Result.withDefault "ERR"
        #{} <- Stdout.line msg1 |> Task.await

        #{} <- Stdout.line "part2:" |> Task.await
        #msg2 = part2 lines |> Result.map Num.toStr |> Result.withDefault "ERR"
        #Stdout.line msg2

    Task.attempt task \result ->
        when result is
            Ok {} -> Task.ok {}
            Err err ->
                msg =
                    when err is
                        FileWriteErr _ PermissionDenied -> "PermissionDenied"
                        FileWriteErr _ Unsupported -> "Unsupported"
                        FileWriteErr _ (Unrecognized _ other) -> other
                        FileReadErr _ _ -> "Error reading file"
                        _ -> "Uh oh, there was an error!"

                {} <- Stderr.line msg |> await

                Task.err 1 # 1 is an exit code to indicate failure

part1 = \lines ->
    sections = splitN lines Str.isEmpty |> List.dropIf List.isEmpty
    seedsStr = List.get sections 0 |> unwrap |> List.get 0 |> unwrap
    seeds = parseSeeds seedsStr
    mappingsStrs = List.dropFirst sections 1 |> List.map (\l -> List.dropFirst l 1)
    mappings = List.map mappingsStrs (\l -> List.map l parseMapping)
    List.map seeds (\s -> chainConvert mappings s) |> List.min

parseNumbers : Str -> List Nat
parseNumbers = \numbersStr ->
    Str.split numbersStr " " |> List.dropIf Str.isEmpty |> List.mapTry Str.toNat |> unwrap

parseSeeds : Str -> List Nat
parseSeeds = \line ->
    {after: seedsStr} = Str.splitFirst line ":" |> unwrap
    parseNumbers seedsStr

expect
    r = parseSeeds "seeds: 79 14 55 13"
    r == [79,14,55,13]

Map : {sourceStart: Nat, destinationStart: Nat, length: Nat}

parseMapping : Str -> Map
parseMapping = \line ->
    numbers = parseNumbers line
    when numbers is
        [destinationStart, sourceStart, length] -> {destinationStart, sourceStart, length}
        _ -> 
            expect List.len numbers == 3
            crash "uhoh"

expect
    r = parseMapping "50 98 2"
    r == {sourceStart: 98, destinationStart: 50, length: 2}


## Tries a single mapping
tryConvert : Map, Nat -> Result Nat [NotMapped]
tryConvert = \{sourceStart, destinationStart, length}, number ->
    if number >= sourceStart && number < sourceStart + length then
        Ok ((number - sourceStart) + destinationStart)
    else
        Err NotMapped

expect
    m = {sourceStart: 98, destinationStart: 50, length: 2}
    r = tryConvert m 0
    r == Err NotMapped

expect
    m = {sourceStart: 98, destinationStart: 50, length: 2}
    r = tryConvert m 50
    r == Err NotMapped

expect
    m = {sourceStart: 98, destinationStart: 50, length: 2}
    r = tryConvert m 98
    r == Ok 50

expect
    m = {sourceStart: 98, destinationStart: 50, length: 2}
    r = tryConvert m 99
    r == Ok 51

expect
    m = {sourceStart: 98, destinationStart: 50, length: 2}
    r = tryConvert m 100
    r == Err NotMapped


## Tries all mapping, defaulting to input
convert : List Map, Nat -> Nat
convert = \maps, number ->
    List.walkUntil maps number \state, map ->
        when tryConvert map state is
            Ok n -> Break n
            Err _ -> Continue state
expect
    m1 = {sourceStart: 98, destinationStart: 50, length: 2}
    m2 = {sourceStart: 50, destinationStart: 48, length: 2}
    r = convert [m1, m2] 0
    r == 0

expect
    m1 = {sourceStart: 98, destinationStart: 50, length: 2}
    m2 = {sourceStart: 50, destinationStart: 48, length: 2}
    r = convert [m1, m2] 50
    r == 48

expect
    m1 = {sourceStart: 98, destinationStart: 50, length: 2}
    m2 = {sourceStart: 50, destinationStart: 48, length: 2}
    r = convert [m1, m2] 98
    r == 50

chainConvert : List (List Map), Nat -> Nat
chainConvert = \mapss, number ->
    List.walk mapss number \state, maps ->
        convert maps state

expect
    m1 = {sourceStart: 98, destinationStart: 50, length: 2}
    m2 = {sourceStart: 50, destinationStart: 48, length: 2}
    r = chainConvert [[m1, m2]] 98
    r == 50

expect
    m1 = {sourceStart: 98, destinationStart: 50, length: 2}
    m2 = {sourceStart: 50, destinationStart: 48, length: 2}
    r = chainConvert [[m1, m2], [m2]] 98
    r == 48

splitN : List a, (a -> Bool) -> List (List a)
splitN = \list, pred ->
    {out: o,curr: c} = List.walk list {out: [], curr: []} \{out, curr}, elem ->
        if pred elem then
            {out: List.append out curr, curr: []}
        else
            {out, curr: List.append curr elem}
    List.append o c

expect 
    res =splitN [1,0,2,5,0,0,3,6] (\e -> e == 0) 
    res == [[1], [2,5], [], [3,6]]

unwrap : Result a b -> a
unwrap = \r ->
    when r is
        Ok v -> v
        Err _ ->
            # expect provides debug output
            expect Result.isOk r
            crash "unwrapped!"
