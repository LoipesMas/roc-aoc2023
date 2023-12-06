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
        #{} <- Stdout.line "part1:" |> Task.await
        #msg1 = part1 lines |> Result.map Num.toStr |> Result.withDefault "ERR"
        #{} <- Stdout.line msg1 |> Task.await

        {} <- Stdout.line "part2:" |> Task.await
        msg2 = part2 lines |> Result.map Num.toStr |> Result.withDefault "ERR"
        Stdout.line msg2

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

part2 = \lines ->
    sections = splitN lines Str.isEmpty |> List.dropIf List.isEmpty
    seedsStr = List.get sections 0 |> unwrap |> List.get 0 |> unwrap
    seedRanges = parseSeedRanges seedsStr
    mappingsStrs = List.dropFirst sections 1 |> List.map (\l -> List.dropFirst l 1)
    mappings = List.map mappingsStrs (\l -> List.map l parseMapping)
    reverseMappings = List.reverse mappings |> List.map (\m -> List.map m inverseMap)
    locations = List.range {start: At 0, end: Length 39999999}
    locationMby = List.walkUntil locations NotFound \state, loc ->
        seed = chainConvert reverseMappings loc
        if List.any seedRanges \r -> inRange r seed then
            Break (Found loc)
        else
            Continue NotFound
    when locationMby is
        Found l -> Ok l
        _ -> crash "whoops, not found"

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

Range : {start: Nat, length: Nat}

inRange : Range, Nat -> Bool
inRange = \{start, length}, x ->
    x >= start && x < start + length

parseSeedRanges : Str -> List Range
parseSeedRanges = \line ->
    numbers = parseSeeds line
    List.chunksOf numbers 2 |> List.walk [] \ranges, pair ->
        when pair is
            [start, length] -> List.append ranges {start, length}
            l -> 
                expect List.len l == 2
                crash "oopsie"

expect
    r = parseSeedRanges "seeds: 79 14 55 13"
    r == [{ length: 14, start: 79 }, { length: 13, start: 55 }]


mapRange : Range, (Nat -> a) -> List a
mapRange = \{start, length}, transform ->
    List.range {start: At start, end: Length length} |> List.map transform

expect
    range = {start: 3, length: 2}
    r = mapRange range \e -> e * 2
    r == [6,8]

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
    if inRange {start: sourceStart, length} number then
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

inverseMap : Map -> Map
inverseMap = \{sourceStart, destinationStart, length} ->
    {sourceStart: destinationStart, destinationStart: sourceStart, length}

expect
    m = inverseMap {sourceStart: 98, destinationStart: 50, length: 2}
    r = tryConvert m 50
    r == Ok 98

expect
    m = inverseMap {sourceStart: 98, destinationStart: 50, length: 2}
    r = tryConvert m 51
    r == Ok 99

expect
    m = inverseMap {sourceStart: 98, destinationStart: 50, length: 2}
    r = tryConvert m 52
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
        result = convert maps state
        #dbg result
        result

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
