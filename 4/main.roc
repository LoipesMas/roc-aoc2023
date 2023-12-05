app "file-read"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task, await },
        "./input" as input : Str,
        # "./test_input" as input : Str,
    ]
    provides [main] to pf

main =
    lines = Str.split input "\n" |> List.dropIf Str.isEmpty
    task =
        {} <- Stdout.line "part1:" |> Task.await
        msg1 = part1 lines |> Result.map Num.toStr |> Result.withDefault "ERR"
        {} <- Stdout.line msg1 |> Task.await

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

part1 : List Str -> Result Nat [NotFound, InvalidNumStr]
part1 = \lines ->
    List.mapTry lines (\l -> parseLine l |> Result.map cardScore) |> Result.map List.sum

part2 : List Str -> Result Nat [NotFound, InvalidNumStr]
part2 = \lines ->
    cards <- List.mapTry lines parseLine |> Result.try
    { score: finalScore } = List.walk cards { winnings: [], score: 0 } \{ winnings, score }, card ->
        (thisMult0, winnings1) = advance winnings
        thisMult = thisMult0 + 1
        thisScore = countWon card
        { winnings: addWinnings winnings1 thisScore thisMult, score: score + thisMult }
    Ok finalScore

# [3,2,0] means 3 copies of the next one, 2 copies of the one after that
Winnings : List Nat

addWinnings : Winnings, Nat, Nat -> Winnings
addWinnings = \w, count, multiplier ->
    extend = List.repeat 0 (Num.subSaturated count (List.len w))
    List.concat w extend
    |> List.mapWithIndex \e, i ->
        if i < count then
            e + multiplier
        else
            e

expect
    addWinnings [3, 2, 0] 4 1 == [4, 3, 1, 1]

expect
    addWinnings [3, 2, 0] 4 2 == [5, 4, 2, 2]

expect
    addWinnings [3, 2, 0] 2 1 == [4, 3, 0]

expect
    addWinnings [3, 2, 0] 0 1 == [3, 2, 0]

advance : Winnings -> (Nat, Winnings)
advance = \w ->
    (List.get w 0 |> Result.withDefault 0, List.dropFirst w 1)

expect
    advance [3, 2, 0] == (3, [2, 0])

expect
    advance [] == (0, [])

Card : { winningNumbers : Set Nat, elfNumbers : Set Nat }

countWon : Card -> Nat
countWon = \{ winningNumbers, elfNumbers } ->
    Set.toList elfNumbers
    |> List.map \e -> if Set.contains winningNumbers e then 1 else 0
    |> List.sum

cardScore : Card -> Nat
cardScore = \card ->
    winCount = countWon card
    if winCount == 0 then
        0
    else
        Num.powInt 2 (winCount - 1)

expect
    card = { winningNumbers: Set.fromList [1, 2, 3, 4, 5], elfNumbers: Set.fromList [2, 3, 4, 8] }
    cardScore card == 4

expect
    card = { winningNumbers: Set.fromList [1, 2, 3, 4, 5], elfNumbers: Set.fromList [] }
    cardScore card == 0

parseNumbers : Str -> Result (Set Nat) [InvalidNumStr]
parseNumbers = \numbersStr ->
    Str.split numbersStr " " |> List.dropIf Str.isEmpty |> List.mapTry Str.toNat |> Result.map Set.fromList

parseLine : Str -> Result Card [NotFound, InvalidNumStr]
parseLine = \line ->
    { after } <- Str.splitFirst line ":" |> Result.try
    { before: winningStr, after: elfStr } <- Str.splitFirst after "|" |> Result.try
    winningNumbers <- parseNumbers winningStr |> Result.try
    elfNumbers <- parseNumbers elfStr |> Result.try
    Ok { winningNumbers, elfNumbers }

expect
    r = parseLine "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    r == Ok { winningNumbers: Set.fromList [41, 48, 83, 86, 17], elfNumbers: Set.fromList [83, 86, 6, 31, 17, 9, 48, 53] }
