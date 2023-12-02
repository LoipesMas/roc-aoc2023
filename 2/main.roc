app "file-read"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task, await },
        pf.File,
        pf.Path,
    ]
    provides [main] to pf

main =
    fileName = "input"
    path = Path.fromStr fileName
    task =
        contents <- File.readUtf8 path |> await
        lines = Str.split contents "\n" |> List.dropIf Str.isEmpty
        gamesR = List.mapTry lines parseLine
        # part 1
        # when gamesR is
        #    Ok games ->
        #        possibleGames = List.keepIf games checkGame
        #        sum = List.map possibleGames (\g -> g.gameNumber ) |> List.mapTry Str.toU32 |> Result.map List.sum
        #        when sum is
        #            Ok s -> Stdout.line (Num.toStr s)
        #            Err _ -> Stdout.line "oof"
        #    Err _ -> Stdout.line "err"
        # part 2
        when gamesR is
            Ok games ->
                minCubeCountPowers = List.map games findMinCubeCountInGame |> List.map minCubeCountPower
                List.sum minCubeCountPowers |> Num.toStr |> Stdout.line

            Err _ -> Stdout.line "err"

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

strMap = \s, l ->
    Str.graphemes s |> l |> Str.joinWith ""

strDropFirst = \s, n ->
    strMap s (\l -> List.dropFirst l n)

minCubeCountPower = \{ red, green, blue } ->
    red * green * blue

findMinCubeCountInGame = \{ reveals } ->
    List.map reveals findMinCubeCountInReveal |> List.walk { red: 0, green: 0, blue: 0 } foldMinCubeCount

findMinCubeCountInReveal = \pairs ->
    List.walk pairs { red: 0, green: 0, blue: 0 } \state, { amount, color } ->
        when color is
            Red -> { state & red: Num.max state.red amount }
            Green -> { state & green: Num.max state.green amount }
            Blue -> { state & blue: Num.max state.blue amount }

foldMinCubeCount = \a, b -> {
    red: Num.max a.red b.red,
    green: Num.max a.green b.green,
    blue: Num.max a.blue b.blue,
}

checkGame = \{ reveals } ->
    List.all reveals checkReveal

checkReveal = \reveal ->
    List.all reveal checkPair

checkPair = \{ amount, color } ->
    maxAmount =
        when color is
            Red -> 12
            Green -> 13
            Blue -> 14
    amount <= maxAmount

parseLine = \line ->
    { before, after } <- Str.splitFirst line ":" |> Result.try
    gameNumber <- Str.splitFirst before " " |> Result.map (\r -> r.after) |> Result.try
    revealStrs = Str.split after ";" |> List.map Str.trim |> List.dropIf Str.isEmpty
    reveals <- List.mapTry revealStrs parseReveal |> Result.try
    Ok { gameNumber, reveals }

parseReveal = \str ->
    colorsStrs = Str.split str "," |> List.map Str.trim
    parsedColors <- List.mapTry colorsStrs parseColorWithAmount |> Result.try
    Ok parsedColors

parseColorWithAmount = \str ->
    { before: amountStr, after: colorStr } <- Str.splitFirst str " " |> Result.try
    amount <- Str.toU32 amountStr |> Result.try
    color <- parseColor colorStr |> Result.try
    Ok { color, amount }

parseColor = \str ->
    if str == "red" then
        Ok Red
    else if str == "green" then
        Ok Green
    else if str == "blue" then
        Ok Blue
    else
        Err UnknownColor

