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

validNames = [
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
]

main =
    fileName = "input"
    path = Path.fromStr fileName
    task =
        contents <- File.readUtf8 path |> await
        lines = Str.split contents "\n" |> List.dropIf Str.isEmpty
        numbersRes = List.mapTry lines findNumbers
        when numbersRes is
            Ok numbers -> List.sum numbers |> Num.toStr |> Stdout.line |> await (\{} -> Task.ok (Ok {}))
            Err e -> dbg e
                Err "Failed to parse numbers" |> Task.ok


    Task.attempt task \result ->
        when result is
            Ok (Ok {}) -> Task.ok {}
            Ok (Err _) -> Stdout.line "Error in tha code"
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

hasNumber = \line -> \n ->
    name <- List.get validNames n |> Result.try

    firstByte <- Str.toUtf8 line |> List.first |> Result.try Num.toNatChecked |> Result.try
    if firstByte >= '0' && firstByte <= '9' then
        Ok (firstByte - '1')
    else if Str.startsWith line name then
        Ok n
    else
        Err NotFound


findNumbers = \line ->
    idxs = List.range {start: At 0, end: Before (Str.countGraphemes line)}
    numIdxs = List.range {start: At 0, end: Before (List.len validNames)}
    foundNumbers = List.keepOks idxs \n ->
        s = strDropFirst line n
        List.keepOks numIdxs (hasNumber s) |> List.first

    first <- foundNumbers |> List.first |> Result.try
    last <- foundNumbers |> List.last |> Result.try
    Ok (((first+1)*10) +last+1)
