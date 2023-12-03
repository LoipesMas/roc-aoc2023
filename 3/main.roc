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
    lines = Str.split input "\n" |> List.dropIf Str.isEmpty
    task =
        states = List.mapWithIndex lines \line, index ->
            parseLine0 line index
        state = List.walk states emptyState foldState
        validNumbers = List.keepIf state.numbers (\n -> checkNumber n state.symbols)
        validNumbersValues = List.map validNumbers (.value)
        Stdout.line (Num.toStr (List.sum validNumbersValues))

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

listTakeWhile : List a, (a -> Bool) -> List a
listTakeWhile = \list, f ->
    List.walkUntil list [] \state, elem ->
        if f elem then
            Continue (List.append state elem)
        else
            Break state

expect listTakeWhile [1,2,3,4,5] (\e -> e <= 3) == [1,2,3]

LineNum : Nat
Range := { start : Nat, end : Nat } implements [Eq { isEq }]
isEq : Range, Range -> Bool
isEq = \@Range x, @Range y ->
    (x.start == y.start) && (x.end == y.end)

expandRange : Range, Nat -> Range
expandRange = \range, amount ->
    (@Range { start, end }) = range
    @Range {
        start: Num.subSaturated start amount,
        end: end + amount,
    }

expect expandRange (@Range { start: 2, end: 3 }) 1 == @Range { start: 1, end: 4 }
expect expandRange (@Range { start: 2, end: 3 }) 3 == @Range { start: 0, end: 6 }

inRange : Nat, Range -> Bool
inRange = \x, @Range { start, end } ->
    (x >= start) && (x <= end)

expect inRange 0 (@Range { start: 1, end: 3 }) |> Bool.not
expect inRange 1 (@Range { start: 1, end: 3 })
expect inRange 2 (@Range { start: 1, end: 3 })
expect inRange 3 (@Range { start: 1, end: 3 })
expect inRange 4 (@Range { start: 1, end: 3 }) |> Bool.not

Number : { value : Nat, span : Range, line : LineNum }
Symbol : { x : Nat, y : LineNum }

checkNumber : Number, List Symbol -> Bool
checkNumber = \number, symbols ->
    xRange = expandRange number.span 1
    yRange = expandRange (@Range { start: number.line, end: number.line }) 1
    List.any symbols (\s -> checkSymbolInRange s xRange yRange)

checkSymbolInRange : Symbol, Range, Range -> Bool
checkSymbolInRange = \symbol, xRange, yRange ->
    (inRange symbol.x xRange) && (inRange symbol.y yRange)

State : { numbers : List Number, symbols : List Symbol }

emptyState = {numbers: [], symbols: []}

foldState : State, State -> State
foldState = \x, y -> {
    numbers: List.concat x.numbers y.numbers,
    symbols: List.concat x.symbols y.symbols,
}

notSymbols = ".0123456789"

parseLine0 : Str, LineNum -> State
parseLine0 = \line, yPos ->
    parseLine line 0 yPos

parseLine : Str, Nat, LineNum -> State
parseLine = \line, xPos, yPos ->
    graphemes = Str.graphemes line
    obj = listTakeWhile graphemes (\e -> e != ".")
    mbyNumber = listTakeWhile obj (\e -> Str.contains notSymbols e) |> Str.joinWith ""
    if Str.isEmpty line then
        emptyState
    else if List.isEmpty obj then
        dotsLen = listTakeWhile graphemes (\e -> e == ".") |> List.len
        lineRest = strDropFirst line dotsLen
        parseLine lineRest (xPos + dotsLen) yPos
    else if Bool.not (Str.contains notSymbols (List.first obj |> Result.withDefault "")) then
        state = {emptyState & symbols: [{x: xPos, y: yPos}]}
        lineRest = strDropFirst line 1
        foldState state (parseLine lineRest (xPos + 1) yPos)
    else if Bool.not (Str.isEmpty mbyNumber) then
        value = when Str.toNat mbyNumber is
            Ok n -> n
            Err _ -> crash "wtf"
        numLen = Str.countGraphemes mbyNumber
        spanEnd = xPos + numLen - 1
        span = @Range {start: xPos, end: spanEnd }
        state = {emptyState & numbers: [{value, span, line: yPos}]}
        lineRest = strDropFirst line numLen
        foldState state (parseLine lineRest (xPos + numLen) yPos)
    else
        crash "unreachable?"
