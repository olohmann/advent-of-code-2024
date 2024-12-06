module Tests

open Xunit
open AdventOfCode.Utils.Board2D
let b : Board2D<int> = [|
                           // 0    1   2   3   4
                  (* 0 *)  [| 1;   2;  3;  4;  5 |]
                  (* 1 *)  [| 6;   7;  8;  9; 10 |]
                  (* 2 *)  [| 11; 12; 13; 14; 15 |]
                  (* 3 *)  [| 16; 17; 18; 19; 20 |]
                  (* 4 *)  [| 21; 22; 23; 24; 25 |]
                       |]

[<Fact>]
let ``left_1`` () =
   
    let pos : Position = { Row = 0; Col = 2 }
    let left = getLeftSeq b pos 2
    
    let expected = [| 3; 2 |]
    
    match left with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"

[<Fact>]
let ``left_2`` () =
   
    let pos : Position = { Row = 0; Col = 2 }
    let left = getLeftSeq b pos 3
    
    let expected = [| 3; 2; 1 |]
    
    match left with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"
    
[<Fact>]
let ``left_3`` () =
   
    let pos : Position = { Row = 0; Col = 2 }
    let left = getLeftSeq b pos 4
    
    match left with
    | Some value -> failwith "Expecting no value to unwrap"
    | None -> ()
    
[<Fact>]
let ``right_1`` () =
   
    let pos : Position = { Row = 0; Col = 2 }
    let right = getRightSeq b pos 2
    
    let expected = [| 3; 4 |]
    
    match right with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"

[<Fact>]
let ``right_2`` () =
   
    let pos : Position = { Row = 0; Col = 2 }
    let right = getRightSeq b pos 3
    
    let expected = [| 3; 4; 5 |]
    
    match right with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"
    
[<Fact>]
let ``right_3`` () =
   
    let pos : Position = { Row = 0; Col = 2 }
    let top = getRightSeq b pos 4
    
    match top with
    | Some value -> failwith "Expecting no value to unwrap"
    | None -> ()
    
[<Fact>]
let ``up_1`` () =
   
    let pos : Position = { Row = 2; Col = 2 }
    let up = getUpSeq b pos 2
    
    let expected = [| 13; 8 |]
    
    match up with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"

[<Fact>]
let ``up_2`` () =
   
    let pos : Position = { Row = 2; Col = 2 }
    let up = getUpSeq b pos 3
    
    let expected = [| 13; 8; 3 |]
    
    match up with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"
    
[<Fact>]
let ``up_3`` () =
   
    let pos : Position = { Row = 2; Col = 2 }
    let up = getUpSeq b pos 4
    
    match up with
    | Some value -> failwith "Expecting no value to unwrap"
    | None -> ()
    
[<Fact>]
let ``down_1`` () =
   
    let pos : Position = { Row = 2; Col = 2 }
    let down = getDownSeq b pos 2
    
    let expected = [| 13; 18 |]
    
    match down with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"

[<Fact>]
let ``down_2`` () =
   
    let pos : Position = { Row = 2; Col = 2 }
    let down = getDownSeq b pos 3
    
    let expected = [| 13; 18; 23 |]
    
    match down with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"
    
[<Fact>]
let ``down_3`` () =
   
    let pos : Position = { Row = 2; Col = 2 }
    let down = getDownSeq b pos 4
    
    match down with
    | Some value -> failwith "Expecting no value to unwrap"
    | None -> ()

[<Fact>]
let ``up_left`` () =
    let pos : Position = { Row = 2; Col = 2 }
    let down = getUpLeftSeq b pos 3
    
    let expected = [| 13; 7; 1 |]
    
    match down with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"
    
[<Fact>]
let ``up_right`` () =
    let pos : Position = { Row = 2; Col = 2 }
    let upRight = getUpRightSeq b pos 3

    let expected = [| 13; 9; 5 |]

    match upRight with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"

[<Fact>]
let ``down_left`` () =
    let pos : Position = { Row = 2; Col = 2 }
    let downLeft = getDownLeftSeq b pos 3

    let expected = [| 13; 17; 21 |]

    match downLeft with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"

[<Fact>]
let ``down_right`` () =
    let pos : Position = { Row = 2; Col = 2 }
    let downRight = getDownRightSeq b pos 3

    let expected = [| 13; 19; 25 |]

    match downRight with
    | Some value -> Assert.Equal<int seq>(expected, value)
    | None -> failwith "No value to unwrap"

[<Fact>]
let ``all_directions`` () =
    let pos : Position = { Row = 2; Col = 2 }
    let allValidDirections = getAllValidDirections b pos 3

    let expected = [
        [| 13; 12; 11 |]  // Left
        [| 13; 7; 1 |]    // UpLeft
        [| 13; 8; 3 |]    // Up
        [| 13; 9; 5 |]    // UpRight
        [| 13; 14; 15 |]  // Right
        [| 13; 19; 25 |]  // DownRight
        [| 13; 18; 23 |]  // Down
        [| 13; 17; 21 |]  // DownLeft
    ]

    let actual = allValidDirections |> List.map Seq.toArray

    Assert.Equal<int array list>(expected, actual)