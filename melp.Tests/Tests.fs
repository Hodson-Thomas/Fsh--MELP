namespace melp.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Lexer.Tokens
open Lexer.Ast
open Lexer.Parser
open Utils.Errors

[<TestClass>]
type TestClass () =

    // ////////////////////////////////////////////////////
    // // Testing Lexer.Tokens
    // ////////////////////////////////////////////////////

    [<TestMethod>]
    member this.``string_to_token 3*x+1`` () = 
        let result = string_to_tokens "3*x+1";
        in
        let expected: Result<Token list, Error> = 
            [
                Token.Int 3; Token.Times; Token.Id "x"; Token.Plus; Token.Int 1
            ] |> Ok in
        Assert.AreEqual(expected,  result)

    
    [<TestMethod>]
    member this.``string_to_token 3 * x + 1`` () = 
        let result = string_to_tokens "3 * x + 1";
        in
        let expected: Result<Token list, Error> = 
            [
                Token.Int 3; Token.Times; Token.Id "x"; Token.Plus; Token.Int 1
            ] |> Ok in
        Assert.AreEqual(expected,  result)


    [<TestMethod>]
    member this.``string_to_token 3x+1`` () = 
        let result = string_to_tokens "3x+1";
        in
        let expected: Result<Token list, Error> = UnknownSequence "3x" |> Error in
        Assert.AreEqual(expected,  result);


    [<TestMethod>]
    member this.``string_to_token -10 * 9.1 + y / 3`` () = 
        let result = string_to_tokens "- 10 * (9.1 + y) / 3" in 
        let expected: Result<Token list, Error> = 
            [ 
                Token.Minus; Token.Int 10; Token.Times; Token.LPar; Token.Float 9.1; Token.Plus; 
                Token.Id "y"; RPar; Divide; Token.Int 3
            ] |> Ok in 
        Assert.AreEqual(expected, result)

    
    // ////////////////////////////////////////////////////
    // // Testing Lexer.Ast
    // ////////////////////////////////////////////////////


    [<TestMethod>]
    member this.``tokens_to_ast 3*x+1`` () = 
        let expect = Block [
            Block [
                Block [Token (Token.Int 3); Token (Token.Times); Token (Token.Id "x")]; 
                Token (Token.Plus); 
                Token (Token.Int 1)
            ]
        ] in
        match string_to_tokens "3*x+1" with
        | Error _ -> Assert.IsTrue(false)
        | Ok tokens ->
            match tokens_to_ast tokens with
            | Error _ -> Assert.IsTrue(false)
            | Ok ast -> 
                Assert.AreEqual(ast, expect)


    [<TestMethod>]
    member this.``tokens_to_ast -10 * 9.1 + y / 3`` () = 
        let expect = Block [
            Block [
                Block [
                    Block [Token (Token.Minus); Token (Token.Int 10)]; 
                    Token (Token.Times);
                    Token (Token.Float 9.1)
                ];
                Token (Token.Plus); 
                Block [
                    Token (Token.Id "y");
                    Token (Token.Divide);
                    Token (Token.Int 3)
                ]
            ]
        ] in
        match string_to_tokens "-10 * 9.1 + y / 3" with
        | Error _ -> Assert.IsTrue(false)
        | Ok tokens ->
            match tokens_to_ast tokens with
            | Error _ -> Assert.IsTrue(false)
            | Ok ast -> 
                Assert.AreEqual(ast, expect)

    
    // ////////////////////////////////////////////////////
    // // Testing Lexer.Parser
    // ////////////////////////////////////////////////////


    [<TestMethod>]
    member this.``ast_to_tree 3*x+1`` () = 
        let expect = Block [
            Block [
                Block [Token (Token.Int 3); Token (Token.Times); Token (Token.Id "x")]; 
                Token (Token.Plus); 
                Token (Token.Int 1)
            ]
        ] in
        match string_to_tokens "3*x+1" with
        | Error _ -> Assert.IsTrue(false)
        | Ok tokens ->
            match tokens_to_ast tokens with
            | Error _ -> Assert.IsTrue(false)
            | Ok ast -> 
                Assert.AreEqual(ast, expect)


    [<TestMethod>]
    member this.``ast_to_tree -10 * 9.1 + y / 3`` () = 
        let expect = Block [
            Block [
                Block [
                    Block [Token (Token.Minus); Token (Token.Int 10)]; 
                    Token (Token.Times);
                    Token (Token.Float 9.1)
                ];
                Token (Token.Plus); 
                Block [
                    Token (Token.Id "y");
                    Token (Token.Divide);
                    Token (Token.Int 3)
                ]
            ]
        ] in
        match string_to_tokens "-10 * 9.1 + y / 3" with
        | Error _ -> Assert.IsTrue(false)
        | Ok tokens ->
            match tokens_to_ast tokens with
            | Error _ -> Assert.IsTrue(false)
            | Ok ast -> 
                Assert.AreEqual(ast, expect)