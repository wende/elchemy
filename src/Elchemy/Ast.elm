module Elchemy.Ast exposing (foldExpression, walkExpressionInwards, walkExpressionOutwards, walkTypeInwards, walkTypeOutwards)

{-| Contains helper functions to manage Elm Expression and Statement.Type ASTs
-}

import Ast.Expression exposing (Expression(..))
import Ast.Statement exposing (Type(..))


{-| Walks a tree of Ast.Statement.Type starting from the bottom branches and goes to the top using a replacer function
-}
walkTypeOutwards : (Type -> Type) -> Type -> Type
walkTypeOutwards f t =
    f <| walkType (walkTypeOutwards f) t


{-| Walks a tree of Ast.Statement.Type starting from the top and goes down the branches using a replacer function
-}
walkTypeInwards : (Type -> Type) -> Type -> Type
walkTypeInwards f t =
    f t |> walkType (walkTypeInwards f)


walkType : (Type -> Type) -> Type -> Type
walkType walkFunction t =
    case t of
        (TypeVariable name) as x ->
            x

        TypeConstructor modulePathAndName args ->
            TypeConstructor modulePathAndName (List.map walkFunction args)

        TypeRecordConstructor name args ->
            TypeRecordConstructor (walkFunction name) <|
                List.map (Tuple.mapSecond walkFunction) args

        TypeTuple args ->
            TypeTuple <| List.map walkFunction args

        TypeRecord args ->
            TypeRecord <| List.map (Tuple.mapSecond walkFunction) args

        TypeApplication l r ->
            TypeApplication (walkFunction l) (walkFunction r)


{-| Walks a tree of Ast.Expression.Expression starting from the bottom branches and goes to the top using a replacer function
-}
walkExpressionOutwards : (Expression -> Expression) -> Expression -> Expression
walkExpressionOutwards f t =
    f <| walkExpression (walkExpressionOutwards f) t


{-| Walks a tree of Ast.Expression.Expression starting from the top and goes down the branches using a replacer function
-}
walkExpressionInwards : (Expression -> Expression) -> Expression -> Expression
walkExpressionInwards f t =
    f t |> walkExpression (walkExpressionInwards f)


walkExpression : (Expression -> Expression) -> Expression -> Expression
walkExpression f t =
    case f t of
        (Variable _) as x ->
            x

        (Character _) as c ->
            c

        (String _) as s ->
            s

        (Integer _) as i ->
            i

        (Float _) as f ->
            f

        List exps ->
            List (List.map (walkExpression f) exps)

        Tuple exps ->
            Tuple (List.map (walkExpression f) exps)

        Access mod field ->
            Access (walkExpression f mod) field

        (AccessFunction field) as af ->
            af

        Record fields ->
            Record <| List.map (Tuple.mapSecond <| walkExpression f) fields

        RecordUpdate name fields ->
            RecordUpdate name <| List.map (Tuple.mapSecond <| walkExpression f) fields

        If check true false ->
            If (walkExpression f check) (walkExpression f true) (walkExpression f false)

        Let assignments body ->
            Let (List.map (Tuple.mapSecond <| walkExpression f) assignments) (walkExpression f body)

        Case target branches ->
            Case (walkExpression f target) (List.map (\( l, r ) -> ( walkExpression f l, walkExpression f r )) branches)

        Lambda args body ->
            Lambda (List.map (walkExpression f) args) (walkExpression f body)

        Application left right ->
            Application (walkExpression f left) (walkExpression f right)

        BinOp op left right ->
            BinOp (walkExpression f op) (walkExpression f left) (walkExpression f right)


{-| Walks a tree of Ast.Expression.Expression starting from the top and goes down the branches using a folder function
-}
foldExpression : (Expression -> acc -> acc) -> acc -> Expression -> acc
foldExpression f acc t =
    let
        rec =
            flip <| foldExpression f
    in
        f t <|
            case t of
                Variable _ ->
                    acc

                Character _ ->
                    acc

                String _ ->
                    acc

                Integer _ ->
                    acc

                Float _ ->
                    acc

                List exps ->
                    List.foldl rec acc exps

                Tuple exps ->
                    List.foldl rec acc exps

                Access mod field ->
                    rec mod acc

                AccessFunction field ->
                    acc

                Record fields ->
                    List.foldl (Tuple.second >> rec) acc fields

                RecordUpdate name fields ->
                    List.foldl (Tuple.second >> rec) acc fields

                If check true false ->
                    acc |> rec check |> rec true |> rec false

                Let assignments body ->
                    List.foldl (\( a, b ) lAcc -> rec a lAcc |> rec b) acc assignments |> rec body

                Case target branches ->
                    acc |> rec target |> (\nAcc -> List.foldl (\( a, b ) lAcc -> rec a lAcc |> rec b) nAcc branches)

                Lambda args body ->
                    List.foldl rec acc args |> rec body

                Application left right ->
                    acc |> rec left |> rec right

                BinOp op left right ->
                    acc |> rec op |> rec left |> rec right
