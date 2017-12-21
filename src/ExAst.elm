module ExAst exposing (walkTypeOutwards, walkTypeInwards)

import Ast.Statement exposing (Type(..))


walkTypeOutwards : (Type -> Type) -> Type -> Type
walkTypeOutwards f t =
    f <|
        case t of
            (TypeVariable name) as x ->
                x

            TypeConstructor modulePathAndName args ->
                TypeConstructor modulePathAndName (List.map (walkTypeOutwards f) args)

            TypeTuple args ->
                TypeTuple <| List.map (walkTypeOutwards f) args

            TypeRecord args ->
                TypeRecord <| List.map (Tuple.mapSecond (walkTypeOutwards f)) args

            TypeRecordConstructor name args ->
                TypeRecordConstructor (walkTypeOutwards f name) <|
                    List.map (Tuple.mapSecond (walkTypeOutwards f)) args

            TypeApplication l r ->
                TypeApplication (walkTypeOutwards f l) (walkTypeOutwards f r)


walkTypeInwards : (Type -> Type) -> Type -> Type
walkTypeInwards f t =
    case f t of
        (TypeVariable name) as x ->
            x

        TypeConstructor modulePathAndName args ->
            TypeConstructor modulePathAndName (List.map (walkTypeInwards f) args)

        TypeRecordConstructor name args ->
            TypeRecordConstructor (walkTypeInwards f name) <|
                List.map (Tuple.mapSecond (walkTypeInwards f)) args

        TypeTuple args ->
            TypeTuple <| List.map (walkTypeInwards f) args

        TypeRecord args ->
            TypeRecord <| List.map (Tuple.mapSecond (walkTypeInwards f)) args

        TypeApplication l r ->
            TypeApplication (walkTypeInwards f l) (walkTypeInwards f r)
