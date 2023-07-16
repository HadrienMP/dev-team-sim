module Difficulty exposing (..)

import List exposing (tail)
import Random exposing (Generator)


type Difficulty
    = Atomic
    | Simple
    | Medium
    | Hard
    | Impossible


asInt : Difficulty -> Int
asInt difficulty =
    case difficulty of
        Atomic ->
            2

        Simple ->
            3

        Medium ->
            5

        Hard ->
            8

        Impossible ->
            13


generator : Generator Difficulty
generator =
    case all of
        head :: tail ->
            Random.uniform head tail

        _ ->
            Random.constant Simple


all : List Difficulty
all =
    next [] |> List.reverse


next : List Difficulty -> List Difficulty
next list =
    case List.head list of
        Nothing ->
            next [ Atomic ]

        Just Atomic ->
            next (Simple :: list)

        Just Simple ->
            next (Medium :: list)

        Just Medium ->
            next (Hard :: list)

        Just Hard ->
            next (Impossible :: list)

        Just Impossible ->
            list
