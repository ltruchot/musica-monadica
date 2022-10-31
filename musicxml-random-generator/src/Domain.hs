{-# LANGUAGE OverloadedStrings #-}

module Domain
    ( Clef (TrebleClef, AltoClef, BassClef), getMXClef,
      TimeSignature (CommonTime, CompoundCommonTime), getMXTimeSignature,
      Note (Do, Re, Mi, Fa, Sol, La, Si), getMXNote,
      Octave (O0, O1, O2, O3, O4, O5, O6, O7, O8), getMxOctave
    ) where

import           Data.Text (Text)

data Clef = TrebleClef | AltoClef | BassClef deriving (Eq, Show)

getMXClef :: Clef -> (Text, Text)
getMXClef enum = case enum of
    TrebleClef -> ("G", "2")
    AltoClef   -> ("C", "3")
    BassClef   -> ("F", "4")

data TimeSignature = CommonTime | CompoundCommonTime deriving (Eq, Show)

getMXTimeSignature :: TimeSignature -> (Text, Text)
getMXTimeSignature enum = case enum of
    CommonTime         -> ("4", "4")
    CompoundCommonTime -> ("6", "8")

data Note = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Show, Ord)

getMXNote :: Note -> Text
getMXNote enum = case enum of
    Do  -> "C"
    Re  -> "D"
    Mi  -> "E"
    Fa  -> "F"
    Sol -> "G"
    La  -> "A"
    Si  -> "B"

data Octave = O0 | O1 | O2 | O3 | O4 | O5 | O6 | O7 | O8

getMxOctave :: Octave -> Text
getMxOctave enum = case enum of
    O0 -> "0"
    O1 -> "1"
    O2 -> "2"
    O3 -> "3"
    O4 -> "4"
    O5 -> "5"
    O6 -> "6"
    O7 -> "7"
    O8 -> "8"
