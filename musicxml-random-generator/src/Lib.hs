{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( generateMX
    ) where

import qualified Data.Map          as Map
import           Data.Maybe        (fromMaybe)
import           Data.Text         (Text)
import           Data.Text.Lazy.IO as TextLazy (putStrLn)
import           Domain            (Clef (BassClef), Note (Do), Octave (O2),
                                    TimeSignature (CommonTime), getMXClef,
                                    getMXNote, getMXTimeSignature, getMxOctave)
import           Text.XML          (Doctype (Doctype), Document (Document),
                                    Element (Element), ExternalID (PublicID),
                                    Name, Node (NodeContent, NodeElement),
                                    Prologue (Prologue), def, renderText)


generateMX :: IO ()
generateMX = TextLazy.putStrLn $ renderText def $ Document (Prologue [] (Just musicXmlDoctype) []) content []

-- meta
musicXmlDoctype :: Doctype
musicXmlDoctype = Doctype "score-partwise" (Just musicXmlPublicId)

musicXmlPublicId :: ExternalID
musicXmlPublicId = PublicID "-//Recordare//DTD MusicXML 4.0 Partwise//EN" "http://www.musicxml.org/dtds/partwise.dtd"


-- helpers
createNodeElement :: Name -> [(Name, Text)] -> [Node] -> Node
createNodeElement name attrs body = NodeElement $ Element name (Map.fromList attrs) body

createNodeContent :: Text -> Node
createNodeContent = NodeContent

-- elements
{-
 - <score-partwise version="4.0">
 - made up of parts, where each part is made up of measures
-}
root :: [Node] -> Node
root = createNodeElement "score-partwise" [("version", "4.0")]

{-
 - <part-list>
 - header that lists the different musical parts in the score
-}
partList :: [Node] -> Node
partList = createNodeElement "part-list" []
{-
 - <movement-title>
 - header that lists the different musical parts in the score
-}
movementTitle :: [Node] -> Node
movementTitle = createNodeElement "movement-title" []

createScorePart :: Text -> Text -> (Node, [Node] -> Node)
createScorePart id name =
  let
    {-
    - <score-part id="P1">
    - required id here must refer to an id for a score-part in the body <part id="P1"> (body = after the <part-list>)
    -}
    scorePart = createNodeElement "score-part" [("id", id)]

    {-
    - <part-name>Part 1</part-name>
    - required element part-name for a score-part
    -}
    partName = createNodeElement "part-name" [] [createNodeContent name]
    {-
     - <part id="P1">
     - part related to a score part
    -}
    part = createNodeElement "part" [("id", id)]
  in (
    scorePart [ partName ],
    part
  )

createMeasure :: Text -> Text -> Text -> TimeSignature -> Clef -> Node
createMeasure number divisionsNumber romanceKey timeInfo clefInfo =
  let
    {-
      mapping between romance keys impliying romance notation + diesis/bemolle (do major (no #), mi (1#), la (2#), etc)
    -}
    romanceKeysMap = Map.fromList [("do", "0")]
    fifthsNumber = fromMaybe "" (Map.lookup romanceKey romanceKeysMap)
    (beatsNumber, beatTypeNumber) = getMXTimeSignature timeInfo
    (signLetter, lineNumber) = getMXClef clefInfo

    {-
    - <measure number="1">
    -}
    measure = createNodeElement "measure" [("number", number)]

    {-------- ATTRIBUTES --------}
    {-
    - <attributes>
    -}
    attributes = createNodeElement "attributes" []
    {-
     - <divisions>1</divisions>
    -}
    divisions = createNodeElement "divisions" []

    {- KEY INFO-}
    {-
     - <key>
    -}
    key = createNodeElement "key" []
    {-
      <fifths>0</fifths>
    -}
    fifths = createNodeElement "fifths" []


    {- TIME INFO -}
    {-
     - <time>
    -}
    time = createNodeElement "time" []
    {-
     - <beats>4</beats>
    -}
    beats = createNodeElement "beats" []
    {-
     - <beat-type>4</beat-type>
    -}
    beatType = createNodeElement "beat-type" []

    {- CLEF INFO-}
    {-
     - <clef>
    -}
    clef = createNodeElement "clef" []
    {-
     - <sign>4</sign>
    -}
    sign = createNodeElement "sign" []
    {-
     - <line>4</line>
    -}
    line = createNodeElement "line" []

    {-------- NOTES --------}
    {-
     - <note>
    -}
    note = createNodeElement "note" []
    {- PITCH -}
    {-
     - <pitch>
    -}
    pitch = createNodeElement "pitch" []
    {-
     - <step>C</step>
    -}
    step = createNodeElement "step" []
    {-
     - <octave>4</octave>
    -}
    octave = createNodeElement "octave" []
    {-  OTHERS -}
    {-
     - <duration>4</duration>
    -}
    duration = createNodeElement "duration" []
    {-
     - <type>4</type>
    -}
    noteType = createNodeElement "type" []



  in measure [
      attributes [
        divisions  [createNodeContent divisionsNumber],
        key [fifths [createNodeContent fifthsNumber]],
        time [
          beats [createNodeContent beatsNumber],
          beatType [createNodeContent beatTypeNumber]
        ],
        clef [
          sign [createNodeContent signLetter],
          line [createNodeContent lineNumber]
        ]
      ],
      note [
        pitch [
          step [createNodeContent (getMXNote Do)],
          octave [createNodeContent (getMxOctave O2)]
        ],
        duration [createNodeContent "4"],
        noteType [createNodeContent "whole"]
      ]
    ]



-- used to unwrap a top level Node to an Element
xmlToString :: Node -> Element
xmlToString (NodeElement x) = x

content :: Element
content =
  let
    (part1Header, part1Body) = createScorePart "P1" "Part 1"
  in xmlToString $ root [
      movementTitle [createNodeContent "Generated score"],
      partList [
        part1Header
      ],
      part1Body [ createMeasure "1" "1" "do" CommonTime BassClef ]
    ]


