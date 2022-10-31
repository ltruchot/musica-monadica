import { OpenSheetMusicDisplay } from "opensheetmusicdisplay";
import "./style.css";

function getMusicXmlScore() {
  return `
  <?xml version="1.0" encoding="UTF-8"?><!DOCTYPE score-partwise PUBLIC "-//Recordare//DTD MusicXML 4.0 Partwise//EN" "http://www.musicxml.org/dtds/partwise.dtd"><score-partwise version="4.0"><movement-title>Generated score</movement-title><part-list><score-part id="P1"><part-name>Part 1</part-name></score-part></part-list><part id="P1"><measure number="1"><attributes><divisions>1</divisions><key><fifths>0</fifths></key><time><beats>4</beats><beat-type>4</beat-type></time><clef><sign>F</sign><line>4</line></clef></attributes><note><pitch><step>C</step><octave>2</octave></pitch><duration>4</duration><type>whole</type></note></measure><measure number="2"><note><pitch><step>G</step><octave>2</octave></pitch><duration>4</duration><type>whole</type></note></measure><measure number="3"><note><pitch><step>D</step><octave>3</octave></pitch><duration>4</duration><type>whole</type></note></measure><measure number="4"><note><pitch><step>A</step><octave>3</octave></pitch><duration>4</duration><type>whole</type></note><barline location="right"><bar-style>light-heavy</bar-style></barline></measure></part></score-partwise
  `;
}

const osmd = new OpenSheetMusicDisplay("osmdContainer");
osmd.setOptions({
  backend: "svg",
  drawTitle: true,
});

osmd.load(getMusicXmlScore()).then(() => osmd.render());
