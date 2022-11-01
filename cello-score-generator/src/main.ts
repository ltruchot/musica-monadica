import { OpenSheetMusicDisplay } from "opensheetmusicdisplay";
import "./style.css";
import req from "./wasm/Lib.req.mjs";
import module from "./wasm/Lib.wasm.mjs";
import * as rts from "./wasm/rts.mjs";

const log = (a: unknown): typeof a => {
  console.log(a);
  return a;
};

const osmd = new OpenSheetMusicDisplay("osmdContainer");
osmd.setOptions({
  backend: "svg",
  drawTitle: true,
});

module
  .then((m) => rts.newAsteriusInstance(Object.assign(req, { module: m })))
  .then((i) => i.exports.generateMX("test"))
  .then((xml: string) => xml.replaceAll(/\\"/g, '"').substring(1).slice(0, -1))
  .then(log)
  .then((xml: string) => osmd.load(xml))
  .then(() => osmd.render());
