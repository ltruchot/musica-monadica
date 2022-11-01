import * as rts from "./rts.mjs";
import module from "./Lib.wasm.mjs";
import req from "./Lib.req.mjs";
module.then(m => rts.newAsteriusInstance(Object.assign(req, {module: m}))).then(i => {
i.exports.main().catch(err => {if (!(err.startsWith('ExitSuccess') || err.startsWith('ExitFailure '))) i.fs.writeNonMemory(2, `Lib: ${err}
`)});
});
