# musicxml-random-generator

OMG asterius is just deprecated in favor of ghc-wasm-meta


@see https://www.tweag.io/blog/2022-11-22-wasm-backend-merged-in-ghc/


So let’s start again...

## An haskell/wasm score generator for music beginners


This is a **WIP** project about generating randomized [MusicXML](https://www.w3.org/2021/06/musicxml40/) score, helping beginners to practice scoped sets of notes.

For now, params will be in [european solfège notation](https://en.wikipedia.org/wiki/Solf%C3%A8ge) (aka "Fixed DO", or "romance language", do-re-mi-fa-sol-la-si over C-D-E-F-G-A-B).

And it will be oriented for **cello**.

--- 

`stack build` to build.

Then, `stack exec musicxml-random-generator-exe` to run.


---

### Podman & asterius
To WASM (on windows + WSL)
- ensure podman is installed on windows: https://github.com/containers/podman/releases
- open vscode "podman-machine-default" command prompt, then:
- `cd /mnt/d/Workspace/windows-only/musica-monadica/musicxml-random-generator`
- `sudo podman run -it --rm -v $(pwd):/workspace -w /workspace docker.io/terrorjack/asterius`

### Direct linking with exported functions (prefered method)
- now we are in a linux bash, cursor in the volume
  - `mkdir -p wasm`
  - `ahc-link --no-main --input-hs /workspace/src/Lib.hs musicxml-random-generator --export-function=generateMX --browser --output-directory /workspace/wasm`
- to consume, copy `/wasm` folder content in a JS project
- `cp ./musicxml-random-generator/wasm ./cello-score-generator/src -rf`
- then, in main JS file
  ```javascript
  import req from "./wasm/Lib.req.mjs";
  import module from "./wasm/Lib.wasm.mjs";
  import * as rts from "./wasm/rts.mjs";
  ```


### Cabal, if only "main" function (so will not work in current state)
- now we are in a linux bash, cursor in the volume
  - `ahc-cabal new-install --installdir exe musicxml-random-generator`
  - `mkdir wasm`
  - `ahc-dist --input-exe exe/musicxml-random-generator-exe --run --browser --bundle --output-dir /workspace/wasm`
- now, from `./wasm` (with NPM static-server installed globally)
  - `static-server`
  - go to `http://localhost:9080/musicxml-random-generator-exe.html`
  - check console


  module
    .then((m) => rts.newAsteriusInstance(Object.assign(req, { module: m })))
    .then((i) => i.exports.generateMX("test"))
  ```