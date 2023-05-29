const fs = require('fs');

const wasmBuffer = fs.readFileSync('m.wasm');
WebAssembly.instantiate(wasmBuffer).then(wasmModule => {
  // Exported function live under instance.exports
  const { f } = wasmModule.instance.exports;
    const r = f();
  console.log(r); // Outputs: 11
});
