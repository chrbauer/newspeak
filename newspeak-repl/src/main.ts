import { Terminal } from "@xterm/xterm";
import "@xterm/xterm/css/xterm.css";


import wabtFactory from "wabt";
import { Readline } from "xterm-readline";

(async () => {
  const term = new Terminal({ cursorBlink: false });
  const host = document.getElementById("app")!;
  host.style.height = "100vh";
  term.open(host);

  const print = (s: string) => term.writeln(s);

  // Shared substrate
    const memory = new WebAssembly.Memory({ initial: 64, maximum: 1024 });
    const table = new WebAssembly.Table({ element: "anyfunc", initial: 256, maximum: 65535 });

    const u8 = new Uint8Array(memory.buffer);
  const dec = new TextDecoder();

  const rts = {
    print: (ptr: number, len: number) => print(dec.decode(u8.subarray(ptr, ptr + len))),
    table_len: () => table.length,
  };
  const imports = { env: { memory, table }, rts };

  const wabt = await wabtFactory();

  function wrapWat(userBody: string, wantsResult = true, alsoRegister = false): string {
    return `
(module
  (import "env" "memory" (memory $mem 64 1024))
(import "env" "table"  (table $table 2 funcref))
  (import "rts" "print"       (func $print (param i32 i32)))
  (import "rts" "table_len"   (func $table_len (result i32)))

  (func $load_next (result i32) (i32.load (i32.const 0)))
  (func $store_next (param $v i32) (i32.store (i32.const 0) (local.get $v)))

  (func $snippet_fun (param $x i32) (result i32)
    ${userBody}
  )

  (func $entry (param $ctx i32) ${wantsResult ? "(result i32)" : ""}
    ${alsoRegister ? `
      (local $i i32)
      (local.set $i (call $load_next))
      (table.set $table  (i32.const 0) $snippet_fun )
      
    ` : ""}
    ${wantsResult ? "(call $snippet_fun (local.get $ctx))" : ""}
  )
  (export "entry" (func $entry))
)`;
  }

  async function buildCallableFromWatBody(userBody: string, opts = { result: true, register: false }) {
      const wat = wrapWat(userBody, opts.result, opts.register);
      console.log(wat);
      const parsed = wabt.parseWat("snippet.wat", wat, {
  features: { reference_types: true, bulk_memory: true, multi_value: true }
});
    const { buffer } = parsed.toBinary({ log: false, write_debug_names: true });
    parsed.destroy();
    const { instance } = await WebAssembly.instantiate(buffer, imports);
    const entry = (instance.exports as any).entry;
    if (typeof entry !== "function") throw new Error("no export `entry`");
    return (ctx = 0) => entry(ctx);
  }

  // ----- xterm-readline -----
  const rl = new Readline(term);
term.loadAddon(rl); 
  rl.setCheckHandler((text) => {
    const trimmed = text.trimEnd();
    // Example: require double-ampersand to continue line
      if (trimmed.endsWith("&&")) return false; // keep reading
    return true; // submit on Enter
  });

  async function processLine(text: string) {
    const src = text.replace(/&&\s*$/,"").trim();
    if (!src) { prompt(); return; }
    try {
      const fn = await buildCallableFromWatBody(src, { result: true, register: true });
      const res = fn(1);
      print(String(res));
    } catch (e: any) {
      print("error: " + e.message);
    }
    prompt();
  }

  function readLine() {
    rl.read("nsp> ").then(processLine);
  }

  function prompt() {
    // next turn
    setTimeout(readLine, 0);
  }

  print("Type a WAT i32 expression as the body of $snippet_fun.");
  print("Example: (i32.add (local.get 0) (i32.const 41))");
  prompt();
  term.focus();
})();
