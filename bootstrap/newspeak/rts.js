// rts.js — runtime + primitives

// --- Heap operations (as before) ---
const heap = new Map();
let nextPtr = 0;

function store(val) {
  const p = nextPtr++;
  heap.set(p, { fields: [val] });
  return p;
}

function fetch(p, idx = 0) {
  const entry = heap.get(p);
  return entry ? entry.fields[idx] : undefined;
}

function update(p, val) {
  const entry = heap.get(p);
  if (entry) entry.fields[0] = val;
}

// --- Primitive functions ---
function int_print(n) {
  console.log(n);
  return null;
}

function int_gr(a, b) {
  return a > b;
}

function int_add(a, b) {
  return a + b;
}

// --- Export everything in one namespace ---
module.exports = {
  store,
  fetch,
  update,
  int_print,
  int_gr,
  int_add
};
