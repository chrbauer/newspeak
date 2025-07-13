// rts.js — runtime + primitives

// --- Heap operations (as before) ---
const heap = new Map();
let nextPtr = 0;


function store(tagOrVal, ...fields) {
  const p = nextPtr++;
  const node = Array.isArray(fields) && fields.length > 0
    ? { tag: tagOrVal, fields }
    : typeof tagOrVal === "string"
      ? { tag: tagOrVal, fields: [] }
      : { tag: null, fields: [tagOrVal] };

  heap.set(p, node);
  return p;
}

function fetch(p, idx = undefined) {
    const entry = heap.get(p);
    if(idx === undefined) {
	return entry;
    }
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
    return  { tag: a > b ? "CTrue" : "CFalse"};
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
