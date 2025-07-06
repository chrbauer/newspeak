// rts.js
const heap = new Map();
let nextPtr = 0;

module.exports = {
  store: (val) => {
    const p = nextPtr++;
    heap.set(p, { fields: [val] });
    return p;
  },
  fetch: (p, idx = 0) => {
    const entry = heap.get(p);
    return entry ? entry.fields[idx] : undefined;
  },
  update: (p, val) => {
    const entry = heap.get(p);
    if (entry) entry.fields[0] = val;
  }
};
