// prim.js

/**
 * Print an integer to stdout.
 * @param {number} n
 * @returns {null}
 */
function int_print(n) {
  console.log(n);
  return null;
}

/**
 * Integer “greater‐than” test.
 * @param {number} a
 * @param {number} b
 * @returns {boolean} true if a > b, false otherwise
 */
function int_gr(a, b) {
  return a > b;
}

/**
 * Integer addition.
 * @param {number} a
 * @param {number} b
 * @returns {number} a + b
 */
function int_add(a, b) {
  return a + b;
}

module.exports = {
  int_print,
  int_gr,
  int_add
};
