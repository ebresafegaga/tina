function absurd(s) {
  throw new Error(s);
}

// for now this is just an equaliy for constant types
// this would break down when objects or lists enter the picture
function equal(a, b) {
  return a === b;
}

const o = { 0: 0 };
const y = { 0: 3, 1: 10, 2: "ooo" };
const v = { 0: 4, 1: 23, 2: 2 };

function run() {
  if (equal(v[0], 4)) {
    let fresh0 = v[1];
    let b = v[2];
    if (equal(fresh0[0], 2)) {
      let x = fresh0[1];
      let y = fresh0[2];
      return { 0: 0, 1: x, 2: y, 3: b };
    } else {
      absurd("Pattern match failure");
    }
  } else {
    if (equal(v[0], 2)) {
      let f = v[1];
      return f;
    } else {
      if (equal(v[0], 0)) {
        return "ogaga";
      } else {
        if (equal(v[0], 1)) {
          return "afoks";
        } else {
          if (equal(v[0], 3)) {
            let a = v[1];
            let b = v[2];
            return { 0: 0, 1: b, 2: a };
          } else {
            absurd("Pattern match failure");
          }
        }
      }
    }
  }
}

run ();

const p = { 0: 0, 1: { 0: 3, 1: 23, 2: "elim" }, 2: 24 };

p[1];
