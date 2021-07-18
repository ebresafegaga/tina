function main() {
  let x_89 = 0;
  let x_90 = 3;
  let x_91 = 10;
  let x_92 = "ooo";
  let x_93 = 4;
  let x_94 = 23;
  let x_95 = 2;

  const o = { 0: x_89 };
  const y = { 0: x_90, 1: x_91, 2: x_92 };
  const v = { 0: x_93, 1: x_94, 2: x_95 };
  let x_96 = v[0];
  let x_97 = 2;
  let x_98 = equal(x_96, x_97);
  if (x_98) {
    let f = v[1];
    return f;
  } else {
    let x_99 = v[0];
    let x_100 = 0;
    let x_101 = equal(x_99, x_100);
    if (x_101) {
      return "ogaga";
    } else {
      let x_102 = v[0];
      let x_103 = 1;
      let x_104 = equal(x_102, x_103);
      if (x_104) {
        return "afoks";
      } else {
        let x_105 = v[0];
        let x_106 = 3;
        let x_107 = equal(x_105, x_106);
        if (x_107) {
          let a = v[1];
          let b = v[2];
          let x_112 = 0;
          return { 0: x_112, 1: b, 2: a };
        } else {
          let x_108 = v[0];
          let x_109 = 4;
          let x_110 = equal(x_108, x_109);
          if (x_110) {
            let x = v[1];
            let b = v[2];
            let x_111 = 0;
            return { 0: x_111, 1: x, 2: b };
          } else {
            return absurd("Pattern match failure");
          }
        }
      }
    }
  }
}

console.log (main ())

function absurd(s) {
  throw new Error(s);
}

// for now this is just an equaliy for constant types 
// this would break down when objects or lists enter the picture
function equal(a, b) {
  return a === b;
}