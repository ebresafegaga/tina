function absurd(s) {
  throw new Error(s);
}

// for now this is just an equaliy for constant types
// this would break down when objects or lists enter the picture
function equal(a, b) {
  return a === b;
}

function hoist() {
  console.log(b);
  let b = 19;
  return b;
}


x_4(x_16);
let x_16 = x_6(x_7, x_15);
let x_15 = x_9(x_13, x_14);
let x_14 = 0;

let x_13 = (comp_0, ks_0) => {
  let x_10 = comp_0[0];
  let x_11 = 0;
  let x_12 = equal(x_10, x_11);
  if (x_12) {
    let l_0 = comp_0[1];
    let idk1_0 = comp_0[2];
    let idk2_0 = comp_0[3];
    return absurd("Unhandled effect");
  } else {
    return absurd("Pattern match failure");
  }
};
