const o = { 0: x_0 };
let if_24 = () => {
  if (x_9) {
    let f = v[1];
    return f;
  } else {
    let x_10 = v[0];
    let x_11 = 0;
    let x_12 = equal(x_10, x_11);
    if (x_12) {
      return "ogaga";
    } else {
      let x_13 = v[0];
      let x_14 = 1;
      let x_15 = equal(x_13, x_14);
      if (x_15) {
        return "afoks";
      } else {
        let x_16 = v[0];
        let x_17 = 3;
        let x_18 = equal(x_16, x_17);
        if (x_18) {
          let a = v[1];
          let b = v[2];
          let x_23 = 0;
          return { 0: x_23, 1: b, 2: a };
        } else {
          let x_19 = v[0];
          let x_20 = 4;
          let x_21 = equal(x_19, x_20);
          if (x_21) {
            let x = v[1];
            let b = v[2];
            let x_22 = 0;
            return { 0: x_22, 1: x, 2: b };
          } else {
            return absurd("Pattern match failure");
          }
        }
      }
    }
  }
};
let x_9 = equal(x_7, x_8);
let x_8 = 2;
let x_7 = v[0];
let x_6 = 2;
let x_5 = 23;
let x_4 = 4;
let x_3 = "ooo";
let x_2 = 10;
let x_1 = 3;
let x_0 = 0;
const y = { 0: x_1, 1: x_2, 2: x_3 };
const v = { 0: x_4, 1: x_5, 2: x_6 };
if_24;
