function main() {
  let x_34 = "afoke";
  let x_35 = 0;
  let x_36 = "ogaga";
  let x_37 = 10;
  let x_38 = { 0: x_35, 1: x_36, 2: x_37 };
  let x_39 = equal(x_34, x_38);
  if (x_39) {
    return false;
  } else {
    let x_40 = 0;
    let x_41 = "ogaga";
    let x_42 = 10;
    let x_43 = { 0: x_40, 1: x_41, 2: x_42 };
    let x_44 = x_43[0];
    let x_45 = 0;
    let x_46 = equal(x_44, x_45);
    if (x_46) {
      let x_61 = 0;
      let x_62 = "ogaga";
      let x_63 = 10;
      let x_64 = { 0: x_61, 1: x_62, 2: x_63 };
      let fresh_32 = x_64[1];
      let x_57 = 0;
      let x_58 = "ogaga";
      let x_59 = 10;
      let x_60 = { 0: x_57, 1: x_58, 2: x_59 };
      let x = x_60[2];
      let x_53 = 0;
      let x_54 = "ogaga";
      let x_55 = 10;
      let x_56 = { 0: x_53, 1: x_54, 2: x_55 };
      let fresh_33 = x_56[3];
      let x_47 = "ogaga";
      let x_48 = equal(x_47, fresh_32);
      if (x_48) {
        let x_49 = 5;
        let x_50 = equal(x_49, fresh_33);
        if (x_50) {
          let x_51 = 0;
          let x_52 = true;
          return { 0: x_51, 1: x_52, 2: x };
        } else {
          return absurd("Pattern match failure");
        }
      } else {
        return absurd("Pattern match failure");
      }
    } else {
      return absurd("Pattern match failure");
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