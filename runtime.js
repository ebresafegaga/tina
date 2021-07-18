
function absurd(s) {
  throw new Error(s);
}

// for now this is just an equaliy for constant types 
// this would break down when objects or lists enter the picture
function equal(a, b) {
  return a === b;
}


function hoist () {
  console.log (b)
  let b = 19;
  return b;
}