const o = x_5(x_17);
let x_230 = x_220(x_221, x_229);
let x_229 = x_223(x_227, x_228);
let x_228 = 0;
let x_227 = (comp_0, ks_0) => {
  let x_224 = comp_0[0];
  let x_225 = 0;
  let x_226 = equal(x_224, x_225);
  if (x_226) {
    let l_0 = comp_0[1];
    let idk1_0 = comp_0[2];
    let idk2_0 = comp_0[3];
    return absurd("Unhandled effect");
  } else {
    return absurd("Pattern match failure");
  }
};
const y = x_25(x_37);
let x_223 = (e1_0, e2_0) => {
  let x_222 = 0;
  return { 0: x_222, 1: e1_0, 2: e2_0 };
};
const v = x_45(x_57);
let x_221 = (x_0, ks_0) => {
  return x_0;
};
const ebresafe = x_63(x_75);
let x_220 = (e1_0, e2_0) => {
  let x_219 = 0;
  return { 0: x_219, 1: e1_0, 2: e2_0 };
};
const class_ = x_84(x_96);
let x_218 = (ks_0) => {
  let x_203 = ks_0[0];
  let x_204 = 0;
  let x_205 = equal(x_203, x_204);
  if (x_205) {
    let k_0 = ks_0[1];
    let ks__0 = ks_0[2];
    let fn_0 = (x_0, ks___0) => {
      let x_213 = 10;
      let x_214 = x_0(x_213);
      let x_216 = (e1_0, e2_0) => {
        let x_215 = 0;
        return { 0: x_215, 1: e1_0, 2: e2_0 };
      };
      let x_217 = x_216(k_0, ks_0);
      return x_214(x_217);
    };
    let x_209 = (___ks____0) => {
      let x_206 = ___ks____0[0];
      let x_207 = 0;
      let x_208 = equal(x_206, x_207);
      if (x_208) {
        let ___k____0 = ___ks____0[1];
        let ___ks____0 = ___ks____0[2];
        return ___k____0(fib, ___ks____0);
      } else {
        return absurd("Pattern match failure");
      }
    };
    let x_211 = (e1_0, e2_0) => {
      let x_210 = 0;
      return { 0: x_210, 1: e1_0, 2: e2_0 };
    };
    let x_212 = x_211(fn_0, ks_prime_0);
    return x_209(x_212);
  } else {
    return absurd("Pattern match failure");
  }
};
const og = x_107(x_119);
const sum = x_145(x_157);
const fib = x_190(x_202);
x_218(x_230);
