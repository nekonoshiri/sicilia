"use strict";

// decode :: string -> string
// (base64 -> plain)
function decode(str) {
  return decodeURIComponent(escape(window.atob(str)));
}

function reverse(str) {
  return str.split("").reverse().join("");
}

function querySelectorArray(selector, element = window.document) {
  return Array.from(element.querySelectorAll(selector));
}

const inputs = querySelectorArray("tr.encoded input");
const outputs = querySelectorArray("tr.decoded span");

inputs.forEach((ipt, ipt_ix) => {
  ipt.addEventListener("input", () => {
    outputs[ipt_ix].innerHTML = decode(reverse(ipt.value));
  }, false);
});
