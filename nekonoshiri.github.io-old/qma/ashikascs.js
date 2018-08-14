"use strict";


// ************************
//
//         せってい
//
// ************************


// QMA の点数は小数点以下第２位までだよ〜
// 点数の合計計算時，浮動小数点での計算では誤差がでるため無理やり丸めるよ！
const qmascore_accuracy = 2;

// アシカスコアを小数点以下第２位で丸めるよ！
const ashika_accuracy = 2;

// ページ移動前にお知らせするよ！
window.onbeforeunload = e => "ページを移ってもだいじょうぶ？";

// ************************
//
//      ゆーてぃりてぃ
//
// ************************


// x を小数点以下 digit 桁で丸めるよ！
function round(x, digit) {
  const cf = Math.pow(10, digit);
  return Math.round(x * cf) / cf;
}

// ジャグ配列の格納する各配列の n 番目の要素を集めて配列にするよ！
// エラー処理はサボってるから気をつけて
function collect(jugArr, n) {
  return jugArr.map(arr => arr[n]);
}

// 配列の合計を求めるよ！
function sum(arr) {
  return arr.reduce((acc, x) => acc + x);
}

// 小数を正確に合計するために 10^n 倍してから合計するよ！
function accurSum(arr, digit) {
  const cf = Math.pow(10, digit);
  return arr.reduce((acc, x) => acc + Math.round(x * cf), 0) / cf;
}

// 配列を降順でソートするよ！
function descSort(arr) {
  return arr.sort((a, b) => b - a);
}

// querySelectorAll して，結果の NodeList を配列にするよ！
function querySelectorArray(selector, element = window.document) {
  return Array.from(element.querySelectorAll(selector));
}


// ************************
//
//          めいん
//
// ************************


const input_players = querySelectorArray("table.input tr.player input");
const input_sums = querySelectorArray("table.input tr.sum span");
const input_scores = querySelectorArray("table.input tr.score").map(e =>
  querySelectorArray("input", e));
const output_players = querySelectorArray("table.output tr.player span");
const output_sums = querySelectorArray("table.output tr.sum span");
const output_scores = querySelectorArray("table.output tr.score").map(e =>
  querySelectorArray("span", e));
const scoretypeRbs = querySelectorArray("p.scoretype input");

// スコアタイプ（アシカスコア以外に惜敗率でも計算できるよ！）
const scoreType = {
  ashika: "ASHIKA SCORE",
  sekihairitsu: "惜敗率",
};

// 現在のスコアタイプを求めるよ！
function getScoreType() {
  if (scoretypeRbs[0].checked) {
    return scoreType.ashika;
  }
  return scoreType.sekihairitsu;
}

// プレーヤー player_ix の inning 戦目のアシカスコアを求めるよ！
// アシカスコアといいつつほかのスコアタイプにも対応しているよ！
// rounded = true にすると結果を小数点第 ashika_accuracy 桁で丸めるよ！
function getAshikaScore(inning, player_ix, rounded = true) {
  const myScore = Number(input_scores[inning][player_ix].value);
  const [score1st, score2nd] = descSort(input_scores[inning].map(
    ipt => Number(ipt.value)
  ));
  let ratio;
  if (getScoreType() === scoreType.ashika) {
    ratio = myScore === score1st ? myScore / score2nd : myScore / score1st;
    ratio *= 100;
  } else {
    ratio = 100 * myScore / score1st;
  }
  if (rounded) {
    ratio = round(ratio, ashika_accuracy);
  }
  return ratio;
}

// プレーヤーの合計得点を求めるよ！
function playerSum(column_ix) {
  return accurSum(collect(input_scores, column_ix).map(
    ipt => Number(ipt.value)
  ), qmascore_accuracy);
}

// プレーヤーの合計アシカスコアを求めるよ！
function ashikaSum(column_ix) {
  const accuracy_margin = 3;
  const result = accurSum(
    input_scores.map((ipt, inning) => {
      const sc = getAshikaScore(inning, column_ix, false);
      return isNaN(sc) ? 0 : sc;
    }), ashika_accuracy + accuracy_margin
  );
  return round(result, ashika_accuracy);
}

// 第 inning 回戦のアシカスコアとアシカスコアの合計を再計算して書き込むよ！
function recalcuration(inning) {
  output_scores[inning].forEach((opt, opt_ix) => {
    opt.innerHTML = getAshikaScore(inning, opt_ix);
    output_sums[opt_ix].innerHTML = ashikaSum(opt_ix);
  });
}

// 全アシカスコアを再計算して書き込むよ！
function recalcuration_all() {
  output_scores.forEach((tr, tr_ix) => {
    tr.forEach((opt, opt_ix) => {
      opt.innerHTML = getAshikaScore(tr_ix, opt_ix);
    });
  });
  input_players.forEach((p, column_ix) => {
    output_sums[column_ix].innerHTML = ashikaSum(column_ix);
  });
}

// プレーヤー名の入力に対するイベントを登録するよ！
// 1. ASHIKA SCORE テーブルのプレーヤー欄に名前を書き込む
input_players.forEach((ipt, i) => {
  ipt.addEventListener("input", () => {
    output_players[i].innerHTML = ipt.value;
  }, false);
});

// 得点の入力に対するイベントを登録するよ！
// 1. プレーヤーの得点の合計を書き込む
// 2. 全プレーヤーのアシカスコアと合計を再計算して書き込む
input_scores.forEach((tr, tr_ix) => {
  tr.forEach((ipt, ipt_ix) => {
    ipt.addEventListener("input", () => {
      input_sums[ipt_ix].innerHTML = playerSum(ipt_ix);
      recalcuration(tr_ix);
    }, false);
  });
});

// 各ラジオボタンにイベントを登録するよ！
// 1. アシカスコア再計算
// 2. テーブル名変更
scoretypeRbs.forEach(rb => {
  rb.addEventListener("change", () => {
    recalcuration_all();
    document.querySelector("table.output caption").innerHTML = getScoreType();
  }, false);
});
