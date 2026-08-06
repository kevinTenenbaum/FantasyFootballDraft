import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

const [{ default: ts }, source] = await Promise.all([
  import("typescript"),
  readFile(new URL("../app/draftOptimization.ts", import.meta.url), "utf8"),
]);
const compiled = ts.transpileModule(source, {
  compilerOptions: { module: ts.ModuleKind.ES2022, target: ts.ScriptTarget.ES2022 },
}).outputText;
const { optimizeDraftRecommendations } = await import(
  `data:text/javascript;base64,${Buffer.from(compiled).toString("base64")}`
);

const player = (id, position, projectedPoints) => ({
  id,
  name: id,
  nflTeam: "TST",
  position,
  projectedPoints,
});

const roster = [
  player("Roster QB", "QB", 300),
  player("Roster RB1", "RB", 260),
  player("Roster RB2", "RB", 250),
  player("Roster WR1", "WR", 240),
  player("Roster WR2", "WR", 230),
  player("Roster TE", "TE", 220),
];
const targetRb = player("Target RB", "RB", 180);
const targetTe = player("Target TE", "TE", 205);
const pool = [
  targetRb,
  targetTe,
  player("Future WR", "WR", 215),
  player("Alternative RB1", "RB", 155),
  player("Alternative RB2", "RB", 145),
  player("Alternative TE1", "TE", 140),
  player("Alternative TE2", "TE", 130),
  player("Alternative QB1", "QB", 190),
  player("Alternative QB2", "QB", 175),
  ...Array.from({ length: 48 }, (_, index) => player(`Depth RB ${index}`, "RB", 140 - index)),
  ...Array.from({ length: 48 }, (_, index) => player(`Depth WR ${index}`, "WR", 200 - index)),
  ...Array.from({ length: 32 }, (_, index) => player(`Depth TE ${index}`, "TE", 125 - index)),
  ...Array.from({ length: 32 }, (_, index) => player(`Depth QB ${index}`, "QB", 165 - index)),
];
const players = [...roster, ...pool];
const maxPick = 168;
const survivalCurve = (nextTurnProbability) => Array.from({ length: maxPick }, (_, index) => {
  const overall = index + 1;
  if (overall <= 23) return 1;
  if (overall <= 26) return 1 - ((overall - 23) / 3) * (1 - nextTurnProbability);
  return nextTurnProbability * Math.exp(-(overall - 26) / 32);
});
const availability = {
  players: Object.fromEntries(players.map((entry) => [
    entry.id,
    { probabilities: survivalCurve(
      entry.id === targetRb.id ? 0.03 :
      entry.id === targetTe.id ? 0.98 :
      entry.id === "Future WR" ? 0.90 : 0.995,
    ) },
  ])),
};
const draftedPlayerIds = new Set(roster.map((entry) => entry.id));
const rosterPlayerIds = roster.map((entry) => entry.id);

function recommendationAt(currentOverall, currentRound, focusTeam) {
  const recommendations = optimizeDraftRecommendations({
    players,
    availability,
    draftedPlayerIds,
    rosterPlayerIds,
    currentOverall,
    currentRound,
    focusTeam,
    teamCount: 12,
    scenarioCount: 200,
  });
  return {
    rb: recommendations.find((entry) => entry.player.id === targetRb.id),
    te: recommendations.find((entry) => entry.player.id === targetTe.id),
  };
}

const noContinuation = recommendationAt(168, 14, 0);
assert.ok(noContinuation.rb && noContinuation.te);
assert.equal(
  noContinuation.rb.candidateRoleProbabilities.starter + noContinuation.rb.candidateRoleProbabilities.flex,
  1,
);
assert.equal(
  noContinuation.te.candidateRoleProbabilities.starter + noContinuation.te.candidateRoleProbabilities.flex,
  1,
);
assert.ok(noContinuation.te.meanScore > noContinuation.rb.meanScore);

const normalRollout = recommendationAt(23, 2, 1);
assert.ok(normalRollout.rb && normalRollout.te);

const summarize = (entry) => ({
  rank: entry.rank,
  projection: entry.player.projectedPoints,
  expectedRoster: Number(entry.meanScore.toFixed(4)),
  starters: Number(entry.meanStarterScore.toFixed(4)),
  bench: Number(entry.meanBenchScore.toFixed(4)),
  roles: Object.fromEntries(Object.entries(entry.candidateRoleProbabilities).map(
    ([role, probability]) => [role, `${(probability * 100).toFixed(1)}%`],
  )),
  nextPickLeaders: entry.futurePickDistributions[0]?.players.slice(0, 3).map((selection) => ({
    player: selection.player.name,
    probability: `${(selection.probability * 100).toFixed(1)}%`,
  })),
  recoveredLater: {
    rb: `${(entry.futurePickDistributions.reduce((total, distribution) => (
      total + (distribution.players.find((selection) => selection.player.id === targetRb.id)?.probability ?? 0)
    ), 0) * 100).toFixed(1)}%`,
    te: `${(entry.futurePickDistributions.reduce((total, distribution) => (
      total + (distribution.players.find((selection) => selection.player.id === targetTe.id)?.probability ?? 0)
    ), 0) * 100).toFixed(1)}%`,
  },
});

console.log(JSON.stringify({
  setup: {
    openSlot: "FLEX",
    rbProjection: targetRb.projectedPoints,
    teProjection: targetTe.projectedPoints,
    rbAvailableNextTurn: "3%",
    teAvailableNextTurn: "98%",
  },
  noContinuation: {
    rb: summarize(noContinuation.rb),
    te: summarize(noContinuation.te),
  },
  normalRollout: {
    rb: summarize(normalRollout.rb),
    te: summarize(normalRollout.te),
  },
}, null, 2));
