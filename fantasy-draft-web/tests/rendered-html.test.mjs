import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const root = new URL("../", import.meta.url);

test("ships a complete projection pool", async () => {
  const projections = JSON.parse(await readFile(new URL("public/projections.json", root), "utf8"));
  assert.ok(projections.length > 900);
  assert.deepEqual([...new Set(projections.map((player) => player.position))].sort(), ["DST", "K", "QB", "RB", "TE", "WR"]);
  assert.equal(projections.filter((player) => player.position === "DST").length, 32);
  assert.ok(projections.filter((player) => player.position === "K").length >= 32);
  assert.ok(projections.every((player) =>
    player.id &&
    player.name &&
    Number.isFinite(player.projectedPoints) &&
    Number.isFinite(player.projectedP10) &&
    Number.isFinite(player.projectedP50) &&
    Number.isFinite(player.projectedP90) &&
    player.projectedP10 <= player.projectedP50 &&
    player.projectedP50 <= player.projectedP90
  ));
});

test("ships a two-point brief for every player", async () => {
  const [projections, summaries] = await Promise.all([
    readFile(new URL("public/projections.json", root), "utf8").then(JSON.parse),
    readFile(new URL("public/player-summaries.json", root), "utf8").then(JSON.parse),
  ]);

  assert.equal(Object.keys(summaries).length, projections.length);
  projections.forEach((player) => {
    const summary = summaries[player.id];
    assert.ok(summary, `Missing player brief for ${player.name}`);
    assert.equal(summary.bullets.length, 2, `Expected two bullets for ${player.name}`);
    assert.ok(summary.bullets.every((bullet) => typeof bullet === "string" && bullet.length > 30));
    assert.match(summary.updated, /^\d{4}-\d{2}-\d{2}$/);
    assert.ok(summary.sourceLabel);
  });
});

test("ships three-season performance history and a 2025 game log", async () => {
  const [projections, history, draftRoom] = await Promise.all([
    readFile(new URL("public/projections.json", root), "utf8").then(JSON.parse),
    readFile(new URL("public/player-history.json", root), "utf8").then(JSON.parse),
    readFile(new URL("app/DraftRoom.tsx", root), "utf8"),
  ]);
  const modeled = projections.filter((player) => ["QB", "RB", "WR", "TE"].includes(player.position));

  assert.deepEqual(history.meta.seasons, [2023, 2024, 2025]);
  assert.equal(history.meta.gameLogSeason, 2025);
  assert.equal(history.meta.scoring, "PPR");
  assert.equal(Object.keys(history.players).length, modeled.length);
  modeled.forEach((player) => {
    const playerHistory = history.players[player.id];
    assert.ok(playerHistory, `Missing history container for ${player.name}`);
    assert.ok(playerHistory.seasons.length <= 3);
    assert.ok(playerHistory.seasons.every((season) =>
      history.meta.seasons.includes(season.season) &&
      Number.isFinite(season.fantasyPoints) &&
      Number.isFinite(season.pointsPerGame)
    ));
    assert.ok(playerHistory.gameLog.every((game) =>
      game.week >= 1 && game.week <= 18 && Number.isFinite(game.fantasyPoints)
    ));
  });
  assert.match(draftRoom, /role="dialog"/);
  assert.match(draftRoom, /Season history/);
  assert.match(draftRoom, /2025 game log/);
});

test("ships additive linear-model explanations for every modeled player", async () => {
  const [projections, explanations, draftRoom] = await Promise.all([
    readFile(new URL("public/projections.json", root), "utf8").then(JSON.parse),
    readFile(new URL("public/player-interpretability.json", root), "utf8").then(JSON.parse),
    readFile(new URL("app/DraftRoom.tsx", root), "utf8"),
  ]);
  const modeled = projections.filter((player) => ["QB", "RB", "WR", "TE"].includes(player.position));

  assert.equal(explanations.meta.season, 2026);
  assert.equal(Object.keys(explanations.players).length, modeled.length);
  modeled.forEach((player) => {
    const explanation = explanations.players[player.id];
    assert.ok(explanation, `Missing model explanation for ${player.name}`);
    assert.ok(explanation.contributions.length >= 14);
    assert.ok(explanation.contributions.every((item) => item.label && item.input && Number.isFinite(item.points)));
    const contributionTotal = explanation.contributions.reduce((total, item) => total + item.points, 0);
    assert.ok(Math.abs(contributionTotal - explanation.projectedPoints) < 0.02, `Contributions do not reconcile for ${player.name}`);
    assert.ok(Math.abs(explanation.projectedPoints - player.projectedPoints) < 0.02, `Explanation does not match board projection for ${player.name}`);
  });
  assert.match(draftRoom, /Model drivers/);
  assert.match(draftRoom, /contribution-chart/);
  assert.match(draftRoom, /Correlated inputs/);
});

test("uses the draft room instead of starter content", async () => {
  const [page, layout, packageJson] = await Promise.all([
    readFile(new URL("app/page.tsx", root), "utf8"),
    readFile(new URL("app/layout.tsx", root), "utf8"),
    readFile(new URL("package.json", root), "utf8"),
  ]);

  assert.match(page, /<DraftRoom \/>/);
  assert.match(layout, /Fantasy Football/);
  assert.match(layout, /\/og\.png/);
  assert.doesNotMatch(`${page}${layout}${packageJson}`, /SkeletonPreview|react-loading-skeleton|Starter Project/);
});

test("ships focus-team availability curves", async () => {
  const [availability, draftRoom] = await Promise.all([
    readFile(new URL("public/availability.json", root), "utf8").then(JSON.parse),
    readFile(new URL("app/DraftRoom.tsx", root), "utf8"),
  ]);

  assert.equal(availability.model.season, 2026);
  assert.equal(availability.model.maxPick, 180);
  assert.ok(availability.model.totalDrafts > 0);
  assert.ok(Object.keys(availability.players).length > 150);
  assert.ok(Object.values(availability.players).every((player) =>
    player.probabilities.length === availability.model.maxPick &&
    player.probabilities.every((probability) => probability >= 0 && probability <= 1)
  ));
  assert.match(draftRoom, /Focus team/);
  assert.match(draftRoom, /nextFocusSlots/);
  assert.match(draftRoom, /availabilityAtFocusPick/);
  assert.match(draftRoom, /Next 2 picks/);
  assert.match(draftRoom, /slots\.length === 2/);
  assert.match(draftRoom, /Now picking/);
  assert.match(draftRoom, /focus-tracker/);
  assert.match(draftRoom, /aria-live="polite"/);
});
