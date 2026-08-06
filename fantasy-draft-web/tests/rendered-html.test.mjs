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
  assert.match(draftRoom, /setDetailPlayerId\(playerId\);\s+showModelInterpretability\(\);/);
});

test("uses the draft room instead of starter content", async () => {
  const [page, layout, packageJson] = await Promise.all([
    readFile(new URL("app/page.tsx", root), "utf8"),
    readFile(new URL("app/layout.tsx", root), "utf8"),
    readFile(new URL("package.json", root), "utf8"),
  ]);

  assert.match(page, /<DraftRoom \/>/);
  assert.match(layout, /Fantasy Football/);
  assert.match(layout, /\/og-optimization\.png/);
  assert.doesNotMatch(`${page}${layout}${packageJson}`, /SkeletonPreview|react-loading-skeleton|Starter Project/);
});

test("keeps the complete pick history in a scrollable region", async () => {
  const [draftRoom, styles] = await Promise.all([
    readFile(new URL("app/DraftRoom.tsx", root), "utf8"),
    readFile(new URL("app/globals.css", root), "utf8"),
  ]);

  assert.match(draftRoom, /const recentPicks = \[\.\.\.picks\]\.reverse\(\);/);
  assert.doesNotMatch(draftRoom, /recentPicks = .*\.slice\(/);
  assert.match(draftRoom, /aria-label="Complete draft pick history"/);
  assert.match(styles, /\.recent-list \{[^}]*max-height:[^}]*overflow-y: auto;/s);
});

test("ships focus-team availability curves", async () => {
  const [availability, draftRoom, projections] = await Promise.all([
    readFile(new URL("public/availability.json", root), "utf8").then(JSON.parse),
    readFile(new URL("app/DraftRoom.tsx", root), "utf8"),
    readFile(new URL("public/projections.json", root), "utf8").then(JSON.parse),
  ]);

  assert.equal(availability.model.season, 2026);
  assert.equal(availability.model.teamCount, 12);
  assert.equal(availability.model.rounds, 16);
  assert.equal(availability.model.maxPick, 192);
  assert.equal(availability.model.calibrationMethod, "slot_position_frequency");
  assert.ok(availability.model.calibrationPoolSize > Object.keys(availability.players).length);
  assert.ok(availability.model.totalDrafts > 0);
  assert.ok(Object.keys(availability.players).length > 150);
  assert.ok(Object.values(availability.players).every((player) =>
    Number.isFinite(player.adp) && player.adp > 0 &&
    player.probabilities.length === availability.model.maxPick &&
    player.probabilities.every((probability) => probability >= 0 && probability <= 1) &&
    player.probabilities.every((probability, index) => index === 0 || probability <= player.probabilities[index - 1])
  ));

  const modeledIds = new Set(projections.filter((player) => ["QB", "RB", "WR", "TE"].includes(player.position)).map((player) => player.id));
  [1, 96, 192].forEach((pick) => {
    const expectedSkillPlayersDrafted = Object.entries(availability.players)
      .filter(([playerId]) => modeledIds.has(playerId))
      .reduce((total, [, player]) => total + 1 - player.probabilities[pick - 1], 0);
    assert.ok(expectedSkillPlayersDrafted <= pick - 1 + 0.01, `Availability exceeds the ${pick - 1} slots before pick ${pick}`);
  });

  const rbIds = new Set(projections.filter((player) => player.position === "RB").map((player) => player.id));
  const expectedRbsDraftedBeforeFinalPick = Object.entries(availability.players)
    .filter(([playerId]) => rbIds.has(playerId))
    .reduce((total, [, player]) => total + 1 - player.probabilities[191], 0);
  assert.ok(expectedRbsDraftedBeforeFinalPick >= 62 && expectedRbsDraftedBeforeFinalPick <= 65);
  const defaultTeams = (draftRoom.match(/const DEFAULT_TEAMS = \[(.*?)\];/s)?.[1].match(/"[^"]+"/g) ?? []).map((team) => JSON.parse(team));
  assert.deepEqual(defaultTeams, [
    "Esperanza's Iguana's",
    "CTE? TBD",
    "P and J Pizzas",
    "Alantrees",
    "Hail Victory",
    "BigPugs",
    "Jocks for Rocks",
    "Orca Whales",
    "Flowers for Lamar",
    "The Bravehearts",
    "Fauci Ouchies",
    "Best us",
  ]);
  assert.equal(defaultTeams.length, availability.model.teamCount);
  assert.match(draftRoom, /isUntouchedLegacySetup/);
  assert.match(draftRoom, /hasSameTeamOrder\(parsed\.teams, LEGACY_DEFAULT_TEAMS\)/);
  assert.match(draftRoom, /parsed\.teams\.length === availability\.model\.teamCount/);
  assert.match(draftRoom, /const \[focusTeams, setFocusTeams\] = useState<number\[]>\(\[0\]\)/);
  assert.match(draftRoom, /return current\.length < 2 \? \[\.\.\.current, index\] : current/);
  assert.match(draftRoom, /id="second-focus-team"/);
  assert.match(draftRoom, /setFocusTeams\(\(validFocusTeams\.length \? validFocusTeams : \[0\]\)\.slice\(0, restoredMockMode \? 1 : 2\)\)/);
  assert.match(draftRoom, /if \(enabled\) setFocusTeams\(\(current\) => \[current\[0\] \?\? 0\]\)/);
  assert.match(draftRoom, /Focus team/);
  assert.match(draftRoom, /nextSelectedFocusSlot/);
  assert.match(draftRoom, /focusedTeamSet\.has\(slot\.teamIndex\)/);
  assert.match(draftRoom, /slot\.teamIndex !== analysisFocusTeam/);
  assert.match(draftRoom, /Availability odds follow whichever focused team picks next/);
  assert.match(draftRoom, /nextFocusSlots/);
  assert.match(draftRoom, /availabilityAtFocusPick/);
  assert.match(draftRoom, /futureProbability \/ currentProbability/);
  assert.match(draftRoom, /slot\.overall === currentOverall\) return 1/);
  assert.match(draftRoom, /availabilityMatchesLeague/);
  assert.match(draftRoom, /Availability paused/);
  assert.match(draftRoom, /Next 2 picks/);
  assert.match(draftRoom, /slots\.length === 2/);
  assert.match(draftRoom, /Now picking/);
  assert.match(draftRoom, /focus-tracker/);
  assert.match(draftRoom, /aria-live="polite"/);
  assert.equal((draftRoom.match(/>ADP<\/th>/g) ?? []).length, 2);
  assert.match(draftRoom, /formatAdp\(availabilityData\.players\[recommendation\.player\.id\]\?\.adp\)/);
  assert.match(draftRoom, /formatAdp\(availabilityData\?\.players\[player\.id\]\?\.adp\)/);
});

test("mock draft mode simulates opponents and stops for the focus team", async () => {
  const [{ default: ts }, source, draftRoom] = await Promise.all([
    import("typescript"),
    readFile(new URL("app/mockDraft.ts", root), "utf8"),
    readFile(new URL("app/DraftRoom.tsx", root), "utf8"),
  ]);
  const compiled = ts.transpileModule(source, {
    compilerOptions: { module: ts.ModuleKind.ES2022, target: ts.ScriptTarget.ES2022 },
  }).outputText;
  const mockDraft = await import(`data:text/javascript;base64,${Buffer.from(compiled).toString("base64")}`);
  const players = [
    { id: "rb-1", position: "RB", positionRank: 1, projectedPoints: 260 },
    { id: "wr-1", position: "WR", positionRank: 1, projectedPoints: 250 },
    { id: "qb-1", position: "QB", positionRank: 1, projectedPoints: 240 },
    { id: "te-1", position: "TE", positionRank: 1, projectedPoints: 230 },
    { id: "rb-2", position: "RB", positionRank: 2, projectedPoints: 220 },
    { id: "wr-2", position: "WR", positionRank: 2, projectedPoints: 210 },
    { id: "dst-1", position: "DST", positionRank: 1, projectedPoints: 0 },
    { id: "k-1", position: "K", positionRank: 1, projectedPoints: 0 },
  ];
  const availability = {
    model: { maxPick: 12 },
    players: Object.fromEntries(players.slice(0, 6).map((player, index) => [
      player.id,
      { probabilities: Array.from({ length: 12 }, (_, pickIndex) => pickIndex < index ? 1 : 0) },
    ])),
  };
  const getPickSlot = (index, teamCount) => {
    const roundIndex = Math.floor(index / teamCount);
    const pickIndex = index % teamCount;
    return {
      teamIndex: roundIndex % 2 === 0 ? pickIndex : teamCount - 1 - pickIndex,
      round: roundIndex + 1,
      pickInRound: pickIndex + 1,
      overall: index + 1,
    };
  };
  const input = { players, availability, teamCount: 4, rounds: 3, focusTeam: 2, getPickSlot, canDraft: () => true };
  const opening = mockDraft.simulateToFocusTeam({ ...input, picks: [] });
  assert.equal(opening.length, 2);
  assert.deepEqual(opening.map((pick) => pick.teamIndex), [0, 1]);
  assert.ok(opening.every((pick) => pick.simulated));

  const focusPick = { playerId: "qb-1", ...getPickSlot(2, 4) };
  const nextTurn = mockDraft.simulateToFocusTeam({ ...input, picks: [...opening, focusPick] });
  assert.equal(nextTurn.length, 5);
  assert.equal(getPickSlot(nextTurn.length, 4).teamIndex, input.focusTeam);
  assert.equal(new Set(nextTurn.map((pick) => pick.playerId)).size, nextTurn.length);
  assert.match(draftRoom, /Mock draft mode/);
  assert.match(draftRoom, /Simulating picks/);
  assert.match(draftRoom, /pick\.simulated/);
  assert.match(draftRoom, /Revert to last pick/);
  assert.match(draftRoom, /while \(lastFocusPickIndex >= 0 && picks\[lastFocusPickIndex\]\.simulated\)/);
  assert.match(draftRoom, /current\.slice\(0, lastFocusPickIndex\)/);
});

test("ships a simulation-ranked recommendation board", async () => {
  const [draftRoom, optimization] = await Promise.all([
    readFile(new URL("app/DraftRoom.tsx", root), "utf8"),
    readFile(new URL("app/draftOptimization.ts", root), "utf8"),
  ]);

  assert.match(draftRoom, /Simulated draft outlook/);
  assert.match(draftRoom, /Best complete-roster paths/);
  assert.match(draftRoom, /Pick value/);
  assert.match(draftRoom, /<th className="numeric">Proj\.<\/th>/);
  assert.match(draftRoom, /optimizer-projection/);
  assert.match(draftRoom, /recommendation\.meanScore - bestSimulationValue/);
  assert.match(draftRoom, /Rank, Pick value, Expected roster, P10, and P90 all come from the same concrete simulated boards/);
  assert.match(draftRoom, /<th className="numeric">VORP<\/th>/);
  assert.match(draftRoom, /recommendation\.player\.projectedPoints - replacementPoints\[recommendation\.player\.position as Position\]/);
  assert.match(draftRoom, /Expected roster/);
  assert.match(draftRoom, />P10</);
  assert.match(draftRoom, />P90</);
  assert.match(draftRoom, /Probability each offensive slot is filled/);
  assert.match(draftRoom, /Projected role and roster score/);
  assert.match(draftRoom, /candidateRoleProbabilities\.starter/);
  assert.match(draftRoom, /Expected roster score split/);
  assert.match(draftRoom, /recommendation\.meanStarterScore/);
  assert.match(draftRoom, /recommendation\.meanBenchScore/);
  assert.match(draftRoom, /Players selected in simulated paths/);
  assert.match(draftRoom, /starterSlotProbabilities/);
  assert.match(draftRoom, /futurePickDistributions/);
  assert.match(draftRoom, /optimizer-player-summary-trigger/);
  assert.match(draftRoom, /showPlayerSummary\(recommendation\.player\.id, event\.currentTarget\)/);
  assert.match(draftRoom, /openPlayerDetails\(recommendation\.player\.id, `optimization-player-detail-trigger-/);
  assert.match(draftRoom, /draftPlayer\(recommendation\.player\.id\)/);
  assert.match(draftRoom, /isCurrentFocusTeam/);
  assert.match(optimization, /OPTIMIZATION_CANDIDATES_PER_GROUP = 3/);
  assert.match(optimization, /PASS_CATCHER/);
  assert.match(optimization, /GROUP_MAXIMUMS.*PASS_CATCHER: 8/);
  assert.match(draftRoom, /PASS_CATCHER_REPLACEMENT_RANK = 62/);
  assert.match(draftRoom, /WR\/TE maximum reached/);
  assert.match(optimization, /OPTIMIZATION_SCENARIOS = 500/);
  assert.match(optimization, /fractionalCompletionValue/);
  assert.match(optimization, /sampleOpponentAvailability/);
  assert.match(optimization, /sampledAvailability\[availabilityIndex\]\.unavailableAt <= overall/);
  assert.match(optimization, /b\.meanScore - a\.meanScore/);
  assert.match(optimization, /b\.p10Score - a\.p10Score/);
  assert.match(optimization, /cvar10Score/);
  assert.match(optimization, /QB_BENCH_WEIGHTS = \[0\.20, 0\.05, 0\.01\]/);
});

test("computes deterministic live recommendations from the shipped board", async () => {
  const [{ default: ts }, source, players, availability] = await Promise.all([
    import("typescript"),
    readFile(new URL("app/draftOptimization.ts", root), "utf8"),
    readFile(new URL("public/projections.json", root), "utf8").then(JSON.parse),
    readFile(new URL("public/availability.json", root), "utf8").then(JSON.parse),
  ]);
  const compiled = ts.transpileModule(source, {
    compilerOptions: { module: ts.ModuleKind.ES2022, target: ts.ScriptTarget.ES2022 },
  }).outputText;
  const optimizer = await import(`data:text/javascript;base64,${Buffer.from(compiled).toString("base64")}`);
  const firstPlayer = players.find((player) => ["QB", "RB", "WR", "TE"].includes(player.position));
  const input = {
    players,
    availability,
    draftedPlayerIds: new Set([firstPlayer.id]),
    rosterPlayerIds: [],
    currentOverall: 2,
    currentRound: 1,
    focusTeam: 1,
    teamCount: 12,
    scenarioCount: 24,
  };

  const first = optimizer.optimizeDraftRecommendations(input);
  const second = optimizer.optimizeDraftRecommendations(input);
  assert.equal(first.length, 9);
  assert.deepEqual(first, second);
  assert.deepEqual(first.map((recommendation) => recommendation.rank), Array.from({ length: 9 }, (_, index) => index + 1));
  for (let index = 1; index < first.length; index += 1) {
    assert.ok(first[index - 1].meanScore >= first[index].meanScore);
  }
  first.forEach((recommendation) => {
    assert.equal(recommendation.scenarioCount, input.scenarioCount);
    assert.ok(Number.isFinite(recommendation.fractionalValue));
    assert.ok(Number.isFinite(recommendation.meanScore));
    assert.ok(recommendation.p10Score <= recommendation.p90Score);
    assert.ok(recommendation.cvar10Score <= recommendation.p10Score + 0.001);
    assert.ok(Number.isFinite(recommendation.meanStarterScore));
    assert.ok(Number.isFinite(recommendation.meanBenchScore));
    assert.ok(Math.abs(recommendation.meanStarterScore + recommendation.meanBenchScore - recommendation.meanScore) < 0.0001);
    const roleProbability = Object.values(recommendation.candidateRoleProbabilities)
      .reduce((total, probability) => total + probability, 0);
    assert.ok(Math.abs(roleProbability - 1) < 0.0001);
    assert.ok(recommendation.probabilityTwoViableRbs >= 0 && recommendation.probabilityTwoViableRbs <= 1);
    assert.equal(recommendation.starterSlotProbabilities.length, 7);
    recommendation.starterSlotProbabilities.forEach((slot) => {
      assert.ok(slot.probability >= 0 && slot.probability <= 1);
    });
    assert.ok(recommendation.futurePickDistributions.length > 0);
    recommendation.futurePickDistributions.forEach((distribution) => {
      const displayedProbability = distribution.players.reduce((total, player) => total + player.probability, 0);
      assert.ok(Math.abs(displayedProbability + distribution.otherProbability - 1) < 0.0001);
    });
  });
  assert.ok(first.filter((recommendation) => (
    recommendation.player.position === "WR" || recommendation.player.position === "TE"
  )).length <= 3);

  const syntheticPlayer = (id, position, projectedPoints) => ({
    id,
    name: id,
    nflTeam: "TST",
    position,
    projectedPoints,
  });
  const syntheticRoster = [
    syntheticPlayer("roster-qb", "QB", 300),
    syntheticPlayer("roster-rb-1", "RB", 260),
    syntheticPlayer("roster-rb-2", "RB", 250),
    syntheticPlayer("roster-wr-1", "WR", 240),
    syntheticPlayer("roster-wr-2", "WR", 230),
    syntheticPlayer("roster-te", "TE", 220),
  ];
  const candidateRb = syntheticPlayer("candidate-rb", "RB", 180);
  const targetTe = syntheticPlayer("target-te", "TE", 205);
  const syntheticPlayers = [
    ...syntheticRoster,
    candidateRb,
    targetTe,
    syntheticPlayer("rb-alt-1", "RB", 150),
    syntheticPlayer("rb-alt-2", "RB", 140),
    syntheticPlayer("te-alt-1", "TE", 130),
    syntheticPlayer("te-alt-2", "TE", 120),
    syntheticPlayer("wr-alt-1", "WR", 160),
    syntheticPlayer("wr-alt-2", "WR", 150),
    syntheticPlayer("wr-alt-3", "WR", 140),
    syntheticPlayer("qb-alt-1", "QB", 190),
    syntheticPlayer("qb-alt-2", "QB", 180),
    syntheticPlayer("qb-alt-3", "QB", 170),
  ];
  const survival = (probabilityAt165 = 1) => Array.from({ length: 168 }, (_, index) => {
    const overall = index + 1;
    if (overall <= 148) return 1;
    if (overall >= 165) return probabilityAt165;
    return 1 - ((overall - 148) / 17) * (1 - probabilityAt165);
  });
  const syntheticAvailability = {
    players: Object.fromEntries(syntheticPlayers.map((player) => [
      player.id,
      { probabilities: survival(player.id === targetTe.id ? 0.25 : 1) },
    ])),
  };
  const calibrated = optimizer.optimizeDraftRecommendations({
    players: syntheticPlayers,
    availability: syntheticAvailability,
    draftedPlayerIds: new Set(syntheticRoster.map((player) => player.id)),
    rosterPlayerIds: syntheticRoster.map((player) => player.id),
    currentOverall: 148,
    currentRound: 13,
    focusTeam: 3,
    teamCount: 12,
    scenarioCount: 400,
  });
  const rbPath = calibrated.find((recommendation) => recommendation.player.id === candidateRb.id);
  assert.ok(rbPath);
  const nextPick = rbPath.futurePickDistributions.find((distribution) => distribution.overall === 165);
  assert.ok(nextPick);
  const empiricalTargetProbability = nextPick.players
    .find((entry) => entry.player.id === targetTe.id)?.probability ?? 0;
  assert.ok(Math.abs(empiricalTargetProbability - 0.25) < 0.07);

  const sharedDepthRoster = [
    syntheticPlayer("depth-qb", "QB", 300),
    syntheticPlayer("depth-rb-1", "RB", 270),
    syntheticPlayer("depth-rb-2", "RB", 260),
    syntheticPlayer("depth-rb-flex", "RB", 250),
    syntheticPlayer("depth-wr-1", "WR", 240),
    syntheticPlayer("depth-wr-2", "WR", 230),
    syntheticPlayer("depth-te-1", "TE", 220),
    syntheticPlayer("depth-wr-bench", "WR", 200),
  ];
  const equalWr = syntheticPlayer("equal-wr", "WR", 190);
  const equalTe = syntheticPlayer("equal-te", "TE", 190);
  const sharedDepthPlayers = [
    ...sharedDepthRoster,
    equalWr,
    equalTe,
    syntheticPlayer("depth-pass-alt", "WR", 180),
    syntheticPlayer("depth-rb-alt-1", "RB", 180),
    syntheticPlayer("depth-rb-alt-2", "RB", 170),
    syntheticPlayer("depth-rb-alt-3", "RB", 160),
    syntheticPlayer("depth-qb-alt-1", "QB", 190),
    syntheticPlayer("depth-qb-alt-2", "QB", 180),
    syntheticPlayer("depth-qb-alt-3", "QB", 170),
  ];
  const sharedDepthRecommendations = optimizer.optimizeDraftRecommendations({
    players: sharedDepthPlayers,
    availability: {
      players: Object.fromEntries(sharedDepthPlayers.map((player) => [
        player.id,
        { probabilities: Array.from({ length: 168 }, () => 1) },
      ])),
    },
    draftedPlayerIds: new Set(sharedDepthRoster.map((player) => player.id)),
    rosterPlayerIds: sharedDepthRoster.map((player) => player.id),
    currentOverall: 168,
    currentRound: 14,
    focusTeam: 0,
    teamCount: 12,
    scenarioCount: 4,
  });
  const equalWrRecommendation = sharedDepthRecommendations.find((entry) => entry.player.id === equalWr.id);
  const equalTeRecommendation = sharedDepthRecommendations.find((entry) => entry.player.id === equalTe.id);
  assert.ok(equalWrRecommendation && equalTeRecommendation);
  assert.equal(equalWrRecommendation.meanScore, equalTeRecommendation.meanScore);
  assert.equal(equalWrRecommendation.candidateRoleProbabilities.bench, 1);
  assert.equal(equalTeRecommendation.candidateRoleProbabilities.bench, 1);
});
