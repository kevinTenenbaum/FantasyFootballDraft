export type OptimizationPosition = "QB" | "RB" | "WR" | "TE";
type OptimizationGroup = "QB" | "RB" | "PASS_CATCHER";

export type OptimizationPlayer = {
  id: string;
  name: string;
  nflTeam: string;
  position: string;
  projectedPoints: number;
};

export type OptimizationAvailability = {
  players: Record<string, { probabilities: number[] }>;
};

export type DraftRecommendation = {
  rank: number;
  player: OptimizationPlayer;
  fractionalValue: number;
  meanScore: number;
  p10Score: number;
  p90Score: number;
  cvar10Score: number;
  meanStarterScore: number;
  meanBenchScore: number;
  candidateRoleProbabilities: Record<CandidateRole, number>;
  probabilityTwoViableRbs: number;
  starterSlotProbabilities: Array<{
    key: string;
    label: string;
    probability: number;
  }>;
  futurePickDistributions: Array<{
    overall: number;
    round: number;
    players: Array<{
      player: OptimizationPlayer;
      probability: number;
    }>;
    otherProbability: number;
  }>;
  scenarioCount: number;
};

export type OptimizationInput = {
  players: OptimizationPlayer[];
  availability: OptimizationAvailability;
  draftedPlayerIds: Set<string>;
  rosterPlayerIds: string[];
  currentOverall: number;
  currentRound: number;
  focusTeam: number;
  teamCount: number;
  scenarioCount?: number;
};

type RosterScore = {
  total: number;
  starter: number;
  bench: number;
};

type CandidateRole = "starter" | "flex" | "bench" | "outside";

type ConcreteRosterScore = RosterScore & {
  roles: Map<string, CandidateRole>;
};

type PlayerPool = Record<OptimizationGroup, OptimizationPlayer[]>;

const OFFENSIVE_ROUNDS = 14;
export const OPTIMIZATION_CANDIDATES_PER_GROUP = 3;
export const OPTIMIZATION_SCENARIOS = 500;
const MODELED_POSITIONS: OptimizationPosition[] = ["QB", "RB", "WR", "TE"];
const OPTIMIZATION_GROUPS: OptimizationGroup[] = ["QB", "RB", "PASS_CATCHER"];
const GROUP_MAXIMUMS: Record<OptimizationGroup, number> = { QB: 4, RB: 8, PASS_CATCHER: 8 };
const BENCH_WEIGHTS = [0.45, 0.25, 0.10];
const QB_BENCH_WEIGHTS = [0.20, 0.05, 0.01];
const VIABLE_RB_POINTS = 150;
const STARTER_SLOTS = [
  { key: "QB1", label: "QB" },
  { key: "RB1", label: "RB1" },
  { key: "RB2", label: "RB2" },
  { key: "WT1", label: "WR/TE1" },
  { key: "WT2", label: "WR/TE2" },
  { key: "WT3", label: "WR/TE3" },
  { key: "FLEX", label: "FLEX" },
] as const;

function isModeledPosition(position: string): position is OptimizationPosition {
  return MODELED_POSITIONS.includes(position as OptimizationPosition);
}

function optimizationGroup(position: OptimizationPosition): OptimizationGroup {
  return position === "WR" || position === "TE" ? "PASS_CATCHER" : position;
}

function weightAtDepth(group: OptimizationGroup, depth: number) {
  const weights = group === "QB" ? QB_BENCH_WEIGHTS : BENCH_WEIGHTS;
  return weights[Math.min(depth, weights.length - 1)];
}

function scoreRoster(roster: OptimizationPlayer[]): ConcreteRosterScore {
  const byGroup = Object.fromEntries(OPTIMIZATION_GROUPS.map((group) => [
    group,
    roster
      .filter((player) => isModeledPosition(player.position) && optimizationGroup(player.position) === group)
      .sort((a, b) => b.projectedPoints - a.projectedPoints),
  ])) as Record<OptimizationGroup, OptimizationPlayer[]>;

  const baseUsed: Record<OptimizationGroup, number> = {
    QB: Math.min(1, byGroup.QB.length),
    RB: Math.min(2, byGroup.RB.length),
    PASS_CATCHER: Math.min(3, byGroup.PASS_CATCHER.length),
  };
  const flexPlayer = [...byGroup.RB.slice(baseUsed.RB), ...byGroup.PASS_CATCHER.slice(baseUsed.PASS_CATCHER)]
    .sort((a, b) => b.projectedPoints - a.projectedPoints)[0];
  const flexGroup = flexPlayer && isModeledPosition(flexPlayer.position)
    ? optimizationGroup(flexPlayer.position)
    : null;
  const used = { ...baseUsed };
  if (flexGroup) used[flexGroup] += 1;
  const starter = OPTIMIZATION_GROUPS.reduce((total, group) => (
    total + byGroup[group].slice(0, used[group])
      .reduce((subtotal, player) => subtotal + player.projectedPoints, 0)
  ), 0);
  const benchOptions = OPTIMIZATION_GROUPS.flatMap((group) => (
    byGroup[group].slice(used[group]).map((player, depth) => ({
      player,
      value: player.projectedPoints * weightAtDepth(group, depth),
    }))
  )).sort((a, b) => b.value - a.value).slice(0, 7);
  const bench = benchOptions.reduce((total, option) => total + option.value, 0);
  const roles = new Map<string, CandidateRole>(roster.map((player) => [player.id, "outside"]));
  OPTIMIZATION_GROUPS.forEach((group) => {
    byGroup[group].slice(0, baseUsed[group]).forEach((player) => roles.set(player.id, "starter"));
  });
  if (flexPlayer) roles.set(flexPlayer.id, "flex");
  benchOptions.forEach(({ player }) => roles.set(player.id, "bench"));
  const best: ConcreteRosterScore = { total: starter + bench, starter, bench, roles };
  return best;
}

type FractionalRosterEntry = {
  player: OptimizationPlayer;
  share: number;
};

function takeFractionalStarters(entries: FractionalRosterEntry[], capacity: number) {
  const remaining = entries
    .filter((entry) => entry.share > 1e-8)
    .map((entry) => ({ ...entry }))
    .sort((a, b) => b.player.projectedPoints - a.player.projectedPoints);
  let value = 0;
  let space = capacity;
  for (const entry of remaining) {
    if (space <= 1e-8) break;
    const used = Math.min(space, entry.share);
    entry.share -= used;
    space -= used;
    value += used * entry.player.projectedPoints;
  }
  return { value, remaining: remaining.filter((entry) => entry.share > 1e-8) };
}

function scoreFractionalRoster(
  shares: Map<string, number>,
  playerById: Map<string, OptimizationPlayer>,
): RosterScore {
  const byGroup = Object.fromEntries(OPTIMIZATION_GROUPS.map((group) => [
    group,
    [...shares.entries()]
      .map(([playerId, share]) => ({ player: playerById.get(playerId), share }))
      .filter((entry): entry is FractionalRosterEntry => (
        entry.player !== undefined &&
        isModeledPosition(entry.player.position) &&
        optimizationGroup(entry.player.position) === group &&
        entry.share > 1e-8
      )),
  ])) as Record<OptimizationGroup, FractionalRosterEntry[]>;

  const qb = takeFractionalStarters(byGroup.QB, 1);
  const rb = takeFractionalStarters(byGroup.RB, 2);
  const passCatchers = takeFractionalStarters(byGroup.PASS_CATCHER, 3);
  const flex = takeFractionalStarters([...rb.remaining, ...passCatchers.remaining], 1);
  const flexRemaining = new Map(flex.remaining.map((entry) => [entry.player.id, entry.share]));
  const benchByGroup = {
    QB: qb.remaining,
    RB: rb.remaining.map((entry) => ({ ...entry, share: flexRemaining.get(entry.player.id) ?? 0 })),
    PASS_CATCHER: passCatchers.remaining
      .map((entry) => ({ ...entry, share: flexRemaining.get(entry.player.id) ?? 0 })),
  } satisfies Record<OptimizationGroup, FractionalRosterEntry[]>;

  const benchFragments = OPTIMIZATION_GROUPS.flatMap((group) => {
    const entries = benchByGroup[group]
      .filter((entry) => entry.share > 1e-8)
      .sort((a, b) => b.player.projectedPoints - a.player.projectedPoints);
    const fragments: Array<{ share: number; pointsPerShare: number }> = [];
    let entryIndex = 0;
    for (let depth = 0; depth < GROUP_MAXIMUMS[group]; depth += 1) {
      let roleCapacity = 1;
      while (roleCapacity > 1e-8 && entryIndex < entries.length) {
        const entry = entries[entryIndex];
        const used = Math.min(roleCapacity, entry.share);
        fragments.push({
          share: used,
          pointsPerShare: entry.player.projectedPoints * weightAtDepth(group, depth),
        });
        entry.share -= used;
        roleCapacity -= used;
        if (entry.share <= 1e-8) entryIndex += 1;
      }
    }
    return fragments;
  }).sort((a, b) => b.pointsPerShare - a.pointsPerShare);

  let benchCapacity = 7;
  let bench = 0;
  for (const fragment of benchFragments) {
    if (benchCapacity <= 1e-8) break;
    const used = Math.min(benchCapacity, fragment.share);
    bench += used * fragment.pointsPerShare;
    benchCapacity -= used;
  }
  const starter = qb.value + rb.value + passCatchers.value + flex.value;
  return { total: starter + bench, starter, bench };
}

function getPickTeam(overall: number, teamCount: number) {
  const index = overall - 1;
  const roundIndex = Math.floor(index / teamCount);
  const pickIndex = index % teamCount;
  return roundIndex % 2 === 0 ? pickIndex : teamCount - 1 - pickIndex;
}

function futureOwnPicks(currentOverall: number, focusTeam: number, teamCount: number) {
  const picks: number[] = [];
  const finalOffensivePick = OFFENSIVE_ROUNDS * teamCount;
  for (let overall = currentOverall + 1; overall <= finalOffensivePick; overall += 1) {
    if (getPickTeam(overall, teamCount) === focusTeam) picks.push(overall);
  }
  return picks;
}

function groupCounts(roster: OptimizationPlayer[]) {
  return Object.fromEntries(OPTIMIZATION_GROUPS.map((group) => [
    group,
    roster.filter((player) => (
      isModeledPosition(player.position) && optimizationGroup(player.position) === group
    )).length,
  ])) as Record<OptimizationGroup, number>;
}

function preparePlayerPool(players: OptimizationPlayer[]): PlayerPool {
  return Object.fromEntries(OPTIMIZATION_GROUPS.map((group) => [
    group,
    players
      .filter((player) => (
        isModeledPosition(player.position) && optimizationGroup(player.position) === group
      ))
      .sort((a, b) => b.projectedPoints - a.projectedPoints || a.name.localeCompare(b.name)),
  ])) as PlayerPool;
}

function withProjectionTierFallbacks(
  players: OptimizationPlayer[],
  availability: OptimizationAvailability,
): OptimizationAvailability {
  const resolvedPlayers = { ...availability.players };
  const matchedByPosition = Object.fromEntries(MODELED_POSITIONS.map((position) => [
    position,
    players.filter((player) => (
      player.position === position && Boolean(availability.players[player.id]?.probabilities)
    )),
  ])) as Record<OptimizationPosition, OptimizationPlayer[]>;

  players.forEach((player) => {
    if (!isModeledPosition(player.position) || resolvedPlayers[player.id]) return;
    const proxy = matchedByPosition[player.position]
      .map((matchedPlayer) => ({
        player: matchedPlayer,
        distance: Math.abs(matchedPlayer.projectedPoints - player.projectedPoints),
      }))
      .sort((a, b) => a.distance - b.distance || b.player.projectedPoints - a.player.projectedPoints)[0]?.player;
    if (proxy) resolvedPlayers[player.id] = availability.players[proxy.id];
  });
  return { ...availability, players: resolvedPlayers };
}

function topPositionCandidates(
  playerPool: PlayerPool,
  draftedPlayerIds: Set<string>,
  roster: OptimizationPlayer[],
  limit = OPTIMIZATION_CANDIDATES_PER_GROUP,
) {
  const counts = groupCounts(roster);
  return OPTIMIZATION_GROUPS.flatMap((group) => {
    if (counts[group] >= GROUP_MAXIMUMS[group]) return [];
    return playerPool[group]
      .filter((player) => !draftedPlayerIds.has(player.id))
      .slice(0, limit);
  });
}

function conditionalAvailability(
  availability: OptimizationAvailability,
  playerId: string,
  currentOverall: number,
  futureOverall: number,
) {
  if (futureOverall <= currentOverall) return 1;
  const probabilities = availability.players[playerId]?.probabilities;
  if (!probabilities) return 1;
  const current = probabilities[currentOverall - 1];
  const future = probabilities[futureOverall - 1];
  if (current === undefined || future === undefined || current <= 0) return 0;
  return Math.max(0, Math.min(1, future / current));
}

function hashString(value: string) {
  let hash = 2166136261;
  for (let index = 0; index < value.length; index += 1) {
    hash ^= value.charCodeAt(index);
    hash = Math.imul(hash, 16777619);
  }
  return hash >>> 0;
}

function mulberry32(seed: number) {
  let state = seed >>> 0;
  return () => {
    state += 0x6D2B79F5;
    let value = state;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  };
}

type SampledPlayerAvailability = {
  playerId: string;
  unavailableAt: number;
};

function sampleOpponentAvailability(
  players: OptimizationPlayer[],
  availability: OptimizationAvailability,
  draftedPlayerIds: Set<string>,
  currentOverall: number,
  teamCount: number,
  seed: number,
) {
  const random = mulberry32(seed);
  const finalPick = OFFENSIVE_ROUNDS * teamCount;
  return players
    .filter((player) => isModeledPosition(player.position) && !draftedPlayerIds.has(player.id))
    .map((player) => {
      const threshold = random();
      let sampledPick = finalPick + 1;
      let lower = currentOverall + 1;
      let upper = finalPick;
      while (lower <= upper) {
        const pick = Math.floor((lower + upper) / 2);
        if (conditionalAvailability(availability, player.id, currentOverall, pick) < threshold) {
          sampledPick = pick;
          upper = pick - 1;
        } else {
          lower = pick + 1;
        }
      }
      return {
        playerId: player.id,
        unavailableAt: sampledPick,
      };
    })
    .sort((a, b) => a.unavailableAt - b.unavailableAt || a.playerId.localeCompare(b.playerId));
}

function chooseGreedyPlayer(
  playerPool: PlayerPool,
  availability: OptimizationAvailability,
  draftedPlayerIds: Set<string>,
  roster: OptimizationPlayer[],
  currentOverall: number,
  nextOwnPick: number | undefined,
) {
  const candidates = topPositionCandidates(playerPool, draftedPlayerIds, roster);
  const currentScore = scoreRoster(roster).total;
  const counts = groupCounts(roster);
  return candidates
    .map((player) => {
      const marginal = scoreRoster([...roster, player]).total - currentScore;
      const nextAvailability = nextOwnPick
        ? conditionalAvailability(availability, player.id, currentOverall, nextOwnPick)
        : 0;
      const scarcity = player.projectedPoints * (1 - nextAvailability) * 0.08;
      const requiredStarterBonus =
        player.position === "RB" && counts.RB < 2 ? player.projectedPoints * 0.10 :
        player.position === "QB" && counts.QB < 1 ? player.projectedPoints * 0.03 : 0;
      return { player, score: marginal + scarcity + requiredStarterBonus };
    })
    .sort((a, b) => b.score - a.score || b.player.projectedPoints - a.player.projectedPoints)[0]?.player;
}

function fractionalCompletionValue(
  input: OptimizationInput,
  playerPool: PlayerPool,
  candidate: OptimizationPlayer,
  roster: OptimizationPlayer[],
  futurePicks: number[],
) {
  const playerById = new Map(input.players.map((player) => [player.id, player]));
  const shares = new Map<string, number>(roster.map((player) => [player.id, 1]));
  shares.set(candidate.id, 1);
  const tailShares = new Map<string, number>();

  [...futurePicks].reverse().forEach((pick) => {
    const likelyCandidates = OPTIMIZATION_GROUPS.flatMap((group) => {
      const eligible = playerPool[group].filter((player) => (
        !input.draftedPlayerIds.has(player.id) &&
        player.id !== candidate.id &&
        conditionalAvailability(input.availability, player.id, input.currentOverall, pick) >= 0.02
      ));
      const likely = eligible.slice(0, OPTIMIZATION_CANDIDATES_PER_GROUP);
      const fallback = playerPool[group]
        .filter((player) => !input.draftedPlayerIds.has(player.id) && player.id !== candidate.id)
        .sort((a, b) => {
          const availabilityDifference = conditionalAvailability(
            input.availability,
            b.id,
            input.currentOverall,
            pick,
          ) - conditionalAvailability(input.availability, a.id, input.currentOverall, pick);
          return availabilityDifference || b.projectedPoints - a.projectedPoints;
        })[0];
      return fallback ? [...new Map([...likely, fallback].map((player) => [player.id, player])).values()] : likely;
    });
    const allFallbacks = OPTIMIZATION_GROUPS.flatMap((group) => playerPool[group]);
    const candidates = [...new Map(likelyCandidates.map((player) => [player.id, player])).values()];
    let pickShareRemaining = 1;

    while (pickShareRemaining > 1e-8) {
      const groupTotals = Object.fromEntries(OPTIMIZATION_GROUPS.map((group) => [
        group,
        [...shares.entries()].reduce((total, [playerId, share]) => {
          const player = playerById.get(playerId);
          return player && isModeledPosition(player.position) && optimizationGroup(player.position) === group
            ? total + share
            : total;
        }, 0),
      ])) as Record<OptimizationGroup, number>;
      const currentValue = scoreFractionalRoster(shares, playerById).total;
      const buildOptions = (playerCandidates: OptimizationPlayer[]) => playerCandidates.flatMap((player) => {
        if (!isModeledPosition(player.position) || input.draftedPlayerIds.has(player.id)) return [];
        const survival = conditionalAvailability(input.availability, player.id, input.currentOverall, pick);
        const tailCapacity = Math.max(0, survival - (tailShares.get(player.id) ?? 0));
        const playerCapacity = Math.max(0, 1 - (shares.get(player.id) ?? 0));
        const group = optimizationGroup(player.position);
        const groupCapacity = Math.max(0, GROUP_MAXIMUMS[group] - groupTotals[group]);
        const share = Math.min(pickShareRemaining, tailCapacity, playerCapacity, groupCapacity);
        if (share <= 1e-8) return [];
        const trialShares = new Map(shares);
        trialShares.set(player.id, (trialShares.get(player.id) ?? 0) + share);
        const marginalPerShare = (
          scoreFractionalRoster(trialShares, playerById).total - currentValue
        ) / share;
        return [{ player, share, marginalPerShare }];
      }).sort((a, b) => (
        b.marginalPerShare - a.marginalPerShare ||
        b.player.projectedPoints - a.player.projectedPoints
      ));
      let options = buildOptions(candidates);
      if (!options.length) options = buildOptions(allFallbacks);
      const best = options[0];
      if (!best) break;
      shares.set(best.player.id, (shares.get(best.player.id) ?? 0) + best.share);
      tailShares.set(best.player.id, (tailShares.get(best.player.id) ?? 0) + best.share);
      pickShareRemaining -= best.share;
    }
  });

  return scoreFractionalRoster(shares, playerById).total;
}

function simulateCandidate(
  input: OptimizationInput,
  playerPool: PlayerPool,
  candidate: OptimizationPlayer,
  roster: OptimizationPlayer[],
  futurePicks: number[],
  sampledAvailability: SampledPlayerAvailability[],
) {
  const simulatedRoster = [...roster, candidate];
  const futureSelections: Array<{ overall: number; player: OptimizationPlayer }> = [];
  const drafted = new Set([...input.draftedPlayerIds, candidate.id]);
  const ownPickSet = new Set(futurePicks);
  let futurePickIndex = 0;
  let availabilityIndex = 0;
  const lastPick = futurePicks.at(-1) ?? input.currentOverall;

  for (let overall = input.currentOverall + 1; overall <= lastPick; overall += 1) {
    while (
      availabilityIndex < sampledAvailability.length &&
      sampledAvailability[availabilityIndex].unavailableAt <= overall
    ) {
      drafted.add(sampledAvailability[availabilityIndex].playerId);
      availabilityIndex += 1;
    }
    if (ownPickSet.has(overall)) {
      const nextOwnPick = futurePicks[futurePickIndex + 1];
      const selected = chooseGreedyPlayer(
        playerPool,
        input.availability,
        drafted,
        simulatedRoster,
        overall,
        nextOwnPick,
      );
      if (selected) {
        simulatedRoster.push(selected);
        drafted.add(selected.id);
        futureSelections.push({ overall, player: selected });
      }
      futurePickIndex += 1;
    }
  }

  const rbCount = simulatedRoster.filter(
    (player) => player.position === "RB" && player.projectedPoints >= VIABLE_RB_POINTS,
  ).length;
  const quarterbackCount = simulatedRoster.filter((player) => player.position === "QB").length;
  const runningBackCount = simulatedRoster.filter((player) => player.position === "RB").length;
  const receiverTightEndCount = simulatedRoster.filter(
    (player) => player.position === "WR" || player.position === "TE",
  ).length;
  const flexDepth = Math.max(0, runningBackCount - 2) + Math.max(0, receiverTightEndCount - 3);
  const starterSlots: Record<(typeof STARTER_SLOTS)[number]["key"], boolean> = {
    QB1: quarterbackCount >= 1,
    RB1: runningBackCount >= 1,
    RB2: runningBackCount >= 2,
    WT1: receiverTightEndCount >= 1,
    WT2: receiverTightEndCount >= 2,
    WT3: receiverTightEndCount >= 3,
    FLEX: flexDepth >= 1,
  };
  const rosterScore = scoreRoster(simulatedRoster);
  return {
    score: rosterScore.total,
    starterScore: rosterScore.starter,
    benchScore: rosterScore.bench,
    candidateRole: rosterScore.roles.get(candidate.id) ?? "outside",
    twoViableRbs: rbCount >= 2,
    starterSlots,
    futureSelections,
  };
}

function quantile(values: number[], probability: number) {
  if (!values.length) return 0;
  const ordered = [...values].sort((a, b) => a - b);
  const index = (ordered.length - 1) * probability;
  const lower = Math.floor(index);
  const upper = Math.ceil(index);
  if (lower === upper) return ordered[lower];
  return ordered[lower] + (ordered[upper] - ordered[lower]) * (index - lower);
}

export function optimizeDraftRecommendations(input: OptimizationInput): DraftRecommendation[] {
  if (input.currentRound > OFFENSIVE_ROUNDS) return [];
  const scenarioCount = input.scenarioCount ?? OPTIMIZATION_SCENARIOS;
  const resolvedInput = {
    ...input,
    availability: withProjectionTierFallbacks(input.players, input.availability),
  };
  const playerPool = preparePlayerPool(resolvedInput.players);
  const playerById = new Map(resolvedInput.players.map((player) => [player.id, player]));
  const roster = resolvedInput.rosterPlayerIds
    .map((playerId) => playerById.get(playerId))
    .filter((player): player is OptimizationPlayer => (
      player !== undefined && isModeledPosition(player.position)
    ));
  const candidates = topPositionCandidates(playerPool, resolvedInput.draftedPlayerIds, roster);
  const futurePicks = futureOwnPicks(resolvedInput.currentOverall, resolvedInput.focusTeam, resolvedInput.teamCount);
  const stateKey = `${resolvedInput.currentOverall}:${[...resolvedInput.draftedPlayerIds].sort().join(",")}`;
  const sampledAvailabilityScenarios = Array.from({ length: scenarioCount }, (_, scenario) => {
    return sampleOpponentAvailability(
      resolvedInput.players,
      resolvedInput.availability,
      resolvedInput.draftedPlayerIds,
      resolvedInput.currentOverall,
      resolvedInput.teamCount,
      hashString(`${stateKey}:${scenario}`),
    );
  });

  return candidates.map((candidate) => {
    const outcomes = sampledAvailabilityScenarios.map((sampledAvailability) => {
      return simulateCandidate(
        resolvedInput,
        playerPool,
        candidate,
        roster,
        futurePicks,
        sampledAvailability,
      );
    });
    const scores = outcomes.map((outcome) => outcome.score).sort((a, b) => a - b);
    const tailCount = Math.max(1, Math.ceil(scores.length * 0.10));
    const starterSlotProbabilities = STARTER_SLOTS.map((slot) => ({
      ...slot,
      probability: outcomes.filter((outcome) => outcome.starterSlots[slot.key]).length / outcomes.length,
    }));
    const candidateRoleProbabilities: Record<CandidateRole, number> = {
      starter: outcomes.filter((outcome) => outcome.candidateRole === "starter").length / outcomes.length,
      flex: outcomes.filter((outcome) => outcome.candidateRole === "flex").length / outcomes.length,
      bench: outcomes.filter((outcome) => outcome.candidateRole === "bench").length / outcomes.length,
      outside: outcomes.filter((outcome) => outcome.candidateRole === "outside").length / outcomes.length,
    };
    const futurePickDistributions = futurePicks.map((overall) => {
      const counts = new Map<string, { player: OptimizationPlayer; count: number }>();
      outcomes.forEach((outcome) => {
        const selection = outcome.futureSelections.find((pick) => pick.overall === overall);
        if (!selection) return;
        const current = counts.get(selection.player.id);
        counts.set(selection.player.id, {
          player: selection.player,
          count: (current?.count ?? 0) + 1,
        });
      });
      const players = [...counts.values()]
        .sort((a, b) => b.count - a.count || b.player.projectedPoints - a.player.projectedPoints)
        .slice(0, 5)
        .map(({ player, count }) => ({ player, probability: count / outcomes.length }));
      return {
        overall,
        round: Math.floor((overall - 1) / resolvedInput.teamCount) + 1,
        players,
        otherProbability: Math.max(0, 1 - players.reduce((total, entry) => total + entry.probability, 0)),
      };
    });
    return {
      rank: 0,
      player: candidate,
      fractionalValue: fractionalCompletionValue(resolvedInput, playerPool, candidate, roster, futurePicks),
      meanScore: scores.reduce((total, score) => total + score, 0) / scores.length,
      p10Score: quantile(scores, 0.10),
      p90Score: quantile(scores, 0.90),
      cvar10Score: scores.slice(0, tailCount).reduce((total, score) => total + score, 0) / tailCount,
      meanStarterScore: outcomes.reduce((total, outcome) => total + outcome.starterScore, 0) / outcomes.length,
      meanBenchScore: outcomes.reduce((total, outcome) => total + outcome.benchScore, 0) / outcomes.length,
      candidateRoleProbabilities,
      probabilityTwoViableRbs: outcomes.filter((outcome) => outcome.twoViableRbs).length / outcomes.length,
      starterSlotProbabilities,
      futurePickDistributions,
      scenarioCount: outcomes.length,
    };
  }).sort((a, b) => (
    b.meanScore - a.meanScore ||
    b.p10Score - a.p10Score ||
    b.p90Score - a.p90Score ||
    b.player.projectedPoints - a.player.projectedPoints
  ))
    .map((recommendation, index) => ({ ...recommendation, rank: index + 1 }));
}
