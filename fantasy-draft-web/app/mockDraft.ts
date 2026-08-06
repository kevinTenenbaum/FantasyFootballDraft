export type MockDraftPosition = "QB" | "RB" | "WR" | "TE" | "DST" | "K";

export type MockDraftPlayer = {
  id: string;
  position: MockDraftPosition;
  positionRank: number;
  projectedPoints: number;
};

export type MockDraftPick = {
  playerId: string;
  teamIndex: number;
  overall: number;
  round: number;
  pickInRound: number;
  simulated?: boolean;
};

type MockDraftAvailability = {
  model: { maxPick: number };
  players: Record<string, { probabilities: number[] }>;
};

type PickSlot = Omit<MockDraftPick, "playerId" | "simulated">;

export function estimateMedianDraftPick(probabilities: number[] | undefined, fallback: number) {
  if (!probabilities?.length) return fallback;
  const medianIndex = probabilities.findIndex((probability) => probability <= 0.5);
  return medianIndex < 0 ? fallback : medianIndex + 1;
}

export function simulateToFocusTeam(input: {
  picks: MockDraftPick[];
  players: MockDraftPlayer[];
  availability: MockDraftAvailability;
  teamCount: number;
  rounds: number;
  focusTeam: number;
  getPickSlot: (index: number, teamCount: number) => PickSlot;
  canDraft: (player: MockDraftPlayer, candidate: MockDraftPick, picks: MockDraftPick[]) => boolean;
}) {
  const simulatedPicks = [...input.picks];
  const draftedIds = new Set(simulatedPicks.map((pick) => pick.playerId));
  const maximumPicks = input.teamCount * input.rounds;

  while (simulatedPicks.length < maximumPicks) {
    const slot = input.getPickSlot(simulatedPicks.length, input.teamCount);
    if (slot.teamIndex === input.focusTeam) break;

    const teamPicks = simulatedPicks.filter((pick) => pick.teamIndex === slot.teamIndex);
    const teamPositions = new Set(teamPicks.map((pick) => (
      input.players.find((player) => player.id === pick.playerId)?.position
    )));
    const requiredPosition = slot.round === input.rounds - 1 && !teamPositions.has("DST")
      ? "DST"
      : slot.round === input.rounds && !teamPositions.has("K")
        ? "K"
        : null;
    const undrafted = input.players.filter((player) => !draftedIds.has(player.id));
    const offensive = undrafted.filter((player) => player.position !== "DST" && player.position !== "K");
    const candidatePool = requiredPosition
      ? undrafted.filter((player) => player.position === requiredPosition)
      : slot.round <= input.rounds - 2 && offensive.length
        ? offensive
        : undrafted;
    const fallbackPick = input.availability.model.maxPick + 1;
    const candidates = candidatePool.sort((a, b) => {
      const aMedian = estimateMedianDraftPick(input.availability.players[a.id]?.probabilities, fallbackPick);
      const bMedian = estimateMedianDraftPick(input.availability.players[b.id]?.probabilities, fallbackPick);
      return aMedian - bMedian || b.projectedPoints - a.projectedPoints || a.positionRank - b.positionRank;
    });

    const player = candidates.find((candidate) => {
      const pick: MockDraftPick = { playerId: candidate.id, ...slot, simulated: true };
      return input.canDraft(candidate, pick, simulatedPicks);
    });
    if (!player) break;

    const pick: MockDraftPick = { playerId: player.id, ...slot, simulated: true };
    simulatedPicks.push(pick);
    draftedIds.add(player.id);
  }

  return simulatedPicks;
}
