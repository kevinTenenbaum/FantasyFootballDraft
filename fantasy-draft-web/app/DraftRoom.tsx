"use client";

import { useCallback, useEffect, useMemo, useRef, useState } from "react";

type Position = "QB" | "RB" | "WR" | "TE" | "DST" | "K";

type Player = {
  id: string;
  name: string;
  nflTeam: string;
  position: Position;
  positionRank: number;
  depthRole: string;
  projectedPoints: number;
  projectedP10: number;
  projectedP50: number;
  projectedP90: number;
  priorPoints: number | null;
  availability: number | null;
  rookie: boolean;
};

type Pick = {
  playerId: string;
  teamIndex: number;
  overall: number;
  round: number;
  pickInRound: number;
};

type PlayerSummary = {
  bullets: string[];
  updated: string;
  sourceLabel: string;
};

type SummaryCard = {
  playerId: string;
  left: number;
  top: number;
  placement: "above" | "below";
};

type HistoryStatLine = {
  completions: number;
  attempts: number;
  passingYards: number;
  passingTds: number;
  interceptions: number;
  carries: number;
  rushingYards: number;
  rushingTds: number;
  targets: number;
  receptions: number;
  receivingYards: number;
  receivingTds: number;
  fantasyPoints: number;
};

type SeasonHistory = HistoryStatLine & {
  season: number;
  team: string;
  games: number;
  pointsPerGame: number;
};

type GameHistory = HistoryStatLine & {
  week: number;
  team: string;
  opponent: string;
};

type PlayerHistoryData = {
  meta: {
    seasons: number[];
    gameLogSeason: number;
    seasonType: string;
    scoring: string;
  };
  players: Record<string, { seasons: SeasonHistory[]; gameLog: GameHistory[] }>;
};

type ModelContribution = {
  key: string;
  label: string;
  points: number;
  input: string;
};

type ModelInterpretabilityData = {
  meta: {
    model: string;
    season: number;
    note: string;
  };
  players: Record<string, {
    position: Position;
    rawPrediction: number;
    projectedPoints: number;
    contributions: ModelContribution[];
  }>;
};

type StatColumn = {
  label: string;
  value: (line: HistoryStatLine) => string;
};

type SavedDraft = {
  phase: "setup" | "draft";
  teams: string[];
  rounds: number;
  picks: Pick[];
  rosterTeam: number;
  focusTeam: number;
};

type AvailabilityData = {
  model: {
    season: number;
    scoringFormat: string;
    teamCount: number;
    rounds: number;
    source: string;
    totalDrafts: number;
    startDate: string;
    endDate: string;
    maxPick: number;
  };
  players: Record<string, { matchMethod: string; probabilities: number[] }>;
};

type RosterEntry = { pick: Pick; player: Player };
type RosterSlot = { id: string; label: string; group: "starter" | "bench" | "reserve"; entry?: RosterEntry };

const STORAGE_KEY = "fantasy-draft-room-v1";
const DEFAULT_TEAMS = [
  "Fourth & Long",
  "Gridiron Guild",
  "Sunday Scaries",
  "Red Zone Club",
  "Waiver Wizards",
  "Pocket Presence",
  "Goal Line Stand",
  "Two Minute Drill",
  "The Audible",
  "Bye Week Bandits",
];
const MODELED_POSITIONS: Position[] = ["QB", "RB", "WR", "TE"];
const POSITIONS: Position[] = [...MODELED_POSITIONS, "DST", "K"];
const REPLACEMENT_RANK: Record<Position, number> = { QB: 14, RB: 36, WR: 46, TE: 16, DST: 12, K: 12 };
const POSITION_MAXIMUMS: Record<Position, number> = { QB: 4, RB: 8, WR: 8, TE: 4, DST: 3, K: 3 };
const ROSTER_SIZE = 16;
const STARTER_COUNT = 9;
const BENCH_COUNT = 7;

function getPickSlot(index: number, teamCount: number) {
  const roundIndex = Math.floor(index / teamCount);
  const pickIndex = index % teamCount;
  const teamIndex = roundIndex % 2 === 0 ? pickIndex : teamCount - 1 - pickIndex;
  return {
    teamIndex,
    round: roundIndex + 1,
    pickInRound: pickIndex + 1,
    overall: index + 1,
  };
}

function initials(name: string) {
  return name
    .split(/\s+/)
    .filter(Boolean)
    .slice(0, 2)
    .map((part) => part[0]?.toUpperCase())
    .join("");
}

function formatPoints(value: number) {
  return value.toFixed(1);
}

function formatProbability(value: number) {
  if (value > 0 && value < 0.01) return "<1%";
  return `${Math.round(value * 100)}%`;
}

function estimateVolatility(player: Player) {
  return Math.max(0, (player.projectedP90 - player.projectedP10) / 2.5631);
}

function median(values: number[]) {
  if (!values.length) return 0;
  const ordered = [...values].sort((a, b) => a - b);
  const midpoint = Math.floor(ordered.length / 2);
  return ordered.length % 2 ? ordered[midpoint] : (ordered[midpoint - 1] + ordered[midpoint]) / 2;
}

function formatSigned(value: number) {
  return `${value >= 0 ? "+" : ""}${formatPoints(value)}`;
}

function hasModelProjection(player: Player) {
  return MODELED_POSITIONS.includes(player.position);
}

function statNumber(value: number) {
  return Number.isInteger(value) ? value.toLocaleString() : value.toFixed(1);
}

function historyColumns(position: Position): StatColumn[] {
  if (position === "QB") {
    return [
      { label: "Cmp/Att", value: (line) => `${statNumber(line.completions)}/${statNumber(line.attempts)}` },
      { label: "Pass Yds", value: (line) => statNumber(line.passingYards) },
      { label: "Pass TD", value: (line) => statNumber(line.passingTds) },
      { label: "INT", value: (line) => statNumber(line.interceptions) },
      { label: "Rush Yds", value: (line) => statNumber(line.rushingYards) },
      { label: "Rush TD", value: (line) => statNumber(line.rushingTds) },
    ];
  }

  if (position === "RB") {
    return [
      { label: "Rush", value: (line) => statNumber(line.carries) },
      { label: "Rush Yds", value: (line) => statNumber(line.rushingYards) },
      { label: "Rush TD", value: (line) => statNumber(line.rushingTds) },
      { label: "Rec", value: (line) => statNumber(line.receptions) },
      { label: "Rec Yds", value: (line) => statNumber(line.receivingYards) },
      { label: "Rec TD", value: (line) => statNumber(line.receivingTds) },
    ];
  }

  return [
    { label: "Targets", value: (line) => statNumber(line.targets) },
    { label: "Rec", value: (line) => statNumber(line.receptions) },
    { label: "Rec Yds", value: (line) => statNumber(line.receivingYards) },
    { label: "Rec TD", value: (line) => statNumber(line.receivingTds) },
    { label: "Rush Yds", value: (line) => statNumber(line.rushingYards) },
    { label: "Rush TD", value: (line) => statNumber(line.rushingTds) },
  ];
}

function assignRoster(picks: Pick[], playerById: Map<string, Player>) {
  const remaining: RosterEntry[] = picks
    .map((pick) => ({ pick, player: playerById.get(pick.playerId) }))
    .filter((entry): entry is RosterEntry => Boolean(entry.player));
  const slots: RosterSlot[] = [];

  function fill(label: string, count: number, eligible: (player: Player) => boolean) {
    const candidates = remaining
      .filter((entry) => eligible(entry.player))
      .sort((a, b) => b.player.projectedPoints - a.player.projectedPoints);
    for (let index = 0; index < count; index += 1) {
      const entry = candidates[index];
      slots.push({ id: `${label}-${index + 1}`, label, group: "starter", entry });
      if (entry) remaining.splice(remaining.findIndex((candidate) => candidate.pick.overall === entry.pick.overall), 1);
    }
  }

  fill("QB", 1, (player) => player.position === "QB");
  fill("RB", 2, (player) => player.position === "RB");
  fill("WR/TE", 3, (player) => player.position === "WR" || player.position === "TE");
  fill("FLEX", 1, (player) => player.position === "RB" || player.position === "WR" || player.position === "TE");
  fill("D/ST", 1, (player) => player.position === "DST");
  fill("K", 1, (player) => player.position === "K");

  remaining.sort((a, b) => a.pick.overall - b.pick.overall);
  for (let index = 0; index < BENCH_COUNT; index += 1) {
    slots.push({ id: `BE-${index + 1}`, label: "BE", group: "bench", entry: remaining[index] });
  }
  slots.push({ id: "IR-1", label: "IR", group: "reserve" });

  return { slots, overflow: remaining.slice(BENCH_COUNT) };
}

function draftRosterIssue(player: Player, candidatePick: Pick, picks: Pick[], playerById: Map<string, Player>) {
  const teamPicks = picks.filter((pick) => pick.teamIndex === candidatePick.teamIndex);
  const positionCount = teamPicks.filter((pick) => playerById.get(pick.playerId)?.position === player.position).length;
  if (positionCount >= POSITION_MAXIMUMS[player.position]) {
    return `${player.position === "DST" ? "D/ST" : player.position} maximum reached (${POSITION_MAXIMUMS[player.position]})`;
  }
  const simulated = assignRoster([...teamPicks, candidatePick], playerById);
  return simulated.overflow.length ? "No eligible starter or bench slot remains" : null;
}

export default function DraftRoom() {
  const [players, setPlayers] = useState<Player[]>([]);
  const [summaries, setSummaries] = useState<Record<string, PlayerSummary>>({});
  const [availabilityData, setAvailabilityData] = useState<AvailabilityData | null>(null);
  const [loadError, setLoadError] = useState("");
  const [ready, setReady] = useState(false);
  const [phase, setPhase] = useState<"setup" | "draft">("setup");
  const [teams, setTeams] = useState(DEFAULT_TEAMS);
  const [rounds, setRounds] = useState(ROSTER_SIZE);
  const [picks, setPicks] = useState<Pick[]>([]);
  const [rosterTeam, setRosterTeam] = useState(0);
  const [focusTeam, setFocusTeam] = useState(0);
  const [selectedId, setSelectedId] = useState<string | null>(null);
  const [position, setPosition] = useState<"ALL" | Position>("ALL");
  const [search, setSearch] = useState("");
  const [sort, setSort] = useState<"projection" | "value" | "upside" | "excess-risk">("value");
  const [setupError, setSetupError] = useState("");
  const [summaryCard, setSummaryCard] = useState<SummaryCard | null>(null);
  const [detailPlayerId, setDetailPlayerId] = useState<string | null>(null);
  const [detailTab, setDetailTab] = useState<"seasons" | "games" | "model">("seasons");
  const [playerHistory, setPlayerHistory] = useState<PlayerHistoryData | null>(null);
  const [historyLoading, setHistoryLoading] = useState(false);
  const [historyError, setHistoryError] = useState("");
  const [modelInterpretability, setModelInterpretability] = useState<ModelInterpretabilityData | null>(null);
  const [interpretabilityLoading, setInterpretabilityLoading] = useState(false);
  const [interpretabilityError, setInterpretabilityError] = useState("");
  const detailCloseRef = useRef<HTMLButtonElement>(null);

  useEffect(() => {
    Promise.all([
      fetch("/projections.json").then((response) => {
        if (!response.ok) throw new Error("Projection data could not be loaded.");
        return response.json() as Promise<Player[]>;
      }),
      fetch("/player-summaries.json")
        .then((response) => response.ok ? response.json() as Promise<Record<string, PlayerSummary>> : {})
        .catch(() => ({} as Record<string, PlayerSummary>)),
      fetch("/availability.json").then((response) => {
        if (!response.ok) throw new Error("Availability data could not be loaded.");
        return response.json() as Promise<AvailabilityData>;
      }),
      Promise.resolve(localStorage.getItem(STORAGE_KEY)),
    ])
      .then(([data, summaryData, availability, saved]) => {
        setPlayers(data);
        setSummaries(summaryData);
        setAvailabilityData(availability);
        if (saved) {
          const parsed = JSON.parse(saved) as SavedDraft;
          if (Array.isArray(parsed.teams) && parsed.teams.length >= 2) {
            setPhase(parsed.phase);
            setTeams(parsed.teams);
            setRounds(ROSTER_SIZE);
            setPicks(parsed.picks ?? []);
            setRosterTeam(parsed.rosterTeam ?? 0);
            setFocusTeam(Math.min(Math.max(parsed.focusTeam ?? 0, 0), parsed.teams.length - 1));
          }
        }
      })
      .catch((error: unknown) => setLoadError(error instanceof Error ? error.message : "Something went wrong."))
      .finally(() => setReady(true));
  }, []);

  useEffect(() => {
    if (!ready) return;
    const state: SavedDraft = { phase, teams, rounds, picks, rosterTeam, focusTeam };
    localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
  }, [phase, teams, rounds, picks, rosterTeam, focusTeam, ready]);

  useEffect(() => {
    if (!detailPlayerId) return;
    const previousOverflow = document.body.style.overflow;
    document.body.style.overflow = "hidden";
    requestAnimationFrame(() => detailCloseRef.current?.focus());
    return () => {
      document.body.style.overflow = previousOverflow;
    };
  }, [detailPlayerId]);

  const draftedIds = useMemo(() => new Set(picks.map((pick) => pick.playerId)), [picks]);
  const playerById = useMemo(() => new Map(players.map((player) => [player.id, player])), [players]);
  const replacementPoints = useMemo(() => {
    return Object.fromEntries(
      POSITIONS.map((pos) => {
        const pool = players
          .filter((player) => player.position === pos)
          .sort((a, b) => b.projectedPoints - a.projectedPoints);
        const rank = Math.min(REPLACEMENT_RANK[pos] - 1, pool.length - 1);
        return [pos, pool[rank]?.projectedPoints ?? 0];
      }),
    ) as Record<Position, number>;
  }, [players]);

  const expectedVolatilityById = useMemo(() => {
    const expected = new Map<string, number>();
    POSITIONS.forEach((pos) => {
      const pool = players.filter((player) => player.position === pos);
      pool.forEach((player) => {
        const peers = pool
          .filter((other) => other.id !== player.id)
          .sort((a, b) => Math.abs(a.projectedPoints - player.projectedPoints) - Math.abs(b.projectedPoints - player.projectedPoints))
          .slice(0, 30);
        expected.set(player.id, median(peers.map(estimateVolatility)));
      });
    });
    return expected;
  }, [players]);

  const available = useMemo(() => {
    const needle = search.trim().toLowerCase();
    return players
      .filter((player) => !draftedIds.has(player.id))
      .filter((player) => position === "ALL" || player.position === position)
      .filter((player) => !needle || `${player.name} ${player.nflTeam}`.toLowerCase().includes(needle))
      .sort((a, b) => {
        if (position === "ALL" && hasModelProjection(a) !== hasModelProjection(b)) {
          return hasModelProjection(a) ? -1 : 1;
        }
        if (sort === "upside") {
          return (b.projectedP90 - replacementPoints[b.position]) - (a.projectedP90 - replacementPoints[a.position]);
        }
        if (sort === "excess-risk") {
          const aExcess = estimateVolatility(a) - (expectedVolatilityById.get(a.id) ?? estimateVolatility(a));
          const bExcess = estimateVolatility(b) - (expectedVolatilityById.get(b.id) ?? estimateVolatility(b));
          return bExcess - aExcess;
        }
        if (sort === "value") {
          const aValue = a.projectedPoints - replacementPoints[a.position];
          const bValue = b.projectedPoints - replacementPoints[b.position];
          return bValue - aValue;
        }
        return b.projectedPoints - a.projectedPoints;
      });
  }, [players, draftedIds, position, search, sort, replacementPoints, expectedVolatilityById]);

  const currentSlot = getPickSlot(picks.length, teams.length);
  const isComplete = picks.length >= teams.length * rounds;
  const currentTeam = teams[currentSlot.teamIndex] ?? "";
  const nextFocusSlots = useMemo(() => {
    const maximum = teams.length * rounds;
    const slots: ReturnType<typeof getPickSlot>[] = [];
    for (let pickIndex = picks.length; pickIndex < maximum; pickIndex += 1) {
      const slot = getPickSlot(pickIndex, teams.length);
      if (slot.teamIndex !== focusTeam) continue;
      slots.push(slot);
      if (slots.length === 2) break;
    }
    return slots;
  }, [focusTeam, picks.length, rounds, teams.length]);
  const availabilityAtFocusPick = useCallback((playerId: string, focusPickIndex: number) => {
    const slot = nextFocusSlots[focusPickIndex];
    if (!slot) return null;
    return availabilityData?.players[playerId]?.probabilities[slot.overall - 1] ?? null;
  }, [availabilityData, nextFocusSlots]);
  const selectedPlayer = selectedId ? playerById.get(selectedId) : undefined;
  const selectedAvailabilitySummary = selectedPlayer ? nextFocusSlots.map((slot, focusPickIndex) => {
    const probability = availabilityAtFocusPick(selectedPlayer.id, focusPickIndex);
    return probability === null ? `No estimate at pick #${slot.overall}` : `${formatProbability(probability)} at pick #${slot.overall}`;
  }).join(" · ") : "";
  const selectedRawVolatility = selectedPlayer ? estimateVolatility(selectedPlayer) : 0;
  const selectedExcessVolatility = selectedPlayer
    ? selectedRawVolatility - (expectedVolatilityById.get(selectedPlayer.id) ?? selectedRawVolatility)
    : 0;
  const selectedCeilingValue = selectedPlayer ? selectedPlayer.projectedP90 - replacementPoints[selectedPlayer.position] : 0;
  const candidatePick = { playerId: selectedId ?? "", ...currentSlot };
  const selectedDraftIssue = selectedPlayer
    ? draftRosterIssue(selectedPlayer, candidatePick, picks, playerById)
    : null;
  const viewedRoster = picks.filter((pick) => pick.teamIndex === rosterTeam);
  const rosterAssignment = assignRoster(viewedRoster, playerById);
  const filledStarters = rosterAssignment.slots.filter((slot) => slot.group === "starter" && slot.entry).length;
  const filledBench = rosterAssignment.slots.filter((slot) => slot.group === "bench" && slot.entry).length;
  const recentPicks = [...picks].reverse().slice(0, 5);
  const summaryPlayer = summaryCard ? playerById.get(summaryCard.playerId) : undefined;
  const activeSummary = summaryCard ? summaries[summaryCard.playerId] : undefined;
  const detailPlayer = detailPlayerId ? playerById.get(detailPlayerId) : undefined;
  const detailHistory = detailPlayerId ? playerHistory?.players[detailPlayerId] : undefined;
  const detailColumns = detailPlayer ? historyColumns(detailPlayer.position) : [];
  const modelExplanation = detailPlayerId ? modelInterpretability?.players[detailPlayerId] : undefined;
  const largestContribution = modelExplanation
    ? Math.max(...modelExplanation.contributions.map((contribution) => Math.abs(contribution.points)), 1)
    : 1;

  function showPlayerSummary(playerId: string, element: HTMLElement) {
    if (!summaries[playerId]) return;
    const bounds = element.getBoundingClientRect();
    const cardWidth = Math.min(340, window.innerWidth - 24);
    const left = Math.max(12, Math.min(bounds.left, window.innerWidth - cardWidth - 12));
    const roomBelow = window.innerHeight - bounds.bottom;
    const placement = roomBelow >= 220 || bounds.top < 220 ? "below" : "above";
    setSummaryCard({
      playerId,
      left,
      top: placement === "below" ? bounds.bottom + 8 : bounds.top - 8,
      placement,
    });
  }

  function openPlayerDetails(playerId: string) {
    setSummaryCard(null);
    setDetailTab("seasons");
    setDetailPlayerId(playerId);
    loadPlayerHistory();
  }

  function closePlayerDetails() {
    const playerId = detailPlayerId;
    setDetailPlayerId(null);
    requestAnimationFrame(() => document.getElementById(`player-detail-trigger-${playerId}`)?.focus());
  }

  function loadPlayerHistory() {
    if (playerHistory || historyLoading) return;
    setHistoryError("");
    setHistoryLoading(true);
    fetch("/player-history.json")
      .then((response) => {
        if (!response.ok) throw new Error("Player history could not be loaded.");
        return response.json() as Promise<PlayerHistoryData>;
      })
      .then(setPlayerHistory)
      .catch((error: unknown) => setHistoryError(error instanceof Error ? error.message : "Player history could not be loaded."))
      .finally(() => setHistoryLoading(false));
  }

  function showModelInterpretability() {
    setDetailTab("model");
    if (modelInterpretability || interpretabilityLoading) return;
    setInterpretabilityError("");
    setInterpretabilityLoading(true);
    fetch("/player-interpretability.json")
      .then((response) => {
        if (!response.ok) throw new Error("Model explanation could not be loaded.");
        return response.json() as Promise<ModelInterpretabilityData>;
      })
      .then(setModelInterpretability)
      .catch((error: unknown) => setInterpretabilityError(error instanceof Error ? error.message : "Model explanation could not be loaded."))
      .finally(() => setInterpretabilityLoading(false));
  }

  function updateTeam(index: number, value: string) {
    setTeams((current) => current.map((team, teamIndex) => (teamIndex === index ? value : team)));
  }

  function addTeam() {
    setTeams((current) => [...current, `Team ${current.length + 1}`]);
  }

  function removeTeam(index: number) {
    if (teams.length <= 2) return;
    setTeams((current) => current.filter((_, teamIndex) => teamIndex !== index));
    setFocusTeam((current) => current === index ? Math.min(index, teams.length - 2) : current > index ? current - 1 : current);
    setRosterTeam((current) => current === index ? Math.min(index, teams.length - 2) : current > index ? current - 1 : current);
  }

  function startDraft() {
    const cleaned = teams.map((team) => team.trim());
    if (cleaned.some((team) => !team)) {
      setSetupError("Every team needs a name.");
      return;
    }
    if (new Set(cleaned.map((team) => team.toLowerCase())).size !== cleaned.length) {
      setSetupError("Team names need to be unique.");
      return;
    }
    setTeams(cleaned);
    setRounds(ROSTER_SIZE);
    setPicks([]);
    setRosterTeam(focusTeam);
    setSelectedId(null);
    setSetupError("");
    setPhase("draft");
  }

  const draftPlayer = useCallback((playerId = selectedId) => {
    if (!playerId || isComplete || draftedIds.has(playerId)) return;
    const player = playerById.get(playerId);
    if (!player) return;
    const slot = getPickSlot(picks.length, teams.length);
    const candidate = { playerId, ...slot };
    if (draftRosterIssue(player, candidate, picks, playerById)) return;
    setPicks((current) => [...current, candidate]);
    setRosterTeam(slot.teamIndex);
    setSelectedId(null);
  }, [selectedId, isComplete, draftedIds, playerById, picks, teams.length]);

  function togglePlayer(playerId: string) {
    setSelectedId((current) => current === playerId ? null : playerId);
  }

  function undoPick() {
    if (!picks.length) return;
    const last = picks[picks.length - 1];
    setPicks((current) => current.slice(0, -1));
    setSelectedId(last.playerId);
    setRosterTeam(last.teamIndex);
  }

  function newDraft() {
    if (picks.length && !window.confirm("Start over? This will clear every pick in the current draft.")) return;
    setPhase("setup");
    setPicks([]);
    setSelectedId(null);
  }

  function exportDraft() {
    const rows = ["overall,round,pick,team,player,position,nfl_team,projected_points"];
    picks.forEach((pick) => {
      const player = playerById.get(pick.playerId);
      const values = [pick.overall, pick.round, pick.pickInRound, teams[pick.teamIndex], player?.name, player?.position, player?.nflTeam, player?.projectedPoints];
      rows.push(values.map((value) => `"${String(value ?? "").replaceAll('"', '""')}"`).join(","));
    });
    const href = URL.createObjectURL(new Blob([rows.join("\n")], { type: "text/csv" }));
    const link = document.createElement("a");
    link.href = href;
    link.download = "fantasy-draft-results.csv";
    link.click();
    URL.revokeObjectURL(href);
  }

  useEffect(() => {
    if (!summaryCard) return;
    const closeSummary = () => setSummaryCard(null);
    window.addEventListener("resize", closeSummary);
    window.addEventListener("scroll", closeSummary, true);
    return () => {
      window.removeEventListener("resize", closeSummary);
      window.removeEventListener("scroll", closeSummary, true);
    };
  }, [summaryCard]);

  useEffect(() => {
    function handleDraftShortcut(event: KeyboardEvent) {
      if (phase !== "draft") return;

      if (detailPlayerId) {
        if (event.key === "Escape") {
          event.preventDefault();
          const playerId = detailPlayerId;
          setDetailPlayerId(null);
          requestAnimationFrame(() => document.getElementById(`player-detail-trigger-${playerId}`)?.focus());
        }
        if (event.key === "Tab") {
          const modal = document.querySelector<HTMLElement>(".player-modal");
          const focusable = Array.from(modal?.querySelectorAll<HTMLElement>("button:not(:disabled), [href], input, select, textarea, [tabindex]:not([tabindex='-1'])") ?? []);
          const first = focusable[0];
          const last = focusable[focusable.length - 1];
          if (first && last && (!modal?.contains(document.activeElement) || (event.shiftKey && document.activeElement === first) || (!event.shiftKey && document.activeElement === last))) {
            event.preventDefault();
            (event.shiftKey ? last : first).focus();
          }
        }
        return;
      }

      if (event.key === "Escape" && summaryCard) {
        event.preventDefault();
        setSummaryCard(null);
        return;
      }

      if (event.key === "Escape" && selectedId) {
        event.preventDefault();
        setSelectedId(null);
        return;
      }

      const target = event.target as HTMLElement | null;
      const isInteractive = Boolean(target?.closest("input, select, textarea, button, a, [contenteditable='true']"));

      if (event.key === "/" && !isInteractive) {
        event.preventDefault();
        document.getElementById("player-search")?.focus();
        return;
      }

      if (isInteractive) return;

      const key = event.key.toLowerCase();
      const direction = event.key === "ArrowDown" || key === "j" ? 1 : event.key === "ArrowUp" || key === "k" ? -1 : 0;

      if (direction && available.length) {
        event.preventDefault();
        const currentIndex = available.findIndex((player) => player.id === selectedId);
        const nextIndex = currentIndex < 0
          ? direction > 0 ? 0 : available.length - 1
          : Math.max(0, Math.min(available.length - 1, currentIndex + direction));
        const nextId = available[nextIndex].id;
        setSelectedId(nextId);
        requestAnimationFrame(() => document.getElementById(`player-row-${nextId}`)?.scrollIntoView({ block: "nearest" }));
        return;
      }

      if (event.key === "Enter" && selectedId && !event.repeat) {
        event.preventDefault();
        draftPlayer(selectedId);
      }
    }

    window.addEventListener("keydown", handleDraftShortcut);
    return () => window.removeEventListener("keydown", handleDraftShortcut);
  }, [available, detailPlayerId, draftPlayer, phase, selectedId, summaryCard]);

  if (!ready) {
    return <main className="loading"><div className="loading-mark">DR</div><p>Loading the board…</p></main>;
  }

  if (loadError) {
    return <main className="loading"><div className="loading-mark">!</div><h1>Projection file unavailable</h1><p>{loadError}</p></main>;
  }

  if (phase === "setup") {
    return (
      <main className="setup-shell">
        <header className="brand-bar">
          <a className="brand" href="#setup"><span className="brand-mark">DR</span><span>Draft Room</span></a>
          <span className="season-pill">2026 projections</span>
        </header>
        <section className="setup-grid" id="setup">
          <div className="setup-copy">
            <p className="eyebrow">League setup</p>
            <h1>Your draft.<br />Every pick in view.</h1>
            <p className="lede">Set the order, choose the length, and enter a live snake draft powered by your projection model.</p>
            <div className="setup-proof">
              <div><strong>{players.length}</strong><span>projected players</span></div>
              <div><strong>6</strong><span>draft positions</span></div>
              <div><strong>Auto</strong><span>local draft saves</span></div>
            </div>
          </div>
          <div className="setup-card">
            <div className="setup-card-head">
              <div><p className="step-label">Step 01</p><h2>Set your draft order</h2></div>
              <span className="team-count">{teams.length} teams</span>
            </div>
            <p className="field-help">Teams draft top to bottom in Round 1, then the order reverses.</p>
            <div className="team-fields">
              {teams.map((team, index) => (
                <div className={`team-field ${focusTeam === index ? "focus" : ""}`} key={index}>
                  <span>{String(index + 1).padStart(2, "0")}</span>
                  <input aria-label={`Team ${index + 1} name`} value={team} onChange={(event) => updateTeam(index, event.target.value)} />
                  <button className="focus-team-button" type="button" aria-label={`${focusTeam === index ? "Focused team" : "Focus"} ${team || `team ${index + 1}`}`} aria-pressed={focusTeam === index} title={focusTeam === index ? "Focus team" : `Highlight ${team || `team ${index + 1}`}`} onClick={() => setFocusTeam(index)}>{focusTeam === index ? "★" : "☆"}</button>
                  <button className="icon-button" type="button" aria-label={`Remove ${team || `team ${index + 1}`}`} onClick={() => removeTeam(index)} disabled={teams.length <= 2}>×</button>
                </div>
              ))}
            </div>
            <button className="add-team" type="button" onClick={addTeam}>+ Add another team</button>
            <div className="round-row">
              <div className="roster-config-label"><span>League roster</span><small>9 starters · 7 bench · 1 IR</small></div>
              <strong>{ROSTER_SIZE} rounds</strong>
            </div>
            {setupError && <p className="form-error" role="alert">{setupError}</p>}
            <button className="primary-button start-button" type="button" onClick={startDraft}>Generate draft <span>→</span></button>
          </div>
        </section>
      </main>
    );
  }

  return (
    <main className="draft-shell">
      <header className="draft-header">
        <a className="brand" href="#board"><span className="brand-mark">DR</span><span>Draft Room</span></a>
        <div className="draft-progress" aria-label={`Draft progress: ${picks.length} of ${teams.length * rounds} picks`}>
          <span style={{ width: `${(picks.length / (teams.length * rounds)) * 100}%` }} />
        </div>
        <div className="header-actions">
          <span className="saved-status"><i /> Saved locally</span>
          <button type="button" onClick={exportDraft} disabled={!picks.length}>Export</button>
          <button type="button" onClick={newDraft}>New draft</button>
        </div>
      </header>

      <section className="on-clock" aria-live="polite">
        <div className="pick-number"><span>Round {isComplete ? rounds : currentSlot.round}</span><strong>{isComplete ? "Final" : `${currentSlot.round}.${String(currentSlot.pickInRound).padStart(2, "0")}`}</strong></div>
        <div className={`clock-team ${!isComplete && currentSlot.teamIndex === focusTeam ? "focus" : ""}`}>
          <div className="clock-status"><span className="live-pick-status"><i aria-hidden="true" />{isComplete ? "Draft complete" : "Now picking"}</span>{!isComplete && currentSlot.teamIndex === focusTeam && <span className="current-focus-badge">★ Focus team</span>}</div>
          <h1>{isComplete ? "Every roster is set" : currentTeam}</h1>
          {!isComplete && <div className={`focus-tracker ${currentSlot.teamIndex === focusTeam ? "picking-now" : ""}`}><span>★ Focus team</span><strong>{teams[focusTeam]}</strong><small>{currentSlot.teamIndex === focusTeam ? "Picking now" : nextFocusSlots.length ? `Next ${nextFocusSlots.map((slot) => `#${slot.overall}`).join(" and ")}` : "No picks remaining"}</small></div>}
        </div>
        <div className="up-next">
          <span>Coming up</span>
          {[1, 2, 3].map((offset) => {
            const slotIndex = picks.length + offset;
            if (slotIndex >= teams.length * rounds) return null;
            const slot = getPickSlot(slotIndex, teams.length);
            return <div className={`next-team ${slot.teamIndex === focusTeam ? "focus" : ""}`} key={offset}><i>{slot.teamIndex === focusTeam ? "★" : initials(teams[slot.teamIndex])}</i><span>{teams[slot.teamIndex]}</span><small>{slot.round}.{String(slot.pickInRound).padStart(2, "0")}</small></div>;
          })}
        </div>
      </section>

      <section className="draft-workspace" id="board">
        <div className="board-panel">
          <div className="panel-title-row">
            <div><p className="eyebrow">Live player pool</p><h2>Available board</h2></div>
            <div className="board-status">
              <span className="keyboard-hint"><kbd>/</kbd> search <kbd>↑</kbd><kbd>↓</kbd> select <kbd>Enter</kbd> draft <kbd>Esc</kbd> clear</span>
              <span className="available-count">{players.length - picks.length} available</span>
            </div>
          </div>
          <div className="focus-bar">
            <div className="focus-team-picker"><span aria-hidden="true">★</span><label htmlFor="focus-team"><small>Focus team</small><select id="focus-team" value={focusTeam} onChange={(event) => setFocusTeam(Number(event.target.value))}>{teams.map((team, index) => <option value={index} key={team}>{team}</option>)}</select></label></div>
            {nextFocusSlots.length ? <p><strong>Next {nextFocusSlots.length === 2 ? "picks" : "pick"} {nextFocusSlots.map((slot) => `#${slot.overall}`).join(" and ")}</strong><span>{nextFocusSlots.map((slot) => `Round ${slot.round}, pick ${slot.pickInRound}`).join(" · ")} · Availability estimates who reaches {teams[focusTeam]}.</span></p> : <p><strong>Draft complete</strong><span>{teams[focusTeam]} has no picks remaining.</span></p>}
            {availabilityData && <small className="model-note">{availabilityData.model.teamCount}-team {availabilityData.model.scoringFormat.toUpperCase()} model · {availabilityData.model.totalDrafts.toLocaleString()} drafts</small>}
          </div>
          <div className="board-tools">
            <div className="position-tabs" role="group" aria-label="Filter by position">
              {(["ALL", ...POSITIONS] as const).map((pos) => <button type="button" className={position === pos ? "active" : ""} onClick={() => setPosition(pos)} key={pos}>{pos === "DST" ? "D/ST" : pos}</button>)}
            </div>
            <label className="search-box"><span>⌕</span><input id="player-search" value={search} onChange={(event) => setSearch(event.target.value)} onKeyDown={(event) => {
              if (event.key !== "Enter" || !search.trim() || !available.length) return;
              event.preventDefault();
              const playerId = available[0].id;
              setSelectedId(playerId);
              event.currentTarget.blur();
              requestAnimationFrame(() => document.getElementById(`player-row-${playerId}`)?.scrollIntoView({ block: "nearest" }));
            }} placeholder="Search player or team" aria-label="Search players" aria-keyshortcuts="/ Enter" /></label>
            <select className="sort-select" aria-label="Sort players" value={sort} onChange={(event) => setSort(event.target.value as "projection" | "value" | "upside" | "excess-risk")}>
              <option value="projection">Projected points</option>
              <option value="value">Value over replacement</option>
              <option value="upside">Late-round upside</option>
              <option value="excess-risk">Excess volatility</option>
            </select>
          </div>
          <div className="player-table-wrap">
            <table className="player-table">
              <thead><tr><th>Rank</th><th>Player</th><th>Pos</th><th className="numeric">Proj.</th><th className="numeric">Value</th><th className="numeric" title="Volatility above or below similarly projected players at the same position">Risk+</th><th className="numeric availability-heading" title={nextFocusSlots.length ? `Modeled chance that the player is still available at ${teams[focusTeam]}'s next ${nextFocusSlots.length} pick${nextFocusSlots.length === 1 ? "" : "s"}` : "No focus-team picks remain"}>Next 2 picks{nextFocusSlots.length > 0 && <small>{nextFocusSlots.map((slot) => `#${slot.overall}`).join(" / ")}</small>}</th><th aria-label="Select player" /></tr></thead>
              <tbody>
                {available.slice(0, 350).map((player, index) => {
                  const value = player.projectedPoints - replacementPoints[player.position];
                  const rawVolatility = estimateVolatility(player);
                  const expectedVolatility = expectedVolatilityById.get(player.id) ?? rawVolatility;
                  const excessVolatility = rawVolatility - expectedVolatility;
                  const modeled = hasModelProjection(player);
                  const selected = selectedId === player.id;
                  const focusAvailabilities = nextFocusSlots.map((_, focusPickIndex) => availabilityAtFocusPick(player.id, focusPickIndex));
                  return (
                    <tr id={`player-row-${player.id}`} key={player.id} className={selected ? "selected" : ""} onClick={() => openPlayerDetails(player.id)} title={`View ${player.name}'s performance history`}>
                      <td className="rank">{index + 1}</td>
                      <td><div className="player-name">
                        <button
                          className="player-summary-trigger"
                          id={`player-detail-trigger-${player.id}`}
                          type="button"
                          aria-label={`View player details for ${player.name}`}
                          aria-haspopup="dialog"
                          onMouseEnter={(event) => showPlayerSummary(player.id, event.currentTarget)}
                          onMouseLeave={() => setSummaryCard(null)}
                          onFocus={(event) => showPlayerSummary(player.id, event.currentTarget)}
                          onBlur={() => setSummaryCard(null)}
                          onClick={(event) => {
                            event.stopPropagation();
                            openPlayerDetails(player.id);
                          }}
                        >
                          <strong>{player.name}</strong><i aria-hidden="true">i</i>
                        </button>
                        <span className="player-meta">{player.nflTeam} · {player.depthRole}{player.rookie ? " · Rookie" : ""}</span>
                      </div></td>
                      <td><span className={`pos-badge pos-${player.position.toLowerCase()}`}>{player.position === "DST" ? "D/ST" : player.position}{modeled ? player.positionRank : ""}</span></td>
                      <td className="numeric points">{modeled ? formatPoints(player.projectedPoints) : "—"}</td>
                      <td className={`numeric value ${value >= 0 ? "positive" : ""}`}>{modeled ? formatSigned(value) : "—"}</td>
                      <td className={`numeric risk ${excessVolatility >= 0 ? "positive" : "negative"}`} title={modeled ? `Raw σ ${formatPoints(rawVolatility)} · Expected σ ${formatPoints(expectedVolatility)} · P10 ${formatPoints(player.projectedP10)} · P90 ${formatPoints(player.projectedP90)}` : "No model projection for this position"}>{modeled ? formatSigned(excessVolatility) : "—"}</td>
                      <td className="numeric availability-probability">{nextFocusSlots.length ? <div className="availability-pair">{nextFocusSlots.map((slot, focusPickIndex) => {
                        const probability = focusAvailabilities[focusPickIndex];
                        return <span key={slot.overall} title={probability === null ? "No supported public ADP match for this player" : `${formatProbability(probability)} chance of being available at pick #${slot.overall}`}><small>#{slot.overall}</small>{probability === null ? "—" : formatProbability(probability)}</span>;
                      })}</div> : "—"}</td>
                      <td><button className="row-select" type="button" aria-label={selected ? `Unselect ${player.name}` : `Select ${player.name}`} aria-pressed={selected} onClick={(event) => { event.stopPropagation(); event.currentTarget.blur(); togglePlayer(player.id); }}>{selected ? "×" : "+"}</button></td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
            {!available.length && <div className="empty-state">No available players match those filters.</div>}
          </div>
          <div className={`draft-dock ${selectedPlayer ? "has-player" : ""}`}>
            {selectedPlayer ? <><div><span>Selected</span><strong>{selectedPlayer.name}</strong><small>{hasModelProjection(selectedPlayer) ? `${selectedPlayer.position}${selectedPlayer.positionRank} · ${selectedPlayer.nflTeam} · ${formatPoints(selectedPlayer.projectedPoints)} pts · P90 ${formatPoints(selectedPlayer.projectedP90)} · Ceiling VOR ${formatSigned(selectedCeilingValue)} · Risk+ ${formatSigned(selectedExcessVolatility)}` : `${selectedPlayer.position === "DST" ? "D/ST" : selectedPlayer.position} · ${selectedPlayer.nflTeam} · No model projection`}{selectedAvailabilitySummary ? ` · ${selectedAvailabilitySummary}` : ""}{selectedDraftIssue ? ` · ${selectedDraftIssue}` : ""}</small></div><button className="primary-button" type="button" onClick={() => draftPlayer()} disabled={isComplete || Boolean(selectedDraftIssue)}>{selectedDraftIssue ?? `Draft to ${currentTeam}`} <span>{selectedDraftIssue ? "!" : "→"}</span></button></> : <p>Select a player from the board to make the next pick.</p>}
          </div>
        </div>

        <aside className="side-rail">
          <section className="roster-card">
            <div className="rail-heading"><div><p className="eyebrow">Team view</p><h2>Roster</h2></div><select value={rosterTeam} onChange={(event) => setRosterTeam(Number(event.target.value))}>{teams.map((team, index) => <option value={index} key={team}>{team}</option>)}</select></div>
            <div className="roster-summary">
              <div><span>START</span><strong>{filledStarters}/{STARTER_COUNT}</strong></div>
              <div><span>BENCH</span><strong>{filledBench}/{BENCH_COUNT}</strong></div>
              <div><span>TOTAL</span><strong>{viewedRoster.length}/{ROSTER_SIZE}</strong></div>
            </div>
            <p className="roster-limits">Max: QB 4 · RB 8 · WR 8 · TE 4 · D/ST 3 · K 3</p>
            <div className="roster-list">
              {rosterAssignment.slots.map((slot) => {
                const player = slot.entry?.player;
                return <div className={`roster-slot ${player ? "filled" : "empty"} roster-${slot.group}`} key={slot.id}>
                  <span className={`slot-label ${player ? `pos-${player.position.toLowerCase()}` : ""}`}>{slot.label}</span>
                  <div>{player ? <><strong>{player.name}</strong><small>{player.nflTeam} · Pick {slot.entry?.pick.overall}</small></> : <><strong>Open {slot.label}</strong><small>{slot.group === "reserve" ? "Reserve slot" : slot.group === "bench" ? "Bench" : "Starter"}</small></>}</div>
                  <b>{player && hasModelProjection(player) ? formatPoints(player.projectedPoints) : "—"}</b>
                </div>;
              })}
              {rosterAssignment.overflow.map((entry) => <div className="roster-slot overflow" key={`overflow-${entry.pick.overall}`}><span className="slot-label">!</span><div><strong>{entry.player.name}</strong><small>Legacy roster overflow · Pick {entry.pick.overall}</small></div><b>—</b></div>)}
            </div>
          </section>

          <section className="recent-card">
            <div className="rail-heading"><div><p className="eyebrow">Draft log</p><h2>Recent picks</h2></div><button type="button" onClick={undoPick} disabled={!picks.length}>↶ Undo</button></div>
            <div className="recent-list">
              {recentPicks.length ? recentPicks.map((pick) => {
                const player = playerById.get(pick.playerId);
                if (!player) return null;
                return <div className="recent-pick" key={pick.overall}><span>{pick.overall}</span><div><strong>{player.name}</strong><small>{teams[pick.teamIndex]}</small></div><b className={`pos-text-${player.position.toLowerCase()}`}>{player.position === "DST" ? "D/ST" : player.position}</b></div>;
              }) : <p className="recent-empty">The first pick is waiting.</p>}
            </div>
          </section>
        </aside>
      </section>
      {summaryCard && summaryPlayer && activeSummary && <aside
        id="player-summary-card"
        className={`player-summary-card ${summaryCard.placement}`}
        style={{ left: summaryCard.left, top: summaryCard.top }}
        role="tooltip"
      >
        <p>AI player brief</p>
        <h3>{summaryPlayer.name}<span className={`pos-badge pos-${summaryPlayer.position.toLowerCase()}`}>{summaryPlayer.position === "DST" ? "D/ST" : summaryPlayer.position}</span></h3>
        <ul>{activeSummary.bullets.map((bullet) => <li key={bullet}>{bullet}</li>)}</ul>
        <small>{activeSummary.sourceLabel} · Updated Aug. 5</small>
      </aside>}
      {detailPlayer && <div className="player-modal-backdrop" role="presentation" onMouseDown={(event) => {
        if (event.target === event.currentTarget) closePlayerDetails();
      }}>
        <section className="player-modal" role="dialog" aria-modal="true" aria-labelledby="player-modal-title">
          <header className="player-modal-header">
            <div>
              <p>Player performance</p>
              <h2 id="player-modal-title">{detailPlayer.name}<span className={`pos-badge pos-${detailPlayer.position.toLowerCase()}`}>{detailPlayer.position === "DST" ? "D/ST" : detailPlayer.position}</span></h2>
              <small>{detailPlayer.nflTeam} · {detailPlayer.depthRole}{hasModelProjection(detailPlayer) ? ` · 2026 projection ${formatPoints(detailPlayer.projectedPoints)} points` : ""}</small>
            </div>
            <button ref={detailCloseRef} className="player-modal-close" type="button" aria-label={`Close ${detailPlayer.name} details`} onClick={closePlayerDetails}>×</button>
          </header>

          <div className="player-modal-tabs" role="tablist" aria-label={`${detailPlayer.name} performance views`}>
            <button id="season-history-tab" type="button" role="tab" aria-selected={detailTab === "seasons"} aria-controls="season-history-panel" className={detailTab === "seasons" ? "active" : ""} onClick={() => setDetailTab("seasons")}>Season history</button>
            <button id="game-log-tab" type="button" role="tab" aria-selected={detailTab === "games"} aria-controls="game-log-panel" className={detailTab === "games" ? "active" : ""} onClick={() => setDetailTab("games")}>2025 game log</button>
            <button id="model-interpretability-tab" type="button" role="tab" aria-selected={detailTab === "model"} aria-controls="model-interpretability-panel" className={detailTab === "model" ? "active" : ""} onClick={showModelInterpretability}>Model drivers</button>
          </div>

          <div className="player-modal-body">
            {detailTab !== "model" && historyLoading && <div className="history-message" role="status"><span className="history-loader" />Loading performance history…</div>}
            {detailTab !== "model" && historyError && !historyLoading && <div className="history-message history-error" role="alert"><strong>History unavailable</strong><span>{historyError}</span><button type="button" onClick={loadPlayerHistory}>Try again</button></div>}

            {!historyLoading && !historyError && detailTab === "seasons" && <div id="season-history-panel" role="tabpanel" aria-labelledby="season-history-tab">
              {detailHistory?.seasons.length ? <>
                <div className="history-table-heading"><div><strong>Regular-season totals</strong><span>Most recent three NFL seasons</span></div><small>PPR scoring</small></div>
                <div className="history-table-wrap">
                  <table className="history-table">
                    <thead><tr><th>Season</th><th>Team</th><th className="numeric">GP</th>{detailColumns.map((column) => <th className="numeric" key={column.label}>{column.label}</th>)}<th className="numeric">FPTS</th><th className="numeric">PPG</th></tr></thead>
                    <tbody>{detailHistory.seasons.map((season) => <tr key={season.season}><td><strong>{season.season}</strong></td><td>{season.team}</td><td className="numeric">{season.games}</td>{detailColumns.map((column) => <td className="numeric" key={column.label}>{column.value(season)}</td>)}<td className="numeric history-points">{formatPoints(season.fantasyPoints)}</td><td className="numeric history-ppg">{formatPoints(season.pointsPerGame)}</td></tr>)}</tbody>
                  </table>
                </div>
              </> : <div className="history-message"><strong>No season history found</strong><span>Historical PPR data is currently available for QB, RB, WR, and TE players with nflverse records from 2023–2025.</span></div>}
            </div>}

            {!historyLoading && !historyError && detailTab === "games" && <div id="game-log-panel" role="tabpanel" aria-labelledby="game-log-tab">
              {detailHistory?.gameLog.length ? <>
                <div className="history-table-heading"><div><strong>2025 weekly results</strong><span>{detailHistory.gameLog.length} regular-season games</span></div><small>PPR scoring</small></div>
                <div className="history-table-wrap game-log-wrap">
                  <table className="history-table game-log-table">
                    <thead><tr><th>Week</th><th>Team</th><th>Opp.</th>{detailColumns.map((column) => <th className="numeric" key={column.label}>{column.label}</th>)}<th className="numeric">FPTS</th></tr></thead>
                    <tbody>{detailHistory.gameLog.map((game) => <tr key={`${game.week}-${game.team}`} className={game.fantasyPoints >= 20 ? "boom-game" : game.fantasyPoints < 8 ? "quiet-game" : ""}><td><strong>{game.week}</strong></td><td>{game.team}</td><td>{game.opponent}</td>{detailColumns.map((column) => <td className="numeric" key={column.label}>{column.value(game)}</td>)}<td className="numeric history-points">{formatPoints(game.fantasyPoints)}</td></tr>)}</tbody>
                  </table>
                </div>
              </> : <div className="history-message"><strong>No 2025 game log found</strong><span>The player may be a rookie, may not have recorded a regular-season stat line, or may be outside the modeled positions.</span></div>}
            </div>}

            {detailTab === "model" && interpretabilityLoading && <div className="history-message" role="status"><span className="history-loader" />Calculating model contributions…</div>}
            {detailTab === "model" && interpretabilityError && !interpretabilityLoading && <div className="history-message history-error" role="alert"><strong>Explanation unavailable</strong><span>{interpretabilityError}</span><button type="button" onClick={showModelInterpretability}>Try again</button></div>}
            {detailTab === "model" && !interpretabilityLoading && !interpretabilityError && <div id="model-interpretability-panel" role="tabpanel" aria-labelledby="model-interpretability-tab">
              {modelExplanation ? <>
                <div className="interpretability-heading">
                  <div><strong>What builds the projection</strong><span>Input value × fitted {detailPlayer.position} coefficient</span></div>
                  <div className="projection-reconciliation"><small>Bars sum to</small><strong>{formatPoints(modelExplanation.projectedPoints)}</strong><span>projected points</span></div>
                </div>
                <div className="contribution-legend"><span><i className="positive" /> Adds points</span><span><i className="negative" /> Subtracts points</span><small>Center line = 0</small></div>
                <div className="contribution-chart" aria-label={`${detailPlayer.name} model point contributions`}>
                  {modelExplanation.contributions.map((contribution) => {
                    const width = Math.abs(contribution.points) / largestContribution * 50;
                    const left = contribution.points < 0 ? 50 - width : 50;
                    return <div className="contribution-row" key={contribution.key} aria-label={`${contribution.label}: ${formatSigned(contribution.points)} points from ${contribution.input}`}>
                      <div className="contribution-label"><strong>{contribution.label}</strong><span>{contribution.input}</span></div>
                      <div className="contribution-track" aria-hidden="true"><i className={contribution.points >= 0 ? "positive" : "negative"} style={{ left: `${left}%`, width: `${width}%` }} /></div>
                      <b className={contribution.points >= 0 ? "positive" : "negative"}>{formatSigned(contribution.points)}</b>
                    </div>;
                  })}
                </div>
                <p className="interpretability-note"><strong>How to read this:</strong> these are additive linear-model contributions, not causal effects. Correlated inputs can create large positive and negative bars that offset one another.</p>
              </> : <div className="history-message"><strong>No model explanation found</strong><span>Interpretability is available for the QB, RB, WR, and TE players produced by the linear projection model.</span></div>}
            </div>}
          </div>
          <footer className="player-modal-footer">{detailTab === "model" ? <><span>Model: position-specific linear regression</span><span>Contributions reconcile to the 2026 mean projection.</span></> : <><span>Source: nflverse weekly player stats</span><span>Fantasy points use the projection model’s PPR scoring rules.</span></>}</footer>
        </section>
      </div>}
    </main>
  );
}
