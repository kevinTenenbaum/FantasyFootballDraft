import {
  optimizeDraftRecommendations,
  type DraftRecommendation,
  type OptimizationInput,
} from "./draftOptimization";

type OptimizationResponse =
  | { recommendations: DraftRecommendation[]; error?: never }
  | { recommendations?: never; error: string };

self.onmessage = (event: MessageEvent<OptimizationInput>) => {
  try {
    const recommendations = optimizeDraftRecommendations(event.data).slice(0, 6);
    self.postMessage({ recommendations } satisfies OptimizationResponse);
  } catch (error) {
    const message = error instanceof Error ? error.message : "Unable to calculate recommendations.";
    self.postMessage({ error: message } satisfies OptimizationResponse);
  }
};

export {};
