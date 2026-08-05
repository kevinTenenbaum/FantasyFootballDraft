import type { Metadata } from "next";
import DraftRoom from "./DraftRoom";

export const metadata: Metadata = {
  title: "Draft Room | Fantasy Football",
  description: "Run a live snake draft with custom teams and model-built player projections.",
};

export default function Home() {
  return <DraftRoom />;
}
