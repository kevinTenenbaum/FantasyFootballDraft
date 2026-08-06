import type { Metadata } from "next";
import "./globals.css";

export const metadata: Metadata = {
  title: "Draft Room | Fantasy Football",
  description: "A live fantasy football draft board with fractional recommendations and shared-scenario roster outcomes.",
  icons: { icon: "/favicon.svg", shortcut: "/favicon.svg" },
  openGraph: {
    title: "Draft Room",
    description: "Make the pick. See the complete-roster paths.",
    images: ["/og-optimization.png"],
  },
};

export default function RootLayout({ children }: Readonly<{ children: React.ReactNode }>) {
  return (
    <html lang="en">
      <body>{children}</body>
    </html>
  );
}
