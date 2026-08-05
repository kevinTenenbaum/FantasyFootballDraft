import type { Metadata } from "next";
import "./globals.css";

export const metadata: Metadata = {
  title: "Draft Room | Fantasy Football",
  description: "A live fantasy football draft board powered by custom projections.",
  icons: { icon: "/favicon.svg", shortcut: "/favicon.svg" },
  openGraph: {
    title: "Draft Room",
    description: "Build the board. Make the pick. Track every roster.",
    images: ["/og.png"],
  },
};

export default function RootLayout({ children }: Readonly<{ children: React.ReactNode }>) {
  return (
    <html lang="en">
      <body>{children}</body>
    </html>
  );
}
