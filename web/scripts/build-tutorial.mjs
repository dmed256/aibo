import { writeFile } from "node:fs/promises";
import "../../elisp/tests/terminal/reference/renderer.js";
import "../../elisp/tests/terminal/reference/scenarios.js";
const names = [
  "home-empty",
  "input-cursor",
  "chat-manager",
  "delegation",
  "chat-bot",
  "sidebar-selected",
  "hidden-collapsed",
  "hidden-expanded",
  "search",
  "search-more",
  "cowork-single",
  "cowork-right",
  "input-overflow",
  "reconnected",
];
const frames = Object.fromEntries(
  names.map((name) => {
    const scenario = globalThis.AIBO_SCENARIOS.find((item) => item.id === name);
    const { runs, cursor, lines } = globalThis.AiboTerminal.render(
      scenario.state,
      120,
      40,
    );
    return [name, { runs, cursor, lines }];
  }),
);
await writeFile(
  new URL("../src/tutorial/frames.json", import.meta.url),
  JSON.stringify({ palette: globalThis.AiboTerminal.palette, frames }) + "\n",
);
