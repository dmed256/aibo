import { Fragment } from "react";

const HELP = [
  ["⌘ K / Ctrl+K", "Search pages, projects, and conversations"],
  ["C-M-h", "Home"],
  ["M-0", "New m# chat"],
  ["M-1 … M-9", "Open a bot from the bot bar"],
  ["C-c b 0 … 9", "Open a manager from the orange bar"],
  ["C-c p s", "Search chats"],
  ["C-c n 0 … z", "Open a notification"],
  ["C-c p n", "Focus notifications"],
  ["C-c p l", "Locations"],
  ["C-c p p", "Projects"],
  ["C-c p h", "Help"],
  ["C-c p c", "Customization"],
  ["M-/", "Swap bot / manager recipient and focus input"],
  ["C-o", "Cycle content / input focus"],
  ["RET", "Newline in input; activate a link or selection"],
  ["M-RET", "Send draft"],
  ["C-g", "Clear input"],
  ["C-c C-i", "Attach clipboard image"],
];

export function Help() {
  return (
    <>
      <h1>Help</h1>
      <div className="help-grid">
        {HELP.map(([key, description]) => (
          <Fragment key={key}>
            <kbd>{key}</kbd>
            <span>{description}</span>
          </Fragment>
        ))}
      </div>
    </>
  );
}
