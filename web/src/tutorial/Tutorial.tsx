import { useState } from "react";
import { followLink } from "../routing";
import data from "./frames.json";
import "./tutorial.css";

type FrameName = keyof typeof data.frames;
const lessons: {
  title: string;
  purpose: string;
  instruction: string;
  frame: FrameName;
}[] = [
  {
    title: "Write a request",
    purpose: "Start a manager with M-0. Managers need no working directory.",
    instruction: "M-/ focuses the input. Write your request; RET adds a line.",
    frame: "input-cursor",
  },
  {
    title: "Send it",
    purpose:
      "The manager answers or assigns a worker to a project and directory.",
    instruction:
      "M-RET sends the draft. Orange badges are managers; purple badges are workers.",
    frame: "delegation",
  },
  {
    title: "Check progress",
    purpose: "The sidebar lists active work and unread results.",
    instruction:
      "C-c n 0 opens the first notification. M-1 opens the first worker; C-c b 0 opens the first manager.",
    frame: "sidebar-selected",
  },
  {
    title: "Inspect details",
    purpose: "System and tool messages are collapsed by default.",
    instruction:
      "Press RET on the hidden-messages row to expand or collapse it.",
    frame: "hidden-expanded",
  },
  {
    title: "Find a chat",
    purpose: "Search by label or title to return to older work.",
    instruction:
      "C-c p s opens search. Select a result and press RET. “Show 50 more” loads the next page.",
    frame: "search-more",
  },
  {
    title: "Work side by side",
    purpose:
      "Cowork mode uses ordinary Emacs windows, without the fixed sidebar.",
    instruction:
      "C-x 3 splits right. A file link opens the conversation on the left and the file on the right.",
    frame: "cowork-right",
  },
  {
    title: "Continue the conversation",
    purpose:
      "Check the input’s recipient before sending. M-0 starts a new manager.",
    instruction:
      "C-o switches between content and input. C-c C-i attaches a clipboard image.",
    frame: "reconnected",
  },
];

export function Terminal({ name }: { name: FrameName }) {
  const frame = data.frames[name];
  return (
    <div
      className="tutorial-terminal"
      role="img"
      aria-label={`Emacs terminal, 120 columns by 40 rows: ${name}`}
    >
      {frame.runs.map((runs, row) => (
        <div className="terminal-line" key={row}>
          {runs.map((run, index) => {
            const [color, backgroundColor] =
              data.palette[run.face as keyof typeof data.palette];
            return (
              <span key={index} style={{ color, backgroundColor }}>
                {run.text}
              </span>
            );
          })}
        </div>
      ))}
    </div>
  );
}

export default function Tutorial({ onHome }: { onHome: () => void }) {
  const [chapter, setChapter] = useState(0);
  const lesson = lessons[chapter]!;
  return (
    <div className="shell tutorial">
      <aside className="sidebar tutorial-sidebar">
        <nav className="tutorial-chapters" aria-label="Tutorial chapters">
          {lessons.map((item, index) => (
            <button
              key={item.title}
              aria-current={chapter === index ? "step" : undefined}
              onClick={() => setChapter(index)}
            >
              <span>{index + 1}</span>
              {item.title}
            </button>
          ))}
        </nav>
      </aside>
      <main className="workspace">
        <header className="workspace-header">
          <div className="toolbar">
            <nav className="breadcrumbs" aria-label="Breadcrumb">
              <ol>
                <li>
                  <a
                    className="text-link"
                    href="/"
                    onClick={(event) => followLink(event, onHome)}
                  >
                    home
                  </a>
                </li>
                <li>
                  <span className="breadcrumb-separator" aria-hidden="true">
                    /
                  </span>
                  <span aria-current="page">tutorial</span>
                </li>
              </ol>
            </nav>
          </div>
        </header>
        <section
          className="content tutorial-content"
          aria-labelledby="lesson-title"
        >
          <div className="tutorial-lesson">
            <div className="lesson-copy">
              <span className="lesson-count">
                {chapter + 1} / {lessons.length}
              </span>
              <h1 id="lesson-title">{lesson.title}</h1>
              <p>{lesson.purpose}</p>
              <p>{lesson.instruction}</p>
            </div>
            <div className="terminal-scroll">
              <Terminal name={lesson.frame} />
            </div>
          </div>
        </section>
      </main>
    </div>
  );
}
