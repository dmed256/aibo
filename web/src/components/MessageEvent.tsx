import { FiChevronRight } from "react-icons/fi";
import type { EventDetails } from "../pages/messageDetails";
import { CodeBlock } from "./CodeBlock";

function Diff({ text }: { text: string }) {
  return (
    <CodeBlock text={text} className="event-diff" language="diff">
      {text.split("\n").map((line, index) => {
        const tone = line.startsWith("@@")
          ? "hunk"
          : /^(---|\+\+\+) /.test(line)
            ? ""
            : line.startsWith("+")
              ? "added"
              : line.startsWith("-")
                ? "removed"
                : "";
        return (
          <span className={`diff-line ${tone}`} key={index}>
            {line}
            {"\n"}
          </span>
        );
      })}
    </CodeBlock>
  );
}

export function MessageEvent({ details }: { details: EventDetails }) {
  return (
    <>
      {details.status && !details.mcp && (
        <div className="event-status">{details.status}</div>
      )}
      {details.body !== undefined && (
        <CodeBlock
          text={details.body}
          language={details.language}
          className="event-text"
        />
      )}
      {details.output && (
        <details className="event-output">
          <summary>
            <FiChevronRight aria-hidden="true" /> output
          </summary>
          <CodeBlock text={details.output} />
        </details>
      )}
      {details.mcp && (
        <div className="mcp-call">
          <div className="mcp-heading">
            <code className="mcp-name">{details.mcp.name}</code>
            {details.status && (
              <span className="event-status">{details.status}</span>
            )}
          </div>
          {details.mcp.arguments && (
            <section className="mcp-section">
              <div className="event-section-label">arguments</div>
              <CodeBlock {...details.mcp.arguments} />
            </section>
          )}
          {details.mcp.results.length > 0 && (
            <section className="mcp-section">
              <div className="event-section-label">result</div>
              {details.mcp.results.map((part, index) => (
                <CodeBlock key={index} {...part} />
              ))}
            </section>
          )}
          {details.mcp.error && (
            <div className="event-error">error: {details.mcp.error}</div>
          )}
        </div>
      )}
      {details.changes &&
        (details.changes.length ? (
          details.changes.map((change, index) => (
            <div className="file-change" key={`${change.path}:${index}`}>
              <div className="file-change-path">
                <span>{change.kind}</span> <code>{change.path}</code>
                {change.moved && (
                  <>
                    {" "}
                    → <code>{change.moved}</code>
                  </>
                )}
              </div>
              {change.diff !== undefined && <Diff text={change.diff} />}
            </div>
          ))
        ) : (
          <p>No file details available</p>
        ))}
    </>
  );
}
