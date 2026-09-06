import { memo, type ReactNode } from "react";
import type { RootContent } from "hast";
import { createLowlight } from "lowlight";
import bash from "highlight.js/lib/languages/bash";
import json from "highlight.js/lib/languages/json";
import diff from "highlight.js/lib/languages/diff";
import { CopyButton } from "./CopyButton";

const highlighter = createLowlight({ bash, json, diff });

function tokens(nodes: RootContent[]): ReactNode {
  return nodes.map((node, index) => {
    if (node.type === "text") return node.value;
    if (node.type !== "element") return null;
    const classes = node.properties.className;
    return (
      <span
        key={index}
        className={Array.isArray(classes) ? classes.join(" ") : undefined}
      >
        {tokens(node.children)}
      </span>
    );
  });
}

export const CodeBlock = memo(function CodeBlock({
  text,
  language,
  className,
  children,
}: {
  text: string;
  language?: string;
  className?: string;
  children?: ReactNode;
}) {
  const highlighted =
    children ??
    (language && highlighter.registered(language) && text.length < 100_000
      ? tokens(highlighter.highlight(language, text).children)
      : text);
  return (
    <div className="code-block">
      <CopyButton text={text} label="copy code" />
      <pre className={className}>
        <code className={language ? `language-${language}` : undefined}>
          {highlighted}
        </code>
      </pre>
    </div>
  );
});
