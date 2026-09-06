import { useEffect, useRef, useState } from "react";
import { FiCheck, FiCopy } from "react-icons/fi";

export function CopyButton({ text, label }: { text: string; label: string }) {
  const [status, setStatus] = useState("");
  const timer = useRef<ReturnType<typeof setTimeout> | undefined>(undefined);
  useEffect(() => () => clearTimeout(timer.current), []);
  const copy = async () => {
    try {
      await navigator.clipboard.writeText(text);
      setStatus("copied");
    } catch {
      setStatus("copy failed");
    }
    clearTimeout(timer.current);
    timer.current = setTimeout(() => setStatus(""), 2000);
  };
  return (
    <button
      type="button"
      className="copy-button"
      aria-label={status || label}
      title={status || label}
      onClick={() => void copy()}
    >
      {status === "copied" ? <FiCheck /> : <FiCopy />}
      <span className="copy-status" role="status">
        {status}
      </span>
    </button>
  );
}
