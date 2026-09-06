import { useEffect, useRef, useState } from "react";
import { ChevronDown } from "lucide-react";
import {
  DropdownMenu,
  DropdownMenuTrigger,
  DropdownMenuContent,
  DropdownMenuRadioGroup,
  DropdownMenuRadioItem,
} from "../components/ui/dropdown-menu";
import { api } from "../api";
import type { CodexModel, ModelSettings } from "../types";

const rows = [
  ["manager", "m"],
  ["bot", "b"],
  ["title", "Title generation"],
] as const;
const efforts = ["low", "medium", "high", "xhigh", "max"];

export function ModelRows({
  models,
  catalog,
  onChange,
}: {
  models: ModelSettings;
  catalog: CodexModel[];
  onChange: (models: ModelSettings) => void;
}) {
  return rows.map(([role, label]) => (
    <div className="model-row" key={role}>
      <h3 className="model-role">
        {role === "title" ? (
          "TITLE GENERATION"
        ) : (
          <span className={`badge ${role}`}>{label}</span>
        )}
      </h3>
      {(["model", "model_reasoning_effort"] as const).map((field) => {
        const key = `${role}_${field}` as keyof ModelSettings;
        const reasoning = field === "model_reasoning_effort";
        const options = reasoning
          ? efforts.map((value) => ({ value, label: value }))
          : catalog.map((model) => ({
              value: model.model,
              label: model.model,
            }));
        const selected = models[key];
        // Retain configured values even when the catalog is unavailable or changes.
        if (selected && !options.some(({ value }) => value === selected)) {
          options.push({ value: selected, label: selected });
        }
        return (
          <div className="model-field" key={key}>
            <label htmlFor={key}>
              {reasoning ? "Reasoning level" : "Model"}
            </label>
            <DropdownMenu modal={false}>
              <DropdownMenuTrigger asChild>
                <button
                  type="button"
                  id={key}
                  className="model-dropdown"
                  aria-label={`${label} ${reasoning ? "reasoning level" : "model"}`}
                >
                  <span className={selected ? undefined : "codex-default"}>
                    {selected ?? "codex default"}
                  </span>
                  <ChevronDown size={14} aria-hidden="true" />
                </button>
              </DropdownMenuTrigger>
              <DropdownMenuContent align="start">
                <DropdownMenuRadioGroup
                  value={selected ?? ""}
                  onValueChange={(value) =>
                    onChange({ ...models, [key]: value || null })
                  }
                >
                  <DropdownMenuRadioItem value="" className="codex-default">
                    codex default
                  </DropdownMenuRadioItem>
                  {options.map(({ value, label }) => (
                    <DropdownMenuRadioItem key={value} value={value}>
                      {label}
                    </DropdownMenuRadioItem>
                  ))}
                </DropdownMenuRadioGroup>
              </DropdownMenuContent>
            </DropdownMenu>
          </div>
        );
      })}
    </div>
  ));
}

export default function Customization() {
  const [models, setModels] = useState<ModelSettings | null>(null);
  const [catalog, setCatalog] = useState<CodexModel[]>([]);
  const [catalogStatus, setCatalogStatus] = useState("Loading Codex models…");
  const [attempt, setAttempt] = useState(0);
  const saves = useRef<Promise<void>>(Promise.resolve());
  const revision = useRef(0);
  const mounted = useRef(false);
  const [failed, setFailed] = useState(false);
  const [status, setStatus] = useState("");
  useEffect(() => {
    mounted.current = true;
    let active = true;
    void api
      .modelSettings()
      .then((models) => {
        if (active) setModels(models);
      })
      .catch((error: unknown) => {
        if (active) setStatus(String(error));
      });
    return () => {
      active = false;
      mounted.current = false;
    };
  }, []);
  useEffect(() => {
    let active = true;
    void api
      .modelCatalog()
      .then((catalog) => {
        if (!active) return;
        setCatalog(catalog);
        setCatalogStatus(catalog.length ? "" : "Codex returned no models.");
      })
      .catch(() => {
        if (active)
          setCatalogStatus(
            "Could not load Codex models. Saved choices are retained.",
          );
      });
    return () => {
      active = false;
    };
  }, [attempt]);

  const updateModels = (next: ModelSettings) => {
    setModels(next);
    setFailed(false);
    setStatus("Saving…");
    const version = ++revision.current;
    // Serialize writes: an older request must never overwrite a newer choice.
    saves.current = saves.current
      .then(() => api.saveModelSettings(next))
      .then(() => {
        if (mounted.current && version === revision.current) setStatus("");
      })
      .catch((error: unknown) => {
        if (mounted.current && version === revision.current) {
          setStatus(`Could not save: ${String(error)}`);
          setFailed(true);
        }
      });
  };

  return (
    <div className="customization">
      <section>
        <h2># MODELS</h2>
        {models ? (
          <ModelRows
            models={models}
            catalog={catalog}
            onChange={updateModels}
          />
        ) : (
          !status && <p>Loading customization…</p>
        )}
        {catalogStatus && (
          <p className="catalog-status" role="status">
            {catalogStatus}{" "}
            {catalogStatus !== "Loading Codex models…" && (
              <button
                type="button"
                onClick={() => {
                  setCatalogStatus("Loading Codex models…");
                  setAttempt((value) => value + 1);
                }}
              >
                Retry
              </button>
            )}
          </p>
        )}
      </section>
      {status && (
        <p className="catalog-status" role="status">
          {status}{" "}
          {failed && models && (
            <button type="button" onClick={() => updateModels(models)}>
              Retry save
            </button>
          )}
        </p>
      )}
    </div>
  );
}
