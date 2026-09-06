import { test, expect } from "bun:test";
import { createElement as h } from "react";
import { renderToStaticMarkup as render } from "react-dom/server";
import { ModelRows } from "../src/pages/Customization";

const models = {
  manager_model: null,
  manager_model_reasoning_effort: null,
  bot_model: "saved-model",
  bot_model_reasoning_effort: "legacy-effort",
  title_model: "gpt-5.6-luna",
  title_model_reasoning_effort: "low",
};

test("model rows retain null and unavailable saved choices", () => {
  for (const catalog of [
    [],
    [{ model: "listed-model", display_name: "Listed" }],
  ]) {
    const html = render(h(ModelRows, { models, catalog, onChange() {} }));
    expect(html.match(/class="model-row"/g)).toHaveLength(3);
    expect(html.match(/aria-haspopup="menu"/g)).toHaveLength(6);
    expect(html.match(/class="codex-default"/g)).toHaveLength(2);
    expect(html.match(/>codex default</g)).toHaveLength(2);
    expect(html).not.toContain("Codex default");
    for (const value of [
      "saved-model",
      "legacy-effort",
      "gpt-5.6-luna",
      "low",
    ]) {
      expect(html).toContain(`>${value}</span>`);
    }
    expect(html).toContain('for="manager_model"');
    expect(html).toContain('id="manager_model"');
  }
});
