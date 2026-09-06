import { api } from "../api";
import type { ChatSummary } from "../types";
import { publicChat } from "./workspace";
import { Store } from "./store";

interface SearchState {
  query: string;
  results: ChatSummary[];
  more: boolean;
  selection: number;
  loading: boolean;
  error: string | null;
}

export class Search extends Store<SearchState> {
  private generation = 0;
  constructor(
    private client = api,
    private projectId?: string,
    query = "",
  ) {
    super({
      query,
      results: [],
      more: false,
      selection: -1,
      loading: true,
      error: null,
    });
  }
  change(query: string): void {
    this.generation++;
    this.update({
      query,
      results: [],
      more: false,
      selection: -1,
      loading: true,
      error: null,
    });
  }
  select(selection: number): void {
    const count = this.state.results.length + Number(this.state.more);
    this.update({ selection: Math.max(-1, Math.min(selection, count - 1)) });
  }
  async fetch(more = false): Promise<void> {
    if (more && (this.state.loading || !this.state.more)) return;
    const generation = ++this.generation;
    const previous = more ? this.state.results : [];
    this.update({ loading: true, error: null });
    try {
      const result = await this.client.chats({
        query: this.state.query,
        limit: 51,
        offset: previous.length,
        ...(this.projectId ? { projectId: this.projectId } : {}),
      });
      if (generation === this.generation) {
        this.update({
          results: [...previous, ...result.slice(0, 50).filter(publicChat)],
          more: result.length > 50,
          loading: false,
        });
      }
    } catch (error) {
      if (generation === this.generation)
        this.update({ loading: false, error: String(error) });
    }
  }
  async activate(): Promise<string | null> {
    if (this.state.more && this.state.selection === this.state.results.length) {
      await this.fetch(true);
      return null;
    }
    if (this.state.error && !this.state.results.length) {
      await this.fetch();
      return null;
    }
    return this.state.results[Math.max(0, this.state.selection)]?.id ?? null;
  }
}
