export class Store<State> {
  private listeners = new Set<() => void>();
  constructor(protected state: State) {}
  getSnapshot = (): State => this.state;
  subscribe = (listener: () => void): (() => void) => {
    this.listeners.add(listener);
    return () => {
      this.listeners.delete(listener);
    };
  };
  protected update(patch: Partial<State>): void {
    this.state = { ...this.state, ...patch };
    this.listeners.forEach((listener) => listener());
  }
}
