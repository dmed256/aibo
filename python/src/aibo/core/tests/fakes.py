from typing import Any


class FakeBouncer:
    def __init__(self) -> None:
        self.started: list[dict[str, object]] = []
        self.turns: list[dict[str, object]] = []
        self.goals: list[str | None] = []
        self.resumed: list[str] = []
        self.interrupted: list[tuple[str, str]] = []

    async def rate_limits(self) -> dict[str, Any]:
        return {}

    async def start_thread(self, **kwargs: object) -> dict[str, Any]:
        self.started.append(kwargs)
        return {"thread": {"id": f"thread-{len(self.started)}"}}

    async def resume_thread(self, thread_id: str) -> dict[str, Any]:
        self.resumed.append(thread_id)
        return {"thread": {"id": thread_id}}

    async def set_goal(self, thread_id: str, objective: str | None) -> dict[str, Any]:
        self.goals.append(objective)
        return {
            "goal": {"objective": objective, "status": "active"} if objective else None
        }

    async def get_goal(self, thread_id: str) -> dict[str, Any]:
        objective = self.goals[-1] if self.goals else None
        return {
            "goal": {"objective": objective, "status": "active"} if objective else None
        }

    async def start_turn(self, **kwargs: object) -> dict[str, Any]:
        self.turns.append(kwargs)
        return {"turn": {"id": "turn-1"}}

    async def interrupt(self, *, thread_id: str, turn_id: str) -> None:
        self.interrupted.append((thread_id, turn_id))
