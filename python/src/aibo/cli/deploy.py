"""Deploy the server and Codex bouncer independently or together."""

import fire

from aibo.cli.redeploy import Deployment, parse_bool


@fire.decorators.SetParseFn(parse_bool, "redeploy_bouncer")
def redeploy(redeploy_bouncer: bool = False) -> None:
    """Deploy server/web/Emacs; optionally also restart the Codex bouncer."""
    Deployment().run(bouncer=redeploy_bouncer)


def bouncer_redeploy() -> None:
    """Redeploy only the Codex bouncer, leaving the server and UI running."""
    Deployment().redeploy_bouncer()


def ensure(url: str | None = None) -> None:
    """Start a local workspace if unavailable, keeping its persistent data."""
    from aibo.cli.local import LocalServices

    deployment = Deployment()
    settings = deployment._read_json(deployment.runtime / "local-settings.json") or {}
    LocalServices(
        deployment, url or settings.get("server_url") or deployment.public_url
    ).run()


def main() -> None:
    fire.Fire(
        {"redeploy": redeploy, "bouncer-redeploy": bouncer_redeploy, "ensure": ensure}
    )


if __name__ == "__main__":
    main()
