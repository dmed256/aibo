import type { ChatSummary, Project } from "../types";
import { publicChat } from "../state/workspace";
import { followLink, projectPath } from "../routing";
import { ChatRow } from "../components/ChatRow";

export function Home({
  chats,
  projects,
  archivedProjects,
  groups,
  onOpen,
  onProject,
}: {
  chats: ChatSummary[];
  projects: Project[];
  archivedProjects: Project[];
  groups: Record<string, ChatSummary[]>;
  onOpen: (id: string) => void;
  onProject?: (project: Project) => void;
}) {
  if (
    !projects.length &&
    !archivedProjects.length &&
    !chats.some(publicChat) &&
    !Object.values(groups).some((entries) => entries.some(publicChat))
  ) {
    return <div className="empty">No conversations yet</div>;
  }
  const group = (title: string, entries: ChatSummary[], project?: Project) => (
    <section className="home-group" key={title}>
      {title && (
        <h2>
          {project && onProject ? (
            <a
              href={projectPath(project.id)}
              className="text-link"
              onClick={(event) => followLink(event, () => onProject(project))}
            >
              {title}
            </a>
          ) : (
            title
          )}
        </h2>
      )}
      {entries
        .filter(publicChat)
        .slice(0, 10)
        .map((chat) => (
          <ChatRow key={chat.id} chat={chat} onOpen={onOpen} />
        ))}
    </section>
  );
  return (
    <>
      {group("", groups[""] ?? [])}
      {[...projects]
        .sort((a, b) => b.name.localeCompare(a.name))
        .map((project) =>
          group(project.name, groups[project.id] ?? [], project),
        )}
      <div className="archived-list">
        {archivedProjects.map((project) => (
          <div key={project.id}>
            {onProject ? (
              <a
                href={projectPath(project.id)}
                className="text-link"
                onClick={(event) => followLink(event, () => onProject(project))}
              >
                {project.name}
              </a>
            ) : (
              project.name
            )}
          </div>
        ))}
      </div>
    </>
  );
}
