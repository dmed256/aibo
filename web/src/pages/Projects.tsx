import type { Project } from "../types";
import { followLink, projectPath } from "../routing";

export function Projects({
  projects,
  archived,
  onProject,
}: {
  projects: Project[];
  archived: Project[];
  onProject?: (project: Project) => void;
}) {
  return (
    <>
      <h1>Projects</h1>
      {projects.map((project) => (
        <section className="project" key={project.id}>
          <h2>
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
          </h2>
          <p>{project.description}</p>
        </section>
      ))}
      {archived.length > 0 && (
        <section className="archived-list">
          <h2>Archived</h2>
          {archived.map((project) => (
            <div key={project.id}>
              {onProject ? (
                <a
                  href={projectPath(project.id)}
                  className="text-link"
                  onClick={(event) =>
                    followLink(event, () => onProject(project))
                  }
                >
                  {project.name}
                </a>
              ) : (
                project.name
              )}
            </div>
          ))}
        </section>
      )}
      {!projects.length && !archived.length && (
        <div className="empty">No projects configured.</div>
      )}
    </>
  );
}
