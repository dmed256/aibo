import type { Location } from "../types";

export function Locations({ locations }: { locations: Location[] }) {
  return (
    <>
      <h1>Locations</h1>
      {locations.length ? (
        <table className="locations-table">
          <thead>
            <tr>
              <th>NAME</th>
              <th>WORKING DIRECTORY</th>
            </tr>
          </thead>
          <tbody>
            {locations.map((location) => (
              <tr key={location.id}>
                <td>{location.name}</td>
                <td>{location.path}</td>
              </tr>
            ))}
          </tbody>
        </table>
      ) : (
        <div className="empty">No locations configured.</div>
      )}
    </>
  );
}
