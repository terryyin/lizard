export function createObserverStreamParser() {
  let tail = "";

  return {
    push(chunk) {
      const lines = `${tail}${chunk}`.split("\n");
      tail = lines.pop();
      const parsed = { directories: [], events: [], terminals: [] };
      for (const line of lines) {
        if (line.startsWith("CI_OBSERVER_RESULT ")) {
          parsed.terminals.push(
            JSON.parse(line.slice("CI_OBSERVER_RESULT ".length)),
          );
          continue;
        }
        if (line.startsWith("CI_OBSERVER ")) {
          parsed.directories.push(
            JSON.parse(line.slice("CI_OBSERVER ".length)).directory,
          );
          continue;
        }
        if (!line.startsWith("{")) continue;
        const record = JSON.parse(line);
        if (record.event?.type?.startsWith("CI_"))
          parsed.events.push(record.event);
      }
      return parsed;
    },
    tail() {
      return tail;
    },
  };
}
