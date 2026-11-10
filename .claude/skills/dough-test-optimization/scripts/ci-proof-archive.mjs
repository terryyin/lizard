// The one file a small zip archive holds, such as a CI proof summary
// artifact (./ci-proof-read.mjs): read from the archive's central directory,
// since a streamed archive's local headers may leave the sizes to a trailing
// data descriptor, then stored or raw-inflated with `node:zlib` and checked
// against its recorded size and CRC-32. Anything else is a `ZipEntryProblem`
// that says why; nothing is written to disk.
import { crc32, inflateRawSync } from "node:zlib";

// The archive's one file could not be read, as its message says.
export class ZipEntryProblem extends Error {}

// The archive's only file is larger than the caller's read limit; carries
// the entry's uncompressed size from the central directory so a proof read
// can name it without inflating.
export class ZipEntryTooLarge extends ZipEntryProblem {
  /**
   * @param {string} entryName
   * @param {number} sizeBytes
   * @param {number} limitBytes
   */
  constructor(entryName, sizeBytes, limitBytes) {
    super(`${entryName} is larger than ${String(limitBytes)} bytes`);
    this.entryName = entryName;
    this.sizeBytes = sizeBytes;
    this.limitBytes = limitBytes;
  }
}

const endOfDirectory = 0x06054b50;
const directoryEntry = 0x02014b50;
const localEntry = 0x04034b50;
const endLength = 22;
const longestComment = 0xffff;

function endOfDirectoryAt(archive) {
  const earliest = Math.max(0, archive.length - endLength - longestComment);
  for (let at = archive.length - endLength; at >= earliest; at -= 1)
    if (archive.readUInt32LE(at) === endOfDirectory) return at;
  throw new ZipEntryProblem("the archive has no zip central directory");
}

function within(archive, at, length) {
  if (at < 0 || at + length > archive.length)
    throw new ZipEntryProblem("the archive is truncated");
}

/**
 * The text of the archive's only file, named NAME, at most LIMITBYTES once
 * extracted.
 * @param {Buffer} archive
 * @param {string} name
 * @param {number} limitBytes
 * @returns {string}
 */
export function onlyZipEntry(archive, name, limitBytes) {
  if (archive.length < endLength)
    throw new ZipEntryProblem("the archive is not a zip archive");
  const end = endOfDirectoryAt(archive);
  if (archive.readUInt16LE(end + 10) !== 1)
    throw new ZipEntryProblem("the archive does not hold exactly one file");
  const entry = archive.readUInt32LE(end + 16);
  within(archive, entry, 46);
  if (archive.readUInt32LE(entry) !== directoryEntry)
    throw new ZipEntryProblem("the archive's central directory is unreadable");
  const flags = archive.readUInt16LE(entry + 8);
  const method = archive.readUInt16LE(entry + 10);
  const checksum = archive.readUInt32LE(entry + 16);
  const packed = archive.readUInt32LE(entry + 20);
  const size = archive.readUInt32LE(entry + 24);
  const nameLength = archive.readUInt16LE(entry + 28);
  const local = archive.readUInt32LE(entry + 42);
  within(archive, entry + 46, nameLength);
  const named = archive.toString("utf8", entry + 46, entry + 46 + nameLength);
  if (named !== name)
    throw new ZipEntryProblem(`the archive holds ${named}, not ${name}`);
  if ((flags & 0x1) !== 0)
    throw new ZipEntryProblem("the archive's file is encrypted");
  if (size > limitBytes) throw new ZipEntryTooLarge(name, size, limitBytes);
  within(archive, local, 30);
  if (archive.readUInt32LE(local) !== localEntry)
    throw new ZipEntryProblem("the archive's file header is unreadable");
  const data =
    local +
    30 +
    archive.readUInt16LE(local + 26) +
    archive.readUInt16LE(local + 28);
  within(archive, data, packed);
  const stored = archive.subarray(data, data + packed);
  let bytes;
  if (method === 0) bytes = stored;
  else if (method === 8) {
    try {
      bytes = inflateRawSync(stored, { maxOutputLength: limitBytes });
    } catch {
      throw new ZipEntryProblem(`${name} could not be inflated`);
    }
  } else throw new ZipEntryProblem(`${name} uses zip method ${String(method)}`);
  if (bytes.length !== size || crc32(bytes) !== checksum)
    throw new ZipEntryProblem(`${name} does not match its recorded checksum`);
  return bytes.toString("utf8");
}
