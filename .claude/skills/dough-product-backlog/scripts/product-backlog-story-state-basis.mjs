// SHA-256 content basis for a readiness assessment. It covers the story's own
// section, the seed's shared context outside other stories' sections (the
// caller supplies that story-scoped text), and a distinct plan. CLI and
// browser share this meaning: normalize line endings only, exclude every
// story-state fence, then digest. Planned work may add a plan digest; when
// the plan is the canonical document itself, digest that document once so the
// basis cannot become self-referential through the block that stores it.

import { findStoryStateBlocks } from "./product-backlog-story-state-block.mjs";

// Compact sync SHA-256 so the shared reader stays free of node:crypto while
// matching the usual UTF-8 hex digest Node's createHash produces.
function rotr(value, bits) {
  return (value >>> bits) | (value << (32 - bits));
}

function sha256Hex(text) {
  const bytes = new TextEncoder().encode(text);
  const K = new Uint32Array([
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
    0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
    0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
  ]);
  let h0 = 0x6a09e667;
  let h1 = 0xbb67ae85;
  let h2 = 0x3c6ef372;
  let h3 = 0xa54ff53a;
  let h4 = 0x510e527f;
  let h5 = 0x9b05688c;
  let h6 = 0x1f83d9ab;
  let h7 = 0x5be0cd19;

  const bitLength = bytes.length * 8;
  const withPad = bytes.length + 1 + 8;
  const blockCount = Math.ceil(withPad / 64);
  const padded = new Uint8Array(blockCount * 64);
  padded.set(bytes);
  padded[bytes.length] = 0x80;
  const view = new DataView(padded.buffer);
  view.setUint32(padded.length - 4, bitLength >>> 0);
  view.setUint32(padded.length - 8, Math.floor(bitLength / 0x100000000));

  const w = new Uint32Array(64);
  for (let block = 0; block < blockCount; block += 1) {
    const offset = block * 64;
    for (let i = 0; i < 16; i += 1) {
      w[i] = view.getUint32(offset + i * 4);
    }
    for (let i = 16; i < 64; i += 1) {
      const s0 = rotr(w[i - 15], 7) ^ rotr(w[i - 15], 18) ^ (w[i - 15] >>> 3);
      const s1 = rotr(w[i - 2], 17) ^ rotr(w[i - 2], 19) ^ (w[i - 2] >>> 10);
      w[i] = (w[i - 16] + s0 + w[i - 7] + s1) >>> 0;
    }
    let a = h0;
    let b = h1;
    let c = h2;
    let d = h3;
    let e = h4;
    let f = h5;
    let g = h6;
    let h = h7;
    for (let i = 0; i < 64; i += 1) {
      const S1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25);
      const ch = (e & f) ^ (~e & g);
      const temp1 = (h + S1 + ch + K[i] + w[i]) >>> 0;
      const S0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22);
      const maj = (a & b) ^ (a & c) ^ (b & c);
      const temp2 = (S0 + maj) >>> 0;
      h = g;
      g = f;
      f = e;
      e = (d + temp1) >>> 0;
      d = c;
      c = b;
      b = a;
      a = (temp1 + temp2) >>> 0;
    }
    h0 = (h0 + a) >>> 0;
    h1 = (h1 + b) >>> 0;
    h2 = (h2 + c) >>> 0;
    h3 = (h3 + d) >>> 0;
    h4 = (h4 + e) >>> 0;
    h5 = (h5 + f) >>> 0;
    h6 = (h6 + g) >>> 0;
    h7 = (h7 + h) >>> 0;
  }

  return [h0, h1, h2, h3, h4, h5, h6, h7]
    .map((value) => value.toString(16).padStart(8, "0"))
    .join("");
}

export function normalizeLineEndings(source) {
  return source.replace(/\r\n/g, "\n").replace(/\r/g, "\n");
}

// Bytes hashed for one Markdown document: line endings normalized, every
// story-state fence removed, nothing else rewritten.
export function digestSource(source) {
  const normalized = normalizeLineEndings(source);
  const endsWithNewline = normalized.endsWith("\n");
  const body = endsWithNewline ? normalized.slice(0, -1) : normalized;
  const lines = body.length === 0 && !endsWithNewline ? [] : body.split("\n");
  const blocks = findStoryStateBlocks(lines, { start: 0, end: lines.length });
  for (const block of [...blocks].reverse()) {
    lines.splice(block.open, block.close - block.open + 1);
  }
  const stripped = lines.join("\n");
  const forHash = endsWithNewline ? `${stripped}\n` : stripped;
  return sha256Hex(forHash);
}

// Builds the assessment basis for already-loaded canonical text and, when
// planned work uses a distinct plan file, that plan's text. Same-file plan
// content is digested once as the document.
export function computeBasis(documentSource, planSource) {
  const document = digestSource(documentSource);
  if (planSource === undefined || planSource === documentSource) {
    return { document };
  }
  return { document, plan: digestSource(planSource) };
}

export function basesEqual(left, right) {
  if (left.document !== right.document) {
    return false;
  }
  return (left.plan ?? null) === (right.plan ?? null);
}
