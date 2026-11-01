# Exact packet storage

The staging whitespace gate rejected unified-diff context lines containing a
single space in the captured `diff.patch`. The raw packet was not normalized.
`final-packet-capsule.json` retains every original file byte and SHA256 in a
gzip/base64 JSON file map. It was reconstructed into a fresh temporary directory
and the installed packet inspector passed; `final-packet-inspection.json` holds
the terminal result. Decode payload with base64, decompress gzip, parse JSON,
then base64-decode each file at its recorded relative path. Inspect the recorded
`packet_relative_path`. The prior local directory in merge-guard.json is its
capture-time location, now represented by this exact recoverable capsule.
No runtime, test, benchmark input or hosted observation changed.
